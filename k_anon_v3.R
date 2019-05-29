library(tidyverse) #데이터 가공
library(smbinning) #준식별자 일반화
library(naniar) #missing 변수체크
library(readxl) #엑셀파일 읽기
library(clipr) #클립보드에 복사
library(furrr) #병렬처리 프로그래밍 
library(tictoc) #소요시간 계산 
library(rsample) #data split(train/test)
library(caret) #머신러닝
library(doParallel) #caret 병렬처리 
library(corrr) #상관계수 
# original data
crd = read_xlsx('data/credit_data.xlsx') %>% 
  rename_all(tolower)
names(crd)
write_rds(crd,'data/crd.rds')

# 변수별 최대,최소 확인 
summary(crd)

# smbinning에 적합하도록 변수변환 
# status는 good/bad를 0/1로 변환
# character변수는 factor로 변환 
# expenses,debt,price 자체로는 변별력이 없으므로 비율변수 생성  
crd1 = crd %>% 
  mutate(status = ifelse(status=='bad',1L,0L)) %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(exp_ovr_inc = income/expenses,
         prc_ovr_asst = ifelse(assets==0,-1,price/assets),
         prc_ovr_inc = price/income,
         debt_ovr_asst = ifelse(assets==0,-1,debt/assets)) %>% 
  data.frame()

# 모든 변수 변별력 요약 
siv = smbinning.sumiv(crd1,'status')
siv %>% write_clip()
smbinning.sumiv.plot(siv)
# expenses,debt,price 변수는 자체로는 변별력 없으므로 제외하고
# 다른 변수와 결합하여 비율변수 생성함 
# 13개 준식별자 중 3개 제외, 비율변수 4개추가 => 총14개 준식별자 

# 원본과의 비교를 위해 train, test로 나눈다 
set.seed(7) # 재수행시 동일하게 split되도록 
spl = initial_split(crd1, prop = 0.7, strata = 'status')
tr = training(spl) # train data
te = testing(spl) # test data

# smbinning을 이용하여 준식별자 일반화(구간화)
vrs = siv$Char[!is.na(siv$IV)] %>% as.character() #siv에서 binning된 변수명 
crd2 = list()
n = 1
pct_repo = list() #최소비중 충족못하는 변수 보고서 
smbl = list() # smbinning결과 보관소 
nr = 1 # pct_repo용 일련번호 
for(i in vrs){
  cls = class(tr[[i]])
  if(cls == 'factor'){
    smb = smbinning.factor(tr,'status',i)
    res = smbinning.factor.gen(tr,smb,'newvar')
  } else {
    smb = smbinning(tr,'status',i,p=0.1) # 최소 구간당 10%이상 
    res = smbinning.gen(tr,smb,'newvar')
  }
  min_pct = filter(smb$ivtable, CntRec >0) %>% pull(PctRec) %>% min()
  if(min_pct < 0.05){
    pct_repo[[nr]] = tibble(varn = i, type=cls, minpct = min_pct)
    nr = nr+1
  }
  crd2[[n]] = select(res, newvar) %>% set_names(i)
  smbl[[n]] = smb
  n = n+1
}
crd2 = bind_cols(crd2)
pct_repo = bind_rows(pct_repo) # job,home,marital
smbs = tibble(nm = vrs, smb = smbl) # 변수별 smb 정보 

#[수작업] 구간별 최소비중 미충족건 수동 binning####
mfac = select(tr,pct_repo$varn,status)
mbin = function(x){
  x = enquo(x)
  group_by(mfac,!!x) %>% 
    summarise(n=n(),pd=mean(status)) %>% 
    mutate(pct = n/sum(n)) %>% 
    arrange(pd)
}
mbin(job)
smb_job = smbinning.factor.custom(mfac,x='job',y='status',
                                  c("'fixed'",
                                    "'freelance','others'",
                                    "'partime'"))
mbin(home)
smb_home = smbinning.factor.custom(mfac,x='home',y='status',
                                   c("'owner'",
                                     "'parents','priv'",
                                     "'rent','ignore','other'"))
mbin(marital)
smb_marital = smbinning.factor.custom(mfac,x='marital',y='status',
                                      c("'married'",
                                        "'widow','single','divorced','separated'"))
mfac1 = list()
mvar = pct_repo$varn # smbinning 수작업한 변수명 
mlen = length(mvar)
smblst = list(smb_job,smb_home,smb_marital)
for(i in 1:mlen){
  mfac1[[i]] = smbinning.factor.gen(mfac,smblst[[i]],'nvar') %>% 
    select(nvar) %>% set_names(mvar[i])
}
mfac1 = bind_cols(mfac1)
crd3 = select(crd2,-one_of(mvar)) %>% bind_cols(mfac1)

# test data도 같은 방법으로 가공 
# smbs에서 smb업데이트 
for(v in mvar){
  i = which(map_chr(smblst,'x')==v)
  new_smb = smblst[[i]]
  smbs[smbs$nm==v,]$smb[[1]] = new_smb
}
te_bin = list()
te_mfac = select(te,names(mfac)) #smb객체는 training한 데이터에 포함된 항목이 다르면 오류남
n = 1
for(i in vrs){
  n_var = select_if(te,is.numeric) %>% select(-status) %>% names()
  smb = filter(smbs,nm==i)$smb[[1]]
  if(i %in% mvar){
    df = te_mfac
  } else {
    df = te
  }
  if(i %in% n_var){
    res = smbinning.gen(df,smb,'nvar') %>% select(nvar) %>% set_names(i)
  } else {
    res = smbinning.factor.gen(df,smb,'fvar') %>% select(fvar) %>% set_names(i)
  }
  te_bin[[n]] = res
  n = n+1
}
te_bin = bind_cols(te_bin)
te_bin$status = te$status

# base모델적용: tr,te
# 비교모델적용: crd4, te_bin

# 변수별 factor의 수 
tibble(vars = vrs) %>% 
  mutate(uniq = map_dbl(vars,~n_distinct(crd2[[.x]]))) %>% 
  print() %>% 
  write_clip()
# vrs = 구간화된 변수명 
kaf = function(s,m=3,k=3){ # s:난수시드,k:k-익명성
  # 기본요건 
  len = length(vrs) # 변수갯수
  #m: 테이블 분할개수
  q = len %/% m # 테이블 항목갯수 동일하게 하기위한 변수갯수
  r = len %% m # 동일변수갯수로 분할 후 나머지 변수갯수 
  
  set.seed(s)
  grp = sample(1:m,size=len,prob=c(rep(q,m-1),q+r)/len,replace=T)
  tbls = character() #분리된 테이블의 항목명 
  kanon = logical() #k익명성 달성여부 
  # 분리테이블별 feature 생성 (참고항목들)
  tot_var = numeric() #분리테이블 총분산
  exp_pc1 = numeric() #1주성분 설명비율
  exp_pc2 = numeric() #2주성분 설명비율
  exp_pc3 = numeric() #3주성분 설명비율 
  varnum = integer() #변수갯수 
  j=1
  for(i in unique(grp)){ #grp: 변수그룹 (111222333...)
    tbl = crd3[,grp==i,drop=F]
    tbls[j] = str_c(names(tbl),collapse=',')
    k_min = group_by_all(tbl) %>% count() %>% pull(n) %>% min()
    kanon[j]= k_min >=k
    pca_mat = summary(prcomp(model.matrix(~.,tbl)))$importance
    tot_var[j] = sum(pca_mat[1,]**2)
    varnum[j] = ncol(tbl)
    exp_pc1[j] = pca_mat[2,1]
    exp_pc2[j] = ifelse(varnum[j]>=2,pca_mat[2,2],0)
    exp_pc3[j] = ifelse(varnum[j]>=3,pca_mat[2,3],0)
    j = j + 1
  }
  res = tibble(s=s,varn =tbls,vnum=varnum,k_min = k_min, k=kanon,tot_var = tot_var,
               pc1=exp_pc1,pc2=exp_pc2,pc3=exp_pc3)
  res2 = map_dfc(vrs,~ifelse(str_detect(tbls,.x),1L,0L)) %>% 
    set_names(vrs) #각 항목들이 테이블에 포함됐는지여부(one-hot)
  res = bind_cols(res,res2)
  return(res)
}
# 1000개의 테이블분할조합 찾기(m=3)
tic()
ktry_1000 = future_map_dfr(1:1000,~kaf(s=.x,m=3,k=3))
toc()
# 60.29 sec elapsed
# 1개의 난수(s당)에 m개의 분할 테이블 나옴 
# k=3를 달성하는 셋 
ktry_1000 %>% 
  group_by(s) %>% 
  filter(n()==sum(k))
# 모두 k=3 달성하는 셋은 하나도 없음 

# 시도횟수내 k=3를 달성하는 그룹들 
kgrp = ktry_1000 %>% 
  filter(k) %>% 
  filter(vnum >1) %>%  #1개변수만 있는 테이블도 제외
  group_by_at(vars(vrs)) %>%
  filter(row_number()==1)
dim(ktry_1000) #2978 22
dim(kgrp) #357 22

# 중복변수 최소화하며 모든 변수 포함하는 조합찾기 
sgrp = group_by(kgrp,s) %>% 
  summarise_at(vars(vrs),max)
sgrp # 326 15
cor_s = gather(sgrp,key,value,-s) %>% 
  spread(s,value) %>% 
  select_if(is.numeric) %>% 
  correlate() %>% 
  shave() %>% 
  stretch(na.rm=T)
# 52975개의 조합 

# noshow 정보 추가 
tic()
cor_s1 = cor_s %>% 
  mutate_at(vars(x,y),as.integer) %>% 
  mutate(feat = future_map2_chr(x,y,function(x,y){
    mid = filter(sgrp,s %in% c(x,y)) %>% select(-s)
    covn = sum(colSums(mid) >0) # 시드 조합시 총 포함변수 갯수
    dupn = sum(colSums(mid) >1,na.rm=T) # 시드 조합시 테이블간 중복항목갯수
    noshow = str_c(names(mid)[colSums(mid)==0],collapse=',') #시드 조합시 누락되는 변수
    res = str_c(covn,dupn,noshow,sep='/')
    return(res)
  })) %>% 
  separate(feat,c('cover_n','dup_n','noshow'),sep='/') %>% 
  mutate_at(vars(cover_n,dup_n),as.numeric) %>% 
  arrange(desc(cover_n),dup_n)
toc()
# 283.52 sec elapsed

nsh = function(...){
  res = group_by_at(crd3,vars(...)) %>% 
    tally() %>% 
    pull(n) %>% 
    min()
  return(res)
} # noshow k=3 체크함수 

# cor_s2: 없는 변수(noshow)로 테이블만들었을떄 k=3달성여부 
# 4변수 이상 조합이면 k=3 달성확률이 급격히 낮아지므로 
# noshow변수가 4이하인 것(총변수의 수 - 커버변수의 수)으로 제한한다
# 총변수의 수 : len = 14
tic()
cor_s2 = cor_s1 %>% 
  filter((len - cover_n) <=4) %>% 
  mutate(noshow_k3 = future_map_lgl(noshow,function(x){
    vars = str_split(x,",",simplify=T) %>% as.character()
    nsh(vars) >=3
  }))
toc()

# cor_s3: 시드조합(x,y)별 noshow까지 k=3 달성되는 조합 
cor_s3 = cor_s2 %>% 
  filter(noshow_k3)

# stbl: 시도(s)별 테이블 분할 변수정보 
stbl = group_by(kgrp,s) %>% 
  summarise(varns = str_c(varn,collapse='/'))

# cor_s4: 시드(s)별 noshow포함 테이블 분할정보 
cs3 = cor_s3 %>% 
  left_join(stbl,by=c('x'='s')) %>% 
  rename(x_tbl = varns) %>% 
  left_join(stbl,by=c('y'='s')) %>% 
  rename(y_tbl = varns) %>% 
  unite('xyn',c('x_tbl','y_tbl','noshow'),sep='/') %>% 
  mutate(slan = max(str_count(xyn,'/'))) 
maxtbl = max(cs3$slan)+1 # 최대 테이블 갯수 
cor_s4 = cs3 %>% 
  select(-slan) %>% 
  mutate(tbl_n = str_count(xyn,'/')+1) %>% 
  separate(xyn,str_c('tbl',1:maxtbl),sep='/') %>% 
  select(-noshow_k3)

# mod_s4: tbl을 formula로 변경 
crd4 = add_column(crd3,status = ifelse(tr$status==1,'bad','good')) %>% 
  mutate(status = factor(status))
mod_s4 = cor_s4 %>% 
  mutate_at(vars(matches('tbl\\d')),~str_replace_all(.,',','+') %>% str_c('status~',.)) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

# 테이블별 k 정보 
k_info = cor_s4 %>% 
  mutate_at(vars(matches('tbl\\d')),
            ~map_int(.,function(x){
              if(!is.na(x)){
                gvar = str_split(x,',',simplify=T) %>% as.character()
                mink = group_by_at(crd4,vars(gvar)) %>% count() %>% pull(n) %>% min()
              } else {
                mink = NA
              }
              return(mink)
            }))
mink = select(k_info,matches('tbl\\d')) %>% apply(1,min,na.rm=T)
maxk = select(k_info,matches('tbl\\d')) %>% apply(1,max,na.rm=T)
k_info$k_rng = str_c(mink,'~',maxk)
k_info

# id별 예측치 산출함수 
# train data로 스코어 만들기 
# crd4$status : bad/good
pred_id = function(id){
  tn = mod_s4[id,'tbl_n',drop=T] #NA 아닌 테이블 번호 
  mods = mod_s4[id,str_c('tbl',1:tn)] %>% gather(key,ff) %>% 
    mutate(model = map(ff,~glm(formula(.),data=crd4,family='binomial'))) %>% 
    mutate(preds = map(model,~predict(.,crd4,type='response')))
  preds = mutate(mods,key=str_replace(key,'tbl','mod')) %>% 
    select(key,preds) %>% 
    spread(key,preds) %>% 
    unnest() %>% 
    add_column(status = crd4$status) %>% 
    add_column(id=id,.before=1)
  return(preds)
}
# test data에 테이블별 스코어 만들기 
# te_bin$status: 1/0
pred_id_te = function(id){
  tn = mod_s4[id,'tbl_n',drop=T] #NA 아닌 테이블 번호 
  te_bin_x = mutate(te_bin,status = ifelse(status==1L,'bad','good') %>% factor)
  mods = mod_s4[id,str_c('tbl',1:tn)] %>% gather(key,ff) %>% 
    mutate(model = map(ff,~glm(formula(.),data=te_bin_x,family='binomial'))) %>% 
    mutate(preds = map(model,~predict(.,te_bin_x,type='response')))
  preds = mutate(mods,key=str_replace(key,'tbl','mod')) %>% 
    select(key,preds) %>% 
    spread(key,preds) %>% 
    unnest() %>% 
    add_column(status = te_bin_x$status) %>% 
    add_column(id=id,.before=1)
  return(preds)
}

tic()
pred_all = future_map_dfr(mod_s4$id,pred_id) %>% 
  select(id,status,everything())
pred_all_te = future_map_dfr(mod_s4$id,pred_id_te) %>% 
  select(id,status,everything())
toc()

# id별 stacking 모델 
tic()
# base모델 1 ####
tr_m = tr %>% mutate(status = ifelse(status==1L,'bad','good') %>% factor)
ctrl = trainControl(method='cv',number=5,
                    classProbs = T,
                    summaryFunction = twoClassSummary)
registerDoParallel(4)
base_model1 = train(status~.,data=tr_m,method='rf',metric='ROC',trControl=ctrl)
stopImplicitCluster()
te_m1 = te %>% add_column(score = predict(base_model1,te,type='prob')[['bad']])
# base모델 2 ####
registerDoParallel(4)
base_model2 = train(status~.,data=crd4,method='rf',metric='ROC',trControl=ctrl)
stopImplicitCluster()
te_m2 = te_bin %>% add_column(score = predict(base_model2,te_bin,type='prob')[['bad']])
toc()
# 135.13 sec elapsed
# anonym모델 모두 생성 ####
ano_mods = list()
ms4 = mod_s4 %>% filter(dup_n == 0) # 10개 
n = 1
for(i in ms4$id){
  tr_md = filter(pred_all,id==i) %>% select_if(~all(!is.na(.))) %>% select(-id)
  registerDoParallel(4)
  ano_mods[[n]] = train(status~.,data=tr_md,method='rf',metric='ROC',trControl=ctrl)
  stopImplicitCluster()
  print(n)
  n = n + 1 
}
saveRDS(ano_mods,'data/ano_mods.rds')

# anonym모델 퍼포먼스 
ano_pf = function(i,metric='auc',co=0.5){
  mod = ano_mods[[i]]
  j = ms4$id[i]
  te_md = filter(pred_all_te,id==j) %>% select_if(~all(!is.na(.))) %>% select(-id) %>% 
    mutate(target = (status=='bad')*1L) %>% data.frame()
  te_md$score = predict(mod, te_md,type='prob')[['bad']]
  smbinning.metrics(te_md,'score','target',cutoff=co,plot=metric)
}

# performance 비교 
smbinning.metrics(te_m1,'score','status',cutoff=0.5,plot='auc') # 원본1:ks(0.50),auc(0.83)
smbinning.metrics(te_m2,'score','status',cutoff=0.5,plot='auc') # 원본2:ks(0.498),auc(0.82)
ano_pf(1) # 0.48, 0.81
ano_pf(2) # 0.47, 0.795
ano_pf(3) # 0.48, 0.81
ano_pf(4) # 0.47, 0.80
ano_pf(5) # 0.44, 0.79
ano_pf(6) # 0.44, 0.79
ano_pf(7) # 0.45, 0.80
ano_pf(8) # 0.46, 0.80
ano_pf(9) # 0.48, 0.80
ano_pf(10) # 0.45, 0.79

library(tidyverse) #데이터 가공
library(smbinning) #준식별자 일반화
library(naniar) #missing 변수체크
library(readxl) #엑셀파일 읽기
library(clipr) #클립보드에 복사
library(furrr) #프로그래밍 
library(tictoc)
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

# 변수변별력 요약 
siv = smbinning.sumiv(crd1,'status')
siv %>% write_clip()
smbinning.sumiv.plot(siv)
# expenses,debt,price 자체는 변별력 없어 제외 
# 13개 준식별자 중 3개 제외, 비율변수 4개추가 => 총14개 준식별자 

# smbinning을 이용하여 준식별자 일반화(구간화)
vrs = siv$Char[!is.na(siv$IV)] %>% as.character()
crd2 = list()
n = 1
for(i in vrs){
  cls = class(crd1[[i]])
  if(cls == 'factor'){
    smb = smbinning.factor(crd1,'status',i)
    res = smbinning.factor.gen(crd1,smb,'newvar')
  } else {
    smb = smbinning(crd1,'status',i)
    res = smbinning.gen(crd1,smb,'newvar')
  }
  crd2[[n]] = select(res, newvar) %>% set_names(i)
  n = n+1
}
crd2 = bind_cols(crd2)
# crd2$status = crd1$status
# 변수별 factor의 수 
tibble(vars = vrs) %>% 
  mutate(uniq = map_dbl(vars,~n_distinct(crd2[[.x]]))) %>% 
  print() %>% 
  write_clip()
# vrs = 구간화된 변수명 
kaf = function(s,m=4,k=3){ # s:난수시드,k:k-익명성
  # 기본요건 
  len = length(vrs) # 변수갯수
  #m: 테이블 분할개수
  q = len %/% m # 테이블 항목갯수 동일하게 하기위한 변수갯수
  r = len %% m # 동일변수갯수로 분할 후 나머지 변수갯수 
  
  set.seed(s)
  grp = sample(1:m,size=len,prob=rep(q,m)/len,replace=T)
  tbls = character() #분리된 테이블의 항목명 
  kanon = logical() #k익명성 달성여부 
  # 분리테이블별 feature 생성 
  tot_var = numeric() #분리테이블 총분산
  exp_pc1 = numeric() #1주성분 설명비율
  exp_pc2 = numeric() #2주성분 설명비율
  exp_pc3 = numeric() #3주성분 설명비율 
  varnum = integer() #변수갯수 
  j=1
  #4개를 지정해도 확률에 의해 3그룹이 되기도 함.
  for(i in unique(grp)){
    tbl = crd2[,grp==i,drop=F]
    tbls[j] = str_c(names(tbl),collapse=',')
    kanon[j]= group_by_all(tbl) %>% count() %>% pull(n) %>% min() >=k
    pca_mat = summary(prcomp(model.matrix(~.,tbl)))$importance
    tot_var[j] = sum(pca_mat[1,]**2)
    varnum[j] = ncol(tbl)
    exp_pc1[j] = pca_mat[2,1]
    exp_pc2[j] = ifelse(varnum[j]>=2,pca_mat[2,2],0)
    exp_pc3[j] = ifelse(varnum[j]>=3,pca_mat[2,3],0)
    j = j + 1
  }
  res = tibble(s=s,varn =tbls,k=kanon,tot_var = tot_var,
               pc1=exp_pc1,pc2=exp_pc2,pc3=exp_pc3,
               vnum=varnum)
  res2 = map_dfc(vrs,~ifelse(str_detect(tbls,.x),1L,0L)) %>% 
    set_names(vrs) #각 항목들이 테이블에 포함됐는지여부(one-hot)
  res = bind_cols(res,res2)
  return(res)
}
# 1000개의 테이블분할조합 찾기(m=3)
tic()
ktry_1000 = future_map_dfr(1:1000,~kaf(s=.x,m=3,k=3))
toc()
# 1개의 난수에 m개의 분할 테이블 나옴 

# k=3를 달성하는 그룹 
kgrp = ktry_1000 %>% 
  filter(k) %>% 
  distinct(seniority,income,records,job,home,
           assets,prc_ovr_asst,debt_ovr_asst,
           exp_ovr_inc,amount,prc_ovr_inc,time,marital,age,.keep_all = T)
dim(ktry_1000) #2991 22
dim(kgrp) #224 22

# 중복변수 최소화하며 모든 변수 포함하는 조합찾기 
sgrp = group_by(kgrp,s) %>% 
  summarise_at(vars(vrs),sum)
sgrp # 217 15
library(corrr)
cor_s = gather(sgrp,key,value,-s) %>% 
  spread(s,value) %>% 
  select_if(is.numeric) %>% 
  correlate() %>% 
  shave() %>% 
  stretch(na.rm=T)
# 42230개의 조합 
tic()
cor_s1 = cor_s %>% 
  mutate_at(vars(x,y),as.integer) %>% 
  mutate(feat = future_map2_chr(x,y,function(x,y){
    mid = filter(sgrp,s %in% c(x,y)) %>% select(-s)
    covn = sum(colSums(mid) >0) # 테이블에 포함된 변수갯수
    dupn = sum(colSums(mid) >1,na.rm=T) # 중복항목갯수
    res = str_c(covn,dupn,sep=',')
    return(res)
  })) %>% 
  separate(feat,c('cover_n','dup_n')) %>% 
  mutate_at(vars(cover_n,dup_n),as.numeric)
toc()
# 97.65 sec
dim(cor_s1)
cor_s1 %>% 
  arrange(desc(cover_n))
# 총변수의 수 : 14
len
# 최대 커버변수의 수 : 11
library(ggthemes)
windowsFonts(ng = windowsFont('나눔고딕'))
theme_set(theme_tufte(10,'ng'))
ggplot(cor_s1,aes(cover_n,dup_n,fill=r))+
  geom_tile(color='white')+
  scale_x_continuous(expand = expand_scale(mult=0),
                     breaks = 0:11)+
  scale_y_continuous(expand = expand_scale(mult=0))+
  scale_fill_gradient2_tableau()

# 80 167 cover=11, dup=2
# 80 466 cover=10, dup=0 외 6개 조합 
g1 = kaf(80) %>% filter(k)
g2 = kaf(466) %>% filter(k)
select(g1,varn)
select(g2,varn)

crd = read_xlsx('data/credit_data.xlsx') %>% 
  select_all(tolower) %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(target = ifelse(status=='bad',1,0)) %>% 
  data.frame()
library(rsample)
spl = initial_split(crd,prop=0.7,strata = 'target')
tr = training(spl)
te = testing(spl)
sumiv = smbinning.sumiv(select(tr,-status),'target')
sumiv
# binning 되는건 자동 binning을 이용하고 
# binning 안되는건 20%씩 수동 binning 한다 
vargb = sumiv %>% 
  mutate(method = case_when(str_detect(Process,'Numeric')~'num',
                            str_detect(Process,'Factor')~'key',
                            TRUE~'otr'))
# key변수와 numeric변수명 구분 
keyv = filter(vargb,method=='key') %>% pull(Char) %>% as.character()
numv = filter(vargb,method=='num') %>% pull(Char) %>% as.character()
otrv = filter(vargb,method=='otr') %>% pull(Char) %>% as.character()

# 변수종류별 binning
library(recipes)
key_bin = map(keyv,~smbinning.factor(tr,'target',.x,maxcat=10))
num_bin = map(numv,~smbinning(tr,'target',.x,p=0.05))
qtls = map(otrv,~unique(quantile(tr[[.x]],probs=seq(0,1,.2))))
names(qtls) = otrv

make_bin = function(df,key_bin=key_bin,num_bin=num_bin){
  key_df = map2(key_bin,keyv,
                ~smbinning.factor.gen(df,.x,chrname=str_c('f_',.y)) %>% 
                  select(str_c('f_',.y))) %>% bind_cols()
  num_df = map2(num_bin,numv,
                ~smbinning.gen(df,.x,chrname=str_c('n_',.y)) %>% 
                  select(str_c('n_',.y))) %>% bind_cols()
  otr_df = map2(otrv,qtls,~cut(df[[.x]],c(-Inf,.y,Inf))) %>% 
    bind_cols()
  names(otr_df) = otrv
  bindf = bind_cols(select(df,status),
                     key_df,num_df,otr_df) %>% 
    select_all(~str_remove(.,'f_|n_'))
  return(bindf)
}
  
bin_tr = make_bin(tr,key_bin,num_bin)
bin_te = make_bin(te,key_bin,num_bin)

dim(bin_tr)
dim(bin_te)

# k-anonymity check #####
library(sdcMicro)
keyv1 = names(bin_tr)[!names(bin_tr) %in% c('status')]
sdc = createSdcObj(bin_tr,keyVars = keyv1)
sdc
sup_ka = kAnon(sdc,k=3)
ka_df = sup_ka@manipKeyVars
sup_ka
library(naniar)
miss_var_summary(ka_df)
idx<-sapply(ka_df,is.na) %>% rowSums()>0
bin_tr[idx,'gb'] = '1,2'
bin_tr1 = bin_tr %>% 
  mutate(gb = ifelse(idx,'1,2,3','1')) %>% 
  separate_rows(gb) %>% 
  select(-gb) 
sdc1 = createSdcObj(bin_tr1,keyVars = keyv1)
sdc1
ldiv = ldiversity(bin_tr1,keyVars = keyv1,ldiv_index = 'status')
# fitting
library(caret)
# randomForest 
# why?
#' 변수 구간화에 따른 모델 퍼포먼스를 최적화 (사람개입 최소화)
ctrl = trainControl(method='cv',number=5,
                    classProbs = T,
                    summaryFunction = twoClassSummary)
baseline = train(status~.-target,data=tr,
                 method = 'rf',
                 metric = 'ROC',
                 trControl = ctrl)
sdc_model = train(status~.,data=bin_tr1,
             method = 'rf',
             metric = 'ROC',
             trControl = ctrl)
te$pred = predict(baseline, te)
te$bprob = predict(baseline, te, type='prob')[['bad']]
bin_te$pred = predict(sdc1, bin_te)
bin_te$target = ifelse(bin_te$status=='bad',1,0)
bin_te$bprob = predict(sdc1, bin_te, type='prob')[['bad']]

smbinning.metrics(te,'bprob','target',report = 1)
smbinning.metrics(bin_te,'bprob','target',report=1)
smbinning.metrics(te,'bprob','target',report=0,plot='auc')
smbinning.metrics(te,'bprob','target',report=0,plot='ks')
smbinning.metrics(bin_te,'bprob','target',report=0,plot='auc')
smbinning.metrics(bin_te,'bprob','target',report=0,plot='ks')

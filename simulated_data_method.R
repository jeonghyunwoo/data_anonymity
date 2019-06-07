# 변별력은 좀 떨어지지만 시간이 훨씬 덜드는 방법
# 원 데이터의 통계적 속성을 이용해서 가상의 유사한 데이터(simulated data)를 만들어 적재하는 방법

library(MASS)
library(tidyverse)
library(caret)
library(recipes)
library(doParallel)
library(smbinning)

# factor 변수를 dummy화 한다(one_hot_encoding)
rec = recipe(~., data=tr) %>%
    step_dummy(all_nominal(),-status) %>%
    prep(retain = TRUE)
tr_dm = juice(rec) 
te_dm = bake(rec, te)

# 통계속성을 만들어낸다 :평균,표준편차,상관계수,변수별 QUANTILE 정보 
mu = colMeans(tr_dm)
std = sapply(tr_dm,sd)
corr_mat = cor(tr_dm)
cov_mat = std %*% t(std)*corr_mat
# 가상의 유사한 데이터를 만든다
tr_sim = mvrnorm(n = nrow(tr_dm), mu=mu, Sigma=cov_mat, empirical =TRUE) %>% data.frame
# 평균,표준편차,상관계수는 똑같지만 개별 변수의 분포는 상이하다 (정규분포화됨)

# 그래서 개별 변수를 기존변수처럼 만들어준다 
# .001 : nrow(tr_dm)/3 = 1039.333, 1/1000 = 0.001
for(v in names(tr_sim)) {
    vc = tr_sim[[v]]
    cut_bin = quantile(vc, probs=seq(0,1,.001))
    cut_lab = quantile(tr_dm[[v]],probs=seq(.001,1,.001))
    new_vc = cut(vc, cut_bin, cut_lab, include.lowest = TRUE) %>% as.character() %>% as.numeric()
    tr_sim[[v]] = new_vc
    }
# 분포 확인 
skim_to_list(tr_dm)
skim_to_list(tr_sim)

# 개별변수 변별력 확인 
siv1 = smbinning.sumiv(tr_dm,'status') # original data
siv2 = smbinning.sumiv(tr_sim,'status') # simulated data
# 개별변수의 정보량(IV)은 떨어진다. 그러나...

tr_dm$target = ifelse(tr_dm$status==1,'bad','good') %>% factor
tr_sim$target = ifelse(tr_sim$status==1,'bad','good') %>% factor
ctrl = trainControl(method='cv',number=5,classProbs=T,summaryFunction=twoClassSummary)
registerDoParallel(4)
fit1 = train(target~.-status,data=tr_dm,method='rf',ntree=101,trControl=ctrl)
fit2 = train(target~.-status,data=tr_sim,method='rf',ntree=101,trControl=ctrl)
stopImplicitCluster()

pred1 = predict(fit1,te,type='prob')[['bad']]
pred2 = predict(fit2,te,type='prob')[['bad']]

tst = data.frame(pred1 = pred1, pred2 = pred2, class = te_dm$status)
smbinning.metrics(tst,'pred1','class') # KS: 0.41, AUC: 0.76
smbinning.metrics(tst,'pred2','class') # KS: 0.33, AUC: 0.73

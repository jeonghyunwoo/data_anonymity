# 변별력이 높으면 10등분,낮으면 3~5등분하자 
# 이렇게 해도 k익명성이 달성 안된다 
# 익명성 달성되는 변수그룹들을 다모아보자
# 그 그룹들을 다 모았을때 그룹간 중복변수는 있더라도 모든 변수가 다 들어오는지 확인하자
# 그룹들을 동일 준식별자로 join했을 때도 k익명성 달성되는지 확인하자 
# 이렇게 중복 항목이 존재하도록 테이블을 분리하자 
# (참고) 쏠림공격과 유사성공격에 대응하는게 t근접성이다
# 쏠림공격은 전체에이즈비율은 1%인데 특정 동질클래스에서는 99%인경우다
# 유사성공격은 l다양성은 만족하지만 민감속성이 비슷비슷한 경우다 
# k익명성은 신원노출을 방어하는 모델이다 
# k익명성은 동일한 준식별자 속성값을 가지는 레코드가 적어도 k개 존재할때 성립한다
# 즉 민감속성값을 고려하지 않는다. 이걸 고려하는 것은 l다양성,t근접성이다. 

library(pacman)
p_load(tidyverse,smbinning,tabplot,readxl)
p_load(skimr,naniar)
crd = read_xlsx('data/credit_data.xlsx') %>% 
  select_all(tolower) %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(target = ifelse(status=='bad',1,0)) %>% 
  data.frame()

ivchk = smbinning.sumiv(select(crd,-status),'target')
ivchk
# expenses, debt, price는 유의한 split이 없게 나온다 
# 파생변수를 만들자 
crd0 = crd %>% 
  mutate(ei_rat = expenses/income,
         # da_rat = debt/assets,
         pam_rat = price/amount)
ivchk1 = smbinning.sumiv(crd0,'target')
ivchk1
smbinning.sumiv.plot(ivchk1)
# strong : >=0.3
# medium : >=0.1
# weak : <0.1
strong = filter(ivchk,IV>=0.3,str_detect(Process,'Numeric')) %>% pull('Char') %>% as.character()
medium = filter(ivchk,IV>=0.1,IV<0.3,str_detect(Process,'Numeric')) %>% pull('Char') %>% as.character()
weak = filter(ivchk,IV<0.1,str_detect(Process,'Numeric')) %>% pull('Char') %>% as.character()


# 준식별자 일반화 
skim_tee(crd)
library(magrittr)
# binning 정보는 따로 저장하고 binning값은 알파벳으로 하자 
# 이렇게 하면 smbinning이용가능 
qtf = function(x,bin=10) { # bin: 등분 수
  qt = quantile(x,probs=seq(0,1,1/bin))
  qt = unique(qt)
  lab = letters[1:length(qt)-1] 
  bin = cut(x,qt,lab,include.lowest = T)
  return(bin)
}
# binning data : crd1
crd1 = crd0 %>%
  select(-target) %>% 
  mutate_at(vars(strong),qtf,bin=10) %>% 
  mutate_at(vars(medium),qtf,bin=5) %>% 
  mutate_at(vars(weak),qtf,bin=3) %>% 
  mutate(target = ifelse(status=='bad',1,0))
# missing chech
miss_var_summary(crd1)
# 변수별 변별력 체크 
ivchk3 = smbinning.sumiv(select(crd1,-status),'target')
nosplt = filter(ivchk3,str_detect(Process,'No')) %>% 
  pull(Char) %>% as.character()
# 테이블분리 
crd2 = select(crd1,-status,-target,-one_of(nosplt))


chk = function(m,seed){
  # m : 분할할 갯수 
  tot_var = ncol(crd2)
  splt = as.integer()
  sh = tot_var %/% m; rs = tot_var %% m
  splt = c(rep(sh,m-1),sh+rs)
  
  set.seed(seed)
  smp = sample(1:m,tot_var,replace=T,prob=splt/tot_var)
  while(min(table(smp))==1|length(unique(smp))<m){ #변수가 하나만 있는 테이블이 있으면 실행
    seed = seed+100
    set.seed(seed)
    smp = sample(1:m,tot_var,replace=T,prob=splt/tot_var)
  }
  grps = list()
  ncomb = character()
  for(i in 1:m){
    grps[[i]] = crd2[,smp==i]
    ncomb[i] = str_c(names(grps[[i]]),collapse=',')
  }
  ncomb = str_c(ncomb,collapse='/')
  
  # k익명성 체크 
  map_lgl(grps,
          ~group_by_all(.) %>% 
            tally() %$%
            min(n) >=3) %>% # 여기서 3은 k임 
            matrix(ncol=m,dimnames=list('',str_c('g',1:m))) %>% 
            data.frame() %>% 
    add_column(ncomb = ncomb) %>% 
    add_column(seed=seed,.before=1)
}
library(tictoc)
library(furrr)
# k=3 미달성 기여도가 큰 변수를 찾아보자!!!!
# 코드구간별 메시지 프린트 스크립트를 넣자 
tic()
m = 5
try1 = future_map_dfr(1:1000,~chk(m,.x))
res1 = try1 %>% mutate(stsfy = ifelse(rowSums(.[,2:(m+1)])==m,1,0))
count(res1,stsfy)
toc()
# 1: 27개 
filter(res1,stsfy==1)
# m:4, n_seed:1000, 22.7 sec elapsed
# factor를 dummy로 만든후
# prcomp를 통해 총분산을 만들자 
# eigenvalue는 무엇인가? 
model.matrix(~.,grp1)

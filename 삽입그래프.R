# data split ####
ds = tibble(origin = nrow(crd),
            train = nrow(tr),
            test = nrow(te)) %>% 
  gather(key,value) %>% 
  mutate(colr = ifelse(key=='origin','a','b')) %>% 
  mutate(key = fct_relevel(key,'origin','train'))
ds1 = count(crd,status) %>% mutate(gb='origin') %>% 
  bind_rows(
    count(tr,status) %>% mutate(gb='train'),
    count(te,status) %>% mutate(gb='test')
  ) %>% 
  mutate(gb = fct_relevel(gb,'origin','train'))

windowsFonts(ng=windowsFont('나눔고딕'))
library(ggthemes)
theme_set(theme_tufte(12,'ng'))
blk = element_blank()
p1<-ggplot(ds,aes(key,value,fill=colr))+
  geom_col(width=.7)+
  geom_text(aes(label=value),size=4,family='ng',
            vjust = -1)+
  scale_fill_manual(values=c('grey','steelblue'))+
  scale_y_continuous(expand = expand_scale(mult=c(0,.2)))+
  labs(x='',y='')+
  theme(legend.position = 'none',
        axis.line.x = element_line(color='grey'),
        axis.ticks.y = blk,
        axis.text.y = blk)
p2<-ds1 %>% group_by(gb) %>% 
  mutate(pct = n/sum(n),pct=scales::percent(pct,accuracy=.1)) %>% 
  ggplot(aes(gb,n,fill=status))+
  geom_col(width=.7,position='fill')+
  geom_text(aes(label=pct),size=4,family='ng',
            position=position_fill(vjust=0.5))+
  scale_fill_manual(values=c('firebrick','grey'))+
  scale_y_continuous(expand = expand_scale(mult=c(0,.2)))+
  labs(x='',y='')+
  theme(legend.position = 'none',
        axis.line.x = element_line(color='grey'),
        axis.ticks.y = blk,
        axis.text.y = blk)
p1
p2

# 일반화 예시 ####
key_bin[[1]]
num_bin[[1]]$ivtable
head(tr)
head(bin_tr)
iltr = select(tr,-status,-target) %>% head()
antr = select(bin_tr,names(iltr)) %>% head()
iltr %>% write_clip()
antr %>% write_clip()

# k익명성 체크 ####

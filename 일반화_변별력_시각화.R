library(Information)
library(tidyverse)
library(ggthemes)

iv20 = create_infotables(data = tr, y='status', bins=20, parallel = TRUE)
iv10 = create_infotables(data = tr, y='status', bins=10, parallel = TRUE)
iv5 = create_infotables(data = tr, y='status', bins=5, parallel = TRUE)
iv3 = create_infotables(data = tr, y='status', bins=3, parallel = TRUE)

# 구간수 감소에 따른 변별력(information value) 감소 시각화
ivsim = bind_rows(
    iv20$Summary %>% add_column(bins=20),
    iv10$Summary %>% add_column(bins=10),
    iv5$Summary %>% add_column(bins=5),
    iv3$Summary %>% add_column(bins=3)
    ) %>% mutate(bins = str_c('bin:',bins)) %>%
    select_all(tolower) %>%
    mutate(bins = fct_inorder(bins),
           variable = fct_inorder(variable)) %>%
    tbl_df()

ggplot(ivsim,aes(variable,iv,group=bins,fill=bins))+
    geom_col(position='dodge',color='white')+
    scale_fill_tableau(palette = 'Tableau20')+
    labs(x = 'Information Value')+
    coord_flip()

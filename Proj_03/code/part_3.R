---
title: "TS_Proj03"
output: html_document
date: "2026-09-27"
---

# load data
library(readxl)
life_table = read_excel('2010_Complete_Life_Table.xlsx', range='A3:S103', col_names=FALSE)
# divide life table by gender
male_cols = c(1, 3, 6, 9, 12, 18); female_cols = c(1, 4, 7, 10, 13, 19)
male_tb = life_table[, male_cols]
colnames(male_tb) = c('age','e_m','q_m', 'l_m', 'L_m','d_m')

female_tb = life_table[, female_cols]
colnames(female_tb) = c('age','e_f','q_f','l_f','L_f','d_f')

head(life_table)
head(male_tb)
head(female_tb)


#(a)
library(dplyr)
r = log(1.07); v = exp(-r)

male_tb_a = male_tb %>% mutate(age=0:100, C=v^(age+1)*d_m,
                               D = v^age*l_m, M=rev(cumsum(rev(C))),
                               A = M/D, Abar = A*(0.07/r))
female_tb_a = female_tb %>% mutate(age=0:100, C=v^(age+1)*d_f,
                                   D = v^age*l_f, M=rev(cumsum(rev(C))),
                                   A = M/D, Abar = A*(0.07/r))
df_a = inner_join(male_tb_a[c('age','Abar')], female_tb_a[c('age','Abar')], by='age')
colnames(df_a) = c('age','Abar_m', 'Abar_f')
gap5 = seq(21, 70, by=5)
df_a[gap5,]


#(b)
m = 12
i_m = m*exp(r/m)-m
d_m = m-m*exp(-r/m)

male_tb_b = male_tb_a %>% mutate(A_m = A*(0.07/i_m), a_m = (1-A_m)/(m*(1-exp(-r/m))))
female_tb_b = female_tb_a %>% mutate(A_m = A*(0.07/i_m), a_m = (1-A_m)/(m*(1-exp(-r/m))))

df_b = inner_join(male_tb_b[c('age','a_m')], female_tb_b[c('age','a_m')], by='age')
colnames(df_b) = c('age','a12_m','a12_f')
df_b[gap5,]



# (c)
# insurance benefit : 1억
ins_benefit=100000000
male_tb_c = male_tb_b %>% mutate('premium(male)' = Abar/a_m*ins_benefit*1/m)
female_tb_c = female_tb_b %>% mutate('premium(female)' = Abar/a_m*ins_benefit*1/m)

df_c = inner_join(male_tb_c[c('age','premium(male)')],
                  female_tb_c[c('age','premium(female)')], by='age')
df_c[gap5,]



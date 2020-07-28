# Load libraries --------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(here)

# -----------------------------------------------------------------------------
setwd("~/Desktop/qpjm")


# Import and tidy -------------------------------------------------------------
# Import data, filter L2 and Instructors
df <- read_csv("data/qp.csv") %>%
  mutate(mint_std = (MINT - mean(MINT)) / sd(MINT)) %>%
  mutate(., lang = recode(lang, `s` = "1", `h` = "0"))

#Aqui he cambiado a Spanish es 1 y hebreo es 0

glimpse(df)
View(df)

# DATOS DEL EPT


df %>% 
  group_by(., GROUP, Frequent, number) %>% 
  summarize(., accuracy = mean(na.omit(EPT)), sd = sd(na.omit(EPT)))

df %>% 
  filter(GROUP == "L2SPANISH" | GROUP == "L2HEB" ) %>% 
  group_by(., L2SPANISH, L2HEB) %>% 
  summarize(., accuracy = mean(na.omit(MINT)), sd = sd(na.omit(MINT)))




#GROUP     Frequent number accuracy     sd
#<chr>     <chr>    <chr>     <dbl>  <dbl>
#  1 L1HEB     f        p        0.992  0.0884
#2 L1HEB     f        s        1      0     
#3 L1HEB     nf       p        0.742  0.439 
#4 L1HEB     nf       s        0.766  0.425 
#5 L1SPANISH f        p        0.984  0.125 
#6 L1SPANISH f        s        0.976  0.152 
#7 L1SPANISH nf       p        0.961  0.195 
#8 L1SPANISH nf       s        0.968  0.176 
#9 L2HEB     f        p        0.417  0.495 
#10 L2HEB     f        s        0.426  0.497 
#11 L2HEB     nf       p        0.0556 0.230 
#12 L2HEB     nf       s        0.0561 0.231 
#13 L2SPANISH f        p        0.7    0.460 
#14 L2SPANISH f        s        0.675  0.470 
#15 L2SPANISH nf       p        0.458  0.500 
#16 L2SPANISH nf       s        0.483  0.502 

mod1 <- glmer( 
  EPT ~ lang + native + Frequent + number +
    (1 | PARTICIPANT) + 
    (1 | ITEM),
  data = df, 
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod1)

#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.8988     0.6461  -1.391    0.164    
#langs         2.7517     0.7033   3.913 9.12e-05 ***
#  native        6.3264     0.6033  10.487  < 2e-16 ***
#  Frequentnf   -3.1215     0.5389  -5.793 6.93e-09 ***
#  numbers       0.0902     0.5001   0.180    0.857   

#---

# DATOS DEL FCT AGR

df$FCTAGR <- as.numeric(df$FCFTAGR)
glimpse(df)

df %>% 
  group_by(., GROUP, Frequent, number) %>% 
  summarize(., accuracy = mean(na.omit(FCTAGR)), sd = sd(na.omit(FCTAGR)))

#GROUP     Frequent number accuracy     sd
#<chr>     <chr>    <chr>     <dbl>  <dbl>
#  1 L1HEB     f        p         0.930 0.257 
#2 L1HEB     f        s         0.992 0.0887
#3 L1HEB     nf       p         0.646 0.480 
#4 L1HEB     nf       s         0.781 0.415 
#5 L1SPANISH f        p         0.977 0.152 
#6 L1SPANISH f        s         1     0     
#7 L1SPANISH nf       p         0.984 0.125 
#8 L1SPANISH nf       s         0.984 0.125 
#9 L2HEB     f        p         0.509 0.502 
#10 L2HEB     f        s         0.593 0.494 
#11 L2HEB     nf       p         0.139 0.347 
#12 L2HEB     nf       s         0.185 0.390 
#13 L2SPANISH f        p         0.775 0.419 
#14 L2SPANISH f        s         0.833 0.374 
#15 L2SPANISH nf       p         0.664 0.474 
#16 L2SPANISH nf       s         0.675 0.470 

mod2 <- glmer(
  FCTAGR ~ lang + native + Frequent + number + lang:number +
    (1 | PARTICIPANT) + 
    (1 | ITEM),
  data = df, 
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod2)

#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    -0.5308     0.4054  -1.309   0.1905    
#langs           3.1118     0.5263   5.913 3.36e-09 ***
#  native          3.7299     0.3943   9.460  < 2e-16 ***
#  Frequentnf     -1.9028     0.2784  -6.835 8.19e-12 ***
#  numbers         0.7269     0.3636   1.999   0.0456 *  
#  langs:numbers  -0.2753     0.5373  -0.512   0.6084  

#--- 

# DATOS DEL FCT ASG

df %>% 
  group_by(., GROUP, Frequent, number) %>% 
  summarize(., accuracy = mean(na.omit(FCTASG)), sd = sd(na.omit(FCTASG)))

#GROUP     Frequent number accuracy    sd
#/<chr>     <chr>    <chr>     <dbl> <dbl>
#  1 L1HEB     f        p         0.891 0.313
#2 L1HEB     f        s         0.930 0.257
#3 L1HEB     nf       p         0.594 0.493
#4 L1HEB     nf       s         0.766 0.425
#5 L1SPANISH f        p         0.953 0.212
#6 L1SPANISH f        s         0.961 0.195
#7 L1SPANISH nf       p         0.953 0.212
#8 L1SPANISH nf       s         0.938 0.243
#9 L2HEB     f        p         0.454 0.500
#10 L2HEB     f        s         0.481 0.502
#11 L2HEB     nf       p         0.111 0.316
#12 L2HEB     nf       s         0.194 0.398
#13 L2SPANISH f        p         0.708 0.456
#14 L2SPANISH f        s         0.867 0.341
#15 L2SPANISH nf       p         0.617 0.488
#16 L2SPANISH nf       s         0.717 0.453

mod3 <- glmer(
  FCTASG ~ lang + native + Frequent + number + 
    (1 | PARTICIPANT) + 
    (1 | ITEM),
  data = df, 
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod3)

#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.9318     0.4259  -2.188   0.0287 *  
#  langs         2.9043     0.4801   6.050 1.45e-09 ***
#  native        3.2931     0.3770   8.734  < 2e-16 ***
#  Frequentnf   -1.5168     0.3216  -4.717 2.39e-06 ***
#  numbers       0.7791     0.3187   2.445   0.0145 *  

# Importancia del MiNT en L2

# quitar los L1

#EPT entre los L2 con MINT

dfl2 <- df %>% filter(., native == '0')
View(dfl2)

mod4 <- glmer(
  EPT ~ lang + Frequent + number + mint_std + lang:mint_std +
    (1 | PARTICIPANT) + 
    (1 | ITEM),
  data = dfl2, 
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod4)

#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     1.23446    0.68973   1.790   0.0735 .  
#langs           5.26551    1.24835   4.218 2.47e-05 ***
#  Frequentnf     -2.82368    0.38596  -7.316 2.56e-13 ***
#  numbers         0.03322    0.32760   0.101   0.9192    
#mint_std        2.39161    0.57905   4.130 3.62e-05 ***
#  langs:mint_std  1.93161    1.05294   1.834   0.0666 .  

#FCT AGR entre los L2 con MINT

mod5 <- glmer(FCTAGR ~ lang + Frequent + number + mint_std +
    (1 | PARTICIPANT) + 
    (1 | ITEM),
  data = dfl2, 
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod5)

#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.2656     0.5581   2.268 0.023347 *  
#  lang1         2.8298     0.5180   5.463 4.67e-08 ***
#  Frequentnf   -1.8787     0.2633  -7.134 9.75e-13 ***
#  numbers       0.4019     0.2502   1.606 0.108263    
#mint_std      1.5770     0.4170   3.782 0.000156 ***
  
  

#FCT ASG entre los L2 con MINT

mod6 <- glmer(
  FCTASG ~ lang + Frequent + number + mint_std + lang:mint_std +
    (1 | PARTICIPANT) + 
    (1 | ITEM),
  data = dfl2, 
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod6)

#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    -0.004956   0.541885  -0.009  0.99270    
#langs           5.523936   1.057141   5.225 1.74e-07 ***
#  Frequentnf     -1.570616   0.284660  -5.518 3.44e-08 ***
#  numbers         0.783267   0.280085   2.797  0.00517 ** 
#  mint_std        0.871766   0.422670   2.063  0.03916 *  
#  langs:mint_std  2.532759   0.869022   2.914  0.00356 ** 

#--- PLOTS

# plot modificados 229-239
plot1 <- df %>%
  mutate(., lang = recode(lang, `0` = "Hebrew", `1` = "Spanish")) %>%
  ggplot(., aes(x = native, y = EPT, shape = Frequent, color = number)) + 
  facet_grid(. ~ lang) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  labs(x = 'Frequency', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 1: Proportion of accurate EPT responses across conditions') + 
  theme_grey(base_size = 16, base_family = 'Times')

ggsave("plot1.png", plot = plot1, dpi = 600, 
       device = "png", path = "./figs", 
       height = 6, width = 9, units = "in")

# grafico sin asignar objeto

df %>%
  mutate(., lang = recode(lang, `0` = "Hebrew", `1` = "Spanish")) %>%
  ggplot(., aes(x = native, y = EPT, shape = Frequent, color = number)) + 
  facet_grid(. ~ lang) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  labs(x = 'Frequency', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 1: Proportion of accurate EPT responses across conditions') + 
  theme_grey(base_size = 16, base_family = 'Times')


df %>%
  mutate(., lang = recode(lang, `0` = "Hebrew", `1` = "Spanish")) %>%
  ggplot(., aes(x = native, y = FCTAGR, shape = Frequent, color = number)) + 
  facet_grid(. ~ lang) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  labs(x = 'Frequency', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 3: Proportion of accurate agreement FCT responses across conditions') + 
  theme_grey(base_size = 16, base_family = 'Times')

df %>%
  mutate(., lang = recode(lang, `0` = "Hebrew", `1` = "Spanish")) %>%
  ggplot(., aes(x = native, y = FCTASG, shape = Frequent, color = number)) + 
  facet_grid(. ~ lang) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  labs(x = 'Frequency', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 5: Proportion of accurate assignment FCT responses across conditions') + 
  theme_grey(base_size = 16, base_family = 'Times')




plot2 <- df %>%
  mutate(., lang = recode(lang, `0` = "Hebrew", `1` = "Spanish")) %>%
  ggplot(., aes(x = native, y = FCTAGR, shape = Frequent, color = number)) + 
  facet_grid(. ~ lang) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  labs(x = 'Frequency', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 2: Proportion of accurate agreement FCT responses across conditions') + 
  theme_grey(base_size = 16, base_family = 'Times')

ggsave("plot2.png", plot = plot2, dpi = 600, 
       device = "png", path = "./figs", 
       height = 6, width = 9, units = "in")

plot3 <- df %>%
  mutate(., lang = recode(lang, `0` = "Hebrew", `1` = "Spanish")) %>%
  ggplot(., aes(x = native, y = FCTASG, shape = Frequent, color = number)) + 
  facet_grid(. ~ lang) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  labs(x = 'Frequency', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 3: Proportion of accurate assignment FCT responses across conditions') + 
  theme_grey(base_size = 16, base_family = 'Times')

ggsave("plot3.png", plot = plot3, dpi = 600, 
       device = "png", path = "./figs", 
       height = 6, width = 9, units = "in")

# filter de los L2
df%>% 
  filter(GROUP == "L2SPANISH" | GROUP == "L2HEB", Frequent == "f") %>% 
  ggplot( aes(x= EPT, y=MINT, fill = factor(EPT))) + 
  geom_boxplot() +
  coord_flip()

#MINT CON LOS TRES TASKS L2SPAN con frecuencia

df%>% 
  filter(GROUP == "L2SPANISH" , Frequent == "f") %>% 
  ggplot( aes(x= EPT, y=MINT, fill = factor(EPT))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x = 'EPT', y = 'MINT', caption = '', 
       title = 'a) Proportion of EPT responses across conditions by MINT with frequent items')

df%>% 
  filter(GROUP == "L2SPANISH" , Frequent == "f") %>% 
  ggplot( aes(x= FCTASG, y=MINT, fill = factor(FCTASG))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x = 'FCTASG', y = 'MINT', caption = '', 
                    title = 'a) Proportion of FCTASG responses across conditions by MINT with frequent items')

df%>% 
  filter(GROUP == "L2SPANISH" , Frequent == "f") %>% 
  ggplot( aes(x= FCTAGR, y=MINT, fill = factor(FCTAGR))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x = 'FCTAGR', y = 'MINT', caption = '', 
       title = 'a) Proportion of FCTAGR responses across conditions by MINT with frequent items')


# MINT con L2span no freqcuente

df%>% 
  filter(GROUP == "L2SPANISH" , Frequent == "nf") %>% 
  ggplot( aes(x= EPT, y=MINT, fill = factor(EPT))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x = 'EPT', y = 'MINT', caption = '', 
       title = 'b) Proportion of EPT responses across conditions by MINT with non- frequent items')

df%>% 
  filter(GROUP == "L2SPANISH" , Frequent == "nf") %>% 
  ggplot( aes(x= FCTASG, y=MINT, fill = factor(FCTASG))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x = 'FCTASG', y = 'MINT', caption = '', 
       title = 'b) Proportion of FCTASG responses across conditions by MINT with non-frequent items')

df%>% 
  filter(GROUP == "L2SPANISH" , Frequent == "nf") %>% 
  ggplot( aes(x= FCTAGR, y=MINT, fill = factor(FCTAGR))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x = 'FCTAGR', y = 'MINT', caption = '', 
       title = 'b) Proportion of FCTAGR responses across conditions by MINT with non-frequent items')


#MINT CON L2HEB EN LOS TRE TASKS FRECUENTE


df%>% 
  filter(GROUP == "L2HEB" , Frequent == "f") %>% 
  ggplot( aes(x= EPT, y=MINT, fill = factor(EPT))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x = 'EPT', y = 'MINT', caption = '', 
     title = 'c) Proportion of EPT responses across conditions by MINT with frequent items')

df%>% 
  filter(GROUP == "L2HEB" , Frequent == "f") %>% 
  ggplot( aes(x= FCTASG, y=MINT, fill = factor(FCTASG))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x = 'FCTASG', y = 'MINT', caption = '', 
     title = 'c) Proportion of FCTASG responses across conditions by MINT with frequent items')

df%>% 
  filter(GROUP == "L2HEB" , Frequent == "f") %>% 
  ggplot( aes(x= FCTAGR, y=MINT, fill = factor(FCTAGR))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x = 'FCTAGR', y = 'MINT', caption = '', 
     title = 'c) Proportion of FCTAGR responses across conditions by MINT with frequent items')

# MIINT CON LOS TRES TASK HEBREO NF

df%>% 
  filter(GROUP == "L2HEB" , Frequent == "nf") %>% 
  ggplot( aes(x= EPT, y=MINT, fill = factor(EPT))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x = 'EPT', y = 'MINT', caption = '', 
     title = 'd) Proportion of EPT responses across conditions by MINT with non-frequent items')

df%>% 
  filter(GROUP == "L2HEB" , Frequent == "nf") %>% 
  ggplot( aes(x= FCTASG, y=MINT, fill = factor(FCTASG))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x = 'FCTASG', y = 'MINT', caption = '', 
     title = 'd) Proportion of FCTASG responses across conditions by MINT with non-frequent items')

df%>% 
  select(GROUP, Frequent, FCTAGR, MINT)  %>%
  filter(GROUP == "L2HEB" , Frequent == "nf") %>% 
  drop_na() %>% 
  ggplot( aes(x= FCTAGR, y=MINT, fill = factor(FCTAGR))) + 
  geom_boxplot() +
  coord_flip() +
  labs(x = 'FCTAGR', y = 'MINT', caption = '', 
     title = 'd) Proportion of FCTAGR responses across conditions by MINT with non-frequent items')




# la mediana de los que contestaron 1 es 35 
# cambiar factor 


# medias de MINT por grupo
df %>% 
  group_by(., GROUP ) %>% 
  summarize(., accuracy = mean(na.omit(MINT)), sd = sd(na.omit(MINT)))




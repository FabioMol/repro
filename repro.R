### =============================
###
### Name: repro
###
### Project: Reproducibility in Invasion Ecology
###
### Purpose: Analyses
###
### Author: Dr. Fabio Mologni
###
### Date: 2023-07-13
###
### =============================
###
### Notes:
### 
### =============================

###Woring directory

setwd("G:/My Drive/2. Work/Research/Reproducibility/repro-analyses")

### Print more records

options(max.print=10000)

### =============================

### packages

library(ggplot2)
library(tidyverse)
library(ggpubr)
library(lme4)

### data

STUDY = read_csv("./by-study.csv")


### =====================================
### ============= By Study ==============
### =====================================

hist(STUDY$score)
hist(STUDY$year)
hist(log(STUDY$year))

repro.random = lmer(score~log(year)+species+(1|journal)+(1|author), STUDY)
summary(repro.random)

repro.year = lm(score~log(year), STUDY)
summary(repro.year)

ggplot(STUDY, aes(log(year), score)) +
  geom_point(aes(size=2)) +
  stat_smooth(method = "lm", se = F, colour = 'black', size=2) +
  labs (x= "log (Year of Publication)", y = "Score") +
  stat_regline_equation(label.y = 0.55, size=8, aes(label = after_stat(eq.label))) +
  stat_regline_equation(label.y = 0.6, size=8, aes(label = ..rr.label..)) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 25, colour='black'),
        axis.title.x = element_text(size = 25, colour='black'),
        axis.text.y = element_text(size = 20, colour='black'),
        axis.text.x = element_text(size = 20, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())



### =====================================
### ============= By Score ==============
### =====================================

#three - data avilability statement
repro.three = glm(three_data~(year), family=quasipoisson, STUDY)
summary(repro.three) #dispersion parameter ~1, poisson is preferred model

repro.three2 = glm(three_data~(year), family=poisson, STUDY)
summary(repro.three2) #NS

#eleven - randomization
repro.eleven = glm(eleven_rando~(year), family=quasipoisson, STUDY)
summary(repro.eleven)

repro.eleven2 = glm(eleven_rando~(year), family=poisson, STUDY)
summary(repro.eleven2)

#twelve - study authorization
STUDY.12 = na.omit(STUDY)

repro.tewlve = glm(twelve_permit~(year), family=quasipoisson, STUDY.12)
summary(repro.tewlve)

repro.tewlve2 = glm(twelve_permit~(year), family=poisson, STUDY.12)
summary(repro.tewlve2)

#thirteen - statistics
STUDY.13 = na.omit(STUDY)

repro.thirteen = glm(thirteen_stats~(year), family=quasipoisson, STUDY.13)
summary(repro.thirteen)

repro.thirteen2 = glm(thirteen_stats~(year), family=poisson, STUDY.13)
summary(repro.thirteen2)

#fourteen - assumptions
STUDY.14 = na.omit(STUDY)

repro.fuorteen = glm(fourteen_assump~(year), family=quasipoisson, STUDY.14)
summary(repro.fuorteen)

repro.fuorteen2 = glm(fourteen_assump~(year), family=poisson, STUDY.14)
summary(repro.fuorteen2)
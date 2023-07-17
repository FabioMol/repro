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

setwd("G:/My Drive/2. Work/Research/Reproducibility/repro-analyses/repro")

### Print more records

options(max.print=10000)

### =============================

### packages

library(ggplot2)
library(tidyverse)
library(ggpubr)
library(lme4)
library(pscl)

### data

STUDY = read_csv("./by-study.csv")


### =====================================
### ============= PREMISES ==============
### =====================================

# We are using Sophia/Bronte/Jordan reproducibility checklist
# Against the papers on impacts extracted for our 2 pilot species
# Russian Olive and Reed Canary Grass

### =====================================
### ============== SCORES ===============
### =====================================

# (1) Identify average score
mean(STUDY$score)

### =====================================
### ============= BY STUDY ==============
### =====================================

# (2) Do scores increase over time? 
# Is there an effect of a) species and b) author and journals?
# used lmer and glm


hist(STUDY$score)
hist(STUDY$year)
hist(log(STUDY$year))

#scores ~ year + fixed and random factors
repro.random = lmer(score~log(year)+species+(1|journal)+(1|author), STUDY)
summary(repro.random)

#scores ~ year
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
### ============= BY ITEM ===============
### =====================================

# (3) What are the areas of weakness and strength? 
# Only areas were we scored between 0.1 and 0.9 were included in
# the following analyses. These areas are 3, 11, 12, 13, 14
# scores by item can be calculated by using mean function (remove nas first). 



# (4) Which scores of 3, 11, 12, 13, and 14 increased over time?
#used glm and zero inflated poisson models


###____________three______________
#three - data availability statement
hist((STUDY$three_data))
plot(three_data~year, STUDY)

repro.three = glm(three_data~(year), family=quasipoisson, STUDY)
summary(repro.three) #NS, dispersion parameter ~1 (poisson)

repro.three2 = glm(three_data~(year), family=poisson, STUDY)
summary(repro.three2) #NS

zip_model_three <- zeroinfl(three_data ~ log(year), data = STUDY, dist = "poisson")
summary(zip_model_three) #not working



###__________eleven____________
#eleven - randomization
hist((STUDY$eleven_rando))
plot(eleven_rando~year, STUDY)

repro.eleven = glm(eleven_rando~(year), family=quasipoisson, STUDY)
summary(repro.eleven) #NS, underdispersd (quasi)

repro.eleven2 = glm(eleven_rando~(year), family=poisson, STUDY)
summary(repro.eleven2) #NS

zip_model_eleven <- zeroinfl(eleven_rando ~ log(year), data = STUDY, dist = "poisson")
summary(zip_model_eleven) #NS, log(year) otherwise too much separation



###__________twelve____________
###twelve - study authorization
STUDY.12 = na.omit(STUDY)

hist((STUDY.12$twelve_permit))
plot(twelve_permit~year, STUDY.12)

repro.tewlve = glm(twelve_permit~(year), family=quasipoisson, STUDY.12)
summary(repro.tewlve) #NS, slighly overdispersed (either)

repro.tewlve2 = glm(twelve_permit~(year), family=poisson, STUDY.12)
summary(repro.tewlve2) #NS

zip_model_twelve <- zeroinfl(twelve_permit ~ log(year), data = STUDY, dist = "poisson")
summary(zip_model_twelve) #NS, log(year) otherwise too much separation



###_________thirteen___________
#thirteen - statistics
STUDY.13 = na.omit(STUDY)

hist((STUDY.13$thirteen_stats)) #is poisson appropriate here?
plot(thirteen_stats~year, STUDY.13)

repro.thirteen = glm(thirteen_stats~(year), family=quasipoisson, STUDY.13)
summary(repro.thirteen) #Significant, underdispersed (quasi)

repro.thirteen2 = glm(thirteen_stats~(year), family=poisson, STUDY.13)
summary(repro.thirteen2) #NS



###_________fourteen___________
#fourteen - assumptions
STUDY.14 = na.omit(STUDY)

hist((STUDY.14$fourteen_assump)) #is poisson appropriate here?
plot(fourteen_assump~year, STUDY.14)

repro.fuorteen = glm(fourteen_assump~(year), family=quasipoisson, STUDY.14)
summary(repro.fuorteen) #Significant, underdispersed (quasi)

repro.fuorteen2 = glm(fourteen_assump~(year), family=poisson, STUDY.14)
summary(repro.fuorteen2) #Significant



###Figures

library(gridExtra)

plot3 = ggplot(STUDY, aes((year), three_data)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  labs (x= "Publication year", y = "Three - Data avilability statement") +
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

plot11 = ggplot(STUDY, aes((year), eleven_rando)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  labs (x= "Publication year", y = "Eleven - Randomization") +
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

plot12 = ggplot(STUDY, aes((year), twelve_permit)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  labs (x= "Publication year", y = "Twelve - Study Authorization") +
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

plot13 = ggplot(STUDY, aes((year), thirteen_stats)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  labs (x= "Publication year", y = "Thirteen - Statistics") +
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

plot14 = ggplot(STUDY, aes((year), fourteen_assump)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  labs (x= "Publication year", y = "Fourteen - Assumptions") +
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

grid.arrange(plot3, plot11, plot12, plot13, plot14) #no trendline for 14.

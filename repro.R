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
library(gridExtra)

### data

STUDY = read_csv("./scores.csv")


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
# Is there an effect of species? 
# used glm and visual representation


hist(STUDY$score)
hist(STUDY$year)
hist(log(STUDY$year))

#hist figure
fig2.a = ggplot(STUDY, aes(x = year)) +
  geom_histogram(binwidth = 5, fill = "grey", color = "black", alpha = 0.7) +
  labs(title = "", x = "Publication Year", y = "Number of Studies") +
  scale_x_continuous(breaks = c(1940, 1960, 1980, 2000, 2020))+
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 25, colour='black'),
        axis.title.x = element_text(size = 25, colour='black'),
        axis.text.y = element_text(size = 20, colour='black'),
        axis.text.x = element_text(size = 20, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')+ 
theme(legend.title=element_blank())


#GLM all studies
repro.year = lm(score~log(year)+species, STUDY)
summary(repro.year)

repro.year = lm(score~log(year), STUDY)
summary(repro.year)

fig2.b = ggplot(STUDY, aes(log(year), score)) +
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

#Figure 2
grid.arrange(fig2.a, fig2.b)


#GLM w/o <1990

STUDY.1 <- subset(STUDY, year > 1990)

hist(STUDY.1$year)
hist(log(STUDY.1$year))

repro.year.1 = lm(score~log(year), STUDY.1)
summary(repro.year.1)

ggplot(STUDY.1, aes(log(year), score)) +
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


#GLM w/o shared authorship

STUDY.2 <- subset(STUDY, duplicates != 1)

hist(STUDY.2$year)
hist(log(STUDY.2$year))

repro.year.2 = lm(score~log(year), STUDY.2)
summary(repro.year.2)

ggplot(STUDY.2, aes(log(year), score)) +
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



#Spearman rank correlation

#w/ <1990
cor_test <- cor.test(STUDY$score, STUDY$year, method = "spearman", exact = FALSE)
print(cor_test)

# Print the conclusion based on the p-value
if (cor_test$p.value < 0.05) {
  print("The correlation is statistically significant.")
} else {
  print("The correlation is not statistically significant.")
}


#w/o <1990
cor_test2 <- cor.test(STUDY.1$score, STUDY.1$year, method = "spearman", exact = FALSE)
print(cor_test2)

# Print the conclusion based on the p-value
if (cor_test2$p.value < 0.05) {
  print("The correlation is statistically significant.")
} else {
  print("The correlation is not statistically significant.")
}

#w/o shared authorship
cor_test3 <- cor.test(STUDY.2$score, STUDY.2$year, method = "spearman", exact = FALSE)
print(cor_test3)

# Print the conclusion based on the p-value
if (cor_test3$p.value < 0.05) {
  print("The correlation is statistically significant.")
} else {
  print("The correlation is not statistically significant.")
}

### =====================================
### ============= BY ITEM ===============
### =====================================

# (3) What are the areas of weakness and strength? 

###Plots for figure including all scores separately

plot1 = ggplot(STUDY, aes((year), one_registration)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = max(STUDY$three_data),
            label = "Score = 0.00", color = "Red", size = 5) +
  labs (x= "", y = "Registration") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot2 = ggplot(STUDY, aes((year), two_material)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = max(STUDY$three_data),
            label = "Score = 0.05", color = "Red", size = 5) +
  labs (x= "", y = "Material") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot3 = ggplot(STUDY, aes((year), three_data)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = max(STUDY$three_data),
            label = "Score = 0.14", color = "Orange", size = 5) +
  labs (x= "", y = "Data avilability") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot4 = ggplot(STUDY, aes((year), four_datalink)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = max(STUDY$three_data),
            label = "Score = 0.05", color = "Red", size = 5) +
  labs (x= "", y = "Data link") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot5 = ggplot(STUDY, aes((year), five_datalicence)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = max(STUDY$three_data),
            label = "Score = 0.02", color = "Red", size = 5) +
  labs (x= "", y = "Data licence") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot6 = ggplot(STUDY, aes((year), six_code)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = max(STUDY$three_data),
            label = "Score = 0.03", color = "Red", size = 5) +
  labs (x= "", y = "Code avilability") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot7 = ggplot(STUDY, aes((year), seven_codelink)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = max(STUDY$three_data),
            label = "Score = 0.00", color = "Red", size = 5) +
  labs (x= "", y = "Code link") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot8 = ggplot(STUDY, aes((year), eight_codelicence)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = max(STUDY$three_data),
            label = "Score = 0.00", color = "Red", size = 5) +
  labs (x= "", y = "Code licence") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot9 = ggplot(STUDY, aes((year), nine_protocol)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = 1.8,
            label = "Score = 0.95", color = "Dark green", size = 5) +
  labs (x= "", y = "Protocol") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot10 = ggplot(STUDY, aes((year), ten_sample)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = 1.8,
            label = "Score = 0.97", color = "Dark green", size = 5) +
  labs (x= "", y = "Sample") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot11 = ggplot(STUDY, aes((year), eleven_rando)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = max(STUDY$three_data),
            label = "Score = 0.36", color = "Orange", size = 5) +
  labs (x= "", y = "Randomization") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot12 = ggplot(STUDY, aes((year), twelve_permit)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = max(STUDY$three_data),
            label = "Score = 0.18", color = "Orange", size = 5) +
  labs (x= "", y = "Permit") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot13 = ggplot(STUDY, aes((year), thirteen_stats)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = max(STUDY$three_data),
            label = "Score = 0.85", color = "Orange", size = 5) +
  labs (x= "", y = "Statistics") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot14 = ggplot(STUDY, aes((year), fourteen_assump)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  geom_text(x = 1955, y = max(STUDY$three_data),
            label = "Score = 0.63", color = "Orange", size = 5) +
  labs (x= "Publication year", y = "Assumptions") +
  ylim(0, 2) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10,
             plot11, plot12, plot13, plot14)



#Draft of modelling if needed

# Only areas were we scored between 0.1 and 0.9 were included in
# the following analyses. These areas are 3, 11, 12, 13, 14
# scores by item can be calculated by using mean function (remove nas first). 

# (4) Which scores of 3, 11, 12, 13, and 14 increased over time?
#used glm and zero inflated poisson models

#Can't use binomial logit link, again needs to be be 0 or 1

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

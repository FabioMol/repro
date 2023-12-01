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
### Date: 2023-11-30
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

# We used a reproducibility checklist adapted from Kast et al. 2023
# to assess the level of reproducibility of a set of studies 
# assessing the impacts of Russian Olive and Reed Canary Grass
# in rpiaria ecosystems of British Columbia

### =====================================
### ============== SCORES ===============
### =====================================

# (1) Identify average score
mean(STUDY$score)

### =====================================
### ============= BY STUDY ==============
### =====================================

# (2) Do scores change over time? 
# Is there an effect of species? 

#histograms
hist(STUDY$score)
hist(STUDY$year)
hist(log(STUDY$year))

#fig2.a - hist showing number of studies over the years
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


#GLM scores ~ time for all studies
repro.year = lm(score~log(year)+species, STUDY)
summary(repro.year)

repro.year = lm(score~log(year), STUDY)
summary(repro.year)

#fig2.b - regression showing scores over the years
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

#arranging both fig2.a & fig2.b
grid.arrange(fig2.a, fig2.b)


#GLM scores ~ time w/o <1990

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


#GLM scores ~ time w/o studies that shared authorship

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



#Spearman rank correlation - scores ~ time

#for all studies
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

# List of variable names (checklist items) for which you want to calculate the mean
variable_names <- c("one_registration", "two_material", "three_data", 
                    "four_datalink", "five_datalicence", "six_code", 
                    "seven_codelink", "eight_codelicence", "nine_protocol",
                    "ten_sample", "eleven_rando", "twelve_permit",
                    "thirteen_stats", "fourteen_assump")

# Initialize an empty vector to store the means
means <- numeric(length(variable_names))

# Loop through the variable names (checklist items) and calculate the mean
for (i in seq_along(variable_names)) {
  var_name <- variable_names[i]
  mean_value <- mean(STUDY[[var_name]], na.rm = TRUE)
  rounded_mean <- round(mean_value, 2)
    means[i] <- rounded_mean
}

#assign scores_categories to each variable
scores_categories <- c("overlooked", "overlooked", "overlooked", 
                    "overlooked", "overlooked", "overlooked", 
                    "overlooked", "overlooked", "addressed",
                    "overlooked", "in progress", "overlooked",
                    "addressed", "in progress")

#create a new_df
ITEMS_means <- data.frame(means = means, items = variable_names, categories = scores_categories)

#reorder variables (checklist items)
ITEMS_means$items <- factor(ITEMS_means$items, levels = c("one_registration", "two_material", "three_data", 
                                                  "four_datalink", "five_datalicence", "six_code", 
                                                  "seven_codelink", "eight_codelicence", "nine_protocol",
                                                  "ten_sample", "eleven_rando", "twelve_permit",
                                                  "thirteen_stats", "fourteen_assump"))

terrain.colors(5)

#Plot the means, color by category
#Colour-blind friendly palette
plot0 = ggplot(ITEMS_means, aes(x = items, y = means, fill=categories)) +
  geom_bar(stat = "identity", colour = "black", alpha = 0.7) +
  labs(title = "", x = "Checklist items", y = "Score means")+
  scale_fill_manual(values = c("#006B05", "#EAB64E", "#E22C2C"))+
  scale_x_discrete(
    labels = c("1", "2", "3", "4", "5", "6", "7", "8",
               "9", "10", "11", "12", "13", "14")) +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')+ 
  theme(legend.title=element_blank())



###Plots scores by study for each variable (checklist items) separately

plot1 = ggplot(STUDY, aes((year), one_registration)) +
  geom_point(aes(size=2), shape=1, stroke=1) +
  labs (x= "", y = "1 - Registration") +
  ylim(0, 1) +
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
  labs (x= "", y = "2 - Material") +
  ylim(0, 1) +
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
  labs (x= "", y = "3 - Data avilability") +
  ylim(0, 1) +
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
  labs (x= "", y = "4 - Data link") +
  ylim(0, 1) +
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
  labs (x= "", y = "5 - Data licence") +
  ylim(0, 1) +
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
  labs (x= "", y = "6 - Code avilability") +
  ylim(0, 1) +
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
  labs (x= "", y = "7 - Code link") +
  ylim(0, 1) +
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
  labs (x= "", y = "8 - Code licence") +
  ylim(0, 1) +
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
  labs (x= "", y = "9 - Protocol") +
  ylim(0, 1) +
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
  labs (x= "", y = "10 - Sample") +
  ylim(0, 1) +
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
  labs (x= "", y = "11 - Randomization") +
  ylim(0, 1) +
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
  labs (x= "", y = "12 - Permit") +
  ylim(0, 1) +
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
  labs (x= "", y = "13 - Statistics") +
  ylim(0, 1) +
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
  labs (x= "Publication year", y = "14 - Assumptions") +
  ylim(0, 1) +
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

grid.arrange(plot0, plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10,
             plot11, plot12, plot13, plot14)


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
library(dplyr)

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

# (1) Identify average, min, max scores
mean(STUDY$score)
min(STUDY$score)
max(STUDY$score)

### =====================================
### ============= BY STUDY ==============
### =====================================

# Averaging scores by year
STUDY_av <- STUDY %>%
  group_by(year) %>%
  summarise(
    score_av = mean(score, na.rm = TRUE),
    study_count = n()
  )

print(score_av)

# Create a plot with both average score and study count
fig.2 <- ggplot(STUDY_av, aes(year)) +
  geom_bar(aes(y = study_count, fill = "Number of Studies"), alpha = 0.5, stat = "identity") +
  geom_line(aes(y = score_av * 5, color = "Average Score"), size = 2) +
  labs(x = "Year of Publication", y = "Number of Studies") +
  scale_fill_manual(values = "grey", name = "") +
  scale_color_manual(values = "black", name = "") +
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 25, colour = 'black'),
    axis.title.x = element_text(size = 25, colour = 'black'),
    axis.text.y = element_text(size = 20, colour = 'black'),
    axis.text.x = element_text(size = 20, colour = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'none'
  ) +
  scale_y_continuous(
    name = "Number of Studies",
    breaks = seq(0, 5, 1),
    sec.axis = sec_axis(~./5, name = "Average Score", breaks = seq(0, 1, 0.2))
  )

# Show the combined plot
print(fig.2)



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

#Plot the means, color by category
#Colour-blind friendly palette

ggplot(ITEMS_means, aes(x = means, y = items, fill = categories)) +
  geom_bar(stat = "identity", colour = "black", alpha = 0.7) +
  labs(title = "", x = "Score means", y = "Checklist items") +
  scale_fill_manual(values = c("black", "grey", "white")) +
  scale_y_discrete(
    labels = c("14 - Assumptions", "13 - Statistics", "12 - Study authorization",
               "11 - Rendomization", "10 - Sample size", "9 - Experimental protocol",
               "8 - Code licence", "7 - Code link", "6 - Code availability statement",
               "5 - Data licence",  "4 - Data link", "3 - Data availability statement",
               "2 - Material availability statement", "1 - Registration"),
    limits = rev(levels(ITEMS_means$items))) +
  theme_bw() +
  theme(
    plot.title = element_blank(),
    axis.title.y = element_text(size = 30, colour = 'black'),
    axis.title.x = element_text(size = 30, colour = 'black'),
    axis.text.y = element_text(size = 20, colour = 'black'),
    axis.text.x = element_text(size = 20, colour = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'none',
    legend.title = element_blank())


#this for supplementary matetrial figure
plot0 = ggplot(ITEMS_means, aes(x = means, y = items, fill = categories)) +
  geom_bar(stat = "identity", colour = "black", alpha = 0.7) +
  labs(title = "", x = "Score means", y = "Checklist items") +
  scale_fill_manual(values = c("black", "grey", "white")) +
  scale_y_discrete(
    labels = c("14", "13", "12", "11", "10", "9", "8", "7",
               "6", "5", "4", "3", "2", "1"),
    limits = rev(levels(ITEMS_means$items))) +
  theme_bw() +
  theme(
    plot.title = element_blank(),
    axis.title.y = element_text(size = 20, colour = 'black'),
    axis.title.x = element_text(size = 20, colour = 'black'),
    axis.text.y = element_text(size = 10, colour = 'black'),
    axis.text.x = element_text(size = 10, colour = 'black'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'none',
    legend.title = element_blank())



###Plots scores by study for each variable (checklist items) separately

plot1 = ggplot(STUDY, aes((year), one_registration)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
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
  geom_point(aes(size=1.5), alpha = 0.3) +
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
  geom_point(aes(size=1.5), alpha = 0.3) +
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
  geom_point(aes(size=1.5), alpha = 0.3) +
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
  geom_point(aes(size=1.5), alpha = 0.3) +
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
  geom_point(aes(size=1.5), alpha = 0.3) +
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
  geom_point(aes(size=1.5), alpha = 0.3) +
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
  geom_point(aes(size=1.5), alpha = 0.3) +
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
  geom_point(aes(size=1.5), alpha = 0.3) +
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
  geom_point(aes(size=1.5), alpha = 0.3) +
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
  geom_point(aes(size=1.5), alpha = 0.3) +
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
  geom_point(aes(size=1.5), alpha = 0.3) +
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
  geom_point(aes(size=1.5), alpha = 0.3) +
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

plot14 = ggplot(STUDY, aes(year, fourteen_assump)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
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


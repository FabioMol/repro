###############################################################################################.
##### -----------------------------------   SCRIPT   -----------------------------------  #####
#####                                    Repro script                                     #####.
###############################################################################################.

#### -----------------------------------------------------------------------------------  #####.
#### A) Metadata
# Name of author(s):       Dr. Fabio Mologni
# Date of creation:        2023-30-11
# Date of latest update:   2025-06-21
# Contact:                 fabio.mologni@gmail.com

## Description of content
# Script for the manuscript 'Transparency and Reproducibility in Invasion Science'
# Script for the manuscript 'Assessing Transparency and Reproducibility in Invasion Science'
# We used a reproducibility checklist to assess the level of reproducibility 
# of a set of studies assessing the impacts of Russian Olive and Reed Canarygrass
# in riparian ecosystems of British Columbia

#### -----------------------------------------------------------------------------------  #####.
#### B) Dependencies & Versions

## R/RStudio Version
#  R version 4.4.1

## Packages
library(ggplot2)       # ver 3.5.1
library(tidyverse)     # ver 2.0.0
library(ggpubr)        # ver 0.6.0
library(lme4)          # ver 1.1.35.5
library(pscl)          # ver 1.5.9
library(gridExtra)     # ver 2.3
library(dplyr)         # ver 1.1.4
library(DiagrammeR)    # ver 1.0.11
library(DiagrammeRsvg) # ver 0.1
library(rsvg)          # ver 2.6.2


#### -----------------------------------------------------------------------------------  #####.
#### C) Code

STUDY = read_csv("./scores.csv")

### =====================================
### ============== SCORES ===============
### =====================================

# convert scores to percentage
STUDY$score_per <- STUDY$score * 100

# identify average, min, max scores
mean(STUDY$score_per)
min(STUDY$score_per)
max(STUDY$score_per)

### =====================================
### ============= BY STUDY ==============
### =====================================

# averaging scores by year
STUDY_av <- STUDY %>%
  group_by(year) %>%
  summarise(
    score_av = mean(score_per, na.rm = TRUE),
    study_count = n()
  )

print(STUDY_av)


# Fig.2 - plot with average score (primary y-axis) and study count (secodnary) over time
fig.2 <- ggplot(STUDY_av, aes(year)) +
  geom_bar(aes(y = study_count*20, fill = "Number of Studies"), alpha = 0.5, stat = "identity") +
  geom_line(aes(y = score_av, color = "Average Score"), size = 2) +
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
    breaks = seq(0, 100, 20),
    labels = function(x) x / 20,
    sec.axis = sec_axis(~., name = "Average Score (%)", breaks = seq(0, 100, 20))
  )
print(fig.2)

# Save
ggsave("Figure 2.png", plot = fig.2, width = 880, height = 880, units = "px", dpi = 105)
ggsave("Figure 2.pdf", plot = fig.2, width = 8.38, height = 8.38, units = "in")



### =====================================
### ============= BY ITEM ===============
### =====================================

# What are the areas of weakness and strength? 

# List of variable names (checklist items) for which you want to calculate the mean
variable_names <- c("one_registration", "two_material", "three_data", 
                    "four_datalink", "five_datalicence", "six_code", 
                    "seven_codelink", "eight_codelicence", "nine_protocol",
                    "ten_sample", "eleven_rando", "twelve_permit",
                    "thirteen_stats", "fourteen_assump")

# Create an empty vector to store the means
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
ITEMS_means <- data.frame(means = means*100, items = variable_names, categories = scores_categories)

#reorder variables (checklist items)
ITEMS_means$items <- factor(ITEMS_means$items, levels = c("one_registration", "two_material", "three_data", 
                                                  "four_datalink", "five_datalicence", "six_code", 
                                                  "seven_codelink", "eight_codelicence", "nine_protocol",
                                                  "ten_sample", "eleven_rando", "twelve_permit",
                                                  "thirteen_stats", "fourteen_assump"))

#fig.3 - Plot the mean score by item, color by category (addressed, overlooked, etc)
fig.3 <- ggplot(ITEMS_means, aes(x = means, y = items, fill = categories)) +
  geom_bar(stat = "identity", colour = "black", alpha = 0.7) +
  labs(title = "", x = "Score means (%)", y = "Checklist items") +
  scale_fill_manual(values = c("black", "grey", "white")) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  scale_y_discrete(
    labels = c("14 - Assumptions", "13 - Statistics", "12 - Study authorization",
               "11 - Randomization", "10 - Sample size", "9 - Experimental protocol",
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
print(fig.3)

# Save
ggsave("Figure 3.png", plot = fig.3, width = 1540, height = 990, units = "px", dpi = 105)
ggsave("Figure 3.pdf", plot = fig.3, width = 14.67, height = 9.43, units = "in")


#fig.S1 - supplementary material figure
plot0 = ggplot(ITEMS_means, aes(x = means, y = items, fill = categories)) +
  geom_bar(stat = "identity", colour = "black", alpha = 0.7) +
  labs(title = "", x = "Score means (%)", y = "Checklist items") +
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

#plots scores by study for each variable (checklist items) separately
repro_theme <-   theme_bw() +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 20, colour='black'),
        axis.title.x = element_text(size = 20, colour='black'),
        axis.text.y = element_text(size = 10, colour='black'),
        axis.text.x = element_text(size = 10, colour='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  theme(legend.title=element_blank())

plot1 = ggplot(STUDY, aes((year), one_registration)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "", y = "1 - Registration (%)") +
  ylim(0, 1) + repro_theme

plot2 = ggplot(STUDY, aes((year), two_material)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "", y = "2 - Material (%)") +
  ylim(0, 1) + repro_theme

plot3 = ggplot(STUDY, aes((year), three_data)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "", y = "3 - Data avilability (%)") +
  ylim(0, 1) + repro_theme

plot4 = ggplot(STUDY, aes((year), four_datalink)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "", y = "4 - Data link (%)") +
  ylim(0, 1) + repro_theme

plot5 = ggplot(STUDY, aes((year), five_datalicence)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "", y = "5 - Data licence (%)") +
  ylim(0, 1) + repro_theme

plot6 = ggplot(STUDY, aes((year), six_code)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "", y = "6 - Code avilability (%)") +
  ylim(0, 1) + repro_theme

plot7 = ggplot(STUDY, aes((year), seven_codelink)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "", y = "7 - Code link (%)") +
  ylim(0, 1) + repro_theme

plot8 = ggplot(STUDY, aes((year), eight_codelicence)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "", y = "8 - Code licence (%)") +
  ylim(0, 1) + repro_theme

plot9 = ggplot(STUDY, aes((year), nine_protocol)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "", y = "9 - Protocol (%)") +
  ylim(0, 1) + repro_theme

plot10 = ggplot(STUDY, aes((year), ten_sample)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "", y = "10 - Sample (%)") +
  ylim(0, 1) + repro_theme

plot11 = ggplot(STUDY, aes((year), eleven_rando)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "", y = "11 - Randomization (%)") +
  ylim(0, 1) + repro_theme

plot12 = ggplot(STUDY, aes((year), twelve_permit)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "", y = "12 - Permit (%)") +
  ylim(0, 1) + repro_theme

plot13 = ggplot(STUDY, aes((year), thirteen_stats)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "", y = "13 - Statistics (%)") +
  ylim(0, 1) + repro_theme

plot14 = ggplot(STUDY, aes(year, fourteen_assump)) +
  geom_point(aes(size=1.5), alpha = 0.3) +
  labs (x= "Publication year (%)", y = "14 - Assumptions") +
  ylim(0, 1) + repro_theme

fig.s1 <- grid.arrange(plot0, plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10,
             plot11, plot12, plot13, plot14)
print(fig.s1)

# Save
ggsave("Figure S1.png", plot = fig.s1, width = 1650, height = 1375, units = "px", dpi = 105)
ggsave("Figure S1.pdf", plot = fig.s1, width = 15.71, height = 13.10, units = "in")


### =====================================
### ============ FLOWCHART ==============
### =====================================

# 1. Header
dot_header <- "digraph screening_flowchart {"

# 2. Overall layout and appearance
dot_graph_attrs <- "
  graph [
    layout = dot,      # Algorithm for node placement (hierarchical)
    rankdir = TB,      # Direction of flow: Top to Bottom
    overlap = false,   # Prevent nodes from overlapping
    splines = false,   # Lines are straight segments (removes curves)
    nodesep = 0.6,     # Minimum space between nodes on the same rank
    ranksep = 1.0      # Minimum vertical space between ranks (layers)
  ];"

# 3. Default Node Attributes: Defines the default appearance for most boxes
dot_default_node_attrs <- "
  node [
    shape = box,               # Shape of the node
    style = 'rounded,filled',  # Rounded corners and filled with color
    fontname = Helvetica,      # Font for text within nodes
    fillcolor = white,         # Default fill color for nodesa
    margin = '0,0.15',         # Padding inside nodes (horizontal, vertical)
    width = 3.5,               # Default width of nodes
    height = 0.7               # Default height of nodes
  ];"

# 4. Source Node Definitions: Defines the individual data source boxes
dot_source_nodes <- "
  // Specific style for source nodes
  node [shape = box, fillcolor=whitesmoke, fontcolor=black, width=3, height=0.6,
  style = 'rounded,filled'];
  src_1 [label = 'Records identified from\\nbibliographic database searches\\n\\n(RO = 402, RCG = 474)'];
  src_other [label = 'Pre-screened articles\\nfrom other sources\\n\\n(RO = 0, RCG = 1)']; // This is the box on the right
"

# 5. Main Flow Node Definitions: Defines the boxes for the screening process
# Resetting node style to default for these, if changed by source_nodes
dot_main_flow_nodes <- "
  // Reset to default box style for main flow if needed
  node [shape = box, style = 'rounded,filled', fontname = Helvetica, fillcolor = white, width = 3, height = 0.7];

  node_initial [label = 'Records after\\nduplicates removed\\n\\n(RO = 369, RCG = 459)'];
  node_after_duplicates [label = 'Records after\\ntitle & abstract screening\\n\\n(RO = 50, RCG = 77)'];
  node_after_title_abstract [label = 'Records after\\nfull text screening\\n\\n(RO = 27, RCG = 32)'];
  node_included_map [label = 'Studies included in the\\nsystematic map\\n\\n(RO = 27, RCG = 33)'];
  node_included_reproducibility [label = 'Studies included in the\\nreproducibility assessment\\n\\n(n = 49)',
    fillcolor = black,
    fontcolor = white
  ];"

# 6. Exclusion Reason Node Definitions: Defines the boxes for exclusion details
dot_exclusion_nodes <- "
  // Style for exclusion reason boxes (notes)
  node [shape = box, fillcolor = lightgrey, width = 3,
  height = 0.6, fontname=Helvetica, style = 'rounded,filled'];
    reason_duplicates [label = 'Duplicates removed\\n\\n(RO = 33, RCG = 15)'];
    reason_title_abstract [label = 'Excluded titles & abstracts\\n\\n(RO = 319, RCG = 382)\\n\\n- Not accessible\\n(RO = 16, RCG = 12)\\n\\n- Not found\\n(RO = 24, RCG = 10)\\n\\n- Not in English\\n(RO = 4, RCG = 0)\\n'];
    reason_full_text [label = 'Excluded full text\\n\\n(RO = 23, RCG = 45)'];
    reason_merged_pre2000 [label = 'Excluded after merging lists\\nand removing pre-2000 records\\n\\n(n = 11)'];"

# 7. Invisible Node Definitions: Helper nodes for layout
dot_invisible_nodes <- "
  // Invisible nodes for guiding edges to side boxes
  node [shape=point, width=0.01, height=0.01, style=invis]; // Making them truly invisible
  inv_dup;
  inv_title_abstract;
  inv_full_text;
  inv_merged_pre2000;
  inv_other_sources;
"

# 8. Edge Definitions: Defines the arrows and connections
dot_edges <- "
  // Edges from sources to the initial total
edge [arrowhead=normal, style=solid];
  src_1 -> node_initial;
  // MODIFIED: Explicitly start edge from the west (:w) side and add minlen hint
  src_other:w -> node_included_map [minlen=2];


  // Invisible edge to help rank src_other to the right
  node_included_map -> src_other [style=invis];


  // Main flow connections
edge [arrowhead=normal, style=solid];
  node_initial -> node_after_duplicates;
  node_after_duplicates -> node_after_title_abstract;
  node_after_title_abstract -> node_included_map;
  node_included_map -> node_included_reproducibility;

  // Connections for exclusion reasons (PRISMA-style side boxes)
edge [arrowhead=normal, style=solid];
node_initial:e -> reason_duplicates:w [minlen=2];
node_after_duplicates:e -> reason_title_abstract:w [minlen=2];
// Removed the three edges departing from reason_title_abstract
node_after_title_abstract:e -> reason_full_text:w [minlen=2];
node_included_reproducibility:s -> reason_merged_pre2000:n [headport=w, tailport=e, minlen=2];

// Keep rank alignment
{rank=same; node_initial; reason_duplicates;}
{rank=same; node_after_duplicates; reason_title_abstract;}
{rank=same; node_after_title_abstract; reason_full_text;}
// Place src_other on the same rank as node_included_map, to the right
{rank=same; node_included_map; src_other;} // The invisible edge helps enforce the order

{rank=same; node_included_reproducibility; reason_merged_pre2000;}
"

# 9. Footer
dot_footer <- "}"

# --- Combine DOT Script Components ---
flowchart_dot_script <- paste(
  dot_header,
  dot_graph_attrs,
  dot_default_node_attrs,
  dot_source_nodes,
  dot_main_flow_nodes,
  dot_exclusion_nodes,
  dot_invisible_nodes,
  dot_edges,
  dot_footer,
  sep = "\n"
)

# Create the graph object from the assembled DOT script
indicator_flowchart <- grViz(flowchart_dot_script)

# display & save
print(indicator_flowchart)

export_svg(indicator_flowchart) %>%
  charToRaw() %>%
  rsvg_png("Figure 1.png", width = 500, height = 1000)

export_svg(indicator_flowchart) %>% charToRaw() %>% rsvg_pdf("Figure 1.pdf")

export_svg(indicator_flowchart) %>% writeLines("Figure 1.svg")


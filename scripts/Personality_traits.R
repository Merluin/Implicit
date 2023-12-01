###########################################################################
#
#  Experiment:  IMPLICIT
#  Programmer:  QUETTIER THOMAS
#  Date:        30/11/2023
#  Description: Test Differences in Terms of Personality Traits
#
#  Update:      30/11/2023
###########################################################################

rm(list=ls()) # Remove all objects from the workspace for a fresh start

# Load Functions and Libraries
devtools::load_all() # Load all packages and functions from the local directory

# Load Data
#   - Purpose: To analyze personality traits data in the context of the IMPLICIT experiment
data <- read_excel("data/full_data.xlsx", sheet = "ssrt") %>%
  select("subject", "group", "STAI.Y2", "BIS11") %>%
  gather(questionnaire, score, c(3,4)) # Transform data to long format for analysis

# 1. ANOVA on Personality Traits:
#   - Objective: Analyze whether there are significant differences in personality traits between groups
#   - Type of ANOVA: 2x2 (Questionnaire as within-subject factor; Group as between-subject factor)
#   - Rationale: Determine if personality traits such as anxiety (STAI.Y2) and impulsivity (BIS11) differ based on group assignment in the experiment

anovaq <- aov_ez("subject", "score", data, within = c("questionnaire"), between = c("group"))
# Conduct post hoc analysis for detailed pairwise comparisons
posthoc <- emmeans(anovaq, pairwise ~ group | questionnaire, adjust = "bonf")

# Note: The results from the ANOVA and post hoc tests will provide insights into how personality traits may influence or be influenced by the experimental conditions of the IMPLICIT study.

#################################################
# 
# END
#
#################################################
#  Script for IMPLICIT Study - Analyzing Personality Traits
#################################################
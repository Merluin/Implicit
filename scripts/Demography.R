###########################################################################
#
#  Experiment:  IMPLICIT
#  Programmer:  QUETTIER THOMAS
#  Date:        30/11/2023
#  Description: Demographic Table Creation
#
#  Update:      30/11/2023
###########################################################################

rm(list=ls()) # Remove all objects from the current workspace for a clean start

# Load Functions and Libraries
devtools::load_all() # Load all packages and functions from the local directory

# Load Data
#   - Purpose: To create a demographic table from the collected data
data <- read_excel("data/full_data.xlsx", sheet = "quest") %>%
  select("subject", "group", "gender", "age") %>%
  mutate(gender = ifelse(gender == "M", "male", "female")) # Recode gender for clarity

# Group-wise Calculations
#   - Objective: Calculate mean age, standard deviation, and count by gender for each group
group_stats <- data %>%
  group_by(group) %>%
  summarise(
    age_mean = mean(age, na.rm = TRUE), # Mean age per group
    age_sd = sd(age, na.rm = TRUE), # Standard deviation of age per group
    count = n(), # Total count of subjects per group
    female = sum(gender == "female"), # Count of female subjects per group
    male = sum(gender == "male") # Count of male subjects per group
  ) %>%
  ungroup()

# Overall Calculations
#   - Objective: Compute overall statistics for the entire dataset
total_stats <- data %>%
  summarise(
    group = "total",
    age_mean = mean(age, na.rm = TRUE), # Overall mean age
    age_sd = sd(age, na.rm = TRUE), # Overall standard deviation of age
    count = n(), # Total count of subjects
    female = sum(gender == "female"), # Total count of female subjects
    male = sum(gender == "male") # Total count of male subjects
  )

# Combine Group-wise and Total Statistics
#   - Purpose: Create a comprehensive table summarizing demographics
descriptive_table <- rbind(group_stats, total_stats) # Combine group and total stats into one table

# Export the Data Frame to an Excel File
#   - Usage: For reporting, analysis, or sharing with collaborators
write.xlsx(descriptive_table, "data/Table1.xlsx", rowNames = FALSE) # Save the table as an Excel file

#################################################
# 
# END
#
#################################################
#  Script for IMPLICIT Study - Demographic Analysis 
#################################################
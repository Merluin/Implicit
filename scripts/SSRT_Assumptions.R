###########################################################################
#
#  Experiment:  IMPLICIT
#  Programmer:  QUETTIER THOMAS
#  Date:        14/11/2023
#  Description: Verification of the Independent Race Model Assumptions
#
#  Update:      01/12/2023
###########################################################################

rm(list=ls()) # Remove all objects from the current workspace

# Load necessary functions and packages
devtools::load_all() # Load all packages and functions from the local directory

# Load data
data <- read_excel("data/full_data.xlsx", sheet = "assunzioni") # Load the dataset from Excel file

# Analysis
# 1. Verification of the Independent Race Model Assumptions:
#    - Objective: Assess if the mean Reaction Time (RT) on Unsuccessful Stop trials was shorter than the mean RT on Go trials.
#    - Method: Comparing mean RTs across different trial types.
#    - Create a table with descriptive SST data for analysis.

# Compute descriptive statistics for SST data
descriptive_stats <- data %>%
  group_by(prime, group) %>%
  summarise(
    InhibitionRate_Mean = mean(pb, na.rm = TRUE) * 100, # Mean inhibition rate
    InhibitionRate_SD = sd(pb, na.rm = TRUE) * 100, # Standard deviation of inhibition rate
    SSD_Mean = mean(ssd, na.rm = TRUE), # Mean Stop Signal Delay
    SSD_SD = sd(ssd, na.rm = TRUE), # Standard deviation of Stop Signal Delay
    SSRT_Mean = mean(ssrtMean, na.rm = TRUE), # Mean Stop Signal Reaction Time
    SSRT_SD = sd(ssrtMean, na.rm = TRUE), # Standard deviation of Stop Signal Reaction Time
    incorrectGo_Mean = mean(incorrectSSRT, na.rm = TRUE), # Mean reaction time for incorrect Go trials
    incorrectGo_SD = sd(incorrectSSRT, na.rm = TRUE), # Standard deviation of reaction time for incorrect Go trials
    GoRT_Mean = mean(RT, na.rm = TRUE), # Mean reaction time for Go trials
    GoRT_SD = sd(RT, na.rm = TRUE), # Standard deviation of reaction time for Go trials
    CorrectGo_Mean = mean(Correct, na.rm = TRUE), # Mean percentage of correct responses in Go trials
    CorrectGo_SD = sd(Correct, na.rm = TRUE), # Standard deviation of correct responses in Go trials
    .groups = "drop"
  )

# Reshape data to long format for easier analysis and visualization
long_format <- descriptive_stats %>%
  pivot_longer(
    cols = -c(prime, group),
    names_to = "metric",
    values_to = "value"
  ) %>%
  separate(metric, into = c("Measure", "Stat"), sep = "_") %>%
  pivot_wider(
    names_from = c(prime, group, Stat),
    values_from = value
  ) %>%
  select("Measure",
         "paura_home_Mean", "paura_home_SD",
         "neutro_home_Mean", "neutro_home_SD",
         "paura_lab_Mean", "paura_lab_SD",
         "neutro_lab_Mean", "neutro_lab_SD") %>%
  mutate(across(-Measure, round, digits = 2)) %>%
  data.frame



# 2. Verification of Staircase Procedure:
#    - Objective: Ensure that the inhibition rate is approximately 50% for all stimuli during both tasks.
#    - Method: Descriptive analysis of inhibition rates.
#    - Rationale: Staircase procedure aims to keep the task difficulty balanced, targeting an inhibition rate around 50%.

# Export the reshaped data to an Excel file for further use or sharing
write.xlsx(long_format, "data/Table2.xlsx", rowNames = FALSE)

# 3. ANOVA on Inhibition Rate:
#    - Objective: Analyze if there are significant differences in the inhibition rate based on Prime and Group.
#    - Type of ANOVA: 2x2 (Prime as within-subject factor; Group as between-subject factor).
#    - Rationale: Determine the impact of emotional priming and group differences on response inhibition.

# Fit the mixed-effects model for Inhibition Rate
InhibitionRate <- aov_ez("subject", "pb", data, within = c("prime"), between = c("group"))
model <- lmer(pb ~ prime * group + (1|subject), data = data)

# Perform ANOVA and calculate partial eta squared for effect size
anova_table <- anova(model)
total_var <- sum(anova_table$'Sum Sq', na.rm = TRUE)
anova_table$eta_sq <- anova_table$'Sum Sq' / (anova_table$'Sum Sq' + total_var - anova_table$'Sum Sq')

# Print the ANOVA results with effect sizes
print(anova_table)

# 4. ANOVA on Percentage of Correct Responses on Go-Trials:
#    - Objective: Examine differences in correct responses to Go-trials based on experimental conditions and groups.
#    - Type of ANOVA: 2 x 2 (Prime as within-subject factor; Group as between-subject factor).
#    - Rationale: Investigate the effect of priming and group on successful Go trial performance.

# Fit the mixed-effects model for Percentage of Correct Responses
CorrectResponses <- aov_ez("subject", "Correct", data, within = c("prime"), between = c("group"))
model <- lmer(Correct ~ prime * group + (1|subject), data = data)

# Perform ANOVA and calculate partial eta squared for effect size
anova_table <- anova(model)
total_var <- sum(anova_table$'Sum Sq', na.rm = TRUE)
anova_table$eta_sq <- anova_table$'Sum Sq' / (anova_table$'Sum Sq' + total_var - anova_table$'Sum Sq')

# Print the ANOVA results with effect sizes
print(anova_table)

# 5. ANOVA on Reaction Times (RTs) Following Go-Trials:
#    - Objective: Assess if reaction times on Go trials are influenced by priming or group differences.
#    - Type of ANOVA: 2 x 2 (Prime as within-subject factor; Group as between-subject factor).
#    - Rationale: Understand how emotional content and neurostimulation affect Go trial response times.

# Fit the mixed-effects model for Reaction Times
ReactionTimes <- aov_ez("subject", "RT", data, within = c("prime"), between = c("group"))
model <- lmer(RT ~ prime * group + (1|subject), data = data)

# Perform ANOVA and calculate partial eta squared for effect size
anova_table <- anova(model)
total_var <- sum(anova_table$'Sum Sq', na.rm = TRUE)
anova_table$eta_sq <- anova_table$'Sum Sq' / (anova_table$'Sum Sq' + total_var - anova_table$'Sum Sq')

# Print the ANOVA results with effect sizes
print(anova_table)

# 6. ANOVA on Stop Signal Delay (SSD) Data:
#    - Objective: Explore how emotional content and neurostimulation affect the ability to inhibit responses.
#    - Type of ANOVA: 2 x 2 (Prime as within-subject factor; Group as between-subject factor).
#    - Rationale: Investigate the effects of emotional priming and group differences on response inhibition latency.

# Fit the mixed-effects model for Stop Signal Delay
StopSignalDelay <- aov_ez("subject", "ssd", data, within = c("prime"), between = c("group"))
# Conduct post-hoc analysis
posthoc <- emmeans(StopSignalDelay, pairwise ~ prime, adjust = "bonf")

model <- lmer(ssd ~ prime * group + (1|subject), data = data)

# Perform ANOVA and calculate partial eta squared for effect size
anova_table <- anova(model)
total_var <- sum(anova_table$'Sum Sq', na.rm = TRUE)
anova_table$eta_sq <- anova_table$'Sum Sq' / (anova_table$'Sum Sq' + total_var - anova_table$'Sum Sq')

# Print the ANOVA results with effect sizes
print(anova_table)

# 7. Conclusion:
#    - Objective: Determine the reliability of SST data and verify the correctness of the inhibition rate assumption.
#    - Rationale: Ensure that the SST provides a reliable estimate of the Stop Signal Reaction Time (SSRT).
#    - Outcome: Assess overall study findings in the context of SST performance and inhibition rates.

# Print the main results for reference
print("Inhibition Rate Analysis:")
print(InhibitionRate)
print("Correct Responses Analysis:")
print(CorrectResponses)
print("Reaction Times Analysis:")
print(ReactionTimes)
print("Stop Signal Delay Analysis:")
print(StopSignalDelay)

#################################################
# 
# END
#
#################################################
#  Script for IMPLICIT Study - SSRT Assumptions
#################################################
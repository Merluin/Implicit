###########################################################################
#
#  Experiment:  IMPLICIT
#  Programmer:  QUETTIER THOMAS
#  Date:        20/11/2023
#  Description: Analyze SSRT and Priming and Group
#
#  Update:      01/12/2023
###########################################################################

# Clear the Workspace
rm(list=ls()) # Remove all objects from the workspace

# Load Required Functions and Libraries
devtools::load_all() # Load necessary packages and functions

# Load and Prepare Data
# Loading SSRT data from Excel and mutating SICI and ICF columns
data <- read_excel("data/full_data.xlsx", sheet = "ssrt") %>%
  mutate(SICI = SICI * 100, ICF = ICF * 100) 

# Reshaping the data for ANOVA
anovadata <- data %>%
  gather("prime", "ssrt", c(3,4))

# Analysis

# 1. ANOVA on SSRT
# Performing a 2x2 ANOVA to examine differences in SSRT based on priming and group factors
anovassrt <- aov_ez("subject", "ssrt", anovadata, within = c("prime"), between = c("group"))
# Conducting post hoc analysis
posthoc <- emmeans(anovassrt, pairwise ~ prime, adjust = "bonf")
# T-test and effect size calculation
res <- t.test(data$neutral, data$fear, paired = TRUE, alternative = "two.sided")
t_to_d(t = res$statistic, res$parameter, paired = TRUE)
# Mixed-effects model for SSRT
model <- lmer(ssrt ~ prime * group + (1|subject), data = anovadata)
effec.tsize <- effectsize::effectsize(model)

# 2. ANOVA on SSRT without AWARE subjects
# Excluding two participants who discriminated the prime fear emotion
unawareanovadata <- anovadata %>%
  filter(subject != 10, subject != 14)
anovassrt <- aov_ez("subject", "ssrt", unawareanovadata, within = c("prime"), between = c("group"))
# Post hoc analysis for the new dataset
posthoc <- emmeans(anovassrt, pairwise ~ prime, adjust = "bonf")
# T-test and effect size calculation for unaware data
dataunaware <- data %>%
  filter(subject != 10, subject != 14)
res <- t.test(dataunaware$neutral, dataunaware$fear, paired = TRUE, alternative = "two.sided")
t_to_d(t = res$statistic, res$parameter, paired = TRUE)
# Mixed-effects model for the unaware dataset
model <- lmer(ssrt ~ prime * group + (1|subject), data = unawareanovadata)
effec.tsize <- effectsize::effectsize(model)

# 3. Plotting the Results
# Convert the post hoc analysis results to a dataframe for plotting
emm_df <- summary(posthoc$emmeans)
emm_df <- as.data.frame(emm_df)
# Creating a plot to visualize the effect of priming on SSRT
ggplot(emm_df, aes(x = prime, y = emmean)) +
  geom_point() +  # Plot means as points
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.1) +  # Error bars for confidence intervals
  theme_minimal() +
  labs(x = "Prime", y = "SSRT Marginal Mean", title = "Post-hoc Analysis of Prime Effect")

#################################################
# 
# END
#
#################################################
# Script for IMPLICIT Study -  SSRT, Priming, and Group
#################################################
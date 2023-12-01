###########################################################################
#
#  Experiment:  IMPLICIT
#  Programmer:  QUETTIER THOMAS
#  Date:        14/11/2023
#  Description: SSRT_simulation - Simulating Stop-Signal Reaction Time (SSRT)
#
#  Update:      01/12/2023
###########################################################################

# Load necessary library
library(ggplot2)

# Step 1: Simulate Go Trial Reaction Times (RTs)
set.seed(123) # Set seed for reproducibility
go_RT <- rnorm(100, mean = 500, sd = 50) # Simulated RTs in milliseconds for Go Trials

# Step 2: Include omissions by adding very slow RTs
omissions <- rep(1000, 5) # Simulate 5 omissions with a very high RT (1000 ms)
go_RT <- c(go_RT, omissions) # Combine original RTs with omissions

# Step 3: Create a cumulative distribution of Go RTs
#    - Purpose: Visualize and determine the median RT for Go Trials
go_RT_sorted <- sort(go_RT) # Sort RTs in ascending order
cum_prob <- (1:length(go_RT_sorted)) / length(go_RT_sorted) # Calculate cumulative probability for each RT

# Plot the cumulative distribution
plot(go_RT_sorted, cum_prob, type = "s", 
     xlab = "Reaction Time (ms)", ylab = "Cumulative Probability", 
     main = "Cumulative Distribution of Go RTs")
abline(h = 0.5, col = "red", lty = 2) # Line at 50% probability to find median

# Step 4: Find the RT that corresponds to the 50% cumulative probability
#    - Purpose: Determine the median RT, which is used to calculate SSRT
ssrt_point <- go_RT_sorted[which.min(abs(cum_prob - 0.5))]

# Plot the median RT point on the curve
points(ssrt_point, 0.5, col = "blue", pch = 19)

# Step 5: Calculate SSRT by subtracting mean SSD from median RT
mean_SSD <- 250 # Mean Stop-Signal Delay (SSD) in milliseconds
SSRT <- ssrt_point - mean_SSD # Calculate SSRT

# Step 6: Calculate SSRT including all responses and premature responses
#    - Purpose: Account for premature responses in the SSRT calculation
# Simulate premature responses
premature_responses <- rnorm(10, mean = 200, sd = 30) # Simulated premature RTs

# Combine with go RTs for a complete RT distribution
all_RT <- c(go_RT, premature_responses)

# Calculate SSRT with the complete RT distribution
all_RT_sorted <- sort(all_RT)
cum_prob_all <- (1:length(all_RT_sorted)) / length(all_RT_sorted)
ssrt_point_all <- all_RT_sorted[which.min(abs(cum_prob_all - 0.5))]
SSRT_all <- ssrt_point_all - mean_SSD

# Print SSRT estimates
cat("Estimated SSRT (without premature responses):", SSRT, "ms\n")
cat("Estimated SSRT (with all responses):", SSRT_all, "ms\n")

# Plot the complete cumulative distribution including all responses
plot(all_RT_sorted, cum_prob_all, type = "s", 
     xlab = "Reaction Time (ms)", ylab = "Cumulative Probability", 
     main = "Cumulative Distribution of All RTs")
abline(h = 0.5, col = "red", lty = 2) # Line at 50% probability
points(ssrt_point_all, 0.5, col = "blue", pch = 19) # Point at 50%

#################################################
# 
# END
#
#################################################
#  Script for IMPLICIT Study - SSRT Simulation 
#################################################
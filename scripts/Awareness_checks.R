###########################################################################
#
#  Experiment:  IMPLICIT
#  Programmer:  QUETTIER THOMAS
#  Date:        28/11/2023
#  Description: Describes Responses to Awareness Questions Regarding Subliminal Priming
#
#  Update:      01/12/2023
###########################################################################

rm(list=ls()) # Remove all objects from the workspace for a clean start

# Load Functions and Libraries
devtools::load_all() # Load necessary functions and libraries

# Load Data
# Loading behavioral and questionnaire data from Excel sheets for analysis
behavioral_data <- read_excel("data/full_data.xlsx", sheet = "ssrt")
questionnaire_data <- read_excel("data/full_data.xlsx", sheet = "quest")

# Merging the Datasets
# Combining behavioral and questionnaire data by subject identifier for integrated analysis
merged_data <- inner_join(behavioral_data, questionnaire_data, by = "subject")

# ANALYSIS

# 1) QUEST 1 Analysis
# "Did you see anything besides the arrow and crosses?"
# Calculates and prints the percentage of a specific response in quest1
sumquest1 <- (sum(as.numeric(merged_data$quest1_classified)) / nrow(merged_data)) * 100
print(sumquest1) # Print the calculated percentage for Quest 1

# 2) QUEST 2 Analysis
# "Was there a stimulus just before the arrow, what was it?"
# Cleaning the 'postura' column and splitting multiple words for analysis
merged_data$quest2_cleaned <- trimws(merged_data$quest2_classified)
split_words_list <- strsplit(as.character(merged_data$quest2_cleaned), " ")
all_words_vector <- unlist(split_words_list) # Create a vector of all words for analysis

# Calculate and print word frequencies for quest2
word_frequencies <- table(all_words_vector)
Quest2Table <- as.data.frame(word_frequencies, stringsAsFactors = FALSE)
names(Quest2Table) <- c("Word", "Frequency")
Quest2Table %>%
  arrange(Frequency) %>%
  print()

# 3) QUEST 3 Analysis
# "Did you see a body?"
# Calculates and prints the percentage of a specific response in quest3
sumquest3 <- (sum(as.numeric(merged_data$quest3_classified)) / nrow(merged_data)) * 100
print(sumquest3) # Print the calculated percentage for Quest 3

# 4) QUEST 4 Analysis
# "What posture did it have?"
# Cleaning and splitting words in the 'postura' column for quest4
merged_data$quest4_cleaned <- trimws(merged_data$quest4_classified)
split_words_list <- strsplit(as.character(merged_data$quest4_cleaned), " ")
all_words_vector <- unlist(split_words_list) # Create a vector of all words for analysis

# Calculate and print word frequencies for quest4
word_frequencies <- table(all_words_vector)
Quest4Table <- as.data.frame(word_frequencies, stringsAsFactors = FALSE)
names(Quest4Table) <- c("Word", "Frequency")
Quest4Table %>%
  arrange(Frequency) %>%
  print()

#################################################
# 
# END
#
#################################################
# Script for IMPLICIT Study Analyzing Awareness Checks
#################################################
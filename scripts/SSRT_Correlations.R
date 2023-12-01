###########################################################################
#
#  Experiment:  IMPLICIT
#  Programmer:  QUETTIER THOMAS
#  Date:        20/11/2023
#  Description: test relashioship between SSRT, SICI and personality traits
#
#  Update:      01/12/2023
###########################################################################

rm(list=ls()) # remove all objects

# Functions
devtools::load_all()

# Load data
data <- read_excel("data/full_data.xlsx", sheet = "ssrt") %>%
  mutate(SICI = SICI*100,
         ICF = ICF*100) 

anovadata <- data%>%
  gather("prime", "ssrt", c(3,4))

cor.sici.data <- data %>%
  filter(group == "lab") %>%
  select( "fear", "neutral", "SICI", "STAI.Y1" ,"STAI.Y2" ,"BIS11" ,"Motoric.impulsivity.MI" ,"Attentional.impulsivity.AI" ,"Non.planning.impulsivity.nPI")

# Analysis
# correlations SICI
cor.sici<-cor.sici.data%>%select("fear", "neutral", "SICI")
cor_results <- rcorr(as.matrix(cor.sici), type="pearson")
cor_matrix <- cor_results$r # Correlation matrix
p_matrix <- cor_results$P # Matrix of p-values
p_matrix_adjusted <- p.adjust(p_matrix, method = "fdr")
p_matrix_adjusted_formatted <- format(p_matrix_adjusted, scientific = FALSE)
cor <- matrix(p_matrix_adjusted_formatted, 3,3, byrow = FALSE)
chart.Correlation(cor.sici, histogram=TRUE, pch="+",method ="pearson")

# correlations Questionaries
cor.q<-cor.sici.data %>%
  select("fear", "neutral", "STAI.Y1", "STAI.Y2", "BIS11" ,"Motoric.impulsivity.MI" ,"Attentional.impulsivity.AI" ,"Non.planning.impulsivity.nPI") %>%
  'colnames<-'(c("fear", "neutral", "STAI.Y1", "STAI.Y2", "BIS11", "MI", "AI", "nPI"))

cor_results <- rcorr(as.matrix(cor.q), type="pearson")
cor_matrix <- cor_results$r # Correlation matrix
p_matrix <- cor_results$P # Matrix of p-values
p_matrix_adjusted <- p.adjust(p_matrix, method = "fdr")
p_matrix_adjusted_formatted <- format(p_matrix_adjusted, scientific = FALSE)
cor <- matrix(p_matrix_adjusted_formatted, 8,8, byrow = FALSE)
chart.Correlation(cor.q, histogram=TRUE, pch="+",method ="pearson")

# correlations full
cor_results <- rcorr(as.matrix(cor.sici.data), type="pearson")
cor_matrix <- cor_results$r # Correlation matrix
p_matrix <- cor_results$P # Matrix of p-values
p_matrix_adjusted <- p.adjust(p_matrix, method = "fdr")
p_matrix_adjusted_formatted <- format(p_matrix_adjusted, scientific = FALSE)
cor <- matrix(p_matrix_adjusted_formatted, 9,9, byrow = FALSE)
chart.Correlation(cor.sici.data, histogram=TRUE, pch="+",method ="pearson")
#################################################
# 
# END
#
####################################### SICI
## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")

##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################

mean(data$retweets)
sd(data$retweets, data$likes) 
table(data$reweets)
describe()
summary(data$Content)
table(data$Content)
table(data$sentiment) 
table(data$likes)
mean(data$likes)


##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################

table(data$Content, data$sentiment)  


##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################

chisq.test(table(data$Content, data$sentiment)) 

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################

# Perform ANOVA
anova <- aov(likes  ~ Content, data = data) 
# Summarize ANOVA results 
summary (anova)  

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################

cor(data$retweets, data$likes)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################

# summarize the linear relationship # 
linear_relationship <- lm(likes ~ retweets, data = data) 
summary (linear_relationship)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################

# Examine the scatter plot #
linear_plot <- plot(data$retweets, data$likes) 
print(linear_plot)
# slope # 
abline(linear_relationship, col = "red")

# mean of the x on the x-axis #
abline(v=32869.43, h = 252346.7, col = "Blue")

mean (data$retweets)
mean (data$likes) 
      
##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################

plot(data$retweets, residuals(linear_relationship))
abline (h=0 ) 




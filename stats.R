library(ggplot2)
library(dplyr)
library(magrittr)
# Read in the data
data <- read.csv('Greatest_Aussie_Groceries_sales_data.csv', header=TRUE, sep=",")
# Change column name of cx to match style of capital X and Y
colnames(data)[colnames(data)=="cx"] <- "cX"

### Mutate data
# Append a deal_feat column for X and Y
data <- mutate(data, deal_feat_Y = deal_Y*10 + feat_Y, deal_feat_X = deal_X*10 + feat_X)
# Append a revenue column for X and Y 
data <- mutate(data, rev_X = oz_X * pX, rev_Y = oz_Y * pY)
# Append a profix column for X and Y 
data <- mutate(data, profit_X = rev_X - cX * pX, profit_Y = rev_Y - cY * pY)

### Some summary Analysis

x <- data %>% filter(., STORE==1) %>% select(., oz_X)
y <-data %>% filter(., STORE==1) %>% select(., pX)
generateLogLogPlots(x,y)



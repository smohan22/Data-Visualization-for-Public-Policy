#Shambhavi Mohan
# This is an exploratory analysis of the DoT FARS data.

setwd("~/Documents/GitHub/Data-Visualization-for-Public-Policy/HW1/")

#import libraries
library(readr)
library(haven)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

#read in files
acc2014 <- read_sas('accident.sas7bdat')
acc2015 <- read_csv('accident.csv')
#check for loaded
ls()


#Combining the two years of FARS data

#change blank to null
acc2014 <- mutate(acc2014, TWAY_ID2 = na_if(TWAY_ID2,""))
table(is.na(acc2014$TWAY_ID2))
#check for dimensions
dim(acc2014)
dim(acc2015)
#Identifying missing data
colnames(acc2014)
colnames(acc2014) %in% colnames(acc2015)
#missing data from acc2014: "ROAD_FNC"
colnames(acc2015)
colnames(acc2015) %in% colnames(acc2014)
#missing data from acc2014: "RUR_URB",FUNC_SYS","RD_OWNER"

#merging files
acc <- bind_rows(acc2014, acc2015)
count(acc, RUR_URB)
#Since RUR_URB was missing from acc2014, when we merged the two datasets, 
#they were blank for acc2014. Hence NA in the merged set




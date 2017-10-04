#Shambhavi Mohan
# This is an exploratory analysis of the DoT FARS data.

#import libraries
library(readr)
library(haven)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# read in files
acc2014 <- read_sas('accident.sas7bdat')
acc2015 <- read_csv('accident.csv')

ls()
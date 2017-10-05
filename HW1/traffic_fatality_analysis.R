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

#PART3: Merging on another data source
fips <- read_csv('fips.csv')
glimpse(fips)

#Change variable details
acc$STATE <- as.character(acc$STATE)
acc$COUNTY <- as.character(acc$COUNTY)
acc$STATE <- str_pad(acc$STATE, width = 2, side = "left", pad = "0")
acc$COUNTY <- str_pad(acc$COUNTY, width = 3, side = "left", pad = "0")
acc <- rename(acc, StateFIPSCode = STATE)
acc <- rename(acc, CountyFIPSCode = COUNTY)
# merge using join to not lose data
acc_full <- acc %>% left_join(fips, by = c("StateFIPSCode","CountyFIPSCode"))

#PART4: Exploratory Data Analysis in R’s dplyr and tidyr package
agg <- acc_full %>%
  group_by(YEAR, StateName) %>%
  summarise(TOTAL = sum(FATALS))
agg_wide <- spread(agg, YEAR, TOTAL)
agg_wide <- mutate(agg_wide, Percent_Diff = (`2015`-`2014`) /`2014`)
agg_wide <- arrange(agg_wide, Percent_Diff )
agg_wide <- filter(agg_wide, Percent_Diff > 0.15)
agg_wide <- filter(agg_wide, !is.na(StateName))

#aggregation using chain
agg <- acc_full %>%
  group_by(YEAR, StateName) %>%
  summarise(TOTAL = sum(FATALS)) %>%
  spread(YEAR, TOTAL) %>%
  mutate(Percent_Diff = (`2015`-`2014`) /`2014`) %>%
  arrange(Percent_Diff ) %>%
  filter(Percent_Diff > 0.15) %>%
  filter(!is.na(StateName))

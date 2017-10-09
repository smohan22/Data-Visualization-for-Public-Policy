setwd("~/Documents/Data Viz/")

#install.packages("xlsx")
#library(xlsx)
#require(xlsx)
#read.xlsx("Food environment data.xls", sheetName = "ACCESS")

library(ggplot2)
library("noncensus")
food_env <- read.csv("food_env.csv")

#mapping full state names to their short forms
# courtsey: https://favorableoutcomes.wordpress.com/2012/10/19/create-an-r-function-to-convert-state-codes-to-full-state-name/

#'x' is the column of a data.frame that holds 2 digit state codes
stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
}

food_env$region<-stateFromLower(food_env$State_x)

agg <- food_env %>% group_by(State_x, region) %>% 
  summarise(access15 = mean(LACCESS_POP15_per1000), access10 = mean(LACCESS_POP10_per1000), insec10 = mean(Insecur_10_12_per1000) , insec13 = mean(Insecur_13_115_per1000), poverty15 = mean(POVRATE15)*10, rest15 = mean(rest_15_1000), obes15 = mean(PCT_OBESE_ADULTS13)*10)

#Graph 1:
curr <- ggplot(agg) + geom_point(aes(State_x, access15, size = 2, color = '2015')) + 
  labs(x = "State", y = "Population per 1000 not able to access food location in 2015",title = "State wise distribution of food location inaccessibility", color = "Year")
curr <- curr + geom_point(aes(State_x, access10, size = 2, color = '2013')) 
+ guides(size = FALSE)
curr + geom_line(aes(State_x, poverty15, group = 1)) + geom_smooth()

#graph 1 improved:
ggplot(agg) + geom_point(aes(x = State_x, y = access15, size = insec13, color = '2015')) + geom_point(aes(x = State_x, y = access10, size = insec10, color = '2010')) + labs(x = "State", y = "Food Insecure Population per 1000", title = "State wise relationship between food insecurity and food desert", color = "Year of measure", size = 'Amount of food insecurity')

#Graph 2: maps
states <- map_data("state")
food_map <- merge(states, agg, by="region")

#ggplot() + geom_polygon(data=food_map, aes(x=long, y=lat, group = group, fill=food_map$insec13),colour="white") + scale_fill_continuous(low = "green", high = "red", guide="colorbar", guide_legend("Intensity of Food insecurity") ) + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) + labs(x = "State", y = "Food Insecure Population per 1000", title = "State wise Food Insecurity in 2013-15")
ggplot() + geom_polygon(data=food_map, aes(x=long, y=lat, group = group, fill=food_map$insec13),colour="white") + scale_fill_continuous(low = "green", high = "red", guide="colorbar", guide_legend("Intensity of Food insecurity") ) + scale_x_continuous() + scale_y_continuous() + labs(x = "State", y = "Food Insecure Population per 1000", title = "State wise Food Insecurity in 2013-15")

#Graph 3
ggplot(food_env) + geom_point(aes(x = State_x, y = rest_15_1000, size = PCT_OBESE_ADULTS13, color = '2015')) + labs(x = "State", y = "Food Insecure Population per 1000", title = "State wise relationship between food insecurity and food desert", color = "Year of measure", size = 'Amount of food insecurity')


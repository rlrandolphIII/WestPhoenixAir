# Apple COVID-19 Mobility Trends Report
# https://www.apple.com/covid19/mobility
# Stay-at-home order from 2020-04-01, 5pm through 2020-05-14

# 2020-03-11 : Jonathan Nez, president of the Navajo Nation declared a state of emergency
# 2020-03-12 : Arizona Governor Doug Ducey declared a public health emergency
# 2020-03-16 : Arizona State University online (similar measures for UofA and NAU)
# 2020-03-18 : Travel restrictions to Navajo Nation
# 2020-03-17 : Tucson mayor Regina Romero declares a local emergency, ordering businesses closed
# 2020-03-20 : University of Arizona cancles May-15 Graduation Ceremonies
# 2020-03-20 : Governor Ducey directive to limit restaurant service and close bars, theaters, and gyms
# 2020-03-20 : First COVID-19 death

###############################################################################
# Set working directory
setwd("./Data")
###############################################################################
# load packages
library(dplyr)
###############################################################################
# set strings as factors to false
options(stringsAsFactors = FALSE)
##### Data ####################################################################
###############################################################################
# import data
apple_Mobility <- read.csv("apple_mobility_Maricopa.csv", header=TRUE, sep = ",")
# convert date from <chr> to <date> datatype
apple_Mobility$Date <- as.Date(apple_Mobility$Date, "%m/%d/%y") 
# remove missing data. Define 'COVID-19 date' as 11-March
apple_Mobility <- apple_Mobility %>% 
  na.omit() %>% 
  mutate(rdate = Date >= as.Date('2020-03-11'))
# plot data
plot(apple_Mobility$Date,apple_Mobility$Driving,type="o",col="blue", 
  main="Apple COVID-19 Mobility Trends Report\n Maricopa County, AZ\n 2020-01-13 through 2020-06-21", 
  xlab="Date",
  ylab="Apple Driving Mobility Index")
  abline(v=as.Date('2020-03-11'), col="red", lwd = 2, lty=2)

  
# This script examines Air Pollution in Phoenix AZ
# Hourly Data for NO2, PM2.5, and Ozone for 2020 (01-Jan to 26-Jun)
###############################################################################
# Set working directory
setwd("./Data")
###############################################################################
# load packages
library(ggplot2)
library(dplyr)
library(lubridate)
###############################################################################
# set strings as factors to false
options(stringsAsFactors = FALSE)
###############################################################################
# Import data
# Hourly Data for NO2, PM2.5, and Ozone for 2020 (01-Jan to 26-Jun)
# West Phoenix Monitor Station #40130019
# 39th Avenue & Earll Drive in Phoenix
WPHX2020 <- read.csv("2020_WestPhoenixPollution.csv", header=TRUE, sep = ",")
# Convert date to <dttm> format (from <chr>) 
# https://www.neonscience.org/dc-convert-date-time-POSIX-r
# as.POSIXct(harMet_15Min$datetime[1],format="%Y-%m-%dT%H:%M")
WPHX2020$Date <- as.POSIXct(WPHX2020$Date, format="%d-%b-%Y  %H:%M") 
# Add column defining pre- and post-event for Regression Discontinuity Analysis
WPHX2020 <- WPHX2020 %>% 
  mutate(rdate = Date >= as.POSIXct("2020-03-11 00:00:00"))
# View data
glimpse(WPHX2020)

# Plot OZONE (ppm)
plot(WPHX2020$Date,WPHX2020$OZONE,type="l",col="blue", 
     main="Hourly Ozone - Phoenix, AZ\n Measurement Site (ID \ Name): West Phoenix \ 40130019\n 01-Jan through 26-Jun", 
     xlab="Date",
     ylab="Ozone (ppm)")
abline(v=as.POSIXct("2020-03-11 00:00:00"), col="red", lwd = 2, lty=2)

# Plot NO2 (ppm)
plot(WPHX2020$Date,WPHX2020$NO2,type="l",col="blue", 
     main="Hourly NO2 - Phoenix, AZ\n Measurement Site (ID \ Name): West Phoenix \ 40130019\n 01-Jan through 26-Jun", 
     xlab="Date",
     ylab="NO2 (ppm)")
abline(v=as.POSIXct("2020-03-11 00:00:00"), col="red", lwd = 2, lty=2)

# Plot PM2.5 (ug/m3)
plot(WPHX2020$Date,WPHX2020$PM25,type="l",col="blue", 
     main="Hourly PM2.5 - Phoenix, AZ\n Measurement Site (ID \ Name): West Phoenix \ 40130019\n 01-Jan through 26-Jun", 
     xlab="Date",
     ylab="PM2.5 (ug/m3)")
abline(v=as.POSIXct("2020-03-11 00:00:00"), col="red", lwd = 2, lty=2)
###############################################################################
# Sort by Pre and Post COVID-19
WPHX2020_preLD <- WPHX2020 %>% 
  filter(rdate == FALSE) %>% 
  na.omit() 

WPHX2020_pstLD <- WPHX2020 %>% 
  filter(rdate == TRUE) %>% 
  na.omit()
###############################################################################
boxplot(WPHX2020_preLD$NO2, WPHX2020_pstLD$NO2, 
        main="Hourly NO2 - Phoenix, AZ\n Measurement Site (ID \ Name): West Phoenix \ 40130019\n 01-Jan through 10-Mar & 11-Mar through 26-Jun ", 
        xlab="Left: pre-Covid, Right: post-Covid",
        ylab="NO2 (ppm)")

boxplot(WPHX2020_preLD$OZONE, WPHX2020_pstLD$OZONE, 
        main="Hourly Ozone - Phoenix, AZ\n Measurement Site (ID \ Name): West Phoenix \ 40130019\n 01-Jan through 10-Mar & 11-Mar through 26-Jun ", 
        xlab="Left: pre-Covid, Right: post-Covid",
        ylab="Ozone (ppm)")

boxplot(log10(WPHX2020_preLD$PM25), log10(WPHX2020_pstLD$PM25), 
        main="Hourly log10(PM2.5) - Phoenix, AZ\n Measurement Site (ID \ Name): West Phoenix \ 40130019\n 01-Jan through 10-Mar & 11-Mar through 26-Jun ", 
        xlab="Left: pre-Covid, Right: post-Covid",
        ylab="log10(PM2.5 (ug/m3)")


# Michael Randolph, 22-June-2020
#
# This R-Script examines pollution in the Phoenix metropolitan area, 
# specifically ozone and particulate matter PM2.5.
# The data was collected from a single monitor (JLG SUPERSITE) in
# Phoenix, AZ.
# The goal is to determine if pollution levels decreased durin the
# Covid-19 lockdown in AZ ( 31-March-2020 through 15-May-2020).
# Data was examined over this timeframe for 2020 and 2019.

###############################################################################
###############################################################################
# Set working directory
setwd("./Data")
# Set it to the folder/directory that you are working from (where the data files are stored)

###############################################################################
###############################################################################
# load packages
library(ggplot2)
library(dplyr)

###############################################################################
###############################################################################
# set strings as factors to false
options(stringsAsFactors = FALSE)

###############################################################################
##### Ozone and PM2.5 Data ####################################################
###############################################################################
# Phoenix Monitor: JLG SUPERSITE #40139997
# 4530 N 17TH AVE, PHOENIX, AZ 85015-3809
# Pulling ozone data - Obtained from https://www.epa.gov/outdoor-air-quality-data/download-daily-data
df_ozone_2020 <- read.csv("ozone_040139997_2020.csv", header=TRUE, sep = ",")
df_ozone_2019 <- read.csv("ozone_040139997_2019.csv", header=TRUE, sep = ",")
df_ozone_2018 <- read.csv("ozone_040139997_2018.csv", header=TRUE, sep = ",")
df_ozone_2017 <- read.csv("ozone_040139997_2017.csv", header=TRUE, sep = ",")
df_ozone_2016 <- read.csv("ozone_040139997_2016.csv", header=TRUE, sep = ",")
df_ozone_2015 <- read.csv("ozone_040139997_2015.csv", header=TRUE, sep = ",")

df_pm25_2020 <- read.csv("pm25_040139997_2020_mod.csv", header=TRUE, sep = ",")
df_pm25_2019 <- read.csv("pm25_040139997_2019_mod.csv", header=TRUE, sep = ",")
df_pm25_2018 <- read.csv("pm25_040139997_2018_mod.csv", header=TRUE, sep = ",")
df_pm25_2017 <- read.csv("pm25_040139997_2017_mod.csv", header=TRUE, sep = ",")
df_pm25_2016 <- read.csv("pm25_040139997_2016_mod.csv", header=TRUE, sep = ",")
df_pm25_2015 <- read.csv("pm25_040139997_2015_mod.csv", header=TRUE, sep = ",")

# reduce dataframe to only two columns: Date and Ozone
df_ozone_2020 <- df_ozone_2020 %>%
  select(Date, "ozone_ppm" = Daily.Max.8.hour.Ozone.Concentration) %>% 
  na.omit()

df_ozone_2019 <- df_ozone_2019 %>%
  select(Date, "ozone_ppm" = Daily.Max.8.hour.Ozone.Concentration) %>% 
  na.omit()

df_ozone_2018 <- df_ozone_2018 %>%
  select(Date, "ozone_ppm" = Daily.Max.8.hour.Ozone.Concentration) %>% 
  na.omit()

df_ozone_2017 <- df_ozone_2017 %>%
  select(Date, "ozone_ppm" = Daily.Max.8.hour.Ozone.Concentration) %>% 
  na.omit()

df_ozone_2016 <- df_ozone_2016 %>%
  select(Date, "ozone_ppm" = Daily.Max.8.hour.Ozone.Concentration) %>% 
  na.omit()

df_ozone_2015 <- df_ozone_2015 %>%
  select(Date, "ozone_ppm" = Daily.Max.8.hour.Ozone.Concentration) %>% 
  na.omit()

#############################

df_pm25_2020 <- df_pm25_2020 %>%
  select(Date, "pm25" = Daily.Mean.PM2.5.Concentration) %>% 
  na.omit()

df_pm25_2019 <- df_pm25_2019 %>%
  select(Date, "pm25" = Daily.Mean.PM2.5.Concentration) %>% 
  na.omit()

df_pm25_2018 <- df_pm25_2018 %>%
  select(Date, "pm25" = Daily.Mean.PM2.5.Concentration) %>% 
  na.omit()

df_pm25_2017 <- df_pm25_2017 %>%
  select(Date, "pm25" = Daily.Mean.PM2.5.Concentration) %>% 
  na.omit()

df_pm25_2016 <- df_pm25_2016 %>%
  select(Date, "pm25" = Daily.Mean.PM2.5.Concentration) %>% 
  na.omit()

df_pm25_2015 <- df_pm25_2015 %>%
  select(Date, "pm25" = Daily.Mean.PM2.5.Concentration) %>% 
  na.omit()

# Convert data (<chr> datatype to <date> datatype)
df_ozone_2020$Date <- as.Date(df_ozone_2020$Date, "%m/%d/%Y")
df_ozone_2019$Date <- as.Date(df_ozone_2019$Date, "%m/%d/%Y")
df_ozone_2018$Date <- as.Date(df_ozone_2018$Date, "%m/%d/%Y")
df_ozone_2017$Date <- as.Date(df_ozone_2017$Date, "%m/%d/%Y")
df_ozone_2016$Date <- as.Date(df_ozone_2016$Date, "%m/%d/%Y")
df_ozone_2015$Date <- as.Date(df_ozone_2015$Date, "%m/%d/%Y")

df_pm25_2020$Date <- as.Date(df_pm25_2020$Date, "%m/%d/%Y")
df_pm25_2019$Date <- as.Date(df_pm25_2019$Date, "%m/%d/%Y")
df_pm25_2018$Date <- as.Date(df_pm25_2018$Date, "%m/%d/%Y")
df_pm25_2017$Date <- as.Date(df_pm25_2017$Date, "%m/%d/%Y")
df_pm25_2016$Date <- as.Date(df_pm25_2016$Date, "%m/%d/%Y")
df_pm25_2015$Date <- as.Date(df_pm25_2015$Date, "%m/%d/%Y")

df_ozone_2020 <- df_ozone_2020 %>% 
  mutate(rdate = Date >= as.Date('2020-03-11'))
df_ozone_2019 <- df_ozone_2019 %>% 
  mutate(rdate = Date >= as.Date('2020-03-11'))
df_ozone_2018 <- df_ozone_2018 %>% 
  mutate(rdate = Date >= as.Date('2020-03-11'))
df_ozone_2017 <- df_ozone_2017 %>% 
  mutate(rdate = Date >= as.Date('2020-03-11'))
df_ozone_2016 <- df_ozone_2016 %>% 
  mutate(rdate = Date >= as.Date('2020-03-11'))
df_ozone_2015 <- df_ozone_2015 %>% 
  mutate(rdate = Date >= as.Date('2020-03-11'))

df_pm25_2020 <- df_pm25_2020 %>% 
  mutate(rdate = Date >= as.Date('2020-03-11'))
df_pm25_2019 <- df_pm25_2019 %>% 
  mutate(rdate = Date >= as.Date('2020-03-11'))
df_pm25_2018 <- df_pm25_2018 %>% 
  mutate(rdate = Date >= as.Date('2020-03-11'))
df_pm25_2017 <- df_pm25_2017 %>% 
  mutate(rdate = Date >= as.Date('2020-03-11'))
df_pm25_2016 <- df_pm25_2016 %>% 
  mutate(rdate = Date >= as.Date('2020-03-11'))
df_pm25_2015 <- df_pm25_2015 %>% 
  mutate(rdate = Date >= as.Date('2020-03-11'))

# transposed dataframe to see column headers and datatype
glimpse(df_ozone_2020)
glimpse(df_ozone_2019)
glimpse(df_ozone_2018)
glimpse(df_ozone_2017)
glimpse(df_ozone_2016)
glimpse(df_ozone_2015)

glimpse(df_pm25_2020)
glimpse(df_pm25_2019)
glimpse(df_pm25_2018)
glimpse(df_pm25_2017)
glimpse(df_pm25_2016)
glimpse(df_pm25_2015)

###############################################################################
# 2020 is not a complete year. Need to match time-frame
# Determine time-frame for 2020 data and adjust 2019 data to match
head(df_ozone_2020)
tail(df_ozone_2020) # observations through 2020-06-18

head(df_ozone_2019)
tail(df_ozone_2019)

head(df_pm25_2020)
tail(df_pm25_2020) # observations through 2020-06-20

head(df_pm25_2019)
tail(df_pm25_2019)

#############################
# Subset 2019 data to match time of 2020 data
df_ozone_2019 <- df_ozone_2019 %>% 
  filter(Date <= as.Date('2019-06-18'))

df_pm25_2019 <- df_pm25_2019 %>% 
  filter(Date <= as.Date('2019-06-20'))

#############################
# Subset Lockdown 2020-03-31 to 2020-05-15
df_ozone_2020_ld <- df_ozone_2020 %>% 
  filter(Date >= as.Date('2020-03-31') & Date <= as.Date('2020-05-15'))

df_ozone_2019_ld <- df_ozone_2019 %>% 
  filter(Date >= as.Date('2019-03-31') & Date <= as.Date('2019-05-15'))

df_ozone_2018_ld <- df_ozone_2018 %>% 
  filter(Date >= as.Date('2018-03-31') & Date <= as.Date('2018-05-15'))

df_ozone_2017_ld <- df_ozone_2017 %>% 
  filter(Date >= as.Date('2017-03-31') & Date <= as.Date('2017-05-15'))

df_ozone_2016_ld <- df_ozone_2016 %>% 
  filter(Date >= as.Date('2016-03-31') & Date <= as.Date('2016-05-15'))

df_ozone_2015_ld <- df_ozone_2015 %>% 
  filter(Date >= as.Date('2015-03-31') & Date <= as.Date('2015-05-15'))

##############
df_pm25_2020_ld  <- df_pm25_2020 %>% 
  filter(Date >= as.Date('2020-03-31') & Date <= as.Date('2020-05-15'))

df_pm25_2019_ld  <- df_pm25_2019 %>% 
  filter(Date >= as.Date('2019-03-31') & Date <= as.Date('2019-05-15'))

df_pm25_2018_ld  <- df_pm25_2018 %>% 
  filter(Date >= as.Date('2018-03-31') & Date <= as.Date('2018-05-15'))

df_pm25_2017_ld  <- df_pm25_2017 %>% 
  filter(Date >= as.Date('2017-03-31') & Date <= as.Date('2017-05-15'))

df_pm25_2016_ld  <- df_pm25_2016 %>% 
  filter(Date >= as.Date('2016-03-31') & Date <= as.Date('2016-05-15'))

df_pm25_2015_ld  <- df_pm25_2015 %>% 
  filter(Date >= as.Date('2015-03-31') & Date <= as.Date('2015-05-15'))
###############################################################################
# Review mean of data for initial compaison
mean(df_ozone_2019$ozone_ppm)
mean(df_ozone_2020$ozone_ppm)

mean(df_pm25_2019$pm25)
mean(df_pm25_2020$pm25)

# Lockdown
mean(df_ozone_2019_ld$ozone_ppm)
mean(df_ozone_2020_ld$ozone_ppm)

mean(df_pm25_2019_ld$pm25)
mean(df_pm25_2020_ld$pm25)

###############################################################################
##### Plot Data ###############################################################
###############################################################################

#############################
# Scatter Plot: Ozone vs Date

ggplot(data=df_ozone_2020, aes(x = Date, y = ozone_ppm)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Daily Ozone - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 2020-01-01 through 2020-06-18",
     x = "Date",
     y = "Ozone (ppm)") + 
  theme_bw(base_size = 12) + 
  scale_y_continuous(limits = c(0, 0.08)) +
  geom_point() + 
  stat_smooth(method = lm)

ggplot(data=df_ozone_2019, aes(x = Date, y = ozone_ppm)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Daily Ozone - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 2019-01-01 through 2019-06-18",
       x = "Date",
       y = "Ozone (ppm)") + 
  theme_bw(base_size = 12) + 
  scale_y_continuous(limits = c(0, 0.08)) +
  geom_point() + 
  stat_smooth(method = lm)

#############################
# Scatter Plot: PM2.5 vs Date

ggplot(data=df_pm25_2020, aes(x = Date, y = pm25)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Daily PM2.5 - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 2020-01-02 through 2020-06-18",
       x = "Date",
       y = "PM2.5 (ug/m3)") + 
  theme_bw(base_size = 12) + 
  scale_y_continuous(limits = c(0, 25)) +
  geom_point() + 
  stat_smooth(method = loess)


ggplot(data=df_pm25_2019, aes(x = Date, y = pm25)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Daily PM2.5 - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 2019-01-01 through 2019-06-18",
       x = "Date",
       y = "PM2.5 (ug/m3)") + 
  theme_bw(base_size = 12) + 
  scale_y_continuous(limits = c(0, 25)) +
  geom_point() + 
  stat_smooth(method = loess)
###############################################################################
boxplot(df_ozone_2019$ozone_ppm, df_ozone_2020$ozone_ppm, 
        main="Daily Ozone - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 01-Jan through 18-Jun", 
        xlab="Left: 2019, Right: 2020",
        ylab="ozone (ppm)")
boxplot(df_pm25_2019$pm25, df_pm25_2020$pm25, 
        main="Daily PM2.5 - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 01-Jan through 18-Jun", 
        xlab="Left: 2019, Right: 2020",
        ylab="PM2.5 (ug/m3)")
boxplot(log10(df_pm25_2019$pm25), log10(df_pm25_2020$pm25), 
        main="Daily PM2.5 - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 01-Jan through 18-Jun", 
        xlab="Left: 2019, Right: 2020",
        ylab="log10(PM2.5) (ug/m3)")
abline(v=as.Date('2020-03-11'), col="red", lwd = 2, lty=2)

## Date Range - AZ Lockdown: 31-Mar through 15-May
boxplot(df_ozone_2019_ld$ozone_ppm, df_ozone_2020_ld$ozone_ppm, 
        main="Daily Ozone - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 31-Mar through 15-May", 
        xlab="Left: 2019, Right: 2020- lockdown",
        ylab="ozone (ppm)")

boxplot(df_pm25_2019_ld$pm25, df_pm25_2020_ld$pm25, 
        main="Daily PM2.5 - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 31-Mar through 15-May", 
        xlab="Left: 2019, Right: 2020 - lockdown",
        ylab="PM2.5 (ug/m3)")

##############
## Date Range - AZ Lockdown: 31-Mar through 15-May
boxplot(df_ozone_2015_ld$ozone_ppm, df_ozone_2016_ld$ozone_ppm, df_ozone_2017_ld$ozone_ppm, df_ozone_2018_ld$ozone_ppm, df_ozone_2019_ld$ozone_ppm, df_ozone_2020_ld$ozone_ppm, 
        main="Daily Ozone - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 31-Mar through 15-May (2015 to 2020)", 
        xlab="2015 -> 2020 (lockdown)",
        ylab="ozone (ppm)")

boxplot(df_pm25_2015_ld$pm25, df_pm25_2016_ld$pm25, df_pm25_2017_ld$pm25, df_pm25_2018_ld$pm25, df_pm25_2019_ld$pm25, df_pm25_2020_ld$pm25, 
        main="Daily PM2.5 - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 31-Mar through 15-May (2015 to 2020)", 
        xlab="2015 -> 2020 (lockdown)",
        ylab="PM2.5 (ug/m3)")

###############################################################################
df_ozone_2019$Week <- as.Date(cut(df_ozone_2019$Date,breaks = "week"))
df_ozone_2020$Week <- as.Date(cut(df_ozone_2020$Date,breaks = "week"))
df_pm25_2019$Week <- as.Date(cut(df_pm25_2019$Date,breaks = "week"))
df_pm25_2020$Week <- as.Date(cut(df_pm25_2020$Date,breaks = "week"))

ggplot(data = df_ozone_2019, 
       aes(Week, ozone_ppm)) +
  stat_summary(fun = sum, geom = "bar") +
  theme_bw(base_size = 12)  +
  labs(title = "Daily Ozone - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 2019-01-01 through 2019-06-18",
       x = "Date (Grouped by Week)",
       y = "Ozone (ppm)")

ggplot(data = df_ozone_2020, 
       aes(Week, ozone_ppm)) +
  stat_summary(fun = sum, geom = "bar") +
  theme_bw(base_size = 12)  +
  labs(title = "Daily Ozone - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 2020-01-01 through 2020-06-18",
       x = "Date (Grouped by Week)",
       y = "Ozone (ppm)")

ggplot(data = df_pm25_2019, 
       aes(Week, pm25)) +
  stat_summary(fun = sum, geom = "bar") +
  theme_bw(base_size = 12)  +
  labs(title = "Daily PM2.5 - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 2019-01-01 through 2019-06-18",
       x = "Date (Grouped by Week)",
       y = "PM2.5 (ug/m3)") + 
  theme_bw(base_size = 12) + 
  scale_y_continuous(limits = c(0, 170))

ggplot(data = df_pm25_2020, 
       aes(Week, pm25)) +
  stat_summary(fun = sum, geom = "bar") +
  theme_bw(base_size = 12)  +
  labs(title = "Daily PM2.5 - Phoenix, AZ\n Measurement Site (ID \ Name): JLG SUPERSITE \ 40139997\n 2020-01-02 through 2020-06-18",
       x = "Date (Grouped by Week)",
       y = "PM2.5 (ug/m3)") + 
  theme_bw(base_size = 12) + 
  scale_y_continuous(limits = c(0, 170))
  


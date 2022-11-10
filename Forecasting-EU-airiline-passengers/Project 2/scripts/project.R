
library(fpp3)
library(dplyr)
library(lubridate)
library(readr)

# Load the data

data <- read.csv("data/estat_avia_paoc_filtered.csv") %>%
  select(freq, tra_meas, tra_cov, geo, TIME_PERIOD, OBS_VALUE)

# EDA of swiss data only 

CH.data <- data %>% 
  filter(tra_meas == "PAS_BRD", # passenger on board
         tra_cov == "INTL", # International transport
         geo == "CH") %>%  # Switzerland
  as_tibble

# Monthly data in Switzerland

CH.Monthly <- CH.data %>% filter(freq == "M") %>% # select monthly data
  mutate(TIME_PERIOD = ym(TIME_PERIOD)) %>% # adding a day (the first one) for each month
  as_tsibble(index = TIME_PERIOD)

# Quaterly data in Switzerland

CH.Quaterly <- CH.data %>% filter(freq == "Q") %>% # select quarterly data
  mutate(TIME_PERIOD = yearquarter(TIME_PERIOD)) %>% # change the format to be usable 
  as_tsibble(index = TIME_PERIOD)

# Annual data in Switzerland

CH.Annual <- CH.data %>% filter(freq == "A") %>% # select annual data
  mutate(TIME_PERIOD = as.Date(TIME_PERIOD, format = "%Y")) %>% # change the format to be usable
  as_tsibble(index = TIME_PERIOD)

# Plot the 3 times series

CH.Annual %>% autoplot(OBS_VALUE) # no missing value
CH.Quaterly %>% autoplot(OBS_VALUE) # missing value 2000 to 2002
CH.Monthly %>% autoplot(OBS_VALUE) # missing value in 2000 to 2003

# Crate annual data from monthly data and deal with missing value

CH.Annual.monthly <- as_tibble(CH.Monthly) %>%
  mutate(YEARm = year(TIME_PERIOD)) %>% # take only the year of the time period
  select(YEARm, OBS_VALUE) %>% 
  group_by(YEARm) %>%
  summarize(MONTHLY = sum(OBS_VALUE)) %>% # compute the sum of data with the same year
  as_tsibble(index = YEARm) %>%
  fill_gaps() # deal with missing value

# Crate annual data from quarterly data and deal with missing value

CH.Annual.quaterly <- as_tibble(CH.Quaterly) %>%
  mutate(YEARq = year(TIME_PERIOD)) %>% # take only the year of the time period
  select(YEARq, OBS_VALUE) %>%
  group_by(YEARq) %>%
  summarize(QUATERLY = sum(OBS_VALUE)) %>% # compute the sum of data with the same year
  as_tsibble(index = YEARq) %>%
  fill_gaps() # deal with missing value

# Transfom the annual data to be comparable with the two others

CH.Annual.original <- CH.Annual %>%
  transmute(YEARLY = OBS_VALUE)

# Merge the 3 datasets 

CH.Annual.comparison <- CH.Annual.original %>% cbind(CH.Annual.monthly, CH.Annual.quaterly) %>%
  as_tsibble(index = YEARm)

# Plot the annual/annual.monthly/annual.quaterly in a single plot
# blue line for monthly data
# red points for the yearly data
# green points for the quaterly data

CH.Annual.comparison %>%
  ggplot(aes(x = YEARm)) + 
  geom_line(aes(y = MONTHLY), col = "blue") + 
  geom_point(aes(y = YEARLY), col = "red") +
  geom_point(aes(y = QUATERLY), col = "green") +
  ylab("Number of passengers")

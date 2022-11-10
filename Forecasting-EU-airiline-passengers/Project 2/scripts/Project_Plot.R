
library(dplyr)
library(patchwork)
library(readr)
library(fpp3)

data <- read.csv("data/estat_avia_paoc_filtered.csv") %>%
  select(freq, tra_meas, tra_cov, geo, TIME_PERIOD, OBS_VALUE)

#### Visualization of International, Natinonal and the Totality of the Air Transport as a Time Series for Europe Countries.  

## Annual representation 


Annual <- as_tibble(data) %>% filter(freq == "A") %>%
  mutate(TIME_PERIOD = as.Date(TIME_PERIOD, format= "%Y")) %>%
  group_by(tra_cov) %>% 
  as_tsibble(index = TIME_PERIOD, key = c(geo, tra_meas,tra_cov,freq))%>%
  summarise(OBS_VALUE = sum(OBS_VALUE))



p1<- Annual %>% autoplot(OBS_VALUE)


## Monthly

Monthly <-as_tibble(data) %>% filter(freq == "M") %>%
  mutate(TIME_PERIOD = ym(TIME_PERIOD)) %>%
  as_tsibble(index = TIME_PERIOD, key = c(geo, tra_meas,tra_cov,freq))%>%
  group_by(tra_cov) %>% summarise(OBS_VALUE = sum(OBS_VALUE))

p2<- Monthly %>% autoplot(OBS_VALUE)


## Quarter

Quarter <-as_tibble(data) %>% filter(freq == "Q") %>%
  mutate(TIME_PERIOD = yearquarter(TIME_PERIOD)) %>%
  as_tsibble(index = TIME_PERIOD, key = c(geo, tra_meas,tra_cov,freq))%>%
  group_by(tra_cov) %>% summarise(OBS_VALUE = sum(OBS_VALUE))

p3<- Quarter %>% autoplot(OBS_VALUE)


(p3 / p2) | p1





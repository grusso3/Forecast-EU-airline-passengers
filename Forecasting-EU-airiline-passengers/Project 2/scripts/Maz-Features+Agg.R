library(broom)
library(tidyverse)
library(dplyr)
### Aggregation ###
library(fpp3)
library(patchwork)
library(ggplot2)
library(stats)
library(tibbletime)

# Load the data

data <- read.csv("data/estat_avia_paoc_filtered.csv") %>%
  select(freq, tra_meas, tra_cov, geo, TIME_PERIOD, OBS_VALUE)

## first we only take monthly boarded total passengers data 
#and transform obsrvd in mln , we also remove the total from tra_cov and the  
# EU sums from geo
Pass.m <- data %>%
  filter(freq == "M", tra_meas == "PAS_BRD", tra_cov != "TOTAL", 
         geo != "EU27_2007",geo != "EU28", geo !="EU27_2020")%>%
  mutate(TIME_PERIOD = yearmonth(TIME_PERIOD), OBS_VALUE = OBS_VALUE/1e06)%>%
  tsibble(index = TIME_PERIOD, key = c( tra_cov, geo)) %>%
  select(TIME_PERIOD, tra_cov, geo , OBS_VALUE)

colnames(Pass.m) <- c("Month", "Type", "Country", "Passengers")




# Features

Pass.m %>% features(Passengers, list(Tot  = sum))

Pass.m %>% features(Passengers, feat_stl) %>%
  arrange(desc(trend_strength))

Pass.m %>% features(Passengers, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, col = Country)) +
  geom_point(size = 2) + facet_wrap(vars(Type))




Pass.m %>% features(Passengers, feature_set(pkgs = "feasts")) %>%
  select_at(vars(contains("season"), Type)) %>%
  select(-season_pacf) %>%
  mutate(
    seasonal_peak_year = glue::glue("Q{seasonal_peak_year+1}"),
    seasonal_trough_year = glue::glue("Q{seasonal_trough_year+1}"),
  ) %>%
  GGally::ggpairs(mapping = aes(colour = Type))


## PCA 

feat.passm <- Pass.m %>% features(Passengers, feature_set(pkgs = "feasts"))
feat.passm <- feat.passm[complete.cases(feat.passm),]

pca.passm<- feat.passm %>% select(-Type, -Country) %>%
  prcomp(scale =T)


ggplot(pca.passm %>% augment(feat.passm) ,
       aes(x = .fittedPC1, y = .fittedPC2, col = Country)) +
  geom_point() + theme(aspect.ratio = 1)


# Aggregation by Grouping

Pass.m.agg <- Pass.m %>% 
  aggregate_key(Type * Country, Passengers = sum(Passengers))


# aggregated by Type
Pass.m.agg %>%
  filter(!is_aggregated(Type), is_aggregated(Country)) %>%
  autoplot(Passengers) +
  ylab("Passengers") + ggtitle("TYPE")


#aggregated by Country
Pass.m.agg %>%
  filter(is_aggregated(Type), !is_aggregated(Country)) %>%
  autoplot(Passengers) +
  ylab("Passengers") + ggtitle("Country")+
  facet_wrap(~Country, scales = "free")


# Aggregation by Parent/Child
# forecasts bottom up using country level aggregation
Agg.pass.model <- Pass.m %>%
  filter_index("2010" ~.) %>%
  group_by(Type, Country) %>%
  summarise(Passengers = sum(Passengers)) %>%
  aggregate_key(Type / Country, Passengers = sum(Passengers)) 


# HTS and forecasting for 24 months
# ETS with bottom up
Agg.pass.model%>%
  model(ets = ETS(Passengers)) %>% # Choose the forecasting model
  reconcile(bu = bottom_up(ets)) %>% # Choose the reconciliation method
 forecast(h=24) %>% autoplot()+ facet_wrap(~Type)

# ETS with top down
Agg.pass.model%>%
  model(ets = ETS(Passengers)) %>% 
  reconcile(td = top_down(ets, method = "average_proportions")) %>% 
  forecast(h=24) %>% autoplot()+ facet_wrap(~Country, scales = "free")

# TSLM with bottom up
Agg.pass.model%>%
  model(tslm = TSLM(Passengers ~ trend()+ season())) %>% # Choose the forecasting model
  reconcile(bu = bottom_up(tslm)) %>% # Choose the reconciliation method
  forecast(h=24) %>% autoplot()+ facet_wrap(~Type)

# ARIMA with bottom up
Agg.pass.model%>%
  model(arima = ARIMA(Passengers)) %>% # Choose the forecasting model
  reconcile(bu = bottom_up(arima)) %>% # Choose the reconciliation method
  forecast(h=24) %>% autoplot()+ facet_wrap(~Type)




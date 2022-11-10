
###########
# Data Transformation and Residuals
######
# Load the data

data <- read.csv("data/estat_avia_paoc_filtered.csv") %>%
  select(freq, tra_meas, tra_cov, geo, TIME_PERIOD, OBS_VALUE)


pass.m.tot <- data %>%
  filter(freq == "M", tra_meas == "PAS_BRD", tra_cov == "TOTAL", 
         geo != "EU27_2007",geo != "EU28", geo !="EU27_2020")%>%
  mutate(TIME_PERIOD = yearmonth(TIME_PERIOD), OBS_VALUE = OBS_VALUE/1e06)%>%
  tsibble(index = TIME_PERIOD, key = c( tra_cov, geo)) %>%
  select(TIME_PERIOD, tra_cov, geo , OBS_VALUE)

colnames(pass.m.tot) <- c("Month", "Type", "Country", "Passengers")

pass.m.tot %>% index_by(Month) %>% summarise(Passengers = sum(Passengers)) %>%
  autoplot()

# We notice the inconsistency in early periods so we will filter starting from 2005
# Also we will group countries by period

pass.m.tot <- pass.m.tot %>% filter_index("2005"~.) %>%
  index_by(Month) %>% summarise(Passengers = sum(Passengers))
pass.m.tot %>% autoplot()

#Residuals study
pass.m.tot %>% ACF(Passengers) %>% autoplot()  # super high autocorrelation
pass.m.tot %>% ACF(diff(Passengers)) %>% autoplot() # still apparent

# we should probably double difference, lets test

pass.m.tot %>% features(Passengers, unitroot_kpss)
# low pvalue ==> we should difference
pass.m.tot %>% features(Passengers, unitroot_ndiffs)
# only 1 time differencing

# maybe the log is better?
pass.m.tot %>% ACF(log(Passengers)) %>% autoplot() # still not great

# logging the difference?
pass.m.tot %>% ACF(diff(log(Passengers))) %>% autoplot()
# looks pretty nicer now



#Transforming the data
pass.m.tot <- pass.m.tot %>%
  mutate(PassLog = log(Passengers)) 


# Passenger dataset before COVID impact
pass.precov <- pass.m.tot %>% filter_index("2005" ~ "2019")
                   
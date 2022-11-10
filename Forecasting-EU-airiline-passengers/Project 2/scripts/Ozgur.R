####################   PROJET 2 #####################☻
library(fpp3)
library(patchwork)
library(ggplot2)

# Load the data

data <- read.csv("data/estat_avia_paoc_filtered.csv") %>%
  select(freq, tra_meas, tra_cov, geo, TIME_PERIOD, OBS_VALUE)

## Transform the data as tsibble to work 

unique(data$tra_meas)
unique(data$tra_cov)

## The differences between passengers on boards(PAS_BRD) and passengers carried (PAS_CRD)
# is that Pass boards counts the transit while the other does not.(https://ec.europa.eu/eurostat/cache/metadata/en/avia_pa_esms.htm) 

## Transform data as tsibble and create 3 data set corresponding to year , month , quarter. 

data <-tibble(data)  

########    YEAR  
dataY <- data %>%
  filter(freq == "A")%>%
  mutate(TIME_PERIOD = as.Date(TIME_PERIOD, format = "%Y"))%>%
  tsibble(index = TIME_PERIOD, key = c(tra_meas , tra_cov, geo))


#########  MONTH
dataM <- data %>%
  filter(freq == "M")%>%
  mutate(TIME_PERIOD = yearmonth(TIME_PERIOD))%>%
  tsibble(index = TIME_PERIOD, key = c(tra_meas , tra_cov, geo))

######### QUARTER

dataQ <- data %>%
  filter(freq == "Q")%>%
  mutate(TIME_PERIOD = yearquarter(TIME_PERIOD))%>%
  tsibble(index = TIME_PERIOD, key = c(tra_meas , tra_cov, geo))



############################### NOW SOME PLOTTING

# That plot shows the total pass_brd situation for the European Country
# As expected by Covid, large decrease has been observed in 2019.

Y <- dataY %>%
  filter(tra_cov == "TOTAL" & tra_meas == "PAS_BRD")%>%
  select(TIME_PERIOD, geo , OBS_VALUE) %>%
  summarise(OBS_VALUE = sum(OBS_VALUE))%>%
  mutate(OBS_VALUE = OBS_VALUE/1e6)

pY <- Y %>% autoplot(OBS_VALUE)



## From 2007, 2008 : Some seasonality has been observed with an increasing trend 
# until 2019.
M <- dataM %>%
  filter(tra_cov == "TOTAL" & tra_meas == "PAS_BRD")%>%
  select(TIME_PERIOD, geo , OBS_VALUE) %>%
  summarise(OBS_VALUE = sum(OBS_VALUE))%>%
  mutate(OBS_VALUE = OBS_VALUE/1e6)

pM <- M %>% autoplot(OBS_VALUE)

### Same as monthly data.

Q <- dataQ %>%
  filter(tra_cov == "TOTAL" & tra_meas == "PAS_BRD")%>%
  select(TIME_PERIOD, geo , OBS_VALUE) %>%
  summarise(OBS_VALUE = sum(OBS_VALUE))%>%
  mutate(OBS_VALUE = OBS_VALUE/1e6)

pQ <- Q %>% autoplot(OBS_VALUE)

(pQ / pM) | pY

################################  AUTOCORRELATION (NOT VERY) 
Y %>% ACF(OBS_VALUE) %>% autoplot() # We have noticed some gaps that appeared in "YEARLY" data that has to be worked carefully.

M %>% ACF(difference(log(OBS_VALUE))) %>% autoplot()

Q %>% ACF(OBS_VALUE) %>% autoplot()


############################################################################
############################################################################
############################################################################
############################################################################

############# HANDLE GAPS WITH Yearly DATA "DONT TAKE INTO ACCOUNT, WE WILL NOT USE YEARLY DATA"
############################################################################
############################################################################
############################################################################
############################################################################

has_gaps(dataY) # True

year_gaps <- dataY %>% 
  count_gaps() 

ggplot(year_gaps, aes(x = geo, colour = geo)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from)) +
  geom_point(aes(y = .to)) +
  coord_flip() +
  theme(legend.position = "bottom")


year_full <- Y %>%
  fill_gaps()
############################################################################
############################################################################
############################################################################
############################################################################






#################################  STL DECOMPOSITION 
# We need to log transform the data first to handle the multiplicative 
# seasonality. 

dcmpM <- M %>% model(STL(OBS_VALUE))
components(dcmpM) %>% autoplot()+ xlab("Month")

dcmpM <- M %>% model(STL(log(OBS_VALUE)))
components(dcmpM) %>% autoplot()+ xlab("Month")


######################## CLASSICAL DECOMPOSITION 

M %>%
  model(
    classical_decomposition(OBS_VALUE, type = "multiplicative") 
      )%>%
components() %>%
  autoplot() 

# In here , we observe a quite nice decomposition with classical method. 
# Since we are dealing with multiplicative seasonality, it automatically takes into account it.



###################  NOW SOME FITTING MODEL 

fit <- M %>%
  model(ETS(OBS_VALUE))


fit <- M %>%
  model(ETS(OBS_VALUE~ error("M") + trend("A") + season("M")))

fit %>% forecast(h = 98) %>% autoplot(M)+
  geom_line(aes(y = .fitted, colour = "Fitted"), data = augment(fit))

coefficients(fit)


fit %>% accuracy() ## Very bad accuracy , clearly Covid has to be studied very carefully.
report(fit)

## EVOLUTION OF STATES
components(fit) %>% autoplot()

# 1)  filter the year from 2008. 
# 2)  Be sure about log transformation STL
# 3) Autocorrelation


###############  TSLM ################

fit_TSLM <- M %>%
  model(TSLM(OBS_VALUE ~ trend() + season()))

report(fit_TSLM)

## Visualization of Regression Model 

augment(fit_TSLM) %>% ggplot(aes(x = TIME_PERIOD)) +
  geom_line(aes(y = OBS_VALUE, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) 

### Forecast ( VERY BADD !!!!!!!!!!!!!!!!!!!!!!!)
fc_TSLM <- forecast(fit_TSLM)
fc_TSLM %>% autoplot(M)



######################  ARIMA ######################

# First we need to render stationary to our data. 

autoplot(M, OBS_VALUE)

M %>% features(OBS_VALUE , unitroot_kpss) # So, we need to reject the hypothesis of 



#fit_ARIMA <- M %>% model(ARIMA(OBS_VALUE))
#report(fit_ARIMA)  ### very high AIC and BIC , not good !

#fit_ARIMA %>% forecast(h = 10 ) %>% autoplot(M)



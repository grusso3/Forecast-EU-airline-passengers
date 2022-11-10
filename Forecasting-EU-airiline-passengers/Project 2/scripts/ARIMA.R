
# LOAD THE RESIDUAL SCRIPT BEFORE

################### 
### ARIMA 
###################

pass.m.tot %>%
  autoplot(Passengers)

# Residuals test
pass.m.tot %>% 
  model(arima = ARIMA(Passengers)) %>% gg_tsresiduals()
# VERY NICE ... I LIKE!!

#Trying Different ARIMA models (Note to always include differencing of 1)
# Default R ARIMA
pass.m.tot %>%
  model(ARIMA(Passengers)) %>%
  forecast(h=24) %>%
  autoplot(pass.m.tot)


# AR1
pass.m.tot %>%
  model(AR1 = ARIMA(Passengers ~ pdq(1,1,0))) %>%
  forecast(h=24) %>%
  autoplot(pass.m.tot)


#MA1
pass.m.tot %>%
  model(MA1 = ARIMA(Passengers ~ pdq(0,1,1))) %>%
  forecast(h=24) %>%
  autoplot(pass.m.tot)

#ARIMA111
pass.m.tot %>%
  model(ARIMA111 = ARIMA(Passengers ~ pdq(1,1,1))) %>%
  forecast(h=24) %>%
  autoplot(pass.m.tot)

### ARIMA211
pass.m.tot %>%
  model(ARIMA211 = ARIMA(Passengers ~ pdq(2,1,1))) %>%
  forecast(h=24) %>%
  autoplot(pass.m.tot)

### ARIMA112
pass.m.tot %>%
  model(ARIMA112 = ARIMA(Passengers ~ pdq(1,1,2))) %>%
  forecast(h=24) %>%
  autoplot(pass.m.tot)

### ARIMA212
pass.m.tot %>%
  model(ARIMA212 = ARIMA(Passengers ~ pdq(2,1,2))) %>%
  forecast(h=24) %>%
  autoplot(pass.m.tot)

# all of the affected by the covid drop

# Lets try and take out the covid impact and see the ARIMA212

### ARIMA112 - NO COVID
pass.m.tot %>% filter_index("2005" ~ "2019") %>%
  model(ARIMA112 = ARIMA(Passengers ~ pdq(2,1,2))) %>%
  forecast(h=60) %>%
  autoplot(pass.m.tot)
# seems like the ARIMA 212 works really well without the effect of COVID


# lets check for the best ARIMA
arima.fit <- pass.m.tot %>% 
  model(arima211 = ARIMA(Passengers ~ pdq(2,1,1)),
        arima012 = ARIMA(Passengers ~ pdq(0,1,2)),
        arima210 = ARIMA(Passengers ~ pdq(2,1,0)),
        stepwise = ARIMA(Passengers),
        search = ARIMA(Passengers, stepwise = FALSE))
arima.fit%>%
glance() %>% arrange(AICc) %>% select(1:7)

# ARIMA search looks best

arima.fit %>% accuracy()

arima.fcst <- pass.precov %>%
  model(ARIMA112 = ARIMA(Passengers, stepwise = FALSE)) %>%
  forecast(h=60)  

arima.fcst %>% autoplot()


# Checking whether 5 years is enough for Recovery given ETS m=forecast model
pass.precov$Passengers[nrow(pass.precov)] - min(pass.m.tot$Passengers)
max(arima.fcst$.mean) - min(arima.fcst$.mean)



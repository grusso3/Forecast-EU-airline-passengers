# LOAD THE RESIDUAL SCRIPT BEFORE

##########
##TSLM##
#########


pass.m.tot %>%
  autoplot(Passengers)

# Residuals test
pass.m.tot %>% 
  model(tslm = TSLM(Passengers)) %>% gg_tsresiduals()
## need to take the log
pass.m.tot %>% 
  model(tslm = TSLM(PassLog)) %>% gg_tsresiduals() #no major imporvement
  

# fitting a TSLM          
tslm.passm.fit <- pass.m.tot%>%
  model(tslm = TSLM(Passengers ~ trend() + season()))

pass.m.tot %>%
  autoplot(Passengers, col = "gray") +
  geom_line(data = augment(tslm.passm.fit), aes(y = .fitted), col = "blue")

augment(tslm.passm.fit) %>%
  ggplot(aes(x = .fitted, y = .innov)) +
  geom_point() +
  scale_x_log10()
# Big issues with residual serial correlation

augment(tslm.passm.fit) %>%
  mutate(month = month(Month, label = TRUE)) %>%
  ggplot(aes(x = month, y = .innov)) +
  geom_boxplot()
# A clear pattern in residual behavior over the months


tidy(tslm.passm.fit) %>% mutate(pceffect = (exp(estimate) - 1) * 100)

augment(tslm.passm.fit) %>%
  features(.innov, ljung_box, dof = 14, lag = 24)
# the very low p-value indicates clear autocorrelation in the residuals.


##  Piece-wise Linear Model
# integrating a piece model for the covid impact


tslmpw.passm.fit <- pass.m.tot %>%
model(piecewise = TSLM(Passengers~ trend(knots = c(2019, 2021))))

augment(tslmpw.passm.fit) %>%
  autoplot(.fitted, color = "red") +
  geom_line(aes(y = Passengers), colour = "black")


## forecasting with the 2 tslm models
tslm.passm.fit %>%
  forecast(h=36) %>%
  autoplot(pass.m.tot)

tslmpw.passm.fit %>%
  forecast(h=36) %>%
  autoplot(pass.m.tot)

# Checking Best model and accuracy
tslm.passm.fit %>%
  glance() %>% arrange(AICc)

tslmpw.passm.fit %>%
  glance() %>% arrange(AICc) 

# clear bias towards normal TSLM 

tslm.passm.fit %>% accuracy()
tslmpw.passm.fit%>% accuracy()


# Predicting Using the standard TSLM model
tslm.fcst <- pass.precov %>%
  model(TSLM(Passengers ~ trend()+season() )) %>%
  forecast(h=120)

tslm.fcst %>% autoplot(pass.m.tot)


# Recovery Period Check ==> approx 10 years
pass.precov$Passengers[nrow(pass.precov)] - min(pass.m.tot$Passengers)
max(tslm.fcst$.mean) - min(tslm.fcst$.mean)

### Using Covid Cases to fit the TSLM

covid <- read.csv("data/Weekly_covid_cases_ECDC.csv") 

colnames(covid)[c(1,8)] <- c("country","Month")


covid<- covid %>%
  select(country, continent, Month, weekly_count) %>%
  filter(continent == "Europe") %>%
  group_by(Month) %>%
  summarise(weekly_count = sum(weekly_count)) %>%
  mutate(Month = as.Date(Month, format = "%d-%m-%y"), weekly_count = weekly_count/1e06) %>%
  tsibble(index =Month , key = weekly_count )

covid$Month <- yearmonth(covid$Month)

cov.pass <- pass.m.tot %>%  filter_index("2020 Jan" ~.) %>%
  select(Month, Passengers) 

cov.pass <- inner_join(cov.pass,covid)

cov.pass %>%
  model(tslm= TSLM(Passengers ~ trend()+season()+ weekly_count)) %>%
  forecast() %>%
  autoplot()

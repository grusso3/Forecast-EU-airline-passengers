################
### ETS
###############

# Checking  the residuals
pass.m.tot %>%
    model(ETS(Passengers)) %>%
    gg_tsresiduals()

# Default ETS model analysis
pass.m.tot %>% model(ETS(Passengers)) %>% report()

# SES with different Alpha levels
pass.m.tot %>%
    model(a1 = ETS(Passengers ~ error("A") + trend("N", alpha = 0.1) + season("N")),
          a2 = ETS(Passengers ~ error("A") + trend("N", alpha = 0.4) + season("N")),
          a3 = ETS(Passengers ~ error("A") + trend("N", alpha = 0.9) + season("N"))) %>%
    forecast(h=36) %>%
    autoplot(pass.m.tot)


# Looking for optimal parameters
pass.m.tot %>%
    model(ETS(Passengers ~ error("A") + trend("N") + season("N"),
              opt_crit = "mse")) %>%
    coefficients()
# alpha at 1 and L at 64.2 (covid impact very heavy at a=1)


# Fitting a model with a high alpha

ets.passm.fit<- pass.m.tot %>%
    model(ETS(Passengers ~ error("A") + trend("N", alpha = 0.9) + season("N"))) 
   
ets.passm.fit %>% forecast(h = 36) %>% autoplot(pass.m.tot) +
    geom_line(aes(y = .fitted, colour = "Fitted"), data=augment(ets.passm.fit)) +
    ylab("Exports (% of GDP)") + xlab("Year")

# Adding trend and seasonality
ets2.passm.fit <- pass.m.tot %>% model(
    aaa = ETS(Passengers ~ error("A") + trend("A") + season("A")),
    mam = ETS(Passengers ~ error("M") + trend("A") + season("M")),
    aam = ETS(Passengers ~ error("A") + trend("A") + season("M")))
ets2.passm.fit  %>% forecast(h = "3 years") %>%
     autoplot(pass.m.tot, level = NULL, size = c(1.2)) + xlab("Year")
# seems like additivity is way better than multiplicativity

ets2.passm.fit%>%
    glance() %>% arrange(AICc) %>% select(1:7)

#lets check the accuracy
ets2.passm.fit %>% accuracy() # clearly biased towards additive models

# Forecasting without Covid 
ets.fcst <- pass.precov%>%
    model(aaa = ETS(Passengers ~ error("A") + trend("A") + season("A"))) %>%
    forecast(h=84) 
ets.fcst %>%
    autoplot(pass.m.tot)

# Checking whether 7 years is enough for Recovery given ETS m=forecast model
pass.precov$Passengers[nrow(pass.precov)] - min(pass.m.tot$Passengers)
max(ets.fcst$.mean) - min(ets.fcst$.mean)



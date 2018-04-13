## https://www.dataiku.com/learn/guide/code/r/time_series.html

library(readxl)
library(forecast)
library(dplyr)

data<-read_excel("Time Series/Items.xlsx", col_types = c("date", "numeric"))

N = ceiling(0.75*nrow(data))
items=pull(data,Items)

ts_items = ts(items[1:N], frequency=12)
plot(ts_items)
test.items <- ts(c(rep(NA, N), items[(N+1):NROW(items)]), frequency=12)

##ets
m_ets = ets(ts_items)
f_ets = forecast(m_ets, h=nrow(data)-N) # forecast to test set into the future

plot(f_ets)
lines(test.items)

rmse_ets <- sqrt(mean((f_ets$mean - test.items)^2, na.rm = TRUE))
rmse_ets

## Auto Arima
m_aa = auto.arima(ts_items)
f_aa = forecast(m_aa, h=nrow(data)-N+1)
plot(f_aa)
lines(test.items)

rmse_aa <- sqrt(mean((f_aa$mean - test.items)^2, na.rm = TRUE))
rmse_aa

## TBATS
m_tbats = tbats(ts_items)
f_tbats = forecast(m_tbats, h=nrow(data)-N)
plot(f_tbats)
lines(test.items)

rmse_tbats <- sqrt(mean((f_tbats$mean - test.items)^2, na.rm = TRUE))
rmse_tbats

##Results
barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),
        col="light blue",
        ylab="AIC")

barplot(c(ETS=rmse_ets, ARIMA=rmse_aa, TBATS=rmse_tbats),
        col="light blue",
        ylab="RMSE")

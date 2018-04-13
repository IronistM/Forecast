# SalesForecastR
My R code for Sales Forecasting
## https://www.dataiku.com/learn/guide/code/r/time_series.html
##https://badrit.com/blog/2017/5/29/sales-forecasting#.Ws3URpch2Uk

library(readxl)
library(forecast)
library(dplyr)

data<-read_excel("Time Series/Items per day Online Global.xlsx", col_types = c("date", "numeric", "numeric", "numeric"))
names(data)=c("Date", "Items","ASP","Sent")

N = ceiling(0.75*nrow(data))
items=pull(data,Items)

ts_items = ts(items[1:N], frequency=7)
plot(ts_items)
test.items <- ts(c(rep(NA, N), items[(N+1):NROW(items)]), frequency=7)

##ets
m_ets = ets(ts_items)
f_ets = forecast(m_ets, h=nrow(data)-N) # forecast to test set into the future

plot(f_ets,xlim=c(80,120))
lines(test.items)

rmse_ets <- sqrt(mean((f_ets$mean - test.items)^2, na.rm = TRUE))
rmse_ets

## Auto Arima
m_aa = auto.arima(ts_items)
f_aa = forecast(m_aa, h=nrow(data)-N)
plot(f_aa,xlim=c(80,120))
lines(test.items)

rmse_aa <- sqrt(mean((f_aa$mean - test.items)^2, na.rm = TRUE))
rmse_aa

## TBATS
m_tbats = tbats(ts_items)
f_tbats = forecast(m_tbats, h=nrow(data)-N)
plot(f_tbats,xlim=c(80,120))
lines(test.items)

rmse_tbats <- sqrt(mean((f_tbats$mean - test.items)^2, na.rm = TRUE))
rmse_tbats

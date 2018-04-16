## https://www.dataiku.com/learn/guide/code/r/time_series.html

library(readxl)
library(forecast)
library(dplyr)

data<-read_excel("Time Series/Items.xlsx", col_types = c("date", "numeric"))
plot(data[,2])

models=c("meanf","naive","snaive","rwf","croston","stlf","ses","holt","hw","splinef","thetaf","ets","auto.arima","tbats")

gkuniforecast = function(data, Np, Ncolumn, tsfreq, model) {
  ## Preparation
  N = ceiling(Np*nrow(data))
  x=pull(data,Ncolumn)
  train.x = ts(x[1:N], frequency=tsfreq)
  test.x <- ts(c(rep(NA, N), x[(N+1):NROW(x)]), frequency=tsfreq)
  
  ## Models
  str1=paste0("m_",model," = ",model,"(train.x)")
  if (Np=1){str2=paste0("f_",model," = forecast(m_",model,", h=6")
  } else {str2=paste0("f_",model," = forecast(m_",model,", h=NROW(x)-N)")}
  str2=paste0("f_",model," = forecast(m_",model,", h=NROW(x)-N)")
  str3=paste0("plot(f_",model,")")
  str4="lines(test.x)"
  str5=paste0("acc=accuracy(f_",model,",test.x)")
  str=paste0(str1,";",str2,";",str3,";",str4,";",str5)
  eval(parse(text=str))
  return(acc)
  }
acc = lapply(models, gkuniforecast, data=data, Np=0.75, Ncolumn=2,tsfreq=12)
names(acc)=models
rmse=unlist(lapply(models,function(x) {acc[[x]]["Test set", "RMSE"]}))
names(rmse)=models
##Results
barplot(rmse,
        col="light blue",
        ylab="RMSE",
        las=2, main=paste0("Min RMSE is ",ceiling(min(rmse))," from model ",names(rmse[which.min(rmse)])))

## Take model with smaller RMSE - in this case stlf
NextMonth=forecast(stlf(ts(data[,2],frequency=12)),h=1)

## Show min, max and mean prediction for next month
X=ceiling(c(NextMonth$mean,NextMonth$upper[2],NextMonth$lower[2]))
boxplot(X, horizontal = TRUE, axes = FALSE, staplewex = 1)
text(x=fivenum(X), labels =fivenum(X), y=1.25)

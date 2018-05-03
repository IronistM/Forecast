library(quantmod)

# get data for FTSE
my.df <- getSymbols(Symbols = '^FTSE', auto.assign = FALSE)
my.df=na.approx(my.df)

difvar=100*(my.df$FTSE.Close-my.df$FTSE.Open)/my.df$FTSE.Open

## https://www.dataiku.com/learn/guide/code/r/time_series.html

library(readxl)
library(forecast)
library(dplyr)
library(prophet)
library(rstan)
library(Hmisc)
library(caret)

data<-as.data.frame(difvar)

##Prophet data prep
dframe=data.frame(ds=rownames(data),y=data$FTSE.Close)
pro_fore=rep(NULL,nrow(dframe)+1)
start=which(dframe$ds =="2018-04-03")
end=nrow(dframe)
for (i in start:end){
m <- prophet(dframe[1:i,])

future <- make_future_dataframe(m, periods = 1)

pro_forecast <- predict(m, future)
pro_fore[i+1]=pro_forecast$yhat[i+1]
}
tail(pro_forecast,1)
pro_fore_sign=sign(pro_fore[-1])
actual_sign=sign(dframe$y)
resprophet=pro_fore_sign[start:end]==actual_sign[start:end]
100*sum(resprophet)/length(resprophet)
##Forecast data prep
tsfreq=5
x=pull(data,1)
train.x = ts(x[1:start], frequency=tsfreq)
##test.x <- ts(c(rep(NA, N), x[(start+1):NROW(x)]), frequency=tsfreq)


stmodels=c("stlf","ses","holt","hw","thetaf","ets","auto.arima","tbats")
for (k in 1:length(stmodels)){
  str4=paste0(stmodels[k],"_f=rep(NULL,nrow(dframe)+1)")
  eval(parse(text=str4))
}
for (j in start:end){
  print(j)
for (i in 1:length(stmodels)){
  print(stmodels[i])
str1=paste0("m_",stmodels[i]," = ",stmodels[i],"(train.x)")
str2=paste0("f_",stmodels[i]," = forecast(m_",stmodels[i],", h=1)")
str3=paste0(stmodels[i],"_f[",j,"+1]=f_",stmodels[i],"$mean[1]")
str8=paste0(str1,";",str2,";",str3)
eval(parse(text=str8))
str9=paste0("print(",stmodels[i],"_f[",j,"+1])")
eval(parse(text=str9))
  }
}
for (l in 3:length(stmodels)){
  print(stmodels[l])
  str5=paste0(stmodels[l],"_f_sign=sign(",stmodels[l],"_f[-1])")
  str6=paste0("res",stmodels[l],"=",stmodels[l],"_f_sign[start:end]==actual_sign[start:end]")
  str7=paste0("print(100*sum(res",stmodels[l],")/length(res",stmodels[l],"))")
  eval(parse(text=paste0(str5,";",str6,";",str7)))
}
print("prohpet")
print(100*sum(resprophet)/length(resprophet))

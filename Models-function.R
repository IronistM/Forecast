## https://www.dataiku.com/learn/guide/code/r/time_series.html

library(readxl)
library(forecast)
library(dplyr)
library(prophet)
library(rstan)
library(Hmisc)
library(caret)

data<-read_excel("Time Series/Items.xlsx", col_types = c("text", "numeric"))
Nper=0.75

stmodels=c("meanf","naive","snaive","rwf","croston","stlf","ses","holt","hw","splinef","thetaf","ets","auto.arima","tbats","prophet")

gkuniforecast = function(data, Np, Ncolumn, tsfreq, model) {
  ## Preparation
  N = ceiling(Np*nrow(data))
  
  ## Models
  if (model=="prophet"){
    df=data
    names(df)=c("ds","y")
    df$ds=as.Date(paste(df$ds,"-01",sep=""), "%Y-%b-%d")
    train.df = df[1:N,]
    na.df=data.frame(ds=rep(NA, N),y=rep(NA, N))
    test.df <- rbind(na.df, df[(N+1):nrow(data),])
    m <- prophet(train.df)
    
    future <- make_future_dataframe(m, periods = nrow(data)-N, freq = 'month')

    pro_forecast <- predict(m, future)
    plot(m, pro_forecast)
    ##prophet_plot_components(m, forecast)
    acc=matrix(rep(NA, 16),nrow=2,ncol=8,dimnames=list(c("Training set", "Test set"),c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Theil's U")))
    acc["Test set","RMSE"]=sqrt(mean((pro_forecast$yhat - test.df)^2, na.rm = TRUE))
  }else{
    x=pull(data,Ncolumn)
    train.x = ts(x[1:N], frequency=tsfreq)
    test.x <- ts(c(rep(NA, N), x[(N+1):NROW(x)]), frequency=tsfreq)
    str1=paste0("m_",model," = ",model,"(train.x)")
    if (Np==1) {str2=paste0("f_",model," = forecast(m_",model,", h=NROW(x)")
    } else {str2=paste0("f_",model," = forecast(m_",model,", h=NROW(x)-N)")}
    str3=paste0("plot(f_",model,")")
    str4="lines(test.x)"
    str5=paste0("acc=accuracy(f_",model,",test.x)")
    str=paste0(str1,";",str2,";",str3,";",str4,";",str5)
    eval(parse(text=str))
  }
  return(acc)
  }
acc = lapply(stmodels, gkuniforecast, data=data, Np=Nper, Ncolumn=2,tsfreq=12)


##Aggregation of results
names(acc)=stmodels
rmse=unlist(lapply(stmodels,function(x) {acc[[x]]["Test set", "RMSE"]}))
names(rmse)=stmodels
##Results
barplot(rmse,
        col="light blue",
        ylab="RMSE",
        las=2, main=paste0("Min RMSE is ",ceiling(min(rmse))," from model ",names(rmse[which.min(rmse)])))

## Take model with smaller RMSE - in this case prophet

## stlf (before prophet)
## NextMonth=forecast(stlf(ts(data[,2],frequency=12)),h=1)

##Prophet is best so far
df=data
names(df)=c("ds","y")
df$ds=as.Date(paste(df$ds,"-01",sep=""), "%Y-%b-%d")

m <- prophet(df)

future <- make_future_dataframe(m, periods = 1, freq = 'month')

pro_forecast <- predict(m, future)
NextMonth=pro_forecast[nrow(pro_forecast),c('yhat', 'yhat_lower', 'yhat_upper')]

## Show min, max and mean prediction for next month
X=ceiling(c(NextMonth$yhat,NextMonth$yhat_upper,NextMonth$yhat_lower))
boxplot(X, horizontal = TRUE, axes = FALSE, staplewex = 1)
text(x=fivenum(X), labels =fivenum(X), y=1.25)

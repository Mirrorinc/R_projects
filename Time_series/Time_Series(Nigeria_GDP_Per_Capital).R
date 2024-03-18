install.packages("tseries")
install.packages("forecast")
library(tseries)
library(readxl)
GDP_PER_CAPITAL <- read_excel("C:/Users/Mirror/Desktop/GDP_PER_CAPITAL.xlsx")
View(GDP_PER_CAPITAL)
Data<-GDP_PER_CAPITAL[2]
Data
#Summary statistics
summary(Data)
tsdata<-ts(Data,start = 1998,end=2022,frequency = 1)
tsdata
#Timeplot
ts.plot(tsdata,main='NIGERIA GDP PER CAPITAL FROM 1998 TO 2022',xlab='years',ylab='GDP PER CAPITAL')
#stationarity test
adf.test(tsdata)
#p value<0.05
adf.test(diff(tsdata))
#time plot for the difference data
ts.plot(diff(tsdata),main='1st Difference',xlab='years',ylab='')
acf((tsdata),main=' ACF Plot')
pacf((tsdata),main=' PACF Plot')
#Unit root testing for stationarity
acf(diff(tsdata),main='1st difference ACF Plot')
pacf(diff(tsdata),main='1st difference PACF Plot')


#Model fitting and selection
model<-arima(tsdata,order=c(1,1,1))
model
model1<-arima(tsdata,order=c(1,1,2))
model1
model2<-arima(tsdata,order=c(1,2,1))
model2
model3<-arima(tsdata,order=c(1,1,3))
model3
model4<-arima(tsdata,order=c(1,2,2))
model4
model5<-arima(tsdata,order=c(2,1,1))
model5
model6<-arima(tsdata,order=c(2,1,2))
model6
model7<-arima(tsdata,order=c(2,2,1))
model7
model8<-arima(tsdata,order=c(2,2,2))
model8
model9<-arima(tsdata,order=c(3,1,2))
model9
model10<-arima(tsdata,order=c(3,1,3))
model10



#ARIMA(p,d,q)
#Model 2 p=2(lag for the AR model),d(difference on the data=2) and q=1(lag for the MA )
model<-arima(tsdata,order=c(1,1,1))
model
#Model validations
#Residual Analysis
residual.analysis<-function(model,std=TRUE){
  if(std==TRUE){
    res.model=residuals(model)
  }else{
    res.model=residuals(model)
  }
  plot(res.model,ylab='standardised residuals',main='Time series plot of Standardised residuals')
  abline(h=0)
  hist(res.model,main='Histogram of Standardised residuals')
  qqnorm(res.model,main='QQ plot of Standardised residuals')
  qqline(res.model,col=2)
}
residual.analysis(model=model)
#Select the best model using the AIC criterion (i.e the model with the lowest AIC value)
#Using the forecast library to plot and predict future values
library(forecast)
#Forecasting the Future values
f=forecast(model,6)
print(f)
#Plotting the Future Values
plot(f,main='Forecast Chart',xlab='years')

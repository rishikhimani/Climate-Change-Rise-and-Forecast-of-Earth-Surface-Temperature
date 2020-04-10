library(fpp)
library(fpp2)
library(ggplot2)
#Plotting a time series for India
new = Dataset[Dataset$Country == "India",]

View(new)

ts_in=ts(new$AverageTemperature,start=c(1980,1),end=c(2012,1),frequency=12)

autoplot(ts_in)+
  ggtitle('Temperature rise')+
  xlab('Year')+
  ylab('Temperature')

# PLotting time series for Mumbai
new2 = new[new$City == "Bombay",]

View(new2)
str(new2)

ts_mum=ts(new2$AverageTemperature,start=c(1900,1),end=c(2012,1),frequency=12)

autoplot(ts_mum)+
  ggtitle('Temperature rise')+
  xlab('Year')+
  ylab('Temperature ')


fit3= stl(ts_mum, s.window = 5)
plot(fit3)

autoplot()+
  ggtitle('Temperature rise')+
  xlab('Year')+
  ylab('Temperature ')

# SPLITTING OF DATASET

train=window(ts_mum, start=c(1980,1), end=c(2010,12))
test=window(ts_mum, start=c(2011,1))


autoplot(train)+
  ggtitle('Temperature rise - TRAIN DATA')+
  xlab('Year')+
  ylab('Temperature ')

mmean=meanf(train, h=24)
nnaive=naive(train, h=24)
seanaive=snaive(train, h=24)

autoplot(train)+
  autolayer(mmean, series="Mean", PI=FALSE)+
  autolayer(nnaive, series="Naive", PI=FALSE)+
  autolayer(seanaive, series="Seasonal Naive", PI=FALSE)+
  xlab('Year')+ylab('Temperature')+
  ggtitle('Temperature Forecast')+
  guides(colour=guide_legend(title='Forecast'))


fit2 <- auto.arima(train, seasonal = FALSE)
summary(fit2)
fitted(fit2)
fc = forecast(fit2, h= 24)
plot(train, col = "black") #----original data
lines(fitted(fit2),col ="blue") #----estimated data
lines(fc$mean, col="red")

#TEST DATA

mmean=meanf(test, h=24)
nnaive=naive(test, h=24)
seanaive=snaive(test, h=24)

autoplot(test)+
 autolayer(mmean, series="Mean", PI=FALSE)+
  autolayer(nnaive, series="Naive", PI=FALSE)+
  autolayer(seanaive, series="Seasonal Naive", PI=FALSE)+
  xlab('Year')+ylab('Temperature')+
  ggtitle('Temperature Forecast')+
  guides(colour=guide_legend(title='Forecast'))

summary()

#fit5 <- auto.arima(test, seasonal = FALSE)
#summary(fit5)
#fitted(fit5)
#fc2 = forecast(fit5, h= 2)
#plot(train, col = "blue") #----original data
#lines(fitted(fit5),col ="black") #----estimated data
#lines(fc2$mean, col="red")


#plot(new2, col = "blue") #----original data
#lines(fitted(fit2),col ="black") #----train data
#lines(fitted(fit5),col ="red") #----test data

Acc = accuracy(fc,test)
summary(Acc)







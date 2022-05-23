library(ggplot2)
library(forecast)
library(lmtest)
passengers.data <- read.csv(file = "C:/Users/HP ZBook/Desktop/Statistics/sgpassengers.csv")
passengers.data$value

#Plot graph
Y <- c(passengers.data$value)
Y <- ts(Y, frequency = 12, start = c(2009,1))
plot(Y, ylab="Passengers", main="Number of air passengers in Singapore from 2009 to 2019")
#ACF
ACF <- acf(Y,main="Correlogram for the Air Passengers Dataset")

#Decomposition  
components <- decompose(Y)
plot(components)
cbind(components$x,components$trend,components$seasonal,components$random)

#First differencing
diffY <- ts(diff(Y))
plot(diffY, xlab="Time Period (Month)", ylab="Y(t)-Y(t-1)",main="First Difference")
#ACF
ACF <- acf(diffY,main="Correlogram for first differencing")
#PACF
pacf(diffY,main="Partial Autocorrelation for first differencing")

#seasonal difference
diffsea <- ts(diff(diffY,lag=12,differences = 1))
plot(diffsea, xlab="Time Period (Month)", ylab="Y(t) - Y(t-12)",main="Seasonal Difference")
#ACF
ACF <- acf(diffsea,main="Correlogram for seasonal differencing")
#PACF
pacf(diffsea,main="Partial Autocorrelation for seasonal differencing")

fit <- auto.arima(Y)
summary(fit)

auto.arima(Y,ic="aic",trace=TRUE)

# fit an ARIMA model and compare
arima(Y, order=c(0,1,1), seasonal = list(order = c(2,1,1), period = 12)) #ARIMA(0,1,1)(2,1,1)[12]
arima(Y, order=c(0,1,2), seasonal = list(order = c(2,1,1), period = 12)) #ARIMA(0,1,2)(2,1,1)[12]
arima(Y, order=c(0,1,1), seasonal = list(order = c(1,1,1), period = 12)) #ARIMA(0,1,1)(1,1,1)[12]
arima(Y, order=c(0,1,2), seasonal = list(order = c(1,1,1), period = 12)) #ARIMA(0,1,2)(1,1,1)[12]
arima(Y, order=c(0,1,1), seasonal = list(order = c(1,1,2), period = 12)) #ARIMA(0,1,1)(1,1,2)[12]
arima(Y, order=c(0,1,2), seasonal = list(order = c(1,1,2), period = 12)) #ARIMA(0,1,2)(1,1,2)[12]
arima(Y, order=c(0,1,1), seasonal = list(order = c(0,1,1), period = 12)) #ARIMA(0,1,1)(0,1,1)[12]
arima(Y, order=c(0,1,2), seasonal = list(order = c(0,1,1), period = 12)) #ARIMA(0,1,2)(0,1,1)[12]
arima(Y, order=c(0,1,1), seasonal = list(order = c(0,1,2), period = 12)) #ARIMA(0,1,1)(0,1,2)[12]
arima(Y, order=c(1,1,2), seasonal = list(order = c(2,1,1), period = 12)) #ARIMA(1,1,2)(2,1,1)[12]
arima(Y, order=c(1,1,2), seasonal = list(order = c(1,1,1), period = 12)) #ARIMA(1,1,2)(1,1,1)[12]
arima(Y, order=c(1,1,2), seasonal = list(order = c(0,1,2), period = 12)) #ARIMA(1,1,2)(0,1,2)[12]
arima(Y, order=c(1,1,1), seasonal = list(order = c(1,1,1), period = 12)) #ARIMA(1,1,1)(1,1,1)[12]
arima(Y, order=c(1,1,1), seasonal = list(order = c(1,1,2), period = 12)) #ARIMA(1,1,1)(1,1,2)[12]
arima(Y, order=c(2,1,2), seasonal = list(order = c(0,1,1), period = 12)) #ARIMA(2,1,2)(0,1,1)[12]
arima(Y, order=c(2,1,2), seasonal = list(order = c(0,1,2), period = 12)) #ARIMA(2,1,2)(0,1,2)[12]
arima(Y, order=c(2,1,2), seasonal = list(order = c(1,1,2), period = 12)) #ARIMA(2,1,2)(1,1,2)[12]
arima(Y, order=c(2,1,2), seasonal = list(order = c(2,1,2), period = 12)) #ARIMA(2,1,2)(2,1,2)[12]
arima(Y, order=c(2,1,2), seasonal = list(order = c(2,1,1), period = 12)) #ARIMA(2,1,2)(2,1,1)[12]

#test significance of the model
ARIMA1 <- arima(Y, order=c(0,1,1),seasonal = list(order = c(0,1,2), period = 12),method="ML")
coeftest(ARIMA1)
ARIMA2 <- arima(Y, order=c(0,1,1),seasonal = list(order = c(0,1,1), period = 12),method="ML")
coeftest(ARIMA2)
ARIMA3 <- arima(Y, order=c(0,1,1),seasonal = list(order = c(1,1,1), period = 12),method="ML")
coeftest(ARIMA3)

bestmodel <- arima(Y, order=c(0,1,1), seasonal = list(order = c(0,1,1), period = 12)) #ARIMA(0,1,1)(0,1,1)[12]
bestmodel

#predict(bestmodel,n.ahead = 5)
passengersforecasts <- forecast(bestmodel, h=24)
passengersforecasts
plot(passengersforecasts)

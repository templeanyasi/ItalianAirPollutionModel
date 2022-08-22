







####################################
# Initial analysis of the Air data # 
####################################

library(astsa) # For datasets and sarima function
library(forecast) # For Arima and forecast functions
load('~/Downloads/Air.RData')

# Create a 1 x 336 matrix containing hourly measurements of the 
# log of True CO over the first 2 weeks (336/24=14):

ind <- 1:336 
T <- length(ind)
y <- t(as.matrix(log(Air[ind,1])))

# Center/scale the exogenous variables (except for time): 

temp <- scale(Air[ind, 3])
rhum <- scale(Air[ind, 4]) 
hum <- scale(Air[ind, 5]) 
time <- time(Air)

# Split the data set into a training set, which we will use to 
# fit the models, and a test set, which we will use to evaluate 
# forecast accuracy. 

h <- 48 # Size of test set 

ytrain <- y[1:(T-h)]
temptrain <- temp[1:(T-h)]
rhumtrain <- rhum[1:(T-h)]
humtrain <- hum[1:(T-h)]
timetrain <- time[1:(T-h)]


ytest <- y[(T-h+1):T]
temptest <- temp[(T-h+1):T]
rhumtest <- rhum[(T-h+1):T]
humtest <- hum[(T-h+1):T]
timetest <- time[(T-h+1):T]

# Fit linear regression model to obtain residuals 
# and then look at the ACF/PACF plot to determine ARMA order:

lmfit <- lm(ytrain ~ timetrain + temptrain + rhumtrain + humtrain, na.action=na.omit)
summary(lmfit)
plot(lmfit$residuals)
acf2(lmfit$residuals)

# Fit an AR(2) model to the residuals.
# Diagnostic plots don't look amazing, but don't look horrible either: 

sarima(lmfit$residuals, p=2, d=0, q=0)

# Go ahead and fit the linear regression with AR(2) errors. 
# The Arima/forecast functions are most convenient for subsequent forecasting: 

fitAR2reg <- Arima(ytrain, order = c(2,0,0), xreg=cbind(time=timetrain, temp=temptrain, rhum=rhumtrain, hum=humtrain), include.mean=TRUE)

# Create data frames / data matrices to forecast the last h values of the series
h <- 48 # The number of steps ahead we want to forecast
T <- nrow(Air)

newdf <- data.frame(time=timetest, temp = temptest, rhum = rhumtest, hum = humtest)

newxreg <- model.matrix(~ -1 + time + temp + rhum + hum, data=newdf)


# Forecast plots
fc <- forecast(fitAR2reg ,  h=h, xreg = newxreg)
plot(fc)

# Calculate RMSE of forecasts: 
sqrt(mean((fc$mean - ytest)^2, na.rm=TRUE))



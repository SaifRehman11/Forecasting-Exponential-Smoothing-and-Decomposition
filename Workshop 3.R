library(forecast)
library(smooth)
library(TStools)

Workshop3 <- read.csv(file = "Workshop 3.csv", header = TRUE)

str(Workshop3)

medium_noise <- ts(Workshop3[,1],frequency = 12)

plot(medium_noise)

#Plot the additive decomposition
decomp(medium_noise, decomposition = "additive", outplot = TRUE)
#Plot the multiplicative decomposition
decomp(medium_noise, decomposition = "multiplicative", outplot = TRUE)

#Or do that using decompose for multiplicative 
plot(decompose(medium_noise, type = "m"))
#And additive seasonality
plot(decompose(medium_noise, type = "a"))

#Find the total number of observations 
medium_noise_length <- length(medium_noise)
#Size of train set
train_length <- 36
#And the forecasting horizon
h <- 12
#Create the training set
train <- ts(medium_noise[1:train_length], frequency = 12)
#Create the test set
test <- ts(medium_noise[(train_length+1):medium_noise_length], frequency = 12)


SMA <- ma(train, order = 3, centre = FALSE)

#Firstly we get rid of NA values
SMA_no_NAs <- SMA[!is.na(SMA)]
#Then form a forcast
SMA3_forecast <- ts(rep(SMA_no_NAs[length(SMA_no_NAs)], 12), frequency = 12)


SMA3_errors <- test - SMA3_forecast

SMA3_ME <- mean(SMA3_errors)
SMA3_MSE <- mean(SMA3_errors^2)
SMA3_MAE <- mean(abs(SMA3_errors))
SMA3_MAPE <- 100 * mean(abs(SMA3_errors)/test)


#Fitting a naive

naive_method <- naive(train, h=h)

#naive_forecast <- forecast(naive_method, h=h)$mean



#Exponential Smoothing

ETS_ANN_0.15 <- ets(train, model = "ANN", alpha = 0.15)
ETS_ANN_0.15

ets(train, "MNN", alpha = 0.15)

ETS_ANN_opt = ets(train, "ANN")
coef(ETS_ANN_opt)

ETS_ANN_0.15_forecast <- forecast(ETS_ANN_0.15, h=h)$mean
plot(forecast(ETS_ANN_0.15), h=h)


ES_ANN_0.15 <- es(train, "ANN", persistance = 0.15, h=h)
ES_ANN_0.15

ES_ANN_opt <- es(train, "ANN", h=6, silent = "a")
coef(ES_ANN_opt)

ES_ANN_opt_forecast <- forecast(ES_ANN_opt, h=h)$mean

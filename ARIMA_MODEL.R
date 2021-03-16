#################FITTING ARIMA MODEL#######################
rm(list = ls())# clean environment 
setwd("C://Users//dreyes//Documents//Projeto Swamp//Code//Model_otimization//Dados_Starch")#Load Workspace

#Load libraries
library(forecast)
library(zoo)
library(lmtest)
library(DescTools)

#Load BD
Palouse <- read.table ("Palouse_part.csv", header = T, sep=",", as.is=T)

#Filling NA use Forward Fill
Palouse <- na.locf(na.locf(Palouse), fromLast = TRUE)

vector<- as.numeric(Palouse[,2])

serie <- ts(vector)

ts.plot(serie)
ggtsdisplay(serie) #ACF and PACF

#unseasoned differentiation
ndiffs(serie)
serie_dif <- diff(serie, 1)
ggtsdisplay(serie_dif)

# splitting training data set to training 90% and testing 10% 
train <- ts(serie[1:657])
teste <- ts(serie[658:730])

#Fit AR Model
fit <- auto.arima(train, stationary = FALSE)
summary(fit)

#Hypothesis test
coeftest(fit)

plot(train)
lines(fit$fitted, col = "blue")

accuracy(train, fit$fitted)

#prediction
predi <- forecast(fit, h=73)
plot(predi)

plot(as.numeric(teste), type = "l")
lines(as.numeric(predi$mean), col = "blue")

accuracy(as.numeric(teste), as.numeric(predi$mean))

tsdiag(fit)

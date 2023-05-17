library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(psych)


##Descriptive
##WORLD SUMMARY

#2022 CFR
setwd('E:\\COVID-19 Test Data')
COVID <- read.csv("Data.csv")


model.3nb <- glm.nb(COVID$No..of.death ~ COVID$No..of.test + COVID$No..of.infected + COVID$No..of.recovered, data = COVID)
summary(model.3nb)

exp(model.3nb$coefficients)
exp(confint(model.3nb))


####time series


# Model building


history <- data.frame(ds = seq(as.Date('2022-06-01'), as.Date('2022-09-30'), by = 'd'),
                      y = COVID$No..of.death)
m3 <- prophet(history)
future <- make_future_dataframe(m3, periods = 30)
fcst3 <- predict(m3, future)
x <-plot(m3, fcst3, xlab="Date", ylab="Daily Deaths Count (n)") + ggtitle("Prophet Model") + theme(
  plot.title = element_text(size=12))
plot(x)



SSE <- sum((history$y[1:122] - fcst3$yhat[c(1:122)])^2)
SST <- sum((history$y[1:122] - mean(history$y[1:122]))^2)
R_square <- 1 - SSE / SST
R_square

last_fcst3 <- fcst3[122,]
rmse <- sqrt(mean((history$y - fcst3$yhat[c(1:122)])^2))
mae <- mean(abs((history$y - fcst3$yhat[c(1:122)])))
final <- cbind(last_fcst3, rmse, mae)
final

#ARIMA
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)

library(zoo)


myts <- ts(COVID$No..of.death,start=c(2022.6), frequency = 365.25)

autoplot(myts)
auto.arima(myts)
Fit<-Arima(myts,order=c(0,1,1))
fcast <- forecast(Fit, h=30)
fcast$x
summary(Fit)

y <- autoplot(fcast, size = 1.5,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Date")+ ylab("Daily Deaths Count (n)") + ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.title = element_text(color = "Black", size = 12),
         legend.text = element_text(color = "Black", size = 12))

plot(y)
#R2
SSE <- sum((resid(Fit[1:122]))^2)
SST <- sum((COVID$No..of.death[1:122] - mean(COVID$No..of.death[1:122]))^2)
R_square <- 1 - SSE / SST
R_square


####SES########

library(tidyverse) 
library(fpp2) 



ses.goog <- ses(myts,
                h = 30) 
summary(ses.goog)
accuracy(ses.goog)
fcast <- forecast(ses.goog, h=30)
z <- autoplot(ses.goog, main=NULL)+
  autolayer(fcast$mean, series="Forecast") +
  autolayer(fitted(ses.goog), series='Fitted') + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("Date") + ylab("Daily deaths Count (n)") + ggtitle("SES Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") +
  theme( legend.title = element_text(color = "Black", size = 12),
         legend.text = element_text(color = "Black", size = 12))



z

SSE <- sum((resid(ses.goog[1:122]))^2)
SST <- sum((COVID$No..of.death[1:122] - mean(COVID$No..of.death[1:122]))^2)
R_square <- 1 - SSE / SST
R_square


tiff("TS1.tiff", units="in", width=8, height=8, res=300)
gridExtra::grid.arrange(x)
dev.off()

tiff("TS2.tiff", units="in", width=8, height=8, res=300)
gridExtra::grid.arrange(y)
dev.off()


tiff("TS3.tiff", units="in", width=8, height=8, res=300)
gridExtra::grid.arrange(z)
dev.off()


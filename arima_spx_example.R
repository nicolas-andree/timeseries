
### /*
# ARIMA, 28.03.2018
# based on some inspriration from
# https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials

library(ggplot2)
library(forecast) #arima
library(tseries)  
library(quantmod)
library(depmixS4)
library(TTR)
library(reshape2)
library(xts)
library(rugarch) 
library(caTools) # rolling window moving averages


#orig file
#setwd('C:/Users/Nicolas/Documents/R/data/Bike_Sharing_example')
#daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)

setwd('C:/Users/Nicolas/Documents/R/data/')
#daily prices
dat = read.csv('MyData.csv') 
daily_data = data.frame(dteday = dat$myTSDate, cnt = dat$closeTS) #daily prices

#weekly log returns
#dat = read.csv('MyTS.csv')   
#daily_data = data.frame(dteday = dat$Index, cnt = dat$logret)

# Apply on SPX instead of original data
#rm(daily_data)


daily_data$Date = as.Date(daily_data$dteday)

count_ts = ts(daily_data[, c('cnt')])
daily_data$clean_cnt = tsclean(count_ts)

ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Data Value')

daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers


# removing time series outliers with tsclean() and insert missing values if there are any
# converts to TS and call ts clean
count_ts = ts(daily_data[, c('cnt')])
daily_data$clean_cnt = tsclean(count_ts)
ggplot() +
   geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Data Value')


# moving average calc
daily_data$cnt_ma = ma(daily_data$clean_cnt, order=15) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=50)

ggplot() +
   geom_line(data = daily_data, aes(x = Date, y = clean_cnt), colour="black") +
   geom_line(data = daily_data, aes(x = Date, y = cnt_ma),    colour="#CC0000")  +
   geom_line(data = daily_data, aes(x = Date, y = cnt_ma30), colour="#000099")  +
   ylab('Data Value')

# decompose data (maybe only interesting for prices not returns)
count_ma = ts(na.omit(daily_data$cnt_ma), frequency=15)
count_ma30 = ts(na.omit(daily_data$cnt_ma30), frequency=50)
# decomp = stl(count_ma, s.window="periodic")
# deseasonal_cnt <- seasadj(decomp)
# plot(decomp)

adf.test(count_ma, alternative = "stationary")
# Acf(count_ma, main='')
# Pacf(count_ma, main='')

#differentiate
#count_d1 = diff(daily_data$cnt, differences = 1)

#diagnostics
plot(daily_data$cnt)
adf.test(daily_data$cnt, alternative = "stationary")

Acf(daily_data$cnt, main='ACF for log return series')
Pacf(daily_data$cnt, main='PACF for log return series')

# Step 6 Fitting an ARIMA model
fit<-auto.arima(daily_data$cnt, seasonal=FALSE)

# Step 7: Evaluate and Iterate

#tsdisplay(residuals(fit), lag.max=45, main='(2,1,1) Model Residuals')

plot(residuals(fit), type = 'l')

# fit2 = arima(daily_data$cnt, order=c(1,1,7))
# tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

fcast <- forecast(fit, h=10)
plot(fcast)
cor(fitted(fit),daily_data$cnt)^2 #R^2
plot(fitted(fit), daily_data$cnt)
#plot(daily_data$cnt-fitted(fit), residuals(fit)) #perfect line



# for backtesting
hold <- window(ts(daily_data$cnt), start=length(daily_data$cnt)-10+1)
fit_no_holdout = arima(ts(daily_data$cnt[-c(807:816)]), order=c(2,1,1))
fcast_no_holdout <- forecast(fit_no_holdout,h=10)
plot(fcast_no_holdout, main=" ")
error<-data.frame(fcast = fcast_no_holdout[["mean"]][1:10], actual=daily_data$cnt[807:816])
lines(c(807:816), daily_data$cnt[807:816],  col = "red")

# playground
fit_ma <- auto.arima(count_ma, seasonal=FALSE)
fit_ma
cor(fitted(fit_ma),count_ma)^2 #R^2


# more playground
sp = daily_data$cnt
spfinal.aic <- Inf
spfinal.order <- c(0,0,0)

#AIC
i = 1
spaic = 0
for (p in 0:3) for (d in 1:1) for (q in 0:3) {
  spcurrent.aic <- AIC(arima(sp, order=c(p, d, q)))
  spaic[i] = spcurrent.aic 
  #print(cat("Order=", c(p, d, q), "AIC=", spcurrent.aic ))
  if (spcurrent.aic < spfinal.aic)
  {
    spfinal.aic <- spcurrent.aic
    spfinal.order <- c(p, d, q)
    spfinal.arima <- arima(sp, order=spfinal.order)
  }
  i = i+1
}
#BIC
# i = 1
# spbic = 0
# spfinal.bic <- Inf
# for (p in 0:4) for (d in 1:1) for (q in 0:6) {
#   spcurrent.bic <- BIC(arima(sp, order=c(p, d, q)))
#   spbic[i] = spcurrent.bic
#   if (spcurrent.bic < spfinal.bic)
#   {
#     spfinal.bic <- spcurrent.bic
#     spfinal.order <- c(p, d, q)
#     spfinal.arima <- arima(sp, order=spfinal.order)
#   }
#   i = i+1
# }


print(spfinal.order)
loopfit = arima(sp, order=spfinal.order)
fcast <- forecast(loopfit, h=10)
plot(fcast)

#comparisons, 4.4.2018
#qqnorm(residuals(loopfit))
#qqnorm(sp)
#hist(sp, 30, main=NULL)
#hist(residuals(loopfit), 30, main=NULL)

#original timeseries returns vs fitted values
plot(diff(fitted(loopfit)), main = "", type = 'p', pch=1, col = "red", cex=0.9, 
     ylim=c(min(residuals(loopfit)),  max(residuals(loopfit))))
lines(diff(sp), col="blue", lty = 3)
title(main="Original values (returns) versus fitted values (dots)", cex.main=0.85)
#plot(fitted(loopfit), sp)
#close.screen(all.screens=TRUE)

# assess how close fitted values come to original values
# scatter fit vs original 
# plot(sp, fitted(loopfit)) 
fr = data.frame(sp = sp, fitted = fitted(loopfit), resid = residuals(loopfit))
lin.mod = lm(sp ~ fitted,  data = fr)
print(summary(lin.mod))

# ARIMA end */
###

# /* GARCH
#to check
final.order = spfinal.order
spReturnsOffset = sp
windowLength = length(sp)
forecasts <- vector(mode="character", length=windowLength)

spec = ugarchspec(
  variance.model=list(garchOrder=c(1,1)),
  mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
  distribution.model="sged"
)  

 # fit garch based on arima
 garch.fit = tryCatch(
     ugarchfit(spec, spReturnsOffset, solver = 'hybrid' ), 
     error=function(e) e, 
     warning=function(w) w )
  
 # inspect forecast
 if(is(garch.fit, "warning")) {
      fore = ugarchforecast(garch.fit, n.ahead=10)
      ind = fore@forecast$seriesFor
   } else {
      fore = ugarchforecast(garch.fit, n.ahead=10)
      ind = fore@forecast$seriesFor
 }
  
# from "A short introduction to the rugarch package"
spec = getspec(garch.fit)
setfixed(spec) <- as.list(coef(garch.fit))
filt1 = ugarchfilter(spec, sp)
#plot(filt1) #gives several options

forc1 = ugarchforecast(garch.fit, n.ahead = 20)
f1 = as.data.frame(attributes(forc1)[[1]])



# GARCH end */ 

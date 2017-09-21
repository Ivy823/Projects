# Projects - Statistical Modeling and Forecasting for the Non-revolving Consumer Loans Owned by Nonfinancial Business monthly Data
install.packages("astsa")
library(astsa)
acc <- read.csv("/Users/Xuanyi/Downloads/acc.csv")
acc.ts <- ts(acc[,2],freq=12,start = 1982)
accc <- read.csv("/Users/Xuanyi/Downloads/acc1.csv")
accc.ts <-ts(accc[,2],freq=12,start = 1982)
### Exploratory analysis ###
plot.ts(acc.ts,main = "Nonrevolving Consumer Loans Owned by Nonfinancial Businesses")
# The time plot is used to detect patterns in the series.
## Identifying the trends objectively:
plot(stl(acc.ts,s.window="periodic")) # decomposition of this time series, it shows some polynomial trend; seasonal trends; 

#### Data Transformation ####
## Box-Cox Transformation:
boxcox(acc.ts~1) # select lambda=0, whcih corresponds to the log transformation.  Variance constant. 
# Since the lambda near to 0,so I do the log transformation

# Logarithmic transformation on the data to stabilize the variance
# A log-transformation may be seen as an appropriate model formulation when a series can only take positive values and has values near zero. 
# Difference data once to remove the trend:
acc.d <-diff(log(acc.ts)) # creates a series of first-order differences
tsdisplay(acc.d,lag.max=100)
# since the acf shows there still have significant value occured seasonally in the following lags, so we take the second difference
# Take the difference at lag 12 (seasonal difference):
acc.dd <- diff(acc.d,lag=12)
tsdisplay(acc.dd,lag=12,lag.max=100)
# To get the ACF and PACF plots of second difference:
acf2(acc.dd, max.lag=50)
# Note the slow decay of the ACF values for diff(logap) at lags S, 2S, 3S, ..., with S=12.
# This suggest we should take the seasonal difference of diff(logap) at lag 12.
# The sample ACF and PACF of the monthly second difference of loans are plotted in Figure..., based on the figure,
# it seems that the ACF is tailing off and the PACF is cutting off at lag 5, so we might consider ARIMA(5,1,0)
pacf(acc.dd)
# The purpose of the correlogram is to determine whether there is autocorrelation in the series, which would require a further model.
acfout <- acf2(acc.dd, max.lag=60) # ACF and PACF plots
# now only a few ACF and PACF values are significantly different from zero, this is what we want.
# The (PACF) correlogram provides sufficient evidence to support the conjecture that the residuals are well approximated by WN. 

###### ARIMA()

# Compute the theoretical autocorrelation function or partial autocorrelation function for an ARMA process.
AR.acf < -ARMAacf(ar=c(0.8403),ma=c(-0.7387),lag.max=50)
plot(AR.acf,type="h",xlab="lag",ylab="ACF",main="ACF of an Seasonal ARMA(1,1)")
AR.pacf < -ARMAacf(ar=c(0.8403),ma=c(-0.7387),lag.max=50,pacf=TRUE)
plot(AR.pacf,type="h",xlab="lag",ylab="PACF",main="PACF of an Seasonal ARMA(1,1)")

lines(density(acc.dd)) # add the kernel density estimates.

#### Model Building ####
# There is a function "auto.arima" in library(forecast) that does automatic search over possible models 
# and returns the best one according to either AIC, AICc or BIC value:
auto.arima(acc.ts)

## Fit several candidate models: 
fit1 <- sarima(acc.ts, p=1, d=1, q=1, P=2, D=1, Q=0, S=12)  # Mixed Seasonal Model ARIMA(5,0,1)(0,1,1)[12]
# Based on the AIC,AICc,BIC, ARIMA(1,0,1) is better
# aic = -63.38  AIC= -2.20
fit2 <- sarima(acc.ts, p=1, d=1, q=1, P=0, D=1, Q=1, S=12)   # Mixed Seasonal Model ARIMA(1,0,1)(0,1,1)[12]
# aic = -64.81  AIC = -2.17
# AIC is used to compare the fitted models, and only statistically significant terms are included in the final model.
fit3 <- sarima(acc.ts, p=1, d=1, q=0, P=2, D=1, Q=2, S=12)
# aic = -66.4  AIC = -2.19
fit4 <- sarima(acc.ts, p=1, d=1, q=1, P=2, D=1, Q=2, S=12)
# aic = -75.72  AIC = -2.32
fit5 <- sarima(acc.ts, p=1, d=1, q=1, P=0, D=1, Q=2, S=12) 
# aic = -61.27  AIC = -2.15
fit6 <- sarima(acc.ts, p=1, d=1, q=0, P=2, D=1, Q=0, S=12) 
# aic = -55.36  AIC = -2.09

fit1$fit
fit2$fit
fit3$fit
fit4$fit
fit5$fit
fit6$fit


#### Model Selection and Model Interpretation ####
# The smallest AIC and therefore provides the best fit to the data.
# Based on the AIC, we choose SARIMA(p=1,d=1,q=1)(P=0,D=2,Q=2)[S=12] as the final model.
# By computing the 95% of the coefficients, both of them are significant:
## Compute the 95% confidence intervals:
L <- fit2$fit$coef - 1.96*sqrt(diag(fit2$fit$var.coef)) # lower bounds
U <- fit2$fit$coef + 1.96*sqrt(diag(fit2$fit$var.coef)) # upper bounds
rbind(L,U) 

#### Residual Diagnostics ####
fit <- arima(acc.ts,order=c(1,1,1),seasonal=c(2,1,2)) # default is MLE method
tsdiag(fit,gof.lag=100)
fitt <- arima(acc.ts,order=c(1,1,1),seasonal=c(0,1,1)) # default is MLE method
tsdiag(fitt,gof.lag=100)
# For the standardized residuals plot, check pattern and outliers.
# For the ACF plot of the residuals, check if the ACF values are within the C.I. for lag >= 1.
# For the p-values plot, check if all the p-values are above 0.05.
# In the third plot, the p-values are computed using the Ljung-Box-Pierce Q-statistic (textbook page 150, equation (3.154))
# We are testing H0: Errors are Uncorrelated vs. H1: Errors are correlated
# Decision rule: If p.value >= .05, do not reject H0 and conclude that Errors are uncorrelated => Residuals are White Noise.
# The residuals are approximately white noise, the model provides a good fit to the data.
Box.test(fit$res,lag=12) 
Box.test(fitt$res,lag=12)


#### Forecasting ####
# Forecasting using the predict function
forecast1 <- predict(fit,n.ahead=12)
## 95% prediction intervals for the predictions:  
( U.pred = forecast1$pred + (1.96*forecast1$se) ) 
( L.pred = forecast1$pred - (1.96*forecast1$se) ) 
rbind(L.pred, U.pred)
## Plot the predictions with their P.I.s:
plot.ts(acc.ts, xlim=c(1982,2002),ylim=c(minx,maxx), 
              main='a Year Ahead Forecasts') 
lines(forecast1$pred, col="red", lwd=5)
lines(U.pred, col="blue", lwd=5)
lines(L.pred, col="blue", lwd=5)

## Forecasting using the forecast function (When using ARIMA function to fit model)
install.packages("forecast")
library(forecast)
forecast2 <- forecast(fit, h=12)
names(forecast2) # different from forecast1, here the lower and upper bounds of the P.I.s are available, but not the standard errors.
forecast2$mean # point predictions, exactly the same as forecast1$pred.
forecast2$lower # the lower bounds for both the 80% and 95% P.I.s
forecast2$upper # the?ARMAacf
# upper bounds for both the 80% and 95% P.I.s
## Plot the predictions with their P.I.s:
plot(forecast2, xlim=c(1982,2001), main='a Year Ahead Forecasts') 
# dark grey represents the 80% P.I.s, light grey represents the 90% P.I.s.
## 95% prediction intervals for the predictions:  
( U.pred = forecast2$pred + (1.96*forecast2$se) ) # upper bound
( L.pred = forecast2$pred - (1.96*forecast2$se) ) # lower bound
rbind(L.pred, U.pred)

# ARIMA forecasting (When using SARIMA function to fit model)
forecast <- sarima.for(acc.ts,n.ahead = 12,p=1,d=1,q=1,P=2,D=1,Q=2,S=12)
forecast12 <- sarima.for(accc.ts,n.ahead = 18, p=1,d=1,q=1,P=2,D=1,Q=2,S=12)

## 95% prediction intervals for the predictions:  
( U.pred = forecast$pred + (1.96*forecast$se) ) # upper bound
( L.pred = forecast$pred - (1.96*forecast$se) ) # lower bound
rbind(L.pred, U.pred)

# Compare the oberserved and forecast values
x0sub <- read.csv("/Users/Xuanyi/Desktop/forecast.csv")
x0sub.ts <- ts(x0sub[,2],freq=12,start = 2001)
plot(forecast12$pred,type="b",col="red",ylim=range(c(x0sub.ts,forecast12$pred)),main="Observed VS. Forecasting values from January 2001 to May 2002")
lines(x0sub.ts,type="b", col="blue")
legend("topleft", legend = c("forecasts", "observed values"), 
       col = c("red", "blue"), lty = c(1, 1), bty = "n")

plot(forecast$pred, type="o")

#### spectral analysis ####
# spectral analysis imposes smoothing techniques on the periodogram.  With certain assumptions, we can also create confidence intervals to estimate the peak frequency regions.  
# Spectral analysis can also be used to examine the association between two different time series.  We???ll look at some of these techniques later in the course.

# Periodogram
# Identifying the season or "period" using the periodogram/spectral/Fourier analysis
## a tool for describing and identifying the dominant cycles in a time series.
# x-axis of the periodogram plot represents the Fourier frequencies and y-axis represents log10(periodogram) by default
# The frequency axis is labeled in multiples of Delta, where Delta is the reciprocal of the value of frequency used in ts().
# Here Delta = 1/12 since we have monthly data, i.e., the frequency of data collection of AirPassengers in unit time (year) is 12 (months)
# The maximum on the frequency axis is determined so that: max*Delta = 1/2. i.e., here max=1/(2*Delta)=6.
# Multiply the frequency axis value (where the peak happens) with Delta to obtain the dominant frequency to see the seasons!!!

## One goal of an analysis is to identify the important frequencies (or periods) in the observed series.  A starting tool for doing this is the periodogram.  
## The periodogram graphs a measure of the relative importance of possible frequency values that might explain the oscillation pattern of the observed data.
acc.per <- spec.pgram(acc.ts,taper=0,log="no",detrend="TRUE")
abline(v=c(0.15,2),lty=2)

acc.per <- spec.pgram(log(acc.ts),taper=0,log="no",detrend="TRUE")
abline(v=c(0.2,1),lty=2)

# the common peaks at w = 1*Delta=1/12, or one cycle per year (12 months)

acc.per$spec[12] # at freq 1/12
acc.per$spec[24] # at freq 1/24
# confidence intervals:
U = qchisq(.025,2) # 0.05063
L = qchisq(.975,2) # 7.37775
2*acc.per$spec[12]/L
2*acc.per$spec[12]/U
2*acc.per$spec[24]/L
2*acc.per$spec[24]/U

# Boxplot: Produce box-and-whisker plot(s) of the given (grouped) values.
boxplot(split(acc.ts,rep(1:12,18)))  

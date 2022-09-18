setwd("C:/Users/Clod/Desktop/R/ECONOMETRICS_FINALPROJECT")
rm(list = ls()) # clean objects workspace

#############################################################################################
#########   COINTEGRATION ANALYSIS: BTC-ETH in the last five years (2017-2022)  #############
#############################################################################################

source("Load_Packages.R")

Dataset <- read.table('Dataset_eth_btc.txt', header = TRUE, sep = ",")
attach(Dataset)

#PLOT to see possible common trend

plot(as.Date(Date), btc_price, col = "blue", type = "l", xlab=" Time ", ylab=" Price ($) " ,
     main = "BTC and ETH: Common Trend?", lty=1:2, cex.axis = 1.1)
lines(as.Date(Date), 10*eth_price, col = "orange" )
legend("topleft", legend=c(" btc ", " eth x 10 "),col=c("blue", "orange"), lty=1:2, cex = 1.2)
grid()

# They seem to share a common trend

###############################################################################################


#And now we perform some test (stationaruty)

#  ----------- Elliot, Rothenberg and Stock (ERS)Unit Root Test ------------- # 
# it is robust when we have a unit root close to 1 but lower than 1
# if the value of the statistic is above the critical value (see the summary) we 
# cannot reject the null hypothesis

# H0: Non-stationary (exixst a unit root)
# H1: stationary (no evidence of a unit root)


btc_urers <- ur.ers(btc_price, type="P-test", model="trend")
summary(btc_urers)

eth_urers <- ur.ers(eth_price, type="P-test", model="trend")
summary(eth_urers)

# both BTC and eth are NONSTATIONARY

# now we repeat the analysis on the difference

dbtc <- diff(btc_price)
deth <- diff(eth_price)

dbtc_urers <- ur.ers(dbtc, type="P-test", model="trend")
summary(dbtc_urers)

deth_urers <- ur.ers(deth, type="P-test", model="trend")
summary(deth_urers)

# in both cases we reject the null hypothesis of existence of a unit root


## ------------------------------ Augmented Dickey Fuller test -------------------------- # 
# H0: non stationary
# H1: stationary

adf.test(as.matrix(btc_price))
adf.test(as.matrix(eth_price))   # high pvalues --> we cannot reject H0

adf.test(as.matrix(dbtc))
adf.test(as.matrix(deth))  # small pvalues --> we  reject H0

####### we plot the differences

par(mfrow=c(1,2))
plot(as.Date(Date[2:length(Date)-1]), dbtc, type='l', main=" BTC first differences ", col="blue", xlab='Time',ylab="")
grid()
plot(as.Date(Date[2:length(Date)-1]), deth, type='l', main = " ETH first differences ", col="red", xlab='Time',ylab="")
grid()



########----------------     MODELING with first differences VS Error Correction Model   -------------- ########

# Now we try to model a linear regression between first differences using the variables

btc_ts <- ts(btc_price) # bitcoin, x variable
eth_ts <- ts(eth_price) # ethereum , y variable

dbtc_ts <- diff(btc_ts)
deth_ts <- diff(eth_ts)

model1 <- dynlm( deth_ts ~ L(deth_ts,1) + L(dbtc_ts,1 ))
summary(model1)
# stargazer(model1) # for latex source code



#  --------------------   Estimation of the cointegrating relationship --------------------- #

model_coint <- lm(eth_ts ~ btc_ts )
summary(model_coint)
# stargazer(model_coint) # --> latex source code for the summary



#  --------------------- stationarity tests on the residuals: ------------------------------- #

ers_residuals_coint_rel <- ur.ers(model_coint$residuals, type="P-test", model="trend")
summary(ers_residuals_coint_rel)
adf.test(model_coint$residuals)
# even if the two time series share a common trend (graphically), 
# we have no evidence of a cointegrating relationship



######## ------------------ plot of the residuals -------------------------- ###########

plot(as.Date(Date), model_coint$residuals, col = "blue", type = "l", xlab=" Time ", ylab="Residuals" ,
     main = "Residuals of ETH vs BTC", lty=1:2, cex.axis = 1.1)
grid()



#  ----------------------------  ERROR CORRECTION MODEL ----------------------------------------- #

ec_model <-dynlm( deth_ts ~ L(deth_ts,1) + L(dbtc_ts,1 ) + L(ts(model_coint$residuals),1))
summary(ec_model)
# stargazer(ec_model) # --> LATEX source code for the summary


# Final considerations (see the paper for more details)

# the cointegrating relationship (which does not exist) has no effects!!! 
# --> we conclude that with cryptocurrencies are high volatile,
# and thus it has no sense to speak about long run relationships as in macroeconomic factors

# in general cryptomarkets are very difficult to study with time series!!


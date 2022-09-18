rm(list = ls()) # clean objects workspace

#  Load packages

source("Load_Packages.R")

# setting the directory
setwd("C:/Users/Clod/Desktop/R/ECONOMETRICS_FINALPROJECT")
getwd() # sanity check

# Import data from the .txt file
Dataset <- read.table('Dataset_btc.txt', header = TRUE, sep = ",")

attach(Dataset[1:1000,])


### ---------------- Model with S&D + IA +  macroeconomic factors ------------------------- ###

Model_4 <- lm(log(btc_price) ~ exrate  + days_destroyed +  addresses + transactions  + total_btc + 
                new_members +  new_posts + wikiviews  +
                djia + oil_price + spx500 ) # --> macroeconomic factors
summary(Model_4)
# stargazer(Model_4)


###############################              ARCH models          ####################################


logreturns <- diff(log(btc_price))
plot(as.Date(date[1:length(date)-1]), logreturns ,type = "l" , xlab=" Time ", ylab=" Logreturn " ,
     main = "Bitcoin logreturns from 2010 to 2016 ", lty=1:2, cex.axis = 1.1 )

# Test for Arch effects on logreturns

archtest_logreturn  <- ArchTest(logreturns, lags = 1, demean = TRUE)
archtest_logreturn
# so we reject H0 (error term is iid (0,sigma^2)) since pvalue p-value < 2.2e-16 
# --> logreturns has arch effects


garch_11 = garchFit( formula = ~ arma(0,0)+garch(1,1), data = logreturns, trace = F)
summary(garch_11)

garch_12 = garchFit( formula = ~ arma(0,0)+garch(1,2), data = logreturns, trace = F)
summary(garch_12)

garch_13 = garchFit( formula = ~ arma(0,0)+garch(1,3), data = logreturns, trace = F)
summary(garch_13)


garch_21 = garchFit( formula = ~ arma(0,0)+garch(2,1), data = logreturns, trace = F)
summary(garch_21)

garch_22 = garchFit( formula = ~ arma(0,0)+garch(2,2), data = logreturns, trace = F)
summary(garch_22)

garch_23 = garchFit( formula = ~ arma(0,0)+garch(2,3), data = logreturns, trace = F)
summary(garch_23)


garch_31 = garchFit( formula = ~ arma(0,0)+garch(3,1), data = logreturns, trace = F)
summary(garch_31)

garch_32 = garchFit( formula = ~ arma(0,0)+garch(3,2), data = logreturns, trace = F)
summary(garch_32)

garch_33 = garchFit( formula = ~ arma(0,0)+garch(3,3), data = logreturns, trace = F)
summary(garch_33)

# store all the AIC/BIC/SIC/HQIC of all the models 
AIC_garch_logreturns = c( garch_11@fit$ics[1], garch_12@fit$ics[1] , garch_13@fit$ics[1] , 
                          garch_21@fit$ics[1], garch_22@fit$ics[1] , garch_23@fit$ics[1] ,
                          garch_31@fit$ics[1], garch_32@fit$ics[1] , garch_33@fit$ics[1])

BIC_garch_logreturns = c( garch_11@fit$ics[2], garch_12@fit$ics[2] , garch_13@fit$ics[2] , 
                          garch_21@fit$ics[2], garch_22@fit$ics[2] , garch_23@fit$ics[2] ,
                          garch_31@fit$ics[2], garch_32@fit$ics[2] , garch_33@fit$ics[2] )

SIC_garch_logreturns = c( garch_11@fit$ics[3], garch_12@fit$ics[3] , garch_13@fit$ics[3] , 
                          garch_21@fit$ics[3], garch_22@fit$ics[3] , garch_23@fit$ics[3] ,
                          garch_31@fit$ics[3], garch_32@fit$ics[3] , garch_33@fit$ics[3] )

HQIC_garch_logreturns = c( garch_11@fit$ics[4], garch_12@fit$ics[4] , garch_13@fit$ics[4] , 
                           garch_21@fit$ics[4], garch_22@fit$ics[4] , garch_23@fit$ics[4] ,
                           garch_31@fit$ics[4], garch_32@fit$ics[4] , garch_33@fit$ics[4] )

which.min(AIC_garch_logreturns)
which.min(BIC_garch_logreturns)
which.min(SIC_garch_logreturns)
which.min(HQIC_garch_logreturns)

# we exploit a GARCH(3,3) model

fl_logret = rev(garch_33@fitted)
est_price = btc_price[length(btc_price)]
for (i in 2:length(btc_price)){
  z = est_price[length(est_price)]*exp(fl_logret[i-1])
  est_price = c(est_price, z )
}

# Comparison between historical price and fitted values using GARCH(3,3) model

plot(as.Date(date), btc_price , col = "blue", type = "l", xlab=" Time ", ylab=" Price ($) " ,
     main = "Bitcoin price (USD) from 2010 to 2016 ", lty=1:2, cex.axis = 1.1)

lines(as.Date(date), est_price, col = "orange" )
legend("bottomright", legend=c(" Historical ", " Model "),col=c("blue", "orange"), lty=1:2, cex = 1.2)
grid()


###############################################################################

# Test for Arch effects on the residuals in model 4 (supply and demand + attractiveness variables)

res4 = Model_4$residuals
archtest_res4  <- ArchTest(res4, lags = 1, demean = TRUE)
archtest_res4

# so we reject H0 (error term is iid (0,sigma^2)) since pvalue p-value < 2.2e-16 
# --> residuals of model 4 have arch effects



garch_11_res4 = garchFit( formula = ~ arma(0,0)+garch(1,1), data = res4, trace = F)
summary(garch_11_res4)

garch_12_res4 = garchFit( formula = ~ arma(0,0)+garch(1,2), data = res4, trace = F)
summary(garch_12_res4)

garch_13_res4 = garchFit( formula = ~ arma(0,0)+garch(1,3), data = res4, trace = F)
summary(garch_13_res4)

garch_21_res4 = garchFit( formula = ~ arma(0,0)+garch(2,1), data = res4, trace = F)
summary(garch_21_res4)

garch_22_res4 = garchFit( formula = ~ arma(0,0)+garch(2,2), data = res4, trace = F)
summary(garch_22_res4)

garch_23_res4 = garchFit( formula = ~ arma(0,0)+garch(2,3), data = res4, trace = F)
summary(garch_23_res4)

garch_31_res4 = garchFit( formula = ~ arma(0,0)+garch(3,1), data = res4, trace = F)
summary(garch_31_res4)

garch_32_res4 = garchFit( formula = ~ arma(0,0)+garch(3,2), data = res4, trace = F)
summary(garch_32_res4)

garch_33_res4 = garchFit( formula = ~ arma(0,0)+garch(3,3), data = res4, trace = F)
summary(garch_33_res4)

# store all the AIC of all the models 
AIC_garch_res4 = c( garch_11_res4@fit$ics[1], garch_12_res4@fit$ics[1] , garch_13_res4@fit$ics[1] , 
                    garch_21_res4@fit$ics[1], garch_22_res4@fit$ics[1] , garch_23_res4@fit$ics[1] ,
                    garch_31_res4@fit$ics[1], garch_32_res4@fit$ics[1] , garch_33_res4@fit$ics[1])

BIC_garch_res4 = c( garch_11_res4@fit$ics[2], garch_12_res4@fit$ics[2] , garch_13_res4@fit$ics[2] , 
                    garch_21_res4@fit$ics[2], garch_22_res4@fit$ics[2] , garch_23_res4@fit$ics[2] ,
                    garch_31_res4@fit$ics[2], garch_32_res4@fit$ics[2] , garch_33_res4@fit$ics[2] )

SIC_garch_res4 = c( garch_11_res4@fit$ics[3], garch_12_res4@fit$ics[3] , garch_13_res4@fit$ics[3] , 
                    garch_21_res4@fit$ics[3], garch_22_res4@fit$ics[3] , garch_23_res4@fit$ics[3] ,
                    garch_31_res4@fit$ics[3], garch_32_res4@fit$ics[3] , garch_33_res4@fit$ics[3] )

HQIC_garch_res4 = c( garch_11_res4@fit$ics[4], garch_12_res4@fit$ics[4] , garch_13_res4@fit$ics[4] , 
                     garch_21_res4@fit$ics[4], garch_22_res4@fit$ics[4] , garch_23_res4@fit$ics[4] ,
                     garch_31_res4@fit$ics[4], garch_32_res4@fit$ics[4] , garch_33_res4@fit$ics[4] )

which.min(AIC_garch_res4)
which.min(BIC_garch_res4)
which.min(SIC_garch_res4)
which.min(HQIC_garch_res4)

# we exploit a GARCH(1,3) model

# Comparison between historical logprice and fitted values (of Model 4)
plot(as.Date(date), log(btc_price) , col = "blue", type = "l", xlab=" Time ", ylab=" LogPrice ($) " ,  ## ----------------- plot 2
     main = "Bitcoin logprice (USD) from 2010 to 2016 ", lty=1:2, cex.axis = 1.1)
lines(as.Date(date), Model_4$fitted.values + garch_13_res4@fitted , col = "orange" )
legend("bottomright", legend=c(" Historical ", " Model ", "Model + garch "),col=c("blue", "red", "orange"), lty=1:2, cex = 1.2)
grid()

Error_model4 = sum(abs(btc_price -  Model_4$fitted.values ))
Error_model4_garch = sum(abs(btc_price -  Model_4$fitted.values - garch_13_res4@fitted ))



##########################################################################################################################



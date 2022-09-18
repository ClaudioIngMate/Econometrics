################################################################################################
######################## ECONOMETRICS 2021/2022 - Final Project ################################

#  ------------------- ' Factors influencing bitcoin price ' -------------------------- #

# --------------- Authors: --------------- # 
# MALIGHETTI Matteo Giuseppe   c.p. 10560903
# MANZONI Claudio              c.p. 10580671
# PRETALLI Giulia              c.p. 10614394 

rm(list = ls()) # clean objects workspace

#  Load packages
source("Load_Packages.R")

# setting the directory
setwd("C:/Users/Clod/Desktop/R/ECONOMETRICS_FINALPROJECT")
getwd() # sanity check

# Import data from the .txt file
Dataset <- read.table('Dataset_btc.txt', header = TRUE, sep = ",")

# Latex for code for a quick dataset view
#kable(Dataset[1:3,1:6], "latex")
#kable(Dataset[1:3,7:13], "latex")

# For ease of use
attach(Dataset)

## -----------------------------  Dataset inspection ----------------------------------------###

head(Dataset, n = 5)    # first n rows of the dataset
tail(Dataset, n = 5)    # last n rows of the dataset
class(Dataset)          # class of the variable 
str(Dataset)            # structure of the object
dim(Dataset)            # dimension of the object

# Plot of the variable that we want to explain / investigate factors influencing it
plot(as.Date(date), log(btc_price) , col = "blue", type = "l", xlab=" Time ", ylab=" Price ($) " ,
     main = "Bitcoin logprice (USD) from 2010 to 2016 ", lty=1:2, cex.axis = 1.1)
grid()

# Plot of the variable that we want to explain / investigate factors influencing it
plot(as.Date(date), btc_price , col = "blue", type = "l", xlab=" Time ", ylab=" Price ($) " ,
     main = "Bitcoin price (USD) from 2010 to 2016 ", lty=1:2, cex.axis = 1.1)
grid()


# useful functions for dataset inspection : mean, var, sd, min, max, median, quantile, kurtosi, skew
#                                           summary, stat.desc, describe


## ------------------------- Investigate correlations ----------------------------------------###

# Heatmap

heatmap(as.matrix(as.numeric(Dataset))  )



data_correlations <-cor(Dataset[sapply(Dataset,is.numeric)])
data_correlations<-melt(data_correlations)
ggplot(data_correlations,aes(x= Var1, y = Var2, fill = value))+
  geom_tile()+ scale_fill_distiller() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# An example of correlation test: btc_price and number of transactions
cor(btc_price, transactions)
cor.test(btc_price, transactions) # Correlation test. Null hypothesis: rho =0, Alternative: rho!=0
# Result -> we reject the null hypothesis of no correlation

# Create a Time series object
data_ts <- xts(x=Dataset, order.by=as.Date(Dataset$date)) 


#################################################################################################

# Now we try some models --> however we have to pay attention to these models
# indeed they are spurious regressions: the time series, indeed, are nonstationary
# (see the bottom of this script for more technical details)


### -------------------- 1.  Model with supply and demand variables -------------------------- ###

# supply and demand variables are exrate, days destroyed , addresses, transactions, total bitcoins

Model_1 <- lm(btc_price ~ exrate  + days_destroyed +  addresses + transactions  + total_btc )
summary(Model_1)
# stargazer(Model_1) # provides LaTeX source code for the model summary


### ------------------- 2. Model with S&D + investment attractiveness ------------------------ ###

Model_2 <- lm(btc_price ~ exrate  + days_destroyed +  addresses + transactions  + total_btc + 
                  new_members +  new_posts + wikiviews) # --> attractiveness factors
summary(Model_2)
# stargazer(Model_2)

### ---------------- 3.  Model with S&D + IA +  macroeconomic factors ------------------------- ###

Model_3 <- lm(btc_price ~ exrate  + days_destroyed +  addresses + transactions  + total_btc + 
                new_members +  new_posts + wikiviews  +
              djia + oil_price + spx500 ) # --> macroeconomic factors
summary(Model_3)
# stargazer(Model_3)

### ---------------- 4. Model with S&D + IA +  macroeconomic factors ------------------------- ###

Model_4 <- lm(log(btc_price) ~ exrate  + days_destroyed +  addresses + transactions  + total_btc + 
                new_members +  new_posts + wikiviews  +
                djia + oil_price + spx500 ) # --> macroeconomic factors
summary(Model_4)
# stargazer(Model_4)


# How we select the best model --> " the lower Information Criterion, the better is the model "

AIC(Model_1, Model_2, Model_3, Model_4) # Akaike Information Criterion
BIC(Model_1, Model_2, Model_3, Model_4) # Bayesian Information Criterion
# from here we deduce that model 4 is the best


# Comparison between historical logprice and fitted values (of Model 4)
plot(as.Date(date), log(btc_price) , col = "blue", type = "l", xlab=" Time ", ylab=" LogPrice ($) " ,
     main = "Bitcoin logprice (USD) from 2010 to 2016 ", lty=1:2, cex.axis = 1.1)
lines(as.Date(date), Model_4$fitted.values, col = "orange" )
legend("bottomright", legend=c(" Historical ", " Model "),col=c("blue", "orange"), lty=1:2, cex = 1.2)
grid()

# ----------------------------  PREDICTIVE CONTENT ????????????  ------------------------------ # 


Dataset_training = Dataset[430:length(Dataset[,1])-400,]
Dataset_test = Dataset[1:429,]

Model_training <- lm(log(btc_price) ~ exrate  + days_destroyed +  addresses + transactions  + total_btc + 
                new_members +  new_posts + wikiviews  +
                djia + oil_price + spx500, data = Dataset_training ) # --> macroeconomic factors
summary(Model_training)

RMSE_training = sqrt(mean(Model_training$residuals^2))

predictions <- predict(Model_training, newdata = Dataset_test)

# Comparison between historical logprice and fitted values (of Model 4)
plot(as.Date(date[1:429]), predictions, col = "blue", type = "l" , xlab=" Time ", ylab="Logprice ($) " , 
     main = "Bitcoin logprice : model VS real data ", lty=1:2, cex.axis = 1.1)

lines(as.Date(date[1:429]), log(btc_price[1:429]) , col = "red", type = "l")
legend("bottomright", legend=c(" Historical ", " Model prediction "),col=c("blue", "red"), lty=1:2, cex = 1.2)
grid()

RMSE_test = sqrt(mean((log(btc_price[1:429])-predictions)^2)) # good, if compared to the one of the training set

# it has some predictive content but we would not rely on this model since it is spurious...



##################################################################################################
#------------------------------     STATIONARITY ANALYSIS        --------------------------------#
##################################################################################################


# One key requirement in doing linear regression is the stationarity of the variables. Let's investigate using the 
# Dickey - Fuller test (H0: nonstationary, we accept H0 with high pvalues)

for (i in 2:13){
  test=adf.test(Dataset[,i])
  print(colnames(Dataset)[i])
  print(test$p.value)
}


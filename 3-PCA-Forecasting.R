
################################################################
################################################################

## Name: Structural Vector Autoregression Modelling 

## 3-PCA-Forecasting: I load the wide dataset of macroeconomic indicators

## Author: Dominik Prugger 
## Date: October 2019



# Begin by deleting all previous working space and loading the required packages
rm(list = ls())


# State the packages required for this analysis
packages <- c("xts", 
              "ggplot2", 
              "tidyverse", 
              "dygraphs", 
              "quantmod", 
              "plyr", 
              "stats", 
              "stargazer", 
              "xlsx", 
              "readxl")

# Check if these packages are already installed on the computer, if not install them 
list.of.packages <- packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

# Use sapply to require all packages in one
sapply(packages, require, character.only = TRUE)


# Specify working directory and load the data
setwd("~/GitHub/Structural-Vector-Autoregression-Modeling")


# Delete old data and read in the new one 
rm(list=ls(all=TRUE))


################################### Step 0: Read in data and clean ####################################

###read in the data
df1 <- read_excel("~/Utrecht/M-Economics/Period 7/Applied Macroeconometrics/Assignments/Assignment 3/FREDQD.xlsx", col_names = FALSE)

#df2 <- read.xlsx("~/Utrecht/M-Economics/Period 7/Applied Macroeconometrics/Assignments/Assignment 3/FREDQD.xlsx", 1, colClasses = c("character", rep("numeric", 248)))
colnames(df1) <- df1[2, ]
df1 <- df1[-2, ]

# define  vector with the groups, just in case
groups <- df1[1, ]

#Now do a date-vector for the rownames
quarters <- seq(as.Date("1959/10/1"), as.Date("2018/10/1"), by = "quarter")
rownames <- c("Group", quarters)


rownames(df1) <- rownames

#now delete the series but also the first row with the groups

df1 <- df1[-1, ]
df1$Series <- NULL

#and now amplify the rownames again
rownames(df1) <- quarters

#check class of data
sapply(df1, class)

df1 <- as.data.frame(sapply(df1, as.numeric))
sapply(df1, mode)



######################################### Exercise 1 ###########################
#Use principal component analysis to extract factors from the data (Note:omit the first series GDPC1!). 
#How much of the total variation in the data is explained by each factor?

df2 <- df1

#Omitting GDPC, as it is the dependent variable 

df2$GDPC1 <- NULL 

#Make a matrix
X <- data.matrix(df2)

## 1. Calculate R = 1/T *X'X, the correlation matrix of X

## Specify the correlation matrix of X
R <- (1/length(quarters)) * crossprod(X, X)

## 2.Do the eigenvector decomposition
e_R <- eigen(R)
R_2 <- e_R$vectors  %*%  diag(e_R$values) %*% t(e_R$vectors)

##Check if both are identical
dim(R_2)
dim(R)

## 3. Check estimated loadings 
# Test if the product of factor loadings result in Identity matrix
test <- t(e_R$vectors) %*% e_R$vectors
#Approximately so; very low off-diagonal elements, e.g. 

test[1, 2]
# 7.806256e-17

##4. Calculate the factors 
factors <- X %*% e_R$vectors

###### Make a dataframe out of them 
df_factors <- as.data.frame(factors)
plot(df_factors$V1, type = "l")

### Estimate the X matrix: 

X_2 <- factors %*% t(X)

count(e_R$values < 0)
dim(e_R$values)

##how much variance is explained by the largest factor, which is 48?
max(e_R$values)
max(e_R$values)/sum(e_R$values)

## Thus nearly 20% of the variance is explained by factor corresponding to the largest eigenvalue. 

#How much of the total variation in the data is explained by the factor matrix? 
#First check how many non-zero eigenvalues exist 
count(e_R$values !=0)
#Result: All 247

eigen <- sort(e_R$values, decreasing = T)

var_exp <- NULL
for(i in 1:length(eigen)){
  var_exp[i] <- eigen[i]/sum(eigen)
}

# Plot the distribution of variance explained 
df_test <- as.data.frame(cbind(var_exp, c(1:247)))
ggplot(df_test, aes(x = V2, y = var_exp)) + 
  geom_line(colour = "blue", size = 1) + xlab("Number of factor") + ylab("% of variance explained") + 
  theme_bw()

#First 10 entries 
var_exp[c(1:10)]

## So whereas the first variable explains around 20 percent of the variation, the second factor already explains only around 8 %
# and the 10th factor around 2 %


##############################################################################################################################
#2. Analyze the factor loadings (potentially it helps to look at average absolute loadings over groups) for the first three 
#factors. Can you give an interpretation to the first three factors?
##############################################################################################################################
library(tidyverse)

#prepare the group vector
groups2 <- groups[-c(1,2)]
group <- as.numeric(groups2)

### Start with first loading

#Create data frame with the factor loadings and the groups 
l1 <- abs(e_R$vectors[, 1])
df_l1 <- as.tibble(as.data.frame(cbind(l1, group)))

#Now plot the average values for each group 
ggplot(df_l1) + 
  geom_bar(aes(y = l1, x = as.factor(group)), position = "dodge", stat = "summary", fun.y = "mean") + 
  xlab("Group of variables") + ylab("Averaged loadings for the first factor") + theme_bw()


### Now do the second loading 
l2 <- abs(e_R$vectors[, 2])
df_l2 <- as.tibble(as.data.frame(cbind(l2, group)))

ggplot(df_l2) + 
  geom_bar(aes(y = l2, x = as.factor(group)), position = "dodge", stat = "summary", fun.y = "mean") + 
  xlab("Group of variables") + ylab("Averaged loadings for the second factor") + theme_bw()

### Now do the third loading 
l3 <- abs(e_R$vectors[, 3]) 
df_l3 <- as.tibble(as.data.frame(cbind(l3, group)))

ggplot(df_l3) + 
  geom_bar(aes(y = l3, x = as.factor(group)), position = "dodge", stat = "summary", fun.y = "mean") + 
  xlab("Group of variables") + ylab("Averaged loadings for the second factor") + theme_bw()



##############################################################################################################################
#3: Take the first series GDPC1 as your dependent variable yt and estimate the following model: 
 
# for p = 4 and k = 0; : : : 12. Note that we use the first lag of the factors here.
# According to AIC or BIC, how many factors should you include?
##############################################################################################################################

loading <- e_R$vectors

## Define a vector for the "slow moving variable groups", as in the lecture slides and Bernanke et al. (2005)
## Aim: I want to be able to make the assumption that a policy shock has no contemporaneous effect on the factors

slow_group <- c(1,2,3,6,7)

#extract the position in the dataframe for all group numbers 1,2,3,6 or 7
position <- which(group %in% slow_group)
group[position]
X_slow <- data.matrix(df2[, position])

## Specify the correlation matrix of X
R <- (1/length(quarters)) * crossprod(X_slow, X_slow)

## 2.Do the eigenvector decomposition
e_R <- eigen(R)
R_2 <- e_R$vectors  %*%  diag(e_R$values) %*% t(e_R$vectors)

#construct slow-moving factors
dim(e_R$vectors)
factors_slow <- X_slow %*% e_R$vectors


#Construct one big matrix
X_r <- cbind(factors_slow, df1$GDPC1)

#check position
X_r[, 151]
df1$GDPC1

#So it adds it to the back

##Now run the matrix regression
beta  <- solve(t(X_r)%*%X_r)%*%t(X_r)%*%factors

#check with
reg1 <- lm(factors ~ X_r -1)
beta_lm <- reg1$coefficients

## Now calculate the slow moving factors by subtracting the portion explained by contemporaneous yt 

factors_c <- factors - df1$GDPC1 %*%t(t(beta)[, 151])

# Now do the full regression and check for AIC 
y <- df1$GDPC1
#lag all factors by one
na237 <- rep(NA, 237)

factors_c_lag <- rbind(na237, factors_c)[-238, ]

X <- cbind(y, c(NA, y[-237]), c(NA, NA, y[-c(236, 237)]), c(NA, NA, NA, y[-c(235,236,237)]), 
                          c(NA, NA, NA, NA, y[-c(234, 235, 236, 237)]), factors_c_lag)

# Now do the AIC and BIC criteria


##One test to see if computed correctly 
reg1 <- lm(X[,1] ~ X[, c(2:6)])
res <- resid(reg1)
res2 <- resid(reg1)^2
AIC <- log(res1) + 2*(4+1)/length(X[, 1])

## One loop for AIC 
AIC <- NULL
for(i in 1:12){
  reg1 <- lm(X[,1] ~ X[, c(2:5+i)])
  res1 <- (1/length(X[, 1]))*resid(reg1)%*% resid(reg1)
  AIC[i] <- log(res1) + 2*(5+i)/length(X[, 1])
}

AIC <- NULL
for(i in 1:12){
  reg1 <- lm(X[,1] ~ X[, c(2:5+i)])
  res1 <- (1/length(X[, 1]))*sum((resid(reg1)^2))
  AIC[i] <- log(res1) + 2*(5+i)/length(X[, 1])
}

## One loop for BIC
BIC <- NULL
for(i in 1:12){
  reg1 <- lm(X[,1] ~ X[, c(2:5+i)])
  res1 <- (1/length(X[, 1]))*resid(reg1)%*% resid(reg1)
  BIC[i] <- log(res1) + log(length(X[, 1]))/length(X[, 1])*(5+i)
}

##define criterias
criteria <- as.data.frame(cbind(AIC, BIC, c(1:12)))

## Now plot
ggplot(criteria, aes(y = AIC, x = V3)) + 
  geom_line(colour = "blue", size = 1) + theme_bw() +scale_x_discrete(name ="Amount of factors included", 
                                                                      limits=c(1:12))

ggplot(criteria, aes(y = BIC, x = V3)) + 
  geom_line(colour = "blue", size = 1) + theme_bw() +scale_x_discrete(name ="Amount of factors included", 
                                                                      limits=c(1:12))


##############################################################################################################################
# 4. Estimate the model in (1) with k = 8 based on data between 1959Q4 and 2003Q3. Note that you first need 
#to determine the factors based on data between 1959Q4 and 2003Q3. Using (1), construct a forecast for 2003Q4. 
# Then, re-estimate the factors and the model using data between 1959Q4 and 2003Q4, and construct a forecast for 
# 2004Q1. Continue until you have a set of forecasts for the period 2003Q4-2018Q3. Plot the forecasts and the actual 
# values. Do the same for a model without factors (so the AR(4) model). Compare the forecast accuracy of both models 
#under different loss functions.

rownames(df1) <- quarters

## Now estimate factors for the time period 

#first do a subset of the data for the timeframe until 2003Q4
quarters[176]
subset <- df1[c(1:176), c(2:248)]
X <- data.matrix(subset)

#Then estimate the normal factors 
R <- (1/length(X[, 1])) * crossprod(X, X)
e_R <- eigen(R)
factors <- X %*% e_R$vectors

#Only take first eight factors
factors <- factors[, c(1:8)]

####### Now run the real regression to get coefficients for the forecast
y <- df1$GDPC1[1:176]

#Do one row of lag for all factors
na237 <- rep(NA, 8)

#join and delete the last row 
factors_c_lag <- rbind(na237, factors)[-177, ]

X <- cbind(y, c(NA, y[-176]), c(NA, NA, y[-c(175, 176)]), c(NA, NA, NA, y[-c(174, 175, 176)]), 
           c(NA, NA, NA, NA, y[-c(173, 174, 175, 176)]), factors_c_lag)

#Run regression
reg1 <- lm(X[,1] ~ X[, c(2:13)])

#Now estimate the yhat for Q4 2003

test <- c(1, X[176, c(1,2,3,4)], factors[176, ])
length(test)
yhat <- t(test) %*% reg1$coefficients


#Now lets do a loop!
#From forecasting 2003Q4 until 2018Q3, I have to do 61 forecasts

forecast <- NULL

for(i in 0:60){
  q             <- quarters[176 + i]
  subset        <- df1[c(1:(176 + i)), c(2:248)]
  X             <- data.matrix(subset)
  R             <- (1/length(X[, 1])) * crossprod(X, X)
  e_R           <- eigen(R)
  factors       <- X %*% e_R$vectors
  factors       <- factors[, c(1:8)]
  y             <- df1$GDPC1[1:(176+i)]
  na237         <- rep(NA, 8)
  factors_c_lag <- rbind(na237, factors)[-(177+i), ]
  X             <- cbind(y, c(NA, y[-(176+i)]), c(NA, NA, y[-c(175 +i, 176+i)]), c(NA, NA, NA, y[-c(174+i, 175+i, 176+i)]), 
                    c(NA, NA, NA, NA, y[-c(173+i, 174+i, 175, 176)]), factors_c_lag)
  reg1 <- lm(X[,1] ~ X[, c(2:13)])
  test <- c(1, X[(176+i), c(1,2,3,4)], factors[(176+i), ])
  yhat <- t(test) %*% reg1$coefficients
  forecast[1+i] <- yhat
  }


### Test the accuracy 

##Construct a data frame with the value, type and time 
df_forecast <- as.data.frame(cbind(forecast, df1$GDPC1[c(177:237)]))
df_forecast <- cbind(df_forecast, as.Date(quarters[177:237]))
colnames(df_forecast) <- c("Forecast", "Actual value", "Time")

#df_forecast <- cbind(forecast, df1$GDPC1[c(177:237)])
#df_forecast_a  <- append(forecast, df1$GDPC1[c(177:237)])
#df_forecast2 <- as.data.frame(cbind(df_forecast_a, c(rep("forecast", 61), rep("Real data", 61))))
#df_forecast <- cbind(df_forecast2, c(quarters[177:237], quarters[177:237]))


#Now plot

ggplot(df_forecast) + 
  geom_line(aes(x = df_forecast$Time, y = df_forecast$Forecast, colour = "AR(4) + 8 factor model"), size = 1) + 
  geom_line(aes(x = df_forecast$Time, y = df_forecast$`Actual value`, colour = "Actual data"), size = 1) +
  xlab('Time: 2003Q4 - 2018Q4') +
  ylab('Predicted and Actual GDPC1')


#### Now estimate a full AR(4) model 

forecast_AR <- NULL

for(i in 0:60){
  y             <- df1$GDPC1[1:(176+i)]
  X             <- cbind(y, c(NA, y[-(176+i)]), c(NA, NA, y[-c(175 +i, 176+i)]), c(NA, NA, NA, y[-c(174+i, 175+i, 176+i)]), 
                         c(NA, NA, NA, NA, y[-c(173+i, 174+i, 175, 176)]))
  reg1 <- lm(X[,1] ~ X[, c(2:5)])
  test <- c(1, X[(176+i), c(1,2,3,4)])
  yhat <- t(test) %*% reg1$coefficients
  forecast_AR[1+i] <- yhat
}

## add it to the forecast df
df_forecast <- cbind(df_forecast, forecast_AR)


## Plot 
ggplot(df_forecast) + 
  geom_line(aes(x = df_forecast$Time, y = df_forecast$Forecast, colour = "AR(4) + 8 factor model"), size = 1) + 
  geom_line(aes(x = df_forecast$Time, y = df_forecast$`Actual value`, colour = "Actual data"), size = 1) +
  geom_line(aes(x = df_forecast$Time, y = df_forecast$forecast_AR, colour = "AR(4) model"), size = 1) +
  xlab('Time: 2003Q4 - 2018Q4') +
  ylab('Predicted and Actual GDPC1')

## From the graph, the AR(4) factor model clearly outperforms the AR(4) model 

## Now use different methods or loss functions to determine which one is better

#1.  sum of residuals

df_forecast$e_factor <- df_forecast$Forecast - df_forecast$`Actual value`
df_forecast$e_AR     <- df_forecast$forecast_AR - df_forecast$`Actual value`

#Compare: 
sum(df_forecast$e_factor)
sum(df_forecast$e_AR)

#2. Sum of squared residuals 

df_forecast$e2_factor <- (df_forecast$Forecast - df_forecast$`Actual value`)^2
df_forecast$e2_AR     <- (df_forecast$forecast_AR - df_forecast$`Actual value`)^2

sum(df_forecast$e2_factor)
sum(df_forecast$e2_AR)

##plot 
ggplot(df_forecast) + 
  geom_line(aes(x = df_forecast$Time, y = df_forecast$e2_factor, colour = "AR(4) + 8 factor model"), size = 1) + 
  geom_line(aes(x = df_forecast$Time, y = df_forecast$e2_AR, colour = "AR(4) model"), size = 1) +
  xlab('Time: 2003Q4 - 2018Q4') +
  ylab('Squared error for both forecast models')
#3. Absolute values of the residuals 
sum(abs(df_forecast$e_factor))
sum(abs(df_forecast$e_AR))

##Also here now the model including 8 factors is better (lesser loss function) than the AR(4) model

#4. Diebold Mariano test 

#Define the difference between loss functions 

df_forecast$diebold <- df_forecast$e2_factor - df_forecast$e2_AR

#run regresssion on the constant 
reg1 <- lm(df_forecast$diebold ~ 1)
reg2 <- NeweyWest(reg1)
summary(reg2)
reg3 <- coeftest(reg1, vcov=NeweyWest(reg1))
summary(reg3)

stargazer(reg3, title = "Diebold-Mariano test comparing AR(4) with and without factors", report = "vct*")
##with a t-value of -1,062, I can't reject the hypothesis that both forecasts have same accuracy

dm.test(df_forecast$e2_factor, df_forecast$e2_AR, h = 1, power = 2, alternative = "two.sided")
##Therefore choose the factor + AR(4)  model which performs better

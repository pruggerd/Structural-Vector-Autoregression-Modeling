# hello world


#####################################################
# Assignment 2 for Applied Macroeconometrics
# Dominik Prugger, Utrecht University, Stdnr: 6236138

#####################################################


## Disclaimer: This do-file has all commands used for Assignment 1. For it to be runnable, it would be needed to 
## one time change the working directory to the used working directory. 
## E.g. one time replacing the four lines where it is called by ctrl + f



#### Step 0: install packages, but only once
install.packages("vars")
install.packages("forecast")

library(xts)
library(ggplot2)
library(tidyverse)
library(dygraphs)
library(quantmod)
library(plyr)
library(stats)
library(vars)
library(forecast)


######################## Assignment 2 ##################################################################
rm(list=ls(all=TRUE))

#set working directory
setwd("~/Utrecht/M-Economics/Period 7/Applied Macroeconometrics/Assignments/Assignment 1/Data")

#load in the data
df1 <- read.csv("stationary_series.csv")
rownames(df1) <- df1$Index
df1 <- df1[, -1]
df1 <- as.xts(df1)


#construct two other dataframes for the different timeframes
df1_e <- df1["/1990"]
df1_t <- df1["1991/"]


## plot the series (here adjust the time axis!)
plot(df1$dif1_earning)
plot(df1$dif1_unemp)
plot(df1$dif1_gnp)

plot(df1_e$dif1_earning)
plot(df1_e$dif1_gnp)
plot(df1_e$dif1_unemp)

plot(df1_t$dif1_earning)
plot(df1_t$dif1_gnp)
plot(df1_t$dif1_unemp)

## Computean AR(1) model for the entire timeframe (pe)

var.1 <- VAR(na.omit(df1), 1, type = "none")
summary(var.1)


##Now check if the same can be said for comparing it with manual regression: Example on earnings
reg1 <- lm(df1$dif1_earning ~ lag(df1$dif1_earning, 1) + lag(df1$dif1_gnp, 1) + lag(df1$dif1_unemp, 1))
reg2 <- lm(df1$dif1_gnp ~ lag(df1$dif1_earning, 1) + lag(df1$dif1_gnp, 1) + lag(df1$dif1_unemp, 1))
reg3 <- lm(df1$dif1_unemp ~ lag(df1$dif1_earning, 1) + lag(df1$dif1_gnp, 1) + lag(df1$dif1_unemp, 1))

A1 <- rbind(c(reg1$coefficients[2:4]),
            c(reg2$coefficients[2:4]),
            c(reg3$coefficients[2:4]))

stargazer(reg1, reg2, reg3, report = "vctp*", title = "Results from manual VAR(1) regressions: Full sample")

stargazer(A1, title="Reduced form coefficients from VAR(1), using manual regression")
#Earning: Significant coefficients for earning (0.148), gnp (0.195) and unemp (0.0035)
#GNP: Significant coefficients for gnp (0.665) and insignificant for earning (-0.0572) and unemp (0.0023)
#Unemp: Significant coefficients for earning (11.635), GNP (-8.753) and unemp (0.545)

## Now do the VAR(1) for the timeframe 1948 - 1990
reg1 <- lm(df1_e$dif1_earning ~ lag(df1_e$dif1_earning, 1) + lag(df1_e$dif1_gnp, 1) + lag(df1_e$dif1_unemp, 1))
reg2 <- lm(df1_e$dif1_gnp ~ lag(df1_e$dif1_earning, 1) + lag(df1_e$dif1_gnp, 1) + lag(df1_e$dif1_unemp, 1))
reg3 <- lm(df1_e$dif1_unemp ~ lag(df1_e$dif1_earning, 1) + lag(df1_e$dif1_gnp, 1) + lag(df1_e$dif1_unemp, 1))

A1 <- rbind(c(reg1$coefficients[2:4]),
            c(reg2$coefficients[2:4]),
            c(reg3$coefficients[2:4]))

stargazer(A1, title = "Reduced form coefficients from VAR(1), using manual regression")
stargazer(reg1, reg2, reg3, report = "vctp*", title = "Results from manual VAR(1) regressions: Full sample")


#Earning: Statistical significant coefficients for gnp (0.237) and unemp (0.0031) and insignificant for earning(0.073)
#GNP: Statistical significan coefficient for gnp (0.696) and insignificant for earning (-0.146) and unemp ( 0.003)
#Unemp: Statistical significant for earning (11.03), gnp (-8.65) and unemp (0.537) 

## Now do the same for time interval 1991 - 2018
## Now do the VAR(1) for the timeframe 1948 - 1990
reg1 <- lm(df1_t$dif1_earning ~ lag(df1_t$dif1_earning, 1) + lag(df1_t$dif1_gnp, 1) + lag(df1_t$dif1_unemp, 1))
reg2 <- lm(df1_t$dif1_gnp ~ lag(df1_t$dif1_earning, 1) + lag(df1_t$dif1_gnp, 1) + lag(df1_t$dif1_unemp, 1))
reg3 <- lm(df1_t$dif1_unemp ~ lag(df1_t$dif1_earning, 1) + lag(df1_t$dif1_gnp, 1) + lag(df1_t$dif1_unemp, 1))

A1 <- rbind(c(reg1$coefficients[2:4]),
            c(reg2$coefficients[2:4]),
            c(reg3$coefficients[2:4]))

stargazer(A1, title = "Reduced form coefficients from VAR(1), using manual regression")
stargazer(reg1, reg2, reg3, report = "vctp", title = "Results from manual VAR(1) regressions: Full sample")

#Earning: Statistical significant coefficienct for earning (0.299), gnp (0.098) and unemp (0.005)
#GNP: Statistical significant coefficients for earning (0.441), gnp (0.607), insignificant for unemp (-0.0014)
#Unemp: Statistical significant coefficients for earning (14.110), GNP (-8.879) and unemp (0.547)



############################################# Exercise 2: Granger Causality ###############################################
####now establish granger causality between the series // we are checking to see if including x
#is useful for predicting y when y's own history is already being used for prediction

################# Granger test for 1948 - 2018
#manually for earnings
reg0 <- lm(df1$dif1_earning ~ lag(df1$dif1_earning, 1) + lag(df1$dif1_gnp, 1) + lag(df1$dif1_unemp, 1))
reg1 <- lm(df1$dif1_earning ~ lag(df1$dif1_earning, 1))
anova_wage <- anova(reg0, reg1)
stargazer(anova_wage, title = "F-test on granger causality (earnings) using anova")
###Result: For earnings 1948 - 2018, H0 of no granger causality of unemp and gnp on earning is rejected

#For gnp
reg0 <- lm(df1$dif1_gnp ~ lag(df1$dif1_earning, 1) + lag(df1$dif1_gnp, 1) + lag(df1$dif1_unemp, 1))
reg1 <- lm(df1$dif1_gnp ~ lag(df1$dif1_gnp, 1))
anova_gnp <- anova(reg0, reg1)
stargazer(anova_gnp, title = "F-test on granger causality (gnp) using anova")

## Result: For gnp 1948 - 2018, H0 of granger causality can't be rejected

#cut the dataframe until April 2018, due to missing values for unemployment
df1 <- df1["/2018-05"]
reg0 <- lm(df1$dif1_unemp ~ lag(df1$dif1_earning, 1) + lag(df1$dif1_gnp, 1) + lag(df1$dif1_unemp, 1))
reg1 <- lm(df1$dif1_unemp ~ lag(df1$dif1_unemp, 1))
anova_unemp <- anova(reg0, reg1)
stargazer(anova_unemp, title = "F-test on granger causality (unemp) using anova")
##Result: For unemp 1948 - 2018, H0 of Granger causality is rejected



######################### Exercise 3: Estimate a VAR(p) model for p = 1; 2; 3; 4 and use various information
######################### criteria to determine which lag order is most appropriate.

#first create Var model with 1-4 lags
#create function
fun.aic <- function(x, p){
  var.fun <- VAR(na.omit(x), p, type = "none")
  myresid <- residuals(var.fun)
  mysigma <- crossprod(myresid) *(1/nquarters(x))
  aic <- log(det(mysigma)) + 2*(p*3^2+3)/nquarters(x)
  print(aic)
}



#Calculate for all three periods
all   <-  c(fun.aic(df1, 1), fun.aic(df1, 2), fun.aic(df1, 3), fun.aic(df1, 4))
e     <-  c(fun.aic(df1_e, 1), fun.aic(df1_e, 2), fun.aic(df1_e, 3), fun.aic(df1_e, 4))
t     <-  c(fun.aic(df1_t, 1), fun.aic(df1_t, 2), fun.aic(df1_t, 3), fun.aic(df1_t, 4))


## create the data frame
df2 <- cbind.data.frame(all, c(rep("all", 4)), c(1:4))
df3 <- cbind.data.frame(e, c(rep("e", 4)), c(1:4))
df4 <- cbind.data.frame(t, c(rep("t", 4)), c(1:4))

colnames(df2) <- c("AIC", "Time", "Lag")
colnames(df3) <- c("AIC", "Time", "Lag")
colnames(df4) <- c("AIC", "Time", "Lag")


df_aic <- rbind(df2, df3, df4)

#Do the same for 20 lags? 
all <- NULL
for(i in 1:20){
  all[i] <- fun.aic(df1,i)
  print(all)
}

e <- NULL
for(i in 1:20){
 e[i] <- fun.aic(df1_e, i) 
 print(e)
}

t <- NULL
for(i in 1:20){
  t[i] <- fun.aic(df1_t, i)
  print(t)
}

class(all)

#Do fitting data frame 
## create the data frame
df2 <- cbind.data.frame(all, c(rep("all", 20)), c(1:20))
df3 <- cbind.data.frame(e, c(rep("e", 20)), c(1:20))
df4 <- cbind.data.frame(t, c(rep("t", 20)), c(1:20))

## 
colnames(df2) <- c("AIC", "Time", "Lag")
colnames(df3) <- c("AIC", "Time", "Lag")
colnames(df4) <- c("AIC", "Time", "Lag")


df_aic_20 <- rbind(df2, df3, df4)

## Now do the Bayesian Schwartz criterium as a second criteria
bic <- function(x, p){
var.fun <- VAR(na.omit(x), p, type = "none")
myresid <- residuals(var.fun)
mysigma <- crossprod(myresid) *(1/nquarters(x))
info <- log(det(mysigma)) + log(nquarters(x))*(p*3^2+3)/nquarters(x)
print(info)
}


#Calculate for all three periods
all_bic   <-  c(bic(df1, 1), bic(df1, 2), bic(df1, 3), bic(df1, 4))
e_bic     <-  c(bic(df1_e, 1), bic(df1_e, 2), bic(df1_e, 3), bic(df1_e, 4))
t_bic     <-  c(bic(df1_t, 1), bic(df1_t, 2), bic(df1_t, 3), bic(df1_t, 4))

df2 <- cbind.data.frame(all_bic, c(rep("all", 4)), c(1:4))
df3 <- cbind.data.frame(e_bic, c(rep("e", 4)), c(1:4))
df4 <- cbind.data.frame(t_bic, c(rep("t", 4)), c(1:4))

colnames(df2) <- c("BIC", "Time", "Lag")
colnames(df3) <- c("BIC", "Time", "Lag")
colnames(df4) <- c("BIC", "Time", "Lag")

### do total data frame

df_bic <- rbind(df2, df3, df4)


####Now plot

ggplot(df_aic, aes(Lag, AIC)) + 
  geom_line(aes(group = Time, colour = factor(Time)), size = 1.5) +
  ggtitle("Aikake Information Criterion for the VAR(4) model")

ggplot(df_bic, aes(x = Lag, y= BIC)) + 
  geom_line(aes(group = Time, colour = factor(Time)), size = 1.5) + 
  ggtitle("Bayesian Information Criterion for the VAR(4) model")

## now plot 20 lags

ggplot(df_aic_20, aes(Lag, AIC)) + 
  geom_line(aes(group = Time, colour = factor(Time)), size = 1.5) 

###################### test for myself: Do the same for 40 lags

#Do the same for 40 lags? 
all <- NULL
for(i in 1:40){
  all[i] <- fun.aic(df1,i)
  print(all)
}

e <- NULL
for(i in 1:40){
  e[i] <- fun.aic(df1_e, i) 
  print(e)
}

t <- NULL
for(i in 1:40){
  t[i] <- fun.aic(df1_t, i)
  print(t)
}

class(all)

#Do fitting data frame 
## create the data frame
df2 <- cbind.data.frame(all, c(rep("all", 40)), c(1:40))
df3 <- cbind.data.frame(e, c(rep("e", 40)), c(1:40))
df4 <- cbind.data.frame(t, c(rep("t", 40)), c(1:40))

## 
colnames(df2) <- c("AIC", "Time", "Lag")
colnames(df3) <- c("AIC", "Time", "Lag")
colnames(df4) <- c("AIC", "Time", "Lag")


df_aic_40 <- rbind(df2, df3, df4)

ggplot(df_aic_40, aes(Lag, AIC)) + 
  geom_line(aes(group = Time, colour = factor(Time)), size = 1.5) 

##### Choice of using 4 lags for "all" and "e", also partly based on "Many empirical VAR studies impose 12 monthly lags or 4 quarterly
#lags., Lütkepohl and Kilian Chapter 2. 
var.test <- VAR(na.omit(df1), lag.max = 20, ic = "AIC", type = "none")
summary(var.test)
VARselect(na.omit(df1), type = "both", lag.max = 20)
#### Based on AIC, allowing for more lags yields an optimal lag number of 13



##################### Exercise 4: Discuss an identification scheme for the structural shocks. Use either long-
# run restrictions or a Cholesky identification. Of course you can read GJ for inspiration
df2 <- data.frame(date=index(df1), coredata(df1))
df2_t <- data.frame(date=index(df1_t), coredata(df1_t))
df2_e <- data.frame(date=index(df1_e), coredata(df1_e))

## First do a var model for all the data set with 4 lags for the full period 
reg1 <- lm(df2$dif1_earning ~ lag(df2$dif1_gnp, 1) + lag(df2$dif1_gnp, 1) + lag(df2$dif1_unemp, 1))
reg2 <- lm(df2$dif1_gnp ~ lag(df2$dif1_earning, 1) + lag(df2$dif1_gnp, 1) + lag(df2$dif1_unemp, 1))
reg3 <- lm(df2$dif1_unemp ~ lag(df2$dif1_earning, 1) + lag(df2$dif1_gnp, 1) + lag(df2$dif1_unemp, 1))
           

##construct coefficient matrix
A1 <- rbind(c(reg1$coefficients[2:length(reg1$coefficients)]), 
            c(reg2$coefficients[2:length(reg2$coefficients)]),
            c(reg3$coefficients[2:length(reg3$coefficients)]))


# now construct an identity matrix
m1 <- solve(as.matrix(diag(3))-A1)
mA1 <- as.matrix(diag(3)) - A1

##Construct sigma 
Sigma <-(1/278)*crossprod(cbind(c(reg1$residuals),
                              c(reg2$residuals),
                               c(reg3$residuals)))
#Get theta^2
theta2 <- m1 %*% Sigma %*% t(m1)

#Perform cholesky on theta
theta <- t(chol(theta2))
Ident_Theta <- as.matrix(diag(3)) - theta
B_Inverse <- A1 %*% theta

#from emiel



## Now do the SVAR command with long-run restrictions
#Imposing the following three restrictions
# 1. Aggregate demand has no long-run impact on the log of real output
# 2. Aggregate demand has no long-run impact on the log of real wages
# 3. Labor-supply shock has no long-run impact on the log of real wages. 
#Given the Zt vector from Gamber and Joutz, that means that the matrix (d.earning, d.gnp, .d.unemp)(d.earning, d.gnp, d.unemp)'
# is lower triangular
###own try

##################### Exercise 5: Plot the appropriate impulse response functions and discuss your findings.
#How do they compare to GJ? Is there a difference between the three periods PE, PR and PT ?
var_all <- VAR(na.omit(df1), p= 1)
U <- resid(var_all)
B <- as.matrix(solve(B_Inverse))
Gamma <- B %*% A1 

Mu <- c(reg1$coefficients[1], reg2$coefficients[1], reg3$coefficients[1])

inv((I-B0_inv*Gam))*B0_inv*gam
Epsilon <- U
for (i in 1:dim(U)){
  Epsilon[i,] <- (B%*%U[i,])
}

# Give a one sd shock to the system
Shock <- c(sd(Epsilon[,1]), sd(Epsilon[,2]), sd(Epsilon[,3]))

# Wage shock
Shock_wage <- c(sd(Epsilon[,1]), 0, 0)
# GNP shock
Shock_GNP <- c(0, sd(Epsilon[,2]), 0)
# Unemployment shock
Shock_unemployment <- c(0, 0, sd(Epsilon[,3]))


### Calculate yt 

yt <- Mu + B_Inverse %*% Shock
yt1 <- Mu + ((B_Inverse %*% A1)^2) %*% B_Inverse %*% Shock
yt2 <- Mu + ((B_Inverse %*% A1)^3) %*% B_Inverse %*% Shock
yt3 <- Mu + ((B_Inverse %*% A1)^4) %*% B_Inverse %*% Shock
yt20 <- Mu + crossprod((crossprod(B_Inverse, A1)^20), crossprod(B_Inverse, Shock_wage))


  # Calculate Yt+h for h = 0:40
# Get Mu
# Some random matrix to the power function from the interwebs, works well


# First create empty IR matrix, then add the IR for every value of h from 1 to 40
# Now we apply a shock to all three ts
IR_wage <- matrix(NA, nrow = 3, ncol = 40)
for (h in 1:40){
  Y <- c(0,0,0) + ((B_Inverse %*% A1)^h) %*%(B_Inverse%*%Shock_wage)
  print(Y)
  IR_wage[,h] <- Y
}

IR_GNP <- matrix(NA, nrow = 3, ncol = 40)
for (h in 1:40){
  Y <- c(0,0,0) + ((B_Inverse %*% A1)^h) %*%(B_Inverse%*%Shock_GNP)
  print(Y)
  IR_GNP[,h] <- Y
}

IR_unemployment <- matrix(NA, nrow = 3, ncol = 40)
for (h in 1:40){
  Y <- c(0,0,0) + ((B_Inverse %*% A1)^h) %*%(B_Inverse%*%Shock_unemployment)
  print(Y)
  IR_unemployment[,h] <- Y
}

# Visualize IR
par(mfrow=c(3,3))
# Wage shock
plot(IR_wage[1,], 
     main = 'Response to a shock in real wage',
     ylab = 'Real wage', 
     xlab = 'h',
     type='l')

plot(IR_GNP[1,], 
     main = 'Response to a shock in GNP',
     ylab = 'Real wage', 
     xlab = 'h',
     type='l')

# Unemployment shock
plot(IR_GNP[1,], 
     main = 'Response to a shock in unemployment',
     ylab = 'Real wage', 
     xlab = 'h',
     type='l')

plot(IR_wage[2,], 
     ylab = 'GNP', 
     xlab = 'h',
     type='l')

plot(IR_GNP[2,], 
     ylab = 'GNP', 
     xlab = 'h',
     type='l')


plot(IR_GNP[2,], 
     ylab = 'GNP', 
     xlab = 'h',
     type='l')


plot(IR_wage[3,], 
     ylab = 'Unemployment', 
     xlab = 'h',
     type='l')

# GNP shock


plot(IR_GNP[3,], 
     ylab = 'Unemployment', 
     xlab = 'h',
     type='l')



plot(IR_GNP[3,], 
     ylab = 'Unemployment', 
     xlab = 'h',
     type='l')



########################################################################################################################
## Now for 1948 - 1990 


## First do a var model for all the data set with 4 lags for the full period 
reg1 <- lm(df2_e$dif1_earning ~ lag(df2_e$dif1_gnp, 1) + lag(df2_e$dif1_gnp, 1) + lag(df2_e$dif1_unemp, 1))
reg2 <- lm(df2_e$dif1_gnp ~ lag(df2_e$dif1_earning, 1) + lag(df2_e$dif1_gnp, 1) + lag(df2_e$dif1_unemp, 1))
reg3 <- lm(df2_e$dif1_unemp ~ lag(df2_e$dif1_earning, 1) + lag(df2_e$dif1_gnp, 1) + lag(df2_e$dif1_unemp, 1))


##construct coefficient matrix
A1 <- rbind(c(reg1$coefficients[2:length(reg1$coefficients)]), 
            c(reg2$coefficients[2:length(reg2$coefficients)]),
            c(reg3$coefficients[2:length(reg3$coefficients)]))


# now construct an identity matrix
m1 <- solve(as.matrix(diag(3))-A1)
mA1 <- as.matrix(diag(3)) - A1

##Construct sigma 
Sigma <-(1/length(df2_e$dif1_earning))*crossprod(cbind(c(reg1$residuals),
                                c(reg2$residuals),
                                c(reg3$residuals)))
#Get theta^2
theta2 <- m1 %*% Sigma %*% t(m1)

#Perform cholesky on theta
theta <- t(chol(theta2))
Ident_Theta <- as.matrix(diag(3)) - theta
B_Inverse <- A1 %*% theta


## Now do the SVAR command with long-run restrictions
#Imposing the following three restrictions
# 1. Aggregate demand has no long-run impact on the log of real output
# 2. Aggregate demand has no long-run impact on the log of real wages
# 3. Labor-supply shock has no long-run impact on the log of real wages. 
#Given the Zt vector from Gamber and Joutz, that means that the matrix (d.earning, d.gnp, .d.unemp)(d.earning, d.gnp, d.unemp)'
# is lower triangular
###own try

##################### Exercise 5: Plot the appropriate impulse response functions and discuss your findings.
#How do they compare to GJ? Is there a difference between the three periods PE, PR and PT ?
var_all <- VAR(na.omit(df1_e), p= 1)
U <- resid(var_all)
B <- as.matrix(solve(B_Inverse))
Gamma <- B %*% A1 
Mu <- c(reg1$coefficients[1], reg2$coefficients[1], reg3$coefficients[1])

Epsilon <- U
for (i in 1:dim(U)){
  Epsilon[i,] <- (B%*%U[i,])
}

# Give a one sd shock to the system
Shock <- c(sd(Epsilon[,1]), sd(Epsilon[,2]), sd(Epsilon[,3]))

# Wage shock
Shock_wage <- c(sd(Epsilon[,1]), 0, 0)
# GNP shock
Shock_GNP <- c(0, sd(Epsilon[,2]), 0)
# Unemployment shock
Shock_unemployment <- c(0, 0, sd(Epsilon[,3]))


# First create empty IR matrix, then add the IR for every value of h from 1 to 40
# Now we apply a shock to all three ts
IR_wage <- matrix(NA, nrow = 3, ncol = 40)
for (h in 1:40){
  Y <- c(0,0,0) + ((B_Inverse %*% A1)^h) %*%(B_Inverse%*%Shock_wage)
  print(Y)
  IR_wage[,h] <- Y
}

IR_GNP <- matrix(NA, nrow = 3, ncol = 40)
for (h in 1:40){
  Y <- c(0,0,0) + ((B_Inverse %*% A1)^h) %*%(B_Inverse%*%Shock_GNP)
  print(Y)
  IR_GNP[,h] <- Y
}

IR_unemployment <- matrix(NA, nrow = 3, ncol = 40)
for (h in 1:40){
  Y <- c(0,0,0) + ((B_Inverse %*% A1)^h) %*%(B_Inverse%*%Shock_unemployment)
  print(Y)
  IR_unemployment[,h] <- Y
}

# Visualize IR
par(mfrow=c(3,3))
# Wage shock
plot(IR_wage[1,], 
     main = 'Response to a shock in real wage',
     ylab = 'Real wage', 
     xlab = 'h',
     type='l')

plot(IR_GNP[1,], 
     main = 'Response to a shock in GNP',
     ylab = 'Real wage', 
     xlab = 'h',
     type='l')

# Unemployment shock
plot(IR_GNP[1,], 
     main = 'Response to a shock in unemployment',
     ylab = 'Real wage', 
     xlab = 'h',
     type='l')

plot(IR_wage[2,], 
     ylab = 'GNP', 
     xlab = 'h',
     type='l')

plot(IR_GNP[2,], 
     ylab = 'GNP', 
     xlab = 'h',
     type='l')


plot(IR_GNP[2,], 
     ylab = 'GNP', 
     xlab = 'h',
     type='l')


plot(IR_wage[3,], 
     ylab = 'Unemployment', 
     xlab = 'h',
     type='l')

# GNP shock


plot(IR_GNP[3,], 
     ylab = 'Unemployment', 
     xlab = 'h',
     type='l')



plot(IR_GNP[3,], 
     ylab = 'Unemployment', 
     xlab = 'h',
     type='l')



########################################################################################################################
## Now for 1991 - 2018 


## First do a var model for all the data set with 4 lags for the full period 
reg1 <- lm(df2_t$dif1_earning ~ lag(df2_t$dif1_gnp, 1) + lag(df2_t$dif1_gnp, 1) + lag(df2_t$dif1_unemp, 1))
reg2 <- lm(df2_t$dif1_gnp ~ lag(df2_t$dif1_earning, 1) + lag(df2_t$dif1_gnp, 1) + lag(df2_t$dif1_unemp, 1))
reg3 <- lm(df2_t$dif1_unemp ~ lag(df2_t$dif1_earning, 1) + lag(df2_t$dif1_gnp, 1) + lag(df2_t$dif1_unemp, 1))


##construct coefficient matrix
A1 <- rbind(c(reg1$coefficients[2:length(reg1$coefficients)]), 
            c(reg2$coefficients[2:length(reg2$coefficients)]),
            c(reg3$coefficients[2:length(reg3$coefficients)]))


# now construct an identity matrix
m1 <- solve(as.matrix(diag(3))-A1)
mA1 <- as.matrix(diag(3)) - A1

##Construct sigma 
Sigma <-(1/length(df2_t$dif1_earning))*crossprod(cbind(c(reg1$residuals),
                                                       c(reg2$residuals),
                                                       c(reg3$residuals)))
#Get theta^2
theta2 <- m1 %*% Sigma %*% t(m1)

#Perform cholesky on theta
theta <- t(chol(theta2))
Ident_Theta <- as.matrix(diag(3)) - theta
B_Inverse <- A1 %*% theta


## Now do the SVAR command with long-run restrictions
#Imposing the following three restrictions
# 1. Aggregate demand has no long-run impact on the log of real output
# 2. Aggregate demand has no long-run impact on the log of real wages
# 3. Labor-supply shock has no long-run impact on the log of real wages. 
#Given the Zt vector from Gamber and Joutz, that means that the matrix (d.earning, d.gnp, .d.unemp)(d.earning, d.gnp, d.unemp)'
# is lower triangular
###own try

##################### Exercise 5: Plot the appropriate impulse response functions and discuss your findings.
#How do they compare to GJ? Is there a difference between the three periods PE, PR and PT ?
var_all <- VAR(na.omit(df1_t), p= 1)
U <- resid(var_all)
B <- as.matrix(solve(B_Inverse))
Gamma <- B %*% A1 
Mu <- c(reg1$coefficients[1], reg2$coefficients[1], reg3$coefficients[1])

Epsilon <- U
for (i in 1:dim(U)){
  Epsilon[i,] <- (B%*%U[i,])
}

# Give a one sd shock to the system
Shock <- c(sd(Epsilon[,1]), sd(Epsilon[,2]), sd(Epsilon[,3]))

# Wage shock
Shock_wage <- c(sd(Epsilon[,1]), 0, 0)
# GNP shock
Shock_GNP <- c(0, sd(Epsilon[,2]), 0)
# Unemployment shock
Shock_unemployment <- c(0, 0, sd(Epsilon[,3]))


# First create empty IR matrix, then add the IR for every value of h from 1 to 40
# Now we apply a shock to all three ts
IR_wage <- matrix(NA, nrow = 3, ncol = 40)
for (h in 1:40){
  Y <- c(0,0,0) + ((B_Inverse %*% A1)^h) %*%(B_Inverse%*%Shock_wage)
  print(Y)
  IR_wage[,h] <- Y
}

IR_GNP <- matrix(NA, nrow = 3, ncol = 40)
for (h in 1:40){
  Y <- c(0,0,0) + ((B_Inverse %*% A1)^h) %*%(B_Inverse%*%Shock_GNP)
  print(Y)
  IR_GNP[,h] <- Y
}

IR_unemployment <- matrix(NA, nrow = 3, ncol = 40)
for (h in 1:40){
  Y <- c(0,0,0) + ((B_Inverse %*% A1)^h) %*%(B_Inverse%*%Shock_unemployment)
  print(Y)
  IR_unemployment[,h] <- Y
}


# Visualize IR
par(mfrow=c(3,3))
# Wage shock
plot(IR_wage[1,], 
     main = 'Response to a shock in real wage',
     ylab = 'Real wage', 
     xlab = 'h',
     type='l')

plot(IR_GNP[1,], 
     main = 'Response to a shock in GNP',
     ylab = 'Real wage', 
     xlab = 'h',
     type='l')

# Unemployment shock
plot(IR_GNP[1,], 
     main = 'Response to a shock in unemployment',
     ylab = 'Real wage', 
     xlab = 'h',
     type='l')

plot(IR_wage[2,], 
     ylab = 'GNP', 
     xlab = 'h',
     type='l')

plot(IR_GNP[2,], 
     ylab = 'GNP', 
     xlab = 'h',
     type='l')


plot(IR_GNP[2,], 
     ylab = 'GNP', 
     xlab = 'h',
     type='l')


plot(IR_wage[3,], 
     ylab = 'Unemployment', 
     xlab = 'h',
     type='l')

# GNP shock


plot(IR_GNP[3,], 
     ylab = 'Unemployment', 
     xlab = 'h',
     type='l')



plot(IR_GNP[3,], 
     ylab = 'Unemployment', 
     xlab = 'h',
     type='l')

# Do this for other time periods too, start at point 4 and repeat from there





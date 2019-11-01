
################################################################
################################################################

## Name: Structural Vector Autoregression Modelling 

## 1-Cleaning-Stationarity; I load and clean the three U.S. macroeconomic time series and check them for seasonality and 
## stationarity 

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
              "stargazer")

# Check if these packages are already installed on the computer, if not install them 
list.of.packages <- packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

# Use sapply to require all packages in one
sapply(packages, require, character.only = TRUE)


# Specify working directory and load the data
setwd("~/GitHub/Structural-Vector-Autoregression-Modeling")


####################################################################################################

############# Step 0: Data cleaning 

earning_a               <- read.csv("earnings_adjusted.csv")
colnames(earning_a)[2]  <- "earning_a"
earning_u               <- read.csv("earnings_unadjusted.csv")
colnames(earning_u)[2]  <- "earning_u"
pricedef_a              <- read.csv("pricedefl_adjusted.csv")
colnames(pricedef_a)[2] <- "pricedef_a"
gnp_a                   <- read.csv("gnp_adjusted.csv")
colnames(gnp_a)[2]      <- "gnp_a"
unemp_a                 <- read.csv("unemp_adjusted.csv")
colnames(unemp_a)[2]    <- "unemp_a"

#Using first columns as rownames
rownames(earning_a)   <- earning_a[ ,1]
earning_a$DATE        <- NULL

rownames(earning_u)   <- earning_u[, 1]
earning_u$DATE        <- NULL

rownames(pricedef_a)  <- pricedef_a[, 1]
pricedef_a$DATE       <- NULL

rownames(gnp_a)       <- gnp_a[, 1]
gnp_a$DATE            <- NULL

rownames(unemp_a)     <- unemp_a[, 1]
unemp_a$DATE          <- NULL


#defining time series
monthly_earning_a     <- as.xts(earning_a)
monthly_earning_u     <- as.xts(earning_u)
q_pricedef_a          <- as.xts(pricedef_a)
q_gnp_a               <- as.xts(gnp_a)
monthly_unemp_a       <- as.xts(unemp_a)

#Changing from monthly to quarterly for earnings and unemp
#specifying endpoints

epearnings          <- seq(1, dim(monthly_earning_a)[1], 3)
q_earning_a         <- period.apply(monthly_earning_a, epearnings, mean)
q_earning_u         <- period.apply(monthly_earning_u, epearnings, mean)

epunemp             <- seq(1, dim(monthly_unemp_a)[1], 3)
q_unemp_a           <- period.apply(monthly_unemp_a, epunemp, mean)


#now merge all into one dataframe
alltime           <- merge(q_earning_a, q_earning_u, q_gnp_a, q_pricedef_a, q_unemp_a)

#deflate the undadjusted earnings

alltime$earningdefl_a <- alltime$earning_a*100/alltime$pricedef_a



#export

tmp <- tempfile()
write.zoo(alltime, file="df1.csv", sep=",")

#####################################################################################################



#####################################################################################

#Assignment Exercise  1: Show graphics for full time-periods of 
#Load data
rm(list=ls(all=TRUE))


df1 <- read.csv("df1.csv")
rownames(df1) <- as.character(df1[, 1])
df1$Index <- NULL

df2 <- as.xts(df1)

#change df1 afterwards

df1$date <- rownames(df1)

#change class
df1$date <- as.Date(df1$date)


#####################################################################################################################################
#Exercise 1: Visualization
#####################################################################################################################################


### first show from the beginning up until Q4 1990, I take as start values 1948

#graphs for earnings
df2 <- df2['1948-01-01/']

#Specify work environment to save the pictures: 
setwd("~/Utrecht/M-Economics/Period 7/Applied Macroeconometrics/Assignments/Assignment 1/Plots")

#Now plot earnings

dygraph(df2$earningdefl_a['1948-01-01/2018-10-01'], xlab = "Q1 1948 - Q4 2018", ylab = "Seasonally adjusted U.S. earnings")

dygraph(df2$earningdefl_a['1948-01-01/1990-10-01'], xlab = "Q1 1948 - Q4 1990", ylab = "Seasonally adjusted U.S. earnings")

dygraph(df2$earningdefl_a['1991-01-01/2018-10-01'], xlab = "Q1 1991 - Q4 2018", ylab = "Seasonally adjusted U.S. earnings")

#Give summary statistics for earnings for all three periods
summary(df2$earningdefl_a['1948-01-01/2018-10-01'])
summary(df2$earningdefl_a['1948-01-01/1990-10-01'])
summary(df2$earningdefl_a['1991-01-01/2018-10-01'])


#graphs for gnp
dygraph(df2$gnp_a['1948-01-01/2018-10-01'], xlab = "Q1 1948 - Q4 2018", ylab = "Seasonally adjusted Gross National Product of the U.S.")
dygraph(df2$gnp_a['1948-01-01/1990-10-01'], xlab = "Q1 1948 - Q4 1990", ylab = "Seasonally adjusted Gross National Product of the U.S.")
dygraph(df2$gnp_a['1991-01-01/2018-10-01'], xlab = "Q1 1991 - Q4 2018", ylab = "Seasonally adjusted Gross National Product of the U.S.")

#give summary statistics for gnp for all three periods
summary(df2$gnp_a['1948-01-01/2018-10-01'])
summary(df2$gnp_a['1948-01-01/1990-10-01'])
summary(df2$gnp_a['1991-01-01/2018-10-01'])


#graphs for unemp for all three periods
dygraph(df2$unemp_a['1948-01-01/2018-10-01'], xlab = "Q1 1948 - Q4 2018", ylab = "Seasonally adjusted U.S. unemployment rate")
dygraph(df2$unemp_a['1948-01-01/1990-10-01'], xlab = "Q1 1948 - Q4 1990", ylab = "Seasonally adjusted U.S. unemployment rate")
dygraph(df2$unemp_a['1991-01-01/2018-10-01'], xlab = "Q1 1991 - Q4 2018", ylab = "Seasonally adjusted U.S. unemployment rate")

#give summary statistics for unemployment for all three periods
summary(df2$unemp_a['1948-01-01/2018-10-01'])
summary(df2$unemp_a['1948-01-01/1990-10-01'])
summary(df2$unemp_a['1991-01-01/2018-10-01'])


#####################################################################################################################################
#Exercise 2: Use BBQ to identify peaks and throughs
#####################################################################################################################################

#find maxima and minima in time series across total 4 periods


##define function for peaks with inequality
ispeak <- function(x){
  peaks <- NULL 
  for (i in 2:(dim(x)[1]-2)){
    
    interval <- c(x[(i-2):(i+2)])
    if (all(!is.na(interval))){ 
      
      if (max(interval)==(interval[3])){
        peaks <- append(peaks, i)
      }
    }
  }
  return(peaks)
}



###my define function for peaks with strict inequality
ispeak2 <- function(x){
  peaks <- NULL 
  for (i in 3:(length(x)-3)){
    
    interval <- c(x[(i-2):(i+2)])
    if (all(!is.na(interval))){ 
      
      if (as.numeric(interval[3]) > as.numeric(interval[2]) & as.numeric(interval[3]) > as.numeric(interval[4]) &
          as.numeric(interval[3]) > as.numeric(interval[1]) & as.numeric(interval[3]) > as.numeric(interval[5])){
        peaks <- append(peaks, i)
      }
    }
  }
  return(peaks)
}


##define function for lows


# without strict inequality 
islow <- function(x){
  lows <- NULL 
  for (i in 2:(dim(x)[1] - 2)){
    
    interval <- c(x[(i-2):(i+2)])
    if (all(!is.na(interval))){
      
      if(min(interval) == interval[3]){
        lows <- append(lows, i)
      }
    }
  }
  return(lows)
}


###my define function for peaks with strict inequality
islow2 <- function(x){
  peaks <- NULL 
  for (i in 3:(length(x)-3)){
    
    interval <- c(x[(i-2):(i+2)])
    if (all(!is.na(interval))){ 
      
      if (as.numeric(interval[3]) < as.numeric(interval[2]) & as.numeric(interval[3]) < as.numeric(interval[4]) &
          as.numeric(interval[3]) < as.numeric(interval[1]) & as.numeric(interval[3]) < as.numeric(interval[5])){
        peaks <- append(peaks, i)
      }
    }
  }
  return(peaks)
}


#### BBQ for peaks and throughs ####################################################

### 1. Earnings ###

#Run the BBQ for earnings and plot the points
earningpeak <- ispeak2(df2$earningdefl_a)
earninglow <- islow2(df2$earningdefl_a)



plot(df2$earningdefl_a, main = "Peaks and throughs of the deflated quarterly earnings")
points(df2$earningdefl_a[ispeak2(df2$earningdefl_a)], col = "green", pch = 4, lwd = 2)
points(df2$earningdefl_a[islow2(df2$earningdefl_a)], col = "red", pch = 3, lwd = 2)

#now without strict inequality
plot(df2$earningdefl_a, main = "Peaks and throughs of the deflated quarterly earnings")
points(df2$earningdefl_a[ispeak2(df2$earningdefl_a)], col = "green", pch = 4, lwd = 2)
points(df2$earningdefl_a[islow2(df2$earningdefl_a)], col = "red", pch = 3, lwd = 2)

#Run the BBQ for unemp
unemppeak <- ispeak2(df2$unemp_a)
unemplow <- islow2(df2$unemp_a)

#plot with strict inequality 
plot(df2$unemp_a, main = "Peaks and throughs of the U.S. unemployment rate")
points(df2$unemp_a[ispeak2(df2$unemp_a)], col = "green", pch = 4, lwd = 3)
points(df2$unemp_a[islow2(df2$unemp_a)], col = "red", pch = 3, lwd = 3)

#try without relaxed inequality 
plot(df2$unemp_a, main = "Peaks and throughs of the U.S. unemployment rate")
points(df2$unemp_a[ispeak(df2$unemp_a)], col = "green", pch = 4, lwd = 3)
points(df2$unemp_a[islow(df2$unemp_a)], col = "red", pch = 3, lwd = 3)

#Set wd again
setwd("~/Utrecht/M-Economics/Period 7/Applied Macroeconometrics/Assignments/Assignment 1/Data")

#Compare to official NBER recessions
#adding recessions
recess                  <- read.csv("nber_recession.csv")
colnames(recess)[2]     <- "recession"
rownames(recess)      <- recess[, 1]
recess$DATE           <- NULL
q_recess              <- as.xts(recess)
q_recess <- q_recess["1946-01-01/2018-12-31"]
q_recess_unemp <- q_recess["1948-01-01/2018-01-01"]


#Do the unemp plot with NBER recessions
plot(df2$unemp_a, main = "Peaks and throughs of the U.S. unemployment rate")
points(df2$unemp_a[ispeak(df2$unemp_a)], col = "green", pch = 4, lwd = 3)
points(df2$unemp_a[islow(df2$unemp_a)], col = "red", pch = 3, lwd = 3)
addEventLines(q_recess_unemp[q_recess_unemp$recession == 1, ], lwd = 0.6)

#Do the earning plot with NBER recessions
plot(df2$earningdefl_a, main = "Peaks and throughs of the deflated quarterly U.S. earnings")
points(df2$earningdefl_a[ispeak(df2$earningdefl_a)], col = "green", pch = 4, lwd = 2)
points(df2$earningdefl_a[islow(df2$earningdefl_a)], col = "red", pch = 3, lwd = 2)
addEventLines(q_recess[q_recess$recession == 1, ], lwd = 0.6)


#####################################################################################################################################
#Exercise 3: X13 - Arima
#####################################################################################################################################
#delete all
rm(list=ls(all=TRUE))

######### Loading data
setwd("~/Utrecht/M-Economics/Period 7/Applied Macroeconometrics/Assignments/Assignment 1/Data")

earning_a               <- read.csv("earnings_adjusted.csv")
colnames(earning_a)[2]  <- "earning_a"
earning_u               <- read.csv("earnings_unadjusted.csv")
colnames(earning_u)[2]  <- "earning_u"

rownames(earning_a)   <- earning_a[ ,1]
earning_a$DATE        <- NULL

rownames(earning_u)   <- earning_u[, 1]
earning_u$DATE        <- NULL

###finalizing the xts
monthly_earning_a     <- as.xts(earning_a)
monthly_earning_u     <- as.xts(earning_u)

############# Compare both series

plot(monthly_earning_a, main = "Monthly U.S. earnings, adjusted for seasonality")
plot(monthly_earning_u, main = "Monthly U.S. earnings, unadjusted for seasonality")

############# Start the ARIMA process, following the Israel CBS

#1a: Crude trend cycle

#define weights
m122    <- c(1, rep(2, 11), 1)/24
m33     <- c(1,2,3,2,1)/9
m35     <- c(1,2,3,3,3,2,1)/15

#now apply m122 to the monthly earnings 


#construct a function for moving average of M2X12
crude_trent <- function(x, y, z){
  test2 <- NULL
  for(i in (7) : (length(x)-7)){
    interval <- c((i-y):(i+y))
    test2[i] <- sum(x[interval]*z)
    test2[c((length(x)-6):length(x))] <- NA
  }
  return(test2)
}

#apply function for the first algorithm: 
ct1 <- crude_trent(monthly_earning_u, 6, m122)

length(ct1)
length(monthly_earning_u)
#deviding the monthly data through the new vector

SI1 <- monthly_earning_u / ct1

plot(SI1, main = "Divided original series by crude cycle") 

#crude biased seasonal factors

seasonal_factors <- function(x,y,z){
  test2 <- NULL 
  for(i in (7+y*12):(length(x)-7-y*12)){
    interval <- c(i-2*12, i -1*12,i, i+1*12, i+2*12)
    test2[i] <- sum(x[interval]*z)
    test2[c((length(x)-6 -y*12):length(x))] <- NA
  }
  return(test2)
}

St0 <- seasonal_factors(SI1, 2, m33)
St0

#Now normalize to get the crude unbiased seasonal adjustments through centering

crude_trent <- function(x, y, z){
  test2 <- NULL
  for(i in (31+y) : (930 - y)){
    interval <- c((i-y):(i+y))
    test2[i] <- sum(x[interval]*z)
    test2[c((length(x)-6):length(x))] <- NA
  }
  return(test2)
}

St1 <- St0 / crude_trent(St0, 6, m122)
plot(St1)
#Now get the first preliminary adjusted series

SA1 <- monthly_earning_u /St1

test <- monthly_earning_u/SA1
plot(test, lwd = 2, main = "Normalized crude unbiased seasonal adjustment through centering")
##### Step 2: Henderson 

#define Henderson moving average vector
H13 <- c(-0.019, -0.028, 0, 0.066, 0.147, 0.214, 0.240, 0.214, 0.147, 0.066, 0, -0.028, -0.019)
SA1[36]

movavg_hend <- function(x,y,z){
  test2 <- NULL
  for(i in (which(!is.na(x))[1]+y):(last(which(!is.na(x)))-y)){
    interval <- c((i-y):(i+y))
    test2[i] <- sum(x[interval]*z)
    test2[c(last(which(!is.na(x)))-y):length(x)] <- NA
  }
  return(test2)
}


Ct2 <- movavg_hend(SA1, 6, H13)
length(Ct2)
length(monthly_earning_u)
#removing Henderson trend from original series

SI2 <- monthly_earning_u / Ct2

plot(SI2)

#Now removing season - irregular component, for the same month, as in 1.3. 


seasonal_factors <- function(x,y,z){
  test2 <- NULL 
  for(i in (which(!is.na(x))[1]+y*12):(last(which(!is.na(x)))-y*12)){
    l <- rep(i, length(z))
    interval <- l + c(-y:y)*12
    test2[i] <- sum(x[interval]*z)
    test2[c(last(which(!is.na(x)))-y):length(x)] <- NA  }
  return(test2)
}


St2 <- seasonal_factors(SI2, 3, m35)
plot(St2)

#final estimate for Step 2

crude_trent <- function(x, y, z){
  test2 <- NULL
  for(i in (which(!is.na(x))+y) : (last(which(!is.na(x)))- y)){
    interval <- c((i-y):(i+y))
    test2[i] <- sum(x[interval]*z)
    test2[c(last(which(!is.na(x)))-y):length(x)] <- NA  
  }
  return(test2)
}

St3 <- St2 /crude_trent(St2, 6, m122)

#then finalize the henderson method by dividing the original timeseries by the seasonal component

SAt2 <- monthly_earning_u / St3
plot(SAt2)

plot(monthly_earning_u / SAt2, main = "Seasonal adjusted U.S. monthly earnings after step 2/Henderson")


###### Part 3: 
#First: again applying a 13-term henderson filter: 

Ct3 <- movavg_hend(SAt2, 6, H13)

#Final irregular filter: 

##Dividing the final sesasonal 
It <- SAt2 / Ct3
plot(It, main = "Final seasonally adjusted series")


#compare to the seasonal component that comes from the Fred?

I2 <- monthly_earning_u/monthly_earning_a
plot(I2, main = "Dividing both monthly series as provided by the FRED")


## Compare both series directly by showing the deviation between them 
diff_arima <- (It - I2)/I2
plot(diff_arima, main = "Quadratic loss function between both seasonal components", ylab = "Difference in percentage points")

#Answer: Both have around a mean of 1 and fluctuate around it, but the fred data looks very much more discrete
#than the data I got by applying ARIMA-X11


#######################################################################################################

############################# Assignment Exercise 1.4. ################################################

#######################################################################################################


#Assignment Exercise  4: Dickey-Fueller test
#Load data

rm(list=ls(all=TRUE))


df1 <- read.csv("df1.csv")
rownames(df1) <- as.character(df1[, 1])
df1$Index <- NULL

df2 <- as.xts(df1)
df2 <- df2["1948/"]
#change df1 afterwards

df1$date <- rownames(df1)

#change class
df1$date <- as.Date(df1$date)

#Step 1: Take logs

df2$logdefl_earning   <- log(df2$earningdefl_a)
df2$log_gnp           <- log(df2$gnp_a)

# Create a time squared variable

df2$time <- c(1:nquarters(df2))
df2$time2 <- df2$time^2

#STep 2: plot the series

plot(df2$logdefl_earning)
plot(df2$log_gnp)

#There is a clear timetrend, autocorrelation, meaning that absolute value of p isn't necessarily smaller than 1. 
#perform dickey fueller test on both series

######################################################################################################################

###### Check for stationarity of earnings ############################################################################

######################################################################################################################


df2$dif1_earning <- diff(df2$earning_a, 1)
plot(df2$dif1_earning)

####What emiel did: 

reg1 <- lm(df2$logdefl_earning ~ df2$time + df2$time2)
summary(reg1)
plot(df2$earningdefl_a)
## Both time and time2 are highly significant 

reg2 <- lm(df2$dif1_earning ~ lag(df2$logdefl_earning, 1) + df2$time + df2$time2)
summary(reg2)

df2$dif1_earning <- diff(df2$logdefl_earning, 1)
plot(df2$dif1_earning)
# Still appears to be a trend! downwards first and then steady, so account for it!

df2$dif1_time2 <- diff(df2$time2, 1)

#dickey fueller! 

reg3 <- lm(diff(df2$dif1_earning, 1) ~ lag(df2$dif1_earning, 1) +df2$dif1_time2)
summary(reg3)
## highly significant lag DF t = -15.871

## Now detrend
reg4 <- lm(df2$dif1_earning ~ df2$time) 
df2$dif1_earning_detr <- resid(reg4)

##Final dickey fueller test:
reg5 <- lm(diff(df2$dif1_earning_detr, 1) ~ lag(df2$dif1_earning_detr, 1))
summary(reg5)

stargazer(reg1, reg2, title = "Results 1", report = "vctp*")
stargazer(reg3, reg4, reg5, title = "Results 1", report = "vctp*")


### T - statistic of -15.99 so stationarity is achievied
### one final plot: 
plot(df2$dif1_earning_detr, main = "Obtained stationary series for earnings: detrendend FD")
summary(df2$dif1_earning_detr)


#################################################################################
#Part two: Unemplyoment series:  
plot(df2$unemp_a)

#create the first difference of unemployment and first lag
df2$dif1_unemp <- diff(df2$unemp_a, 1)
df2$lag1_unemp <- lag(df2$unemp_a, 1)

###plot first differences to see if exists a time trend
plot(df2$dif1_unemp)
### Deciding to include a time trend
reg1 <- lm(df2$dif1_unemp ~ df2$lag1_unemp + time(df2))
summary(reg1)

### Stationarity is not rejected, thus take first differences of the series
df2$dif2_unemp <- diff(df2$dif1_unemp, 1)
df2$lag1_dif1_unemp <- lag(df2$dif1_unemp, 1)
plot(df2$dif1_unemp)

#### as no visible time trend: Don't include time operator
reg2 <- lm(df2$dif2_unemp ~ df2$lag1_dif1_unemp)
summary(reg2)

stargazer(reg1, reg2, title = "Regression results for unemployment", report = "vctp*")

plot(df2$dif1_unemp, main = "Obtained stationary series for unemployment: FD")


#### T-value of -7,624 rejecting the unit root assumption

########## Part 3: Obtaining stationary data for log(gnp)
plot(df2$log_gnp) ### There appears to be a linear trend, maybe also quadratic but not sure

reg1 <- lm(df2$log_gnp ~ df2$time)
summary(reg1)
#Very significant time trend 

#create FD of gnp
df2$dif1_gnp <- diff(df2$log_gnp, 1)
plot(df2$dif1_gnp)


reg2 <- lm(df2$dif1_gnp ~ df2$time)
summary(reg2) 
###Significant time trend
##now detrend

df2$dif1_gnp_detr <- resid(reg2)

#test for final dickey fueller
reg3 <- lm(diff(df2$dif1_gnp_detr, 1) ~ lag(df2$dif1_gnp_detr))
summary(reg3) ## T-value of -11.76, so unit reject is significantly rejected

plot(df2$dif1_gnp_detr)

stargazer(reg1, reg2, reg3, title = "Regression results for GNP", report = "vctp*")

plot(df2$dif1_gnp, main = "Obtained stationary series for unemployment: detrended FD")

#saving the stationary dataseries
df3 <- cbind(df2$dif1_earning_detr, df2$dif1_gnp_detr,df2$dif1_unemp)
df3[1,1] <- NA
df3[1,2] <- NA
setwd("~/Utrecht/M-Economics/Period 7/Applied Macroeconometrics/Assignments/Assignment 1/Data")
tmp <- tempfile()
write.zoo(df3, file="stationary_series.csv", sep=",")




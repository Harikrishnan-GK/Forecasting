# ########## #
# EXERCISE-2 #
# ########## #

# ########## #
# Unemployment #
# ########## #

# ########## #
# Libraries  #
# ########## #

library(readxl)
library(fpp2)

# ################# #
# Working directory #
# ################# #

setwd("C:/Users/hgopalakrishnan/OneDrive - IESEG/Desktop/MBD-21/Forecasting/Assignment") # Specify you own working directory here.


# ###################
# unemployment rate #
# ###################
data <- read.csv("unemployment.csv")
#air <- ts(data[,2], frequency = 12, start = c(2003,1))

unemp <- ts(data[,2], frequency = 12, start = c(1990,1))

# ######################### #
# Explanatory data analysis #
# ######################### #

# Check for missing values
sum(is.na(unemp)) #no null values


#table summary
summary(unemp)

tsdisplay(unemp)

#Plot the raw data using base plot function
plot(unemp,xlab="Year", ylab = "Number of Unemployed",main="Monthly Unemployed numbers from Feb 1990 to Mar 2022")

# Split the data in training and test set
unemp_train <- window(unemp, end=c(2017,12))
unemp_test <- window(unemp, start=c(2018,1), end=c(2020,3)) #27 months
unemp_covid <- window(unemp, start=c(2020,4))
unemp_full <- window(unemp, end=c(2020,3))

plot(unemp)

plot(unemp_full)
tsdisplay(unemp_full)

#Time-series-plot
autoplot(unemp_full) +
  ggtitle("Monthly unemployment Number from Feb 1990 to Mar 2020") +
  ylab("No.of unemployed people") +
  xlab("Year")


#Seasonal-plot
ggseasonplot(unemp_full, year.labels=TRUE, year.labels.left=TRUE) +
  ggtitle("Monthly unemployment Number from Feb 1990 to Mar 2020") +
  ylab("No.of unemployed people") +
  xlab("Year")



#Seasonal subseries plot
ggsubseriesplot(unemp_full) +
  ggtitle("Monthly unemployment Number from Feb 1990 to Mar 2020") +
  ylab("Mean unemployed people") +
  xlab("Year")




#ACF plot
ggAcf(unemp_full)
tsdisplay(unemp_full)

#month plot
monthplot(unemp_full, main="Monthly unemployment rate", 
          ylab = "No.of people",
          xlab="Quarter", type="l")

# ########## #
# Box-Cox transformation #
# ########## #

# Box-Cox transformation
###################################
# Box-Cox transformation

BoxCox.lambda(unemp_full)
l <- 0

# Plot the data
# plot(unemp_full, col="red")
# lines(unemp_test, col="blue")
# 
# fit <- snaive(unemp_full,lambda= l)
# plot(fit)


# ########## #
# Modelling #
# ########## #

# ########## #
# STL models #
# ########## #

h=27
unemp_m1 <- stlf(unemp_train, method="naive", h=h) 
unemp_m2 <- stlf(unemp_train, method="rwdrift", h=h)
unemp_m3 <- stlf(unemp_train, method="ets", h=h)
unemp_m4 <- stlf(unemp_train, method="arima", h=h)


unemp_m5 <- stlf(unemp_train, method="naive", h=h, lambda = l, biasadj = TRUE) #s.window function check 
unemp_m6 <- stlf(unemp_train, method="rwdrift", h=h, lambda = l, biasadj = TRUE)
unemp_m7 <- stlf(unemp_train, method="ets", h=h, lambda = l, biasadj = TRUE)
unemp_m8 <- stlf(unemp_train, method="arima", h=h, lambda = l, biasadj = TRUE)

#list of models
models <- c("STL naive", "STL rwdrift", "STL ets", "STL arima",
            "STL naive lambda", "STL rwdrift lambda", "STL ets lambda","STS Arima lambda")

n <- length(models); n   #number of models

#naming of models for the given data set
m <- "unemp_m"

#naming of training and test set
trainset <- unemp_train 
testset <- unemp_test

#prepare the accuracy tables
a_train <- matrix(nrow = 0, ncol = 5)
a_test  <- matrix(nrow = 0, ncol = 5)

#prepare the residual diagnostic table
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")

#for loop to collect measures
for(i in 1:n) 
{
  #accuracy measures
  assign(paste0("a_m", i), accuracy(get(paste0(m, i)), testset)[,c(2,3,5,6)])
  a_train <- rbind(a_train, c(i, get(paste0("a_m", i))[1,]))
  a_test <- rbind(a_test, c(i, get(paste0("a_m", i))[2,]))
  
  #residual diagnostics  
  assign(paste0("res_m", i), checkresiduals(get(paste0(m, i)), plot = FALSE))
  res_matrix[i,] <- c(i, get(paste0("res_m", i))$statistic, 
                      get(paste0("res_m", i))$parameter, 
                      get(paste0("res_m", i))$p.value)
}

rownames(a_train) <- models
colnames(a_train)[1] <- "nr"
rownames(a_test) <- models
colnames(a_test)[1] <- "nr"

a_train
a_test
round(res_matrix, digits = 4)

#selected model-residual diagnostics 3 stl + ETS
checkresiduals(unemp_m7)


#selected model-forecast
plot(unemp_m7)


#selected model-summary
summary(unemp_m7)

#########################################################################
# ########## #
# ETS models #
# ########## #
ets_1 <- ets(unemp_train, model = "AAA", damped = FALSE)
fit_ets1 <- forecast(ets_1, h=h)
ets_2 <- ets(unemp_train, model = "MAA", damped = FALSE)
fit_ets2 <- forecast(ets_2, h=h)
ets_3 <- ets(unemp_train, model = "MAM", damped = FALSE)
fit_ets3 <- forecast(ets_3, h=h)
ets_4 <- ets(unemp_train, model = "AAA", damped = TRUE)
fit_ets4 <- forecast(ets_4, h=h)     
ets_5 <- ets(unemp_train, model = "MAA", damped = TRUE)
fit_ets5 <- forecast(ets_5, h=h)     
ets_6 <- ets(unemp_train, model = "MAM", damped = TRUE)
fit_ets6 <- forecast(ets_6, h=h)     
ets_7 <- ets(unemp_train, model = "AAA", damped = FALSE, lambda = l, biasadj = TRUE)
fit_ets7 <- forecast(ets_7, h=h)     
ets_8 <- ets(unemp_train, model = "AAA", damped = TRUE, lambda = l, biasadj = TRUE)
fit_ets8 <- forecast(ets_8, h=h) 



#check resuidues
checkresiduals(fit_ets1)
checkresiduals(fit_ets2)
checkresiduals(fit_ets3)
checkresiduals(fit_ets4)
checkresiduals(fit_ets5)
checkresiduals(fit_ets6)
checkresiduals(fit_ets7)
checkresiduals(fit_ets8)


#accuracy
accuracy(fit_ets1,unemp_test)
accuracy(fit_ets2,unemp_test)
accuracy(fit_ets3,unemp_test)
accuracy(fit_ets4,unemp_test)
accuracy(fit_ets5,unemp_test)
accuracy(fit_ets6,unemp_test)
accuracy(fit_ets7,unemp_test)
accuracy(fit_ets8,unemp_test)



#summary
summary(fit_ets3)

#selected model-forecast
plot(fit_ets3)

# ################ #
# Model comparison #
# ################ #
final2_m1 <- stlf(unemp_train, method="ets", h=h, lambda = l, biasadj = TRUE)           # stl ets
final2_m2 <- forecast(ets_3, h=h) #ETS (M,A,M)


#Check for residual diagnostics

#list of models
models <- c(
            "STL Random walk", 
            "ETS-M,A,M"
            )

n <- length(models); n   #number of models

#naming of models for the given data set
m <- "final2_m"

#naming of training and test set
trainset <- unemp_train
testset <- unemp_test

#prepare the accuracy tables
a_train <- matrix(nrow = 0, ncol = 5)
a_test  <- matrix(nrow = 0, ncol = 5)

#prepare the residual diagnostic table
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")

#for loop to collect measures
for(i in 1:n) 
{
  #accuracy measures
  assign(paste0("a_m", i), accuracy(get(paste0(m, i)), testset)[,c(2,3,5,6)])
  a_train <- rbind(a_train, c(i, get(paste0("a_m", i))[1,]))
  a_test <- rbind(a_test, c(i, get(paste0("a_m", i))[2,]))
  
  #residual diagnostics  
  assign(paste0("res_m", i), checkresiduals(get(paste0(m, i)), plot = FALSE))
  res_matrix[i,] <- c(i, get(paste0("res_m", i))$statistic, 
                      get(paste0("res_m", i))$parameter, 
                      get(paste0("res_m", i))$p.value)
}

rownames(a_train) <- models
colnames(a_train)[1] <- "nr"
rownames(a_test) <- models
colnames(a_test)[1] <- "nr"

a_train
a_test
round(res_matrix, digits = 4)

# ########## #
# SELECTED model #
# ########## #


# ########## #
# Forecast #
# ########## #

unemp_stl_ets <- stlf(unemp_train, method="ets", h=27, lambda = l, biasadj = TRUE)

plot(unemp_stl_ets)

# ########################### #
# Comparison with covid data #
# ########################### #

#air_stl_arima2 <- stlf(air, method="arima", h=14)

plot(unemp_full,ylab="Number of Unemployed", xlab="Year")
lines(unemp_stl_ets$mean,col = 'red')
legend("topright",lty=1,col='red',legend=c("STL + ETS"))






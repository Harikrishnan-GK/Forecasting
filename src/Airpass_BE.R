# ########## #
# EXERCISE-1 #
# ########## #

# ########## #
# Airline_BE #
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


# ################# #
# Reading the Data  #
# ################# #
data <- read_excel("DataSets2022.xlsx", sheet="Airpass_BE")

#Convert the data to time series data
air <- ts(data[,2], frequency = 12, start = c(2003,1))


# ######################### #
# Explanatory data analysis #
# ######################### #

# Check for missing values
sum(is.na(air)) #no null values


#table summary
summary(air)


#Plot the raw data using base plot function
plot(air,xlab="Date", ylab = "Number of Passengers",main="Monthly Air Passenger numbers from Jan 2003 to Jul 2021")


#Box plot function
boxplot(air~cycle(air),xlab="Date", ylab = "Number of Passengers" ,main ="Monthly Air Passengers Boxplot from Jan 2003 to Jul 2021")



plot(air)

# Split the data in training and test set
air_train <- window(air, end=c(2017,12))
air_test <- window(air, start=c(2018,1), end=c(2020,2))
air_covid <- window(air, start=c(2020,3), end=c(2021,10))
air_tt <- window(air, end=c(2020,2))

h <- length(air_test)

#check the cycle of the data
#cycle(data)

# ########## #
# Exercise-1 #
# ########## #

#Time-series-plot
autoplot(air_tt) +
  ggtitle("Monthly Airline Passengers from Jan 2003-Feb 2020") +
  ylab("Number of Passengers") +
  xlab("Year")


#Seasonal-plot
ggseasonplot(air_tt, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Number of Passengers") +
  ggtitle("Seasonal plot: Monthly Airline Passengers")



#Seasonal subseries plot
ggsubseriesplot(air_tt) +
  ylab("Number of Passengers") +
  ggtitle("Seasonal subseries plot: Monthly Airline Passengers")




#ACF plot
ggAcf(air_tt)
tsdisplay(air_tt)


monthplot(air, main="Seasonal subseries plot", 
          ylab = "Passengers",
          xlab="Quarter", type="l")




# ########## #
# Exercise-2 #
# ########## #

# Box-Cox transformation

BoxCox.lambda(air_tt)#; l
l <- 0
#lambda <- 1




seasonplot(air, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal plot",
           ylab="Passengers",
           col=rainbow(20), 
           pch=19)

monthplot(air_tt, main="Seasonal subseries plot", 
          ylab = "Passengers",
          xlab="Quarter", type="l")

# ########## #
# Exercise-3 #
# ########## #
# naive methods
air_m1 <- snaive(air_train, h=26,lambda = l)            # seasonal naive-with lambda

plot(air_tt, main="Monthly Airline Passengers", ylab="Number of Passengers", xlab="Year")
lines(air_m1$mean, col=4)
legend("bottomright",lty=1,col=c(4),legend=c("Seasonal naive"))

#Box-Ljung test
#Check for residueal diagnostics
checkresiduals(air_m1)


#Accuracy

accuracy(air_m1,air_test)

#seasonal naive-forecast
plot(air_m1, ylab="Number of Passengers", xlab="Year")

air_m11 <- snaive(air_train, h=26)            # seasonal naive-without lambda

#Box-Ljung test
#Check for residueal diagnostics
checkresiduals(air_m11)


#Accuracy

accuracy(air_m11,air_test)


#seasonal naive-forecast
plot(air_m11, ylab="Number of Passengers", xlab="Year")

# ########## #
# Exercise-4 #
# ########## #


#Forecasting by decomposition
h=26
air_m1 <- stlf(air_train, method="naive", h=26) 
air_m2 <- stlf(air_train, method="rwdrift", h=26)
air_m3 <- stlf(air_train, method="ets", h=26)
air_m4 <- stlf(air_train, method="arima", h=26)


air_m5 <- stlf(air_train, method="naive", h=26, lambda = l, biasadj = TRUE) #s.window function check 
air_m6 <- stlf(air_train, method="rwdrift", h=26, lambda = l, biasadj = TRUE)
air_m7 <- stlf(air_train, method="ets", h=26, lambda = l, biasadj = TRUE)
air_m8 <- stlf(air_train, method="arima", h=26, lambda = l, biasadj = TRUE)


#list of models
models <- c("STL naive", "STL rwdrift", "STL ets", "STL arima",
            "STL naive lambda", "STL rwdrift lambda", "STL ets lambda","STL arima lambda")

n <- length(models); n   #number of models

#naming of models for the given data set
m <- "air_m"

#naming of training and test set
trainset <- air_train 
testset <- air_test

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

#selected model-residual diagnostics
checkresiduals(air_m4)
checkresiduals(air_m2)

#selected model-forecast
plot(air_m4)
plot(air_m2)


# ########## #
# Exercise-5 #
# ########## #

#ETS models
ets_1 <- ets(air_train, model = "AAA", damped = FALSE)
fit_ets1 <- forecast(ets_1, h=h)
ets_2 <- ets(air_train, model = "MAA", damped = FALSE)
fit_ets2 <- forecast(ets_2, h=h)
ets_3 <- ets(air_train, model = "MAM", damped = FALSE)
fit_ets3 <- forecast(ets_3, h=h)
ets_4 <- ets(air_train, model = "AAA", damped = TRUE)
fit_ets4 <- forecast(ets_4, h=h)     
ets_5 <- ets(air_train, model = "MAA", damped = TRUE)
fit_ets5 <- forecast(ets_5, h=h)     
ets_6 <- ets(air_train, model = "MAM", damped = TRUE)
fit_ets6 <- forecast(ets_6, h=h)     
ets_7 <- ets(air_train, model = "AAA", damped = FALSE, lambda = l, biasadj = TRUE)
fit_ets7 <- forecast(ets_7, h=h)     
ets_8 <- ets(air_train, model = "AAA", damped = TRUE, lambda = l, biasadj = TRUE)
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
accuracy(fit_ets1,air_test)
accuracy(fit_ets2,air_test)
accuracy(fit_ets3,air_test)
accuracy(fit_ets4,air_test)
accuracy(fit_ets5,air_test)
accuracy(fit_ets6,air_test)
accuracy(fit_ets7,air_test)
accuracy(fit_ets8,air_test)


#selected model-forecast
plot(fit_ets8)

#summary
summary(fit_ets8)

# ########## #
# Exercise-6 #
# ########## #

#Auto arima model

autoarima1 <- auto.arima(air_train, stepwise = TRUE, approximation = FALSE,lambda = l)
fit_arima1<- forecast(autoarima1, h=h)

summary(fit_arima1)

accuracy(fit_arima1, air_test)

checkresiduals(fit_arima1)

#Parameters altered -ARIMA model

autoarima_air <- auto.arima(air_train, stepwise = TRUE, approximation = FALSE,lambda = l,max.p = 10,max.q = 10,max.P = 10,
                            max.Q = 10,
                            max.order = 10,
                            max.d = 10,
                            max.D = 10)
fit_arima<- forecast(autoarima_air, h=h)

summary(fit_arima)

accuracy(fit_arima, air_test)

checkresiduals(fit_arima)

#selected model-forecast
plot(fit_arima)



# ########## #
# Exercise-7 #
# ########## #


#selected Models-comparison
final_m1 <- snaive(air_train, h=26,lambda = l)            # seasonal naive
final_m2 <- stlf(air_train, method="arima", h=26) #STL Arima
final_m3 <- forecast(ets_8, h=h) #A-ad-A
final_m4 <- forecast(autoarima_air, h=h) #auto arima

#Check for residueal diagnostics

#list of models
models <- c("Seasonal Naive method", 
            "STL Arima", 
            "ETS-AAdA", 
            "ARIMA")

n <- length(models); n   #number of models

#naming of models for the given data set
m <- "final_m"

#naming of training and test set
trainset <- air_train
testset <- air_test

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

#selected final model
#STL-Arima

#summary
summary(air_m4)

# ########## #
# Exercise-8 #
# ########## #

air_stl_arima <- stlf(air_tt, method="arima", h=34)

plot(air_stl_arima)

# ########## #
# Exercise-9 #
# ########## #

#air_stl_arima2 <- stlf(air, method="arima", h=14)

plot(air,ylab="Number of Passengers", xlab="Year")
lines(air_stl_arima$mean,col = 'red')
legend("topleft",lty=1,col='red',legend=c("STL + Arima"))


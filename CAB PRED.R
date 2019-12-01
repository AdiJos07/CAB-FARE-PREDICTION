#cleaning the environment
rm(list = ls())
# Setting the working directory
setwd("C:\\Users\\aditya joshi\\Edwisor\\cab fare prediction")
#get data
getwd()
#loading the data
train_cab = read.csv("train_cab.csv")

##Exploratory Data Analysis

class(train_cab)
#dataframe
head(train_cab)
#shows first 6 observations
str(train_cab)
#the strucutre of the dataframe
colnames(train_cab)
#all the column names in the dataframe
class(train_cab$fare_amount)
#the variable is a factor variable however we need to change it to numerical variable

#converting factor into numeric
train_cab$fare_amount = as.numeric(train_cab$fare_amount)
#class of fare_amount
class(train_cab$fare_amount)
#the variable is now numeric

# Fare_amount has some missing values around 25, but they got replaced with 1, so we are removing those observations with 1  
train_cab = subset(train_cab, fare_amount != 1)
View(train_cab)


####################################MISSING VALUE ANALYSIS##############################################
sum(is.na(train_cab))
# Line of code to know missing values in respective columns
apply(train_cab, 2, function(x) {sum(is.na(x))})

# Let's create a Missing Values dataframe for more clarity
missing_values = data.frame(apply(train_cab, 2, function(x){sum(is.na(x))}))
missing_values$Columns = row.names(missing_values)
names(missing_values)[1] = "Missing_values_percentage"
View(missing_values)

# Now, moving to calculate % of missing values
missing_values$Missing_values_percentage = (missing_values$Missing_values_percentage/nrow(train_cab)) * 100

# Have a glance at descending order of missing values percentage
missing_values = missing_values[order(-missing_values$Missing_values_percentage),]
row.names(missing_values) = NULL

# Re-ordering columns for better understanding
missing_values = missing_values[,c(2,1)]

# We got idea that the missing values percentage is negligible, so we are dropping the observations with missing values
train_cab = na.omit(train_cab)

sum(is.na(train_cab))
#Total number of missing values are 0

##OUTLIER ANALYSIS
#FIRST WE TRY TO MANUALLY PLOT THE OUTLIERS
#fair_amount variable
# Descending order to know outliers
train_cab = train_cab[order(-train_cab$fare_amount),]

# Ascending order to know inliers
train_cab = train_cab[order(train_cab$fare_amount),]
#no outliers in fare_amount

#longitude and latitude
#as per the knowledge latitudes must range from -90 to +90 and longitudes must range from -180 to +180
#so we will just use summary function to check outliers
summary(train_cab)

#In the console we can see that only pickup_longitude has one value out of range i.e Max.:401.08
#removing outlier
train_cab = subset(train_cab, (pickup_latitude > -90 & pickup_latitude < +90))
summary(train_cab)

#the outlier has been removed
# There are no outliers in pickup_longitude, pickup_latitude,dropoff_longitude, dropoff_latitude
#passenger_count
#practically we can have minimum of 1 and maximum of 6 passengers.
# Descending order to know outliers
train_cab = train_cab[order(-train_cab$passenger_count),]
#Ascending order to know inliers
train_cab = train_cab[order(train_cab$passenger_count),]
#outliers detected
#removing outliers from passenger_count
train_cab = subset(train_cab, passenger_count > 0 & passenger_count < 7) # Removed the outliers and inliers
train_cab = subset(train_cab, passenger_count != 0.12) # Removed observation with passenger_count value as 0.12
View(train_cab)

# as longitude and latitude are still there With the concept of feature engineering,we decided to create distance variable
#Using Haversine Formula,we can create distance variable. 
gcd.hf = function(long1, lat1, long2, lat2) {
  R = 6371 # Earth mean radius [km]
  delta.long = (long2 - long1)
  delta.lat = (lat2 - lat1)
  a = sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c = 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d)
}

#creating new variable distance while storing the values of all 4 variables
for (i in 1:nrow(train_cab)){
  train_cab$distance[i] = gcd.hf(train_cab$pickup_longitude[i], train_cab$pickup_latitude[i], 
                                 train_cab$dropoff_longitude[i], train_cab$dropoff_latitude[i])
}

#deleting the 4 variables which has been converted into distance
train_cab = subset(train_cab, select = -c(pickup_latitude, pickup_longitude, dropoff_latitude, dropoff_longitude))

#checking outliers in variable distance
train_cab = train_cab[order(-train_cab$distance),]
train_cab = train_cab[order(train_cab$distance),]

# Removing the outliers from the distance variable
train_cab = subset(train_cab, distance > 0 & distance <500)

#replacing UTC in pickup_datetime variable with ''
train_cab$pickup_datetime = gsub('\\ UTC', '', train_cab$pickup_datetime)
train_cab$Date = as.Date(train_cab$pickup_datetime)

train_cab$year = substr(as.character(train_cab$Date),1,4)
train_cab$month = substr(as.character(train_cab$Date),6,7)
train_cab$weekday = weekdays(as.POSIXct(train_cab$Date), abbreviate = F)
train_cab$Date = substr(as.character(train_cab$Date),9,10)
train_cab$hour <- substr(as.factor(train_cab$pickup_datetime),12,13)
View(train_cab)
train_cab$time = NULL
train_cab$pickup_datetime = NULL
###########################VISUALIZATION##################################
library(ggplot2)
library(scales)
library(psych)
library(gplots)

# Visualization between fare_amount and weekday.
ggplot(data = train_cab, aes(x = reorder(weekday,-fare_amount), y = fare_amount))+
  geom_bar(stat = "identity")+
  labs(title = "Fare Amount Vs. days", x = "Days of the week", y = "Fare")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.text.x = element_text( color="black", size=6, angle=45))
# We can understand that - Thursday and Saturday rides obtains the highest fare_amount 

# Visualization between fare_amount and months.
ggplot(train_cab, aes(x = reorder(month,-fare_amount), y = fare_amount))+ 
  geom_bar(stat = "identity")+
  labs(title = "Fare Amount Vs. Month", x = "Month", y = "Fare")+
  theme(axis.text.x = element_text( color="navy blue", size=8))
#--> We can observe that - specific months liks January, March, June collects the highest fare_amount

# Visualization between fare_amount and years.
ggplot(data = train_cab, aes(x = reorder(year,-fare_amount), y = fare_amount))+
  geom_bar(stat = "identity")+
  labs(title = "Fare Amount Vs. days", x = "Days of the week", y = "Fare")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.text.x = element_text( color="black", size=6, angle=45))
#--> We can say, in year 2009 and 2010 there were rides which got high fare_amount

# Visualization between fare_amount and hour.
ggplot(data = train_cab, aes(x = hour, y = fare_amount))+
  geom_bar(stat = "identity")+
  labs(title = "Fare Amount Vs. days", x = "Days of the week", y = "Fare")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.text.x = element_text( color="black", size=6, angle=45))
#--> Rides taken during 6 pm to 8 pm gives highest fare_amount

# Visualization between fare_amount and passenger_count.
ggplot(data = train_cab, aes(x = passenger_count, y = fare_amount))+
  geom_bar(stat = "identity")+
  labs(title = "Fare Amount Vs. days", x = "Days of the week", y = "Fare")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.text.x = element_text( color="black", size=6, angle=45))
#--> Rides with single passenger will lead to highest fares

# Visualization between fare_amount and distance.
ggplot(train_cab,aes(distance,fare_amount)) + 
  geom_point(alpha=0.5) +
  labs(title = "Fare amount based on distance", x = "Distance", y = "Fare Amount")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()
#--> It's an obvious fact, fare_amount is directly proportional to distance

#######################FEATURE SELECTION##################################
# Storing continuous variables
num_var = c('fare_amount', 'distance')

## Storing categorical variables
cat_var = c('Date', 'year', 'month', 'weekday','passenger_count','hour')

# Keeping data in another object
train_cab2 = train_cab
train_cab = train_cab2


# Correlation Analysis is performed between num_var (continuous independent variables) & fare_amount (continuous target variable)

library(corrgram)

corrgram(train_cab[,num_var],order=FALSE,upper.panel = panel.pie,
         text.panel = panel.txt,
         main= "Correlation Analysis between numeric variables")


for(i in cat_var){
  print(i)
  Anova_test_result = summary(aov(formula = fare_amount~train_cab[,i],train_cab))
  print(Anova_test_result)
}
#-->> From the result, we can observe, passenger_count, weekday and Date has p value > 0.05, by which, we accept null hypothesis.
# Deleting the below given continuous and categorical variables from day, as we found out they won't add any value to the model.
train_cab = subset(train_cab, select = -c(passenger_count, weekday, Date))

####################################################FEATURE SCALING#################################################
# Checking distance variable distribution using histogram 
hist(train_cab$distance)
#--> The diagram represents, it is left skewed
#Checking summary of distance
summary(train_cab$distance)
#--> Maximum value is 499.8405. We will go for normilisation

# We are going to define function using log
signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

# Applying log function to distance variable
train_cab$distance = signedlog10(train_cab$distance)

# Checking distance distribution after applying function 
hist(train_cab$distance)

# Let's look at summary again
summary(train_cab$distance)

# Let's manually remove the outliers in distance variable
train_cab = subset(train_cab, distance > 0)
###############################################MODEL EVALUATION##########################################################
# Let's clean R Environment, as it uses RAM which is limited

install.packages("DataCombine")
library(DataCombine)
rmExcept("train_cab")

# Defining Error Metric, which are used to evaluate the model

# Defining R Square function - Correlation of original and predicted values.

Rsquare = function(y,y1){
  cor(y,y1)^2
}

# Defining RMSE function - Root Mean Squared Errors (Calculated Errors)
# RMSE over MAPE, Why? - Because, RMSE gives weight to large errors during calculation and it penalizes large errors 

RMSE = function(y,y1){
  sqrt(mean((y - y1)^2))
}

train_cab2 = train_cab
train_cab = train_cab2

set.seed(123)
train_index = sample(1:nrow(train_cab),0.8*nrow(train_cab)) # Using Simple Random Sampling Technique
train= train_cab[train_index,]
test= train_cab[-train_index,]

####################################################LINEAR REGRESSION MODE###############################################
# Code for development of model
LRModel = lm(fare_amount~., train)

# Let's have a view on the summary
summary(LRModel)

# Predicting model on train data
LRTrain = predict(LRModel, train[-1])

# Predicting model on test data
LRTest = predict(LRModel, test[-1])

# Calculating RMSE for Train Data
LRRMSE_Train = RMSE(LRTrain, train[,1])

# Calculating RMSE for Test Data
LRRMSE_Test = RMSE(LRTest, test[,1])

# Calculating Rsquare for Train Data
LRR2_Train = Rsquare(train[,1], LRTrain)

# Calculating Rsquare for Test Data
LRR2_Test = Rsquare(test[,1], LRTest)

#########################################DECISION TREE#######################################################

# Code to build Decision Tree
library(rpart)

DTModel = rpart(fare_amount~., train, method = "anova")

# Predicting model on train data
DTTrain = predict(DTModel, train[-1])

# Predicting model on test data
DTTest = predict(DTModel, test[-1])

# Calculating RMSE for Train Data
DTRMSE_Train = RMSE(DTTrain, train[,1])

# Calculating RMSE for Test Data
DTRMSE_Test = RMSE(DTTest, test[,1])

# Calculating Rsquare for Train Data
DTR2_Train = Rsquare(train[,1], DTTrain)

# Calculating Rsquare for Test Data
DTR2_Test = Rsquare(test[,1], DTTest)

################################################RANDOM FOREST##########################################3

# Code for development of model

library(randomForest)

RFModel = randomForest(fare_amount~., train, ntree = 500, method = "anova")

# Predicting model on train data
RFTrain = predict(RFModel, train[-1])

# Predicting model on test data
RFTest = predict(RFModel, test[-1])

# Calculating RMSE for Train Data
RFRMSE_Train = RMSE(RFTrain, train[,1])

# Calculating RMSE for Test Data
RFRMSE_Test = RMSE(RFTest, test[,1])

# Calculating Rsquare for Train Data
RFR2_Train = Rsquare(train[,1], RFTrain)

# Calculating Rsquare for Test Data
RFR2_Test = Rsquare(test[,1], RFTest)

#SAVING IN DATAFRAME#

Result = data.frame("Model" = c("Linear Regression", "Decision Tree", "Random Forest"),
                    "RMSE_Values_Train" = c(LRRMSE_Train, DTRMSE_Train, RFRMSE_Train),
                    "RMSE_Values_Test" = c(LRRMSE_Test, DTRMSE_Test, RFRMSE_Test),
                    "Rsquare_Values_Train" = c(LRR2_Train, DTR2_Train, RFR2_Train),
                    "Rsquare_Values_Test" = c(LRR2_Test, DTR2_Test, RFR2_Test))

#--> Random Forest gives the most optimized values. We are going to freeze Random Forest

# Now, it's time to bring the test data and perform the steps as performed on train_cab and predict the fare_amount
# Loading the data into our R environment
test_cab = read.csv("test.csv")

#EXPLORITORY DATA ANALYSIS#
class(test_cab) # Its DataFrame
head(test_cab) # Let's have a look on first 6 observations
dim(test_cab) # 9914 observations & 6 variables
str(test_cab) # Have a look on the structure, pickup_datetime is in factor
summary(test_cab) # With a glance, we can get that, pickup_latitude and passenger_count don't have outliers
names(test_cab) # In names, we can get to see as perfect naming for respective variables.

# We are going to change pickup_datetime from factor to datetime
# But first, let's replace UTC in pickup_datetime variable with ''
test_cab$pickup_datetime = gsub('\\ UTC', '', test_cab$pickup_datetime)
test_cab$Date = as.Date(test_cab$pickup_datetime)

# We are familiar with working with variables like, hour, day, date, year but not a complete datetime variable.
# We are also not dealing with time series analysis, so, now, we are going to split the pickup_datetime into its subsets
test_cab$year = substr(as.character(test_cab$Date),1,4)
test_cab$month = substr(as.character(test_cab$Date),6,7)
test_cab$weekday = weekdays(as.POSIXct(test_cab$Date), abbreviate = F)
test_cab$Date = substr(as.character(test_cab$Date),9,10)
test_cab$time = substr(as.factor(test_cab$pickup_datetime),12,13)

# Now, we are going to delete pickup_datetime variable, as we have already have its substitutes
test_cab = subset(test_cab, select = -c(pickup_datetime))

#MISSING VALUE ANALYSIS# 

# Line of code to know the sum of missing values in dataset
sum(is.na(test_cab)) # Total number of missing values are 0

#OUTLIER ANALYSIS#

# Coming to outliers, let's check summary and check for outliers
summary(test_cab)
#--> It clearly shows, there are no outliers.

# Now, let's create distance using Haversine Formula as did in train_cab

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the Haversine formula (hf)
gcd.hf = function(long1, lat1, long2, lat2) {
  R = 6371 # Earth mean radius [km]
  delta.long = (long2 - long1)
  delta.lat = (lat2 - lat1)
  a = sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c = 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

# Now, we are going to apply the function, over all the rows to create a new variable - distance
for (i in 1:nrow(test_cab)){
  test_cab$distance[i] = gcd.hf(test_cab$pickup_longitude[i], test_cab$pickup_latitude[i], 
                                test_cab$dropoff_longitude[i], test_cab$dropoff_latitude[i])
}

# Dimension Reduction - we are going to delete latitude and longitude variables as we obtained distance from these four variables
test_cab = subset(test_cab, select = -c(pickup_latitude, pickup_longitude, dropoff_latitude, dropoff_longitude))

# Now, its time to check outliers in distance
test_cab = test_cab[order(-test_cab$distance),]
test_cab = test_cab[order(test_cab$distance),]

# Removing the outliers from the distance variable
test_cab = subset(test_cab, distance > 0 & distance <500)

# To match the number of features of test data to the model, we are going to delete few variables
test_cab = subset(test_cab, select = -c(passenger_count, weekday, Date))

# We are going to normalize the data in distance variable

# Checking distance variable distribution using histogram 
hist(test_cab$distance)
#--> The diagram represents, it is left skewed

# Checking summary of distance
summary(test_cab$distance)
#--> Maximum value is 499.8466. We will go for normilisation

# We are going to define function using log
signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

# Applying log function to distance variable
test_cab$distance = signedlog10(test_cab$distance)

# Checking distance distribution after applying function 
hist(test_cab$distance)

# Let's look at summary again
summary(test_cab$distance)

# Let's manually remove the outliers in distance variable
test_cab = subset(test_cab, distance > 0)

# We are near to predict our cab fare using Random Forest

# Code for development of model

RFModel = randomForest(fare_amount~., train, ntree = 500, method = "anova")

# Predicting model on test_cab data
RFTest_cab = predict(RFModel, test_cab)

# Adding our obtained predictions as Predicted Fare Amount variable to test_cab dataset
test_cab$Predicted_fare_amount = RFTest_cab

#predicted values
head(test_cab)

#OBJECTIVE ACHIEVED.


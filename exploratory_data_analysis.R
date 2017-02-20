#########################
#
# AirBNB Kaggle Challenge
# Link: https://www.kaggle.com/yyamamoto/airbnb-recruiting-new-user-bookings
# GOAL: predict which country a new user's first booking destination will be. 
#
# Exploratory Data Analysis
# Jake Moody 
#
#########################

# Load Libraries
library(rpart)
library(randomForest)
library(lubridate)
library(stringr)
library(plotly)
library(caret)

# IMPORT DATA
train <- read.csv("/Users/JakeMoody/Dropbox/GitHub Projects/data/airbnb/train_users_2.csv")
test <- read.csv("/Users/JakeMoody/Dropbox/GitHub Projects/data/airbnb/test_users.csv")
sessions <- read.csv("/Users/JakeMoody/Dropbox/GitHub Projects/data/airbnb/sessions.csv")
sample_sub <- read.csv("/Users/JakeMoody/Dropbox/GitHub Projects/data/airbnb/sample_submission_NDF.csv")
countries <- read.csv("/Users/JakeMoody/Dropbox/GitHub Projects/data/airbnb/countries.csv")
age_gender_bkts <- read.csv("~/Dropbox/GitHub Projects/data/airbnb/age_gender_bkts.csv")

# EXAMINE DATA

head(train, n=5)
head(sessions)
head(age_gender_bkts)
head(countries)

is.na(train)

table(train$country_destination)
prop.table(table(train$country_destination))
str(train)
str(sessions)

###### CLEAN DATA ###### 
# Clean variables

# Clean dates
train$date_first_booking <- as.Date(train$date_first_booking, format="%Y-%m-%d") # transform to date object
train$date_account_created <- as.Date(train$date_account_created, format="%Y-%m-%d") # transform to date object
train$timestamp_first_active <- as.Date(as.character(train$timestamp_first_active),"%Y%m%d%H%M%S") # transform to date object

# Clean customer characteristics
#train$gender <- as.factor(gsub("-unknown-", "NA", train$gender))

# Clean browser data
#train$first_browser <- as.factor(gsub("-unknown-", "NA", train$first_browser))
#table(train$gender)

# Fix age values
train <- train[train$age <= 100,] # only ages less than 100

train <- train[!is.na(train$country_destination),] # quick hack to remove NAs

###### CREATE DUMMY VARIABLES ###### 
# Time between events
train$time_to_booking <- as.numeric(train$date_first_booking - train$date_account_created) # time it took between creating an account and 1st booking
train$time_to_account <- as.numeric(train$date_account_created - train$timestamp_first_active) # time between first visit to creating an account
train$total_time_to_booking <- as.numeric(train$date_first_booking - train$timestamp_first_active) # total time difference

# PREDICT NA age VALUES
predicted_age <- rpart(age ~ signup_app + first_device_type + first_browser,  data = train[!is.na(train$age),], method = "anova")
train$age[is.na(train$age)] <- predict(predicted_age, train[is.na(train$age),])

hist(train$age)
boxplot(train$age)

# millenial bracket 
train$millenial <- NA
train$millenial[train$age < 35] <- 1
train$millenial[train$age >= 35] <- 0

# Decision Trees
my_tree <- rpart(country_destination ~ language + gender + age, data = train, method = "class")

# Clean dates
test$date_first_booking <- as.Date(test$date_first_booking, format="%Y-%m-%d") # transform to date object
test$date_account_created <- as.Date(test$date_account_created, format="%Y-%m-%d") # transform to date object
test$timestamp_first_active <- as.Date(as.character(test$timestamp_first_active),"%Y%m%d%H%M%S") # transform to date object

# Clean customer characteristics
#train$gender <- as.factor(gsub("-unknown-", "NA", train$gender))

# Clean browser data
#train$first_browser <- as.factor(gsub("-unknown-", "NA", train$first_browser))
#table(train$gender)

# Fix age values
test <- test[test$age <= 100,] # only ages less than 100

###### CREATE DUMMY VARIABLES ###### 
# Time between events
test$time_to_booking <- as.numeric(test$date_first_booking - test$date_account_created) # time it took between creating an account and 1st booking
test$time_to_account <- as.numeric(test$date_account_created - test$timestamp_first_active) # time between first visit to creating an account
test$total_time_to_booking <- as.numeric(test$date_first_booking - test$timestamp_first_active) # total time difference

# PREDICT NA age VALUES
predicted_age <- rpart(age ~ signup_app + first_device_type + first_browser,  data = test[!is.na(test$age),], method = "anova")
test$age[is.na(test$age)] <- predict(predicted_age, test[is.na(test$age),])

test<- test[!is.na(test$id),] # quick hack to remove NAs


## APPLY PREDICTION DECISION TREE

my_prediction <- predict(my_tree, test, type = "class")
my_solution <- data.frame(id = test$id, country = my_prediction)



write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
nrow(my_solution)


class(train$language)
class(train$gender)
class(train$country_destination)
# RANDOM FOREST 
set.seed(111)
my_forest <- randomForest(as.factor(country_destination) ~ age + gender, data = train, importance = TRUE, ntree = 1000)

my_prediction <- predict(my_forest, test)

## fix error
unique(train$language)
unique(test$language)

unique(train$gender)
unique(test$gender)


my_solution <- data.frame(id = test$id, country = my_prediction)

write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
str(train)

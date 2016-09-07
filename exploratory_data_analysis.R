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
train$gender <- as.factor(gsub("-unknown-", "NA", train$gender))

# Clean browser data
train$first_browser <- as.factor(gsub("-unknown-", "NA", train$first_browser))

###### CREATE DUMMY VARIABLES ###### 
# Time between events
train$time_to_booking <- as.numeric(train$date_first_booking - train$date_account_created) # time it took between creating an account and 1st booking
train$time_to_account <- as.numeric(train$date_account_created - train$timestamp_first_active) # time between first visit to creating an account
train$total_time_to_booking <- as.numeric(train$date_first_booking - train$timestamp_first_active) # total time difference


# 
train$millenial <- NA
train$millenial[train$age < 35] <- 1
train$millenial[train$age >= 35] <- 0
unique(train$affiliate_channel)
prop.table(table(train$millenial, train$country_destination), 1)

unique(train$first_browser)
# PREDICT NA VALUES
predicted_age <- rpart(age ~ language + gender + signup_app + first_device_type + first_browser , data = train[!is.na(train$age),], method = "class")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])


# Decision Trees

# RANDOM FOREST 
my_forest <- randomForest(country_destination ~ age + language, data = train, importance = TRUE, ntree = 1000)



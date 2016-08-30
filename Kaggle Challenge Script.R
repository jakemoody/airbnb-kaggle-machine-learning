#########################
#
# AirBNB Kaggle Challenge 
#
# Jake Moody 
#
#########################

# Load Libraries
library(rpart)
library(randomForest)

# https://www.kaggle.com/yyamamoto/airbnb-recruiting-new-user-bookings/expolatory-analysis-and-aggregations/comments
# GOAL: predict which country a new user's first booking destination will be. 
# Load files
train <- read.csv("train_users_2.csv")
test <- read.csv("test_users.csv")
sessions <- read.csv("sessions.csv")
sample_sub <- read.csv("sample_submission_NDF.csv")
countries <- read.csv("countries.csv")
age_gender_bkts <- read.csv("age_gender_bkts.csv")

head(train, n=5)
head(sessions)
head(age_gender_bkts)
head(countries)

is.na(train)


# PREDICT NA VALUES
predicted_age <- rpart(country_destination ~ age +  language, data = train[!is.na(train$age),], method = "class")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

# RANDOM FOREST 
my_forest <- randomForest(country_destination ~ age + language, data = train, importance = TRUE, ntree = 1000)



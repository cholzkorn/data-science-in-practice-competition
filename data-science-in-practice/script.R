# The dataset contains daily visitor data from a leisure attraction
# located at the border of two counties in Germany. The task is to predict
# the column called 'label' for the test set.

#### DATA DESCRIPTION #########################################################

# For some features, no description is available, so they may be dummy features
# (also called 1-hot encoded variables, binary indicators,...), numerical
# features, etc., but not categorical ones. For the following features we
# provide additional information:

# school holiday
  # 0 = no school holiday
  # 1 = school holiday only in county #1
  # 2 = school holiday only in county #2
  # 3 = school holiday both in county #1 and in county #2

# bank holiday
  # 0 = no bank holiday
  # 1 = bank holiday only in county #1
  # 2 = bank holiday only in county #2
  # 3 = bank holiday both in county #1 and in county #2

# Additionally, daily weather data for the location of the leisure
# attraction is provided.




#### LOADING THE DATA #########################################################

library(tidyverse)

train <- as.tibble(read.csv("data_train.csv"))
test <- as.tibble(read.csv("data_test.csv"))
weather <- as.tibble(read.csv("weather.csv"))



#### DATA EXPLORATION #########################################################

summary(train) # nothing unusual here

## UNIQUE VALUES
# Looking at the unique values of the training data
unique(train[1]) # dates - many unique values
unique(train[2]) # bank holiday: only 0, 3 and 2
unique(train[3]) # binary
unique(train[4]) # binary
unique(train[5]) # binary
unique(train[6]) # feature_3: 3.2, 4.3, 4.6
unique(train[7]) # feature_4: 6.7, 7.3, 7.6
unique(train[8]) # feature_5: 1.7, 2.3, 2.6
unique(train[9]) # feature_6: 3.7, 4.3, 4.6
unique(train[10]) # binary
unique(train[11]) # school holiday: 0, 1, 2, 3
unique(train[12]) # 0 - no information
unique(train[13]) # 0 - no information
unique(train[14]) # binary
unique(train[15]) # many values

range(train[15]) # values between (142, 3761)

## Dropping unnecessary columns
train <- train[-c(12, 13)]
length(train)

# Checking type of date
# install.packages("date")
library(date)
date::is.date(train[1])

train[1] <- as.Date(train[[1]], "%Y-%m-%d")
train[1,1] # earliest date: 2005-03-20
train[nrow(train), 1] # latest date: 2009-12-31

# Adding month row:
library(lubridate)

train <- as.tibble(cbind(train, month = as.factor(month(as.POSIXlt(train[[1]])))))


# Checking for outliers in visitor number and seasonal effects
ggplot(data=train, aes(x=date, y=label)) +
  geom_point() +
  geom_rect(aes(ymin=0, ymax=4000, xmin=train[1],xmax=train[nrow(train), 1],
                fill=month), alpha =0.007)

# There seems to be a seasonal effect, so we will keep month in the DF





#### WEATHER DATA #############################################################

weather
summary(weather)
View(weather)

# Maybe drop unnecessary columns: wind_Speed_max
# weather <- weather[ , -which(names(weather) %in% c("wind_speed_max"))]

# Train and Weather

nrow(train) == nrow(weather)

# We need to join train and weather on the Day for it to be useful

weather[1] <- as.Date(weather[[1]], "%Y-%m-%d") # convert to date

# Join dataframes
wtrain <- merge(train, weather, by="date")
wtrain

# Omitting all rows with NAs

wtrain <- as.tibble(na.omit(wtrain))
wtrain




#### OH-ENCODING #############################################################

# school holiday and bank holiday need to be encoded using One Hot Encoding

# First, we create the encoded matrices

school_f <- as.factor(wtrain$school_holiday)
bank_f <- as.factor(wtrain$bank_holiday)

oh_school <- as.tibble(model.matrix(~ school_f))
colnames(oh_school) <- c("school_holiday_0", "school_holiday_1", "school_holiday_2", "school_holiday_3")

oh_bank <- as.tibble(model.matrix(~ bank_f))
colnames(oh_bank) <- c("bank_holiday_0", "bank_holiday_2", "bank_holiday_3")

# Next, we drop the original columns from the df

wtrain <- wtrain[ , -which(names(wtrain) %in% c("bank_holiday", "school_holiday"))]

# And finally add the OH-encoded columns to get our df ready for modeling

mtrain <- as.tibble(cbind(wtrain, oh_school, oh_bank))
View(mtrain)




#### MODELLING ################################################################

# From the dataframe mtrain we get our predictors and our label

X <- as.tibble(wtrain[ , -which(names(mtrain) %in% c("label"))])
y <- as.tibble(wtrain$label)

# Linear model

lmodel <- lm(y, data=X)
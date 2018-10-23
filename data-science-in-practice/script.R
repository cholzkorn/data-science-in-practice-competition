rm(list=ls())

# https://stackoverflow.com/questions/52862458/replace-na-with-grouped-means-in-r

# notes:

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
sample_submission <- as.tibble(read.csv("sample_submission.csv"))


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

test <- test[-c(12, 13)]
length(test)

# Checking type of date
# install.packages("date")
library(lubridate)
is.Date(train[1])

train[1] <- as.Date(train[[1]], "%Y-%m-%d")
train[1,1] # earliest date: 2005-03-20
train[nrow(train), 1] # latest date: 2009-12-31

test[1] <- as.Date(test[[1]], "%Y-%m-%d")
test[1,1]
test[nrow(test), 1]

# Adding month row:
library(lubridate)

train <- as.tibble(cbind(train, month = as.factor(month(as.POSIXlt(train[[1]])))))
test <- as.tibble(cbind(test, month = as.factor(month(as.POSIXlt(test[[1]])))))

# Adding weekday row:
train <- as.tibble(cbind(train, wday = as.factor(weekdays(as.POSIXlt(train[[1]])))))
test <- as.tibble(cbind(test, wday = as.factor(weekdays(as.POSIXlt(test[[1]])))))

# Checking for outliers in visitor number and seasonal effects
ggplot(data=train, aes(x=date, y=label)) +
  geom_point() +
  theme(legend.position = "none") +
  geom_rect(aes(ymin=0, ymax=4000, xmin=train[1],xmax=train[nrow(train), 1],
                fill=month), alpha =0.007) +
  scale_fill_manual(name = "Legende", 
                    values = c("blue", "blue", "green", "green", "green",
                               "yellow", "yellow", "yellow", "brown", "brown", "brown", "blue")) +
  labs(title="Saisonalit?t der Besucherzahlen",
       y="Besucherzahlen", x="Zeit", caption="Blau = Winter, Gelb = Sommer") +
  geom_point()

# There seems to be a seasonal effect, so we will keep month in the DF





#### WEATHER DATA #############################################################

weather
summary(weather) # Lots of NAs!

# Maybe drop unnecessary columns: wind_Speed_max
# weather <- weather[ , -which(names(weather) %in% c("wind_speed_max"))]

# Train and Weather

nrow(train) == nrow(weather)

# We need to join train and weather on the Day for it to be useful

weather[1] <- as.Date(weather[[1]], "%Y-%m-%d") # convert to date


#### MEAN INJECTION ############################################################

# Creating month variable in the weather df for grouping
weather <- as.tibble(cbind(weather, month = as.factor(month(as.POSIXlt(weather[[1]])))))

weather <- weather %>%  group_by(month) %>%
  mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE),.)))

# dropping the month variable again, since we would duplicate it when merging
weather <- weather[ , -which(names(weather) %in% c("month"))]

#### JOIN DATAFRAMES
wtrain <- merge(train, weather, by="date")
wtrain

wtest<- merge(test, weather, by="date")
wtest

# Omitting all rows with NAs in the train set

wtrain <- as.tibble(na.omit(wtrain))
wtrain

#### OH-ENCODING #############################################################

# school holiday and bank holiday need to be encoded using One Hot Encoding

# We check the values again
unique(wtrain$school_holiday)
unique(wtrain$bank_holiday)

unique(wtest$school_holiday)
unique(wtest$bank_holiday)

# First, we create the encoded matrices

# For train

school_f <- as.factor(wtrain$school_holiday)
bank_f <- as.factor(wtrain$bank_holiday)

oh_school <- as.tibble(model.matrix(~ school_f))
colnames(oh_school) <- c("school_holiday_0", "school_holiday_1", "school_holiday_2", "school_holiday_3")

oh_bank <- as.tibble(model.matrix(~ bank_f))

oh_bank <- cbind(oh_bank)
colnames(oh_bank) <- c("bank_holiday_0", "bank_holiday_2", "bank_holiday_3")

# For test

school_ftst <- as.factor(wtest$school_holiday)
bank_ftst <- as.factor(wtest$bank_holiday)

oh_schooltst <- as.tibble(model.matrix(~ school_ftst))

colnames(oh_schooltst) <- c("school_holiday_0", "school_holiday_1", "school_holiday_2", "school_holiday_3")

oh_banktst <- as.tibble(model.matrix(~ bank_ftst))

colnames(oh_banktst) <- c("bank_holiday_0", "bank_holiday_2", "bank_holiday_3")


# Next, we drop the original columns and dates from the df

wtrain <- wtrain[ , -which(names(wtrain) %in% c("bank_holiday", "school_holiday", "date"))]

wtest <- wtest[, -which(names(wtest) %in% c("bank_holiday", "school_holiday", "date"))]

# And finally add the OH-encoded columns to get our df ready for modeling

mtrain <- as.tibble(cbind(wtrain, oh_school, oh_bank))


# Adding empty label for mtest

mtest <- as.tibble(cbind(wtest, oh_schooltst, oh_banktst, label = 0))


#### MODELLING ################################################################



#### LINEAR MODEL

library(modelr)

# From the dataframe mtrain we get our predictors and our label

X_train <- as.tibble(mtrain[ , -which(names(mtrain) %in% c("label"))])
y_train <- as.tibble(mtrain$label)

y_test <- as.tibble(mtest)

# Linear model

lmodel <- lm(mtrain$label ~ ., data=mtrain)
summary(lmodel)

lmodel2 <- lm(mtrain$label ~ mtrain$month + mtrain$school_holiday_3 +
                mtrain$school_holiday_1 + mtrain$feature_0 +
                mtrain$air_temperature_daily_max, data=mtrain)

lm_pred <- predict(lmodel2, y_test)


#### RANDOM FOREST

library(randomForest)

rf <- randomForest(mtrain$label ~ ., data=mtrain, importance=TRUE)

rf_pred <- predict(rf, y_test)

varImpPlot(rf, type = 1, main ="Accuracy Decrease")
varImpPlot(rf, type = 2, main = "Gini Decrease")


# Write csv

tree_submit <- tibble(date = test$date, visitors=rf_pred)
tree_submit
write.csv(tree_submit, file = "tree_submit.csv", row.names = FALSE)










#########################

# ---- BOOSTING ----------------------------------------------------------------------------- #

############## Finally, we try to get even better predictions with boosting. For that, we try
############## a few different models with different settings for the shrinkage factor (eta)
############## and the maximum depth of the tree (max_depth)

library(xgboost)

# First we set up the folds and the parameter list:

X_train_xgb <- data.matrix(cbind(X_train, label = 0))
y_train_xgb <- y_train[[1]]

N <- nrow(X_train_xgb)
fold_number <- sample(1:5, N, replace = TRUE)
params <- data.frame(eta = rep(c(.05, .1, .3, .5, .7), 5),
                     max_depth = rep(c(2, 3, 6, 12, 24), rep(5,5))
)

# Now we apply the preceding algorithm to compute the error for each model and each fold
# using five folds

error <- matrix(0, nrow = 25, ncol =5)
for(i in 1:nrow(params)){
  for (k in 1:5){
    fold_idx <- (1:N)[fold_number == k]
    xgb <- xgboost(data = X_train_xgb, label = y_train_xgb,
                   params = list(eta = params[i, "eta"],
                                 max_depth = params[i, "max_depth"]),
                   objective = "reg:linear", nrounds = 100, verbose = 0)
    pred <- predict(xgb, X_train_xgb)
    error[i, k] <- mean(y_train_xgb - pred)
  }
}

# The errors are stored as a matrix with the models along the rows and folds along
# the columns.

avg_error <- 100 * rowMeans(error)
xgb_mdls_errors <- as.tibble(cbind(params, avg_error))
xgb_mdls_errors_abs <- as.tibble(cbind(params, avg_error_abs = abs(avg_error)))

# We get the smallest error for eta = 0.5 and max_depth = 12

xgb_winner <- arrange(xgb_mdls_errors_abs, avg_error_abs)[1,]
xgb_winner

# Therefore we now build this model and compute the rmse.

xgb_finalmodel <- xgboost(data = X_train_xgb, label = y_train_xgb,
                          objective = "reg:linear", nrounds = 100,
                          eta = xgb_winner$eta, xgb_winner$max_depth)

# Transform test to data.matrix, so that we can compare the three models.

y_test_xgb <- data.matrix(y_test)

xgb_pred <- predict(xgb_finalmodel, y_test_xgb)
xgb_submit <- as.tibble(cbind(test$date, xgb_pred))

write.csv(xgb_submit, file = "xgb_submit.csv", row.names = FALSE)


# Deep Neural Network using H2O

library(h2o)
library(caret)

h2o.init()

h2o_train <- as.h2o(mtrain)
h2o_test <- as.h2o(y_test)

h2o_model <- h2o.deeplearning(x = setdiff(names(mtrain), c("label")),
                              y = "label",
                              training_frame = h2o_train,
                              standardize = TRUE,         # standardize data
                              hidden = c(100, 100, 100, 100),       # 4 layers of 100 nodes each
                              rate = 0.01,                # learning rate
                              epochs = 200,               # iterations/runs over data
                              seed = 1234                 # reproducability seed
)

h2o_pred <- as.data.frame(h2o.predict(h2o_model, h2o_test))

print(h2o_cm <- confusionMatrix(h2o_pred$predict, test$label))

# Submitting predictions

h2o_submit <- as.tibble(cbind(test$date, h2o_pred))

write.csv(h2o_submit, file = "h2o_submit.csv", row.names = FALSE)




# PLOT DIFFERENCES

colors    <- c( "c1" = "blue", "c2" = "red" )

ggplot(data = test, aes(x = test$date)) +
  geom_point(aes(y = rf_pred, color="c1")) +
  geom_point(aes(y = xgb_pred, color="c2")) +
  labs(y="visitors", x="time") +
  scale_color_manual(name = "Model", 
                     breaks = c("c1", "c2"), 
                     values = colors,
                     labels = c("RF", "XGB")) +
  theme(legend.position = "top")

# Mixing models: random forest and h2o

mix <- (rf_pred + h2o_pred) / 2

mix_submit <- as.tibble(cbind(date = test$date, visitors = mix))
write.csv(mix_submit, file = "mix.csv", row.names = FALSE)

# BEST SCORE UNTIL NOW! Mixing models: random forest and xgb

mix <- (rf_pred + xgb_pred) / 2

mix_submit <- as.tibble(cbind(date = test$date, visitors = mix))
write.csv(mix_submit, file = "mix.csv", row.names = FALSE)



# Combination 3


mix3 <- (rf_pred + ((h2o_pred + xgb_pred)/2)) / 2

mix3_submit <- as.tibble(cbind(date = test$date, visitors = mix3))
write.csv(mix3_submit, file = "mix3.csv", row.names = FALSE)


# Weighted combination

library(matrixStats)

pred_mx <- as.matrix(cbind(rf_pred, xgb_pred, h2o_pred))

weighted_mix <- rowWeightedMeans(pred_mx, w = c(1, 0.35, 0.45), rows = NULL)

weighted_submit <- as.tibble(cbind(date = test$date, visitors = weighted_mix))

write.csv(weighted_submit, file = "weighted_submit.csv", row.names = FALSE)


# Rescaling
library(scales)

mix_re <- as.tibble(read.csv("mix.csv"))

train_range <- range(train$label)

mix_re$visitors <- rescale(mix_re$visitors, to=c(280, 3200))

write.csv(mix_re, file = "mix_re.csv", row.names = FALSE)

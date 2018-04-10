
# Importing the dataset
stayzilla <- read.csv(file="stayzilla_clean.csv", header=TRUE, sep=",")

# Changing to factor
stayzilla$property_type <- as.factor(stayzilla$property_type)
stayzilla$city_class_cat <- as.factor(stayzilla$city_class_cat) 
stayzilla$service_value <- as.factor(stayzilla$service_value)

stayzilla$room_type_cat <- as.factor(stayzilla$room_type_cat) 

stay_logis <- stayzilla

#classification to predict the Property type.

#dividing into train and test

## 75% of the sample size
smp_size <- floor(0.75 * nrow(stay_logis))

## set the seed to make your partition reproducible
set.seed(123)

# splitting the train and test data
train_ind <- sample(seq_len(nrow(stay_logis)), size = smp_size)

train <- stay_logis[train_ind, ]
test <- stay_logis[-train_ind, ]

train$property_type <- relevel(train$property_type, ref="1")
test$property_type <- relevel(test$property_type, ref="1")

# Load the package
library(nnet)
# Run the model (multinomial logistic regression)
model <- multinom(property_type ~ image_count + amenities_cnt + description_cnt + rm_price +similar_hotel_cnt, data=train)
summary(model)
#prediction
pred <- predict(model, test)

library(caret)
library(ggplot2)
confusionMatrix(pred, test$property_type)

#random forest
library(randomForest)

model <- randomForest(property_type ~ image_count +amenities_cnt + description_cnt + rm_price +similar_hotel_cnt, data=train)
summary(model)

# Make and test predictions
pred_rf <- predict(model, newdata = test)
#accuracy
library(caret)
confusionMatrix(pred_rf, test$property_type)
varImp(model)

# Build the NB model
library(e1071)
library(caret)
model <- naiveBayes(property_type ~ image_count +amenities_cnt + description_cnt + rm_price +similar_hotel_cnt, data=train)
summary(model)

# Predict
prediction <- predict(model, newdata = test)

library(caret)
library(ggplot2)
confusionMatrix(prediction, test$property_type)



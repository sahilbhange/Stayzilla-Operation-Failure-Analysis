library(stringr)
# Importing dataset
stayzilla1 <- read.csv(file="stayzilla_com-travel_sample1.csv", header=TRUE, sep=",")

stayzilla2 <- read.csv(file="Stayzilla_clean.csv", header=TRUE, sep=",", row.names = 1)

# stayzilla1: being used for Exploratory analysis

# stayzilla2: being used for Linear regression and SVM.

##############################################

# Data preprocessing on stayzilla1

# removing features which are not of any use
stayzilla1 <- stayzilla1[,-c(4,5,9,10,12:17,21:23,26,27,30,32:35)]

# http://stla.github.io/stlapblog/posts/Numextract.html

# Function to extarct numbers from a string
Numextract <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}

# Extracting numbers from occupancy field

stayzilla1$adults = as.numeric(strsplit(Numextract(substr(stayzilla1$occupancy,1,4))," "))

stayzilla1$kids = as.numeric(strsplit(Numextract(substr(stayzilla1$occupancy,4,12))," "))

# Extracting numbers from room price field
stayzilla1$roomPrice = as.numeric(strsplit(Numextract(substr(stayzilla1$room_price,1,10))," "))

# Dropping column which are now not usefull
stayzilla1$room_price <- NULL
stayzilla1$room_types <- NULL

# Correcting city names and property types with different spellings
stayzilla1$city[stayzilla1$city == "Alapuzzha"] <- "Alappuzha"
stayzilla1$city[stayzilla1$city == "Chittaurgarh"] <- "Chittorgarh"
stayzilla1$city[stayzilla1$city == "Jhunjhunun"] <- "Jhunjhunu"
stayzilla1$city[stayzilla1$city == "Koduma"] <- "Kodarma"
stayzilla1$city[stayzilla1$city == "Yamuna Nagar"] <- "Yamunanagar"
stayzilla1$property_type[stayzilla1$property_type == "Spa"] <- "Hotel"

# Changing wrong room price to correct one
stayzilla1$roomPrice[stayzilla1$roomPrice == 103889] <- 10389

# Replacing blanks with NA values
stayzilla1$description[stayzilla1$description == ""] <- NA
stayzilla1$amenities[stayzilla1$amenities == ""] <- NA

# Checking percentage of Missing Data
missPerc <- function(x){
  y = (sum(is.na(x)==TRUE)/length(x))*100
  paste(round(y,0),"%")
}
# percentage of missing values
data.frame(missingCount = sapply(stayzilla1,function(x){(sum(is.na(x)==TRUE))}), missingPercent = sapply(stayzilla1,missPerc))

# Missing values are in Description and amenities field which we can not impute being text description so taking complete cases

# Taking all complete cases without NA values
stayzilla1 <- stayzilla1[complete.cases(stayzilla1),]

# again viewing data structure
str(stayzilla1)

#################### Exploratory Analysis and Data Visualization

library(ggplot2)
library(ggmap)

map <- get_map(geocode("India"), scale = 2,zoom = 5)

ggmap(map)

points <- ggmap(map) + geom_point(aes(x = longitude, y = latitude, size = stayzilla1$roomPrice),data = stayzilla1,col = "orange", alpha = .5)

updatedMap <- points + scale_size_continuous(name = "roomPrice")
updatedMap

# Box plot to see outliers in room price
boxplot(log(stayzilla1$roomPrice), main = "Room price box plot")

#Bar plots

op <- par(mar=c(11,4,4,4))
# Bar plot showing Occupancy details

barplot(table(stayzilla1$occupancy), main = "Occupancy", ylab = "count",col = "yellow",border = "red", las = 2)

# Bar plot showing Property types
barplot(table(stayzilla1$property_type), main = "Property Type", xlab = "categories", ylab = "count", col = "orange",border = "red")

# Bar plot showing Property verification status
barplot(table(stayzilla1$service_value), main = "Verified vs Non-verified Properties", xlab = "service_value categories", ylab = "count",col = "orange",border = "yellow")

rm(op)

# histogram for price
qplot(stayzilla1$roomPrice, geom = "histogram",fill=I("yellow"), 
col=I("red"),alpha = I(.9), breaks = seq(0,11000,by=500), main = "Histogram for Room Price", xlab = "room price")


# top 10 cities of Stayzilla property
op <- par(mar=c(11,4,4,4))

df1 <- as.data.frame(table(stayzilla1$city))
names(df1) <- c("city", "propertyCount")
df2 <- head(df1[order(df1$propertyCount, decreasing = TRUE), ],10)

barplot(df2$propertyCount, names.arg = df2$city,las=2, main = "Top 10 Cities of Stayzilla property", ylab = "count",col = "orange",border = "yellow")

# top 10 Stayzilla property by Room price
df3 <- stayzilla1[,c("city","property_name", "roomPrice")]
df3 <- head(df3[order(df3$roomPrice, decreasing = TRUE), ],10)
op <- par(mar=c(14,4,4,2))
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

ggplot(data=df3, aes(x=property_name, y=roomPrice)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=city), vjust=-0.3, size=3.5)+
  theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
rm(op)

# property type
df4 <- as.data.frame(table(stayzilla1$property_type))
names(df4) <- c("propertyType", "Count")
df4 <- df4[order(df4$Count, decreasing = TRUE), ]
op <- par(mar=c(10,4,4,2))
barplot(df4$Count, names.arg = df4$propertyType, las=2,col = df4$propertyType,  legend.text = df4$propertyType, args.legend = list(x="topright"), main = "Stayzilla Property Types")
rm(op)

#############################################################

# Data preprocessing on stayzilla2

# changing to factors
stayzilla2$property_type <- as.factor(stayzilla2$property_type)
stayzilla2$service_value<- as.factor(stayzilla2$service_value)
stayzilla2$room_type_cat <- as.factor(stayzilla2$room_type_cat)
stayzilla2$city_class_cat <- as.factor(stayzilla2$city_class_cat)

# viewing structure of the data
str(stayzilla2)


##################### Linear regression

# creating training and testing dataset using random sampling
set.seed(121)
index <- sample(1:nrow(stayzilla2),round(nrow(stayzilla2)*.80))
train <- stayzilla2[index,c(-15,-16)]
test <- stayzilla2[-index,c(-15,-16)]

# Building Linear regression model using all the features except property names
mod1 <- lm(rm_price ~., data = train[,c(-1)])
mod1$xlevels[["room_types"]] <- union(mod1$xlevels[["room_types"]], levels(test$room_types))
summary(mod1)

# Function to calculate RMSE
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Predict using train
pred <- predict(mod1, train[,c(-1)])
# summarize accuracy
error1 <- mod1$residuals
lrPredictionRMSE1 <- rmse(error1)
lrPredictionRMSE1 # 2584.529

# Predict using test
pred <- predict(mod1, test[,c(-1)])
# summarize accuracy
error2 <- mod1$residuals
lrPredictionRMSE2 <- rmse(error2)
lrPredictionRMSE2 # 2584.529

# Building stepwise regression to get the best model with important feature set to be used
library(MASS)
step <- stepAIC(mod1)
step$anova

# Building Linear regression model using all the features suggested by stepwise regression
mod2 <- lm(rm_price ~ city_class + room_types + property_type + service_value + description_cnt + similar_hotel_cnt + Adults, data = train[,c(-1)])
summary(mod2)

# Predict using train
pred <- predict(mod2, train[,c(-1)])
# summarize accuracy
error1 <- mod2$residuals
lrPredictionRMSE1 <- rmse(error1)
lrPredictionRMSE1 # 2590.172

# Predict using test
pred <- predict(mod2, test[,c(-1)])
# summarize accuracy
error2 <- mod2$residuals
lrPredictionRMSE2 <- rmse(error2)
lrPredictionRMSE2 # 2590.172

##################### SVM regression
# Regression using SVM

library(e1071)
# Performing regression using SVM considering important features suggested by stepwise regression
modSVM1 <- svm(rm_price ~ city_class + room_types + property_type + service_value + description_cnt + similar_hotel_cnt + Adults, train[,c(-1)])

summary(modSVM1)

### Calculating training RMSE

predictedY2T = predict(modSVM1,train[,c(-1)])
error2T <- train$rm_price - predictedY2T
svmPredictionT_RMSE <- rmse(error2T)
print(svmPredictionT_RMSE) # 3039.239
### Calculating testing RMSE
predictedY2 = predict(modSVM1,test[,c(-1)])
error2 <- test$rm_price - predictedY2
svmPredictionRMSE <- rmse(error2)
print(svmPredictionRMSE) # 2808.717


## SVM using Cost function to get best SVM model
set.seed(1)
model3 <- tune(svm, rm_price ~ city_class + room_types + property_type + Accept_Rate + Response_time + description_cnt + similar_hotel_cnt + Adults, data = train[,c(-1)], ranges = list(epsilon=seq(0,1,0.1), cost = seq(1,10,1)))

# summary(model3)
best_model <- model3$best.model
summary(best_model)

### Calculating training RMSE
best_predictionT <- predict(best_model,train[,c(-1)])
best_errorT <- train$rm_price - best_predictionT
bestPredictionT_RMSE <- rmse(best_errorT)
print(bestPredictionT_RMSE) # 2934.162
### Calculating testing RMSE
best_prediction <- predict(best_model,test[,c(-1)])
best_error <- test$rm_price - best_prediction
bestPredictionRMSE <- rmse(best_error)
print(bestPredictionRMSE) # 2752.776
############################################




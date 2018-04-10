# Importing the dataset
Stayzilla  <- read.csv(file.choose(), header = TRUE, sep = ",")

# Viewing the dataset
View(Stayzilla)

# Checking column names
names(Stay_zilla)

# checking dimension of the dataset
dim(Stayzilla)


# Removing the missing information records from the data
Stayzilla <- Stayzilla[-c(1140:1207),]

dim(Stayzilla)

# To separate Acceptance rate and repsonse time informtion from the additional_info column
require(reshape)
Stayzilla_inter = transform(Stayzilla, additional_info = colsplit(additional_info, split = "\\|", names = c('Accept_Rate', 'Response_time')))

head(Stayzilla_inter$additional_info$Accept_Rate)

head(Stayzilla_inter$additional_info$Accept_Rate)

library(stringr)

# function to extract number from the string
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
}

head(Stayzilla_inter$additional_info$Accept_Rate)

# Extracting Acceptance rate and response time
Stayzilla$Accept_Rate  <- (numextract(Stayzilla_inter$additional_info$Accept_Rate))
Stayzilla$Response_time  <- (numextract(Stayzilla_inter$additional_info$Response_time))

# COunt the number of amenities provided for the property
head(Stayzilla$amenities)

sapply(gregexpr("\\W+", head(Stayzilla$amenities)), length) + 1

Stayzilla$amenities_cnt <- sapply(gregexpr("\\W+", Stayzilla$amenities), length) + 1


# COunt the number of words in the discription field provided for the property
Stayzilla$description_cnt <- sapply(gregexpr("\\W+", Stayzilla$description), length) + 1

# COunt the number of similar hotels in the given area for the property
Stayzilla$similar_hotel_cnt <- sapply(gregexpr("\\W+", Stayzilla$similar_hotel), length) + 1

# Extract the numeric price value from the room_proce column
Stayzilla$rm_price <- numextract(Stayzilla$room_price)


# COunt the number of adults and kids property can occupy

Stayzilla_new = Stayzilla

temp <- head(Stayzilla_new$occupancy)

names(temp)

# Split the content on the "Adult" pattern to separate the kids and adults occupancy
Stayzilla_inter = transform(Stayzilla_new, occupancy = colsplit(occupancy, split = "Adult", names = c('Adults', 'Kids')))

#Stayzilla$Accept_Rate  <- (numextract(Stayzilla_inter$occupancy$Kids))

# Extracting the value of adults and kids from occupancy
Stayzilla$Kids <- (numextract(Stayzilla_inter$occupancy$Kids))

Stayzilla$Adults <- (numextract(Stayzilla_inter$occupancy$Adults))

max((numextract(Stayzilla_inter$occupancy$Adults)))


head(Stayzilla_inter$occupancy$Kids)

head(Stayzilla$Accept_Rate)

Stayzilla$temp_rate <- (ifelse(Stayzilla$Accept_Rate=="6",Stayzilla$Response_time,Stayzilla$Accept_Rate))

Stayzil_temp <- Stayzilla 

Stayzil_temp$Accept_Rate <- ifelse(Stayzilla$Accept_Rate==1 ,Stayzilla$Response_time,Stayzil_temp$Accept_Rate)

Stayzil_temp$Response_time <- ifelse(Stayzilla$Response_time>24, 1,Stayzil_temp$Response_time)


# Make values numeric as data exctracted is the character data
Stayzil_temp$Response_time <- as.numeric(Stayzil_temp$Response_time)
Stayzil_temp$Accept_Rate <- as.numeric(Stayzil_temp$Accept_Rate)
Stayzilla$Response_time <- as.numeric(Stayzilla$Response_time)
Stayzilla$Accept_Rate <- as.numeric(Stayzilla$Accept_Rate)

table(Stayzilla$property_type)
table(Stayzilla$room_types)

# Convert the categorical service type to numeric values for clustering
Stayzilla$service_value <- ifelse(Stayzilla$service_value=="Not Verified" ,0,1)

# Convert the categorical property type to numeric values for clustering
Stayzilla$property_type <- ifelse(Stayzil_temp$property_type=="Apartment" ,1,Stayzilla$property_type)


# Fill the missing image count vaues with the 0 values as there is no image URL present in the data means no image available on the site
Stayzilla$image_count[is.na(Stayzilla$image_count)] <- 0


# Select only numeric data field values from the pre-processed data for clustering
Stayzilla_final <- Stayzilla[,c(5,7,10,13,14,15,16,17,18,19,20)]

View(Stayzilla_final)

Stayzilla_final$property_type <- as.factor(Stayzilla_final$property_type)

Stayzilla_final$service_value <- as.factor(Stayzilla_final$service_value)

Stayzilla_final$Kids <- as.factor(Stayzilla_final$Kids)
Stayzilla_final$Adults <- as.factor(Stayzilla_final$Adults)


# Make price to numeric
Stayzilla_final$rm_price <- as.numeric(Stayzilla_final$rm_price)


table(Stayzilla_final$Accept_Rate)


Stayzilla_final$city_class  <- Stayzilla_formated$city_classification
Stayzilla_final$room_types  <- Stayzilla_formated$room_type

table(Stayzilla_final$city_class)
table(Stayzilla_final$room_types)

unique(Stayzilla_final$room_types)

Stayzilla_final$room_type_cat <- ifelse(Stayzilla_final$room_types=="1 BHK", 1,Stayzilla_final$room_types)
table(Stayzilla_final$room_type_cat)


# convert the city_class_cat data into numerical categoric data
Stayzilla_final$city_class_cat <- ifelse(Stayzilla_final$city_class=="Metropolitan", 1,Stayzilla_final$city_class)


# correct the wrong value for the price
Stayzilla_clean$rm_price[Stayzilla_clean$rm_price == 103889] <- 10389

View(Stayzilla_clean)

###########################################################################################
#############################       Clustering Gower          #############################
########################################################################################### 
library(dplyr) # for data cleaning
#install.packages("ISLR")
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
#install.packages("Rtsne")
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization


View(Stayzilla_clean[,c(-1,-2,-3)])


Stayzilla_clean  <- read.csv(file.choose(), header = TRUE, sep = ",")

Stayzilla_clean$amenities_cnt <- as.factor(Stayzilla_clean$amenities_cnt)

View(Stayzilla_clean)

# https://www.r-bloggers.com/clustering-mixed-data-types-in-r/

gower_dist <- daisy(Stayzilla_clean[,c(-1,-2,-3)],
                    metric = "gower",
                    type = list(logratio = 3))

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

# Output most similar pair
View(Stayzilla_clean[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ])


# Output most dissimilar pair
View(Stayzilla_clean[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ])



sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "#clusters",
     ylab = "Sil Width")
lines(1:10, sil_width)


# Cluster Visualization
pam_fit <- pam(gower_dist, diss = TRUE, k = 4)

pam_results <- Stayzilla_clean %>%
  dplyr::select(-property_name) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary


View(Stayzilla_final[pam_fit$medoids, ])


tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         property_name = Stayzilla_clean$property_name)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

write.csv(Stayzilla_clean, file = "Stayzilla_clean.csv")

head(tsne_data)

tsne_data %>%
  filter(X > 10 & X < 20,
         Y > 20 & Y < 10) %>%
  left_join(Stayzilla_final, by = "property_name") %>%
  collect %>%
  .[["property_name"]]



head(tsne_data$cluster)


Merged_data <- merge(Stayzilla_clean, tsne_data, by="property_name")

View(Merged_data[Merged_data$cluster=="4",])

View(Merged_data[Merged_data$cluster=="3",])

View(Merged_data[Merged_data$cluster=="2",])

View(Merged_data[Merged_data$cluster=="1",])






#################   BIKE RENTAL PREDICTION  #################################


####NAME : GRACE MARIYA P J 
###### POSITION : R PROGRAMMING INTERNSHIP
####    TASKS:

#1.EXPLOITARY DATA ANALYSIS(EDA)
#2.ATTRIBUTES DISTRIBUTION AND TRENDS
#3.SPLIT DATASET INTO TRAIN AND TEST DATASET
#4.CREATE MODEL USING THE RANDOM FOREST ALGORITHM
#5.PREDICT THE PERFORMANCE OF THE MODEL ON THE DATASET


# TASK: 1   -------Exploitary Data Analysis   -------------
#                  -------------------------

#1.1 ------Load dataset and Load relevant libraries---------

#Import dataset
bike <- read_excel("C:/Users/User/Downloads/1657875746_day.xlsx")

#Load relevant libraries

library(ggplot2)# Load the ggplot2 library
library(caTools) #required library for data splition , split into train and test dataset
library(randomForest)
#check and view dataset

View(bike)             #display dataset
dim(bike)              #Check dimension of dataset, It has 731 rows and 16 columns
summary(bike)          #just read and understanding overall dataset


# 1.2---------Perform data type conversion of the attributes-----

#check data types of each attributes
str(bike)
#All attributes are 'double' , so no need for convert data types or type conversion


# 1.3 -------carry out the missing value analysis----
#check how many missing values
sum(is.na(bike))       #Check this dataset has any null or missing values, It has no missing values


########## TASK 1 completed!..............

#### TASK 2: Attribute Distribution and Trends ######################################

#2.1 --------Plot monthly distribution of the total number of bikes rented --------

# Plotting the monthly distribution
ggplot(bike, aes(x = mnth, y = cnt)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = "Month", y = "Total Number of Bikes Rented", title = "Monthly Distribution of Rented Bikes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#2.2 --------Plot yearly distribution of the total number of bikes rented --------

# Plotting the yearly distribution
ggplot(bike, aes(x = yr, y = cnt)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Year", y = "Total Number of Bikes Rented", title = "Yearly Distribution of Rented Bikes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#2.3 --------Plot boxplot for outliers analysis --------

# Create a boxplot for outliers analysis
ggplot(bike, aes(x = "", y = cnt)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.color = "red") +
  labs(y = "Number of Bikes Rented", title = "Boxplot for Outliers Analysis") +
  theme_minimal()



########## TASK 2 completed!..............

#### TASK 3: Split dataset into train and test set ######################################

set.seed(123)
split = sample.split(bike$cnt, SplitRatio=0.8)
table(bike$cnt)
#Creating the training and test set seperately
train = subset(bike, split== TRUE)
test = subset(bike,split == FALSE)
train
test

dim(train)
dim(test)

########## TASK 3 completed!..............


#### TASK 4: Create model using the random forest algorithm ######################################

# Train a random forest model
model<- randomForest(cnt ~ mnth + yr, data = train)

# Make predictions on the test set
predict <- predict(model, newdata = test)

# Print the predictions
print(predict)


# Create a data frame with the features for prediction
new_data <- data.frame(mnth = 6, yr = 2022)  

# Make a prediction for the specific input features
predictFeature <- predict(model, newdata = new_data)

# Print or inspect the prediction
print(predictFeature)
########## TASK 4 completed!..............


#### TASK 5: Predict the performance of the model on the test dataset ######################################

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(predict - test$cnt))
new_mae<-mean(abs(predictFeature - test$cnt))

# Print the MAE
cat("Mean Absolute Error:", mae, "\n")

cat("Mean Absolute Error from feature based:", new_mae, "\n")


########## TASK 5 completed!..............

# Load required packages
library(funModeling)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(corrgram)
library(dplyr)
library(rstatix)
library(corrplot)
library(faraway)
library(olsrr)
library(leaps)
library(plyr)
library(xtable)
library(rpart)
require(ipred)
require(tree)


#Numerical summary
summary(df)
#graphicalsummary
# Plot of SalePrice against Year Sold
plot(df$YrSold, df$SalePrice, xlab = "Year Sold", ylab = "Price", main = "Price vs.Year Sold")
# Boxplot of SalePrice for each House Condition
boxplot(df$SalePrice ~ df$HouseCond, xlab = "Neighborhood", ylab = "Sale price")
## Boxplot of SalePrice for each Overall Quality
boxplot(df$SalePrice ~ df$OverallQual, xlab = "Overall Quality", ylab = "Sale price")
## Barplot of the count of houses for each House Condition
barplot(table(df$HouseCond), xlab = "House Condition")
## Density plot of Overall Quality
plot(density(df$OverallQual), main="Density Plot of OverallQual")
## Pie chart of the count of houses for each House Condition
pie(table(df$HouseCond), main = "House Condition", col = c("purple", "red", "blue"))
legend("topright", legend = names(table(df$HouseCond)), fill = c("red", "green", "blue"))


# Load the data from a CSV file
df <- read.csv("C:\\Users\\91909\\Downloads\\house-data.csv")
########################2nd QUESTION################################
# Create a new column called HouseCond based on the OverallCond variable
# If OverallCond <= 3, set HouseCond to "Poor"
# If OverallCond <= 6, set HouseCond to "Average"
# If OverallCond > 6, set HouseCond to "Good"
df$HouseCond <- ifelse(df$OverallCond <= 3, "Poor", 
                       ifelse(df$OverallCond <= 6, "Average", "Good"))

# Remove the columns with missing values from the data frame using the select function from the dplyr package
data <- df %>% select(-c("Id", "Alley", "PoolQC", "Fence", "MiscFeature", "Utilities", "Street", "LotConfig", "Neighborhood",
                         "Condition1", "Condition2", "RoofMatl", "Exterior1st", "ExterQual", "BsmtQual", "Heating",
                         "Functional", "GarageType", "GarageArea", "PavedDrive", "PoolArea", "SaleType", "MasVnrArea",
                         "BsmtQual"))

head(data)
#Label encoding
# Convert the categorical columns to a factor variable
data$BldgType <- factor(data$BldgType)
data$HouseStyle <- factor(data$HouseStyle)
data$RoofStyle <- factor(data$RoofStyle)
data$ExterCond <- factor(data$ExterCond)
data$Foundation <- factor(data$Foundation)
data$BsmtCond <- factor(data$BsmtCond)
data$KitchenQual <- factor(data$KitchenQual)
data$GarageCond <- factor(data$GarageCond)
data$SaleCondition <- factor(data$SaleCondition)
data$HouseCond <- factor(data$HouseCond)


#Use label encoding to convert the factor variable to numeric labels
#Subtract 1 from each value to start the labels at 0 instead of 1
data$BldgType <- as.numeric(data$BldgType) - 1
data$HouseStyle <- as.numeric(data$HouseStyle) - 1
data$RoofStyle <- as.numeric(data$RoofStyle) - 1
data$ExterCond <- as.numeric(data$ExterCond) - 1
data$Foundation <- as.numeric(data$Foundation) - 1
data$BsmtCond <- as.numeric(data$BsmtCond) - 1
data$KitchenQual <- as.numeric(data$KitchenQual) - 1
data$GarageCond <- as.numeric(data$GarageCond) - 1
data$SaleCondition <- as.numeric(data$SaleCondition) - 1
data$HouseCond <- as.numeric(data$HouseCond) - 1


#colSums() returns the number of missing values in each column of the data data frame
colSums(is.na(data))

#The head() function prints the first few rows of the data data frame
head(data)

#Label encoding

Convert the categorical columns to a factor variable
data$BldgType <- factor(data$BldgType)
data$HouseStyle <- factor(data$HouseStyle)
data$RoofStyle <- factor(data$RoofStyle)
data$ExterCond <- factor(data$ExterCond)
data$Foundation <- factor(data$Foundation)
data$BsmtCond <- factor(data$BsmtCond)
data$KitchenQual <- factor(data$KitchenQual)
data$GarageCond <- factor(data$GarageCond)
data$SaleCondition <- factor(data$SaleCondition)
data$HouseCond <- factor(data$HouseCond)

#Use label encoding to convert the factor variable to numeric labels
#Subtract 1 from each value to start the labels at 0 instead of 1
data$BldgType <- as.numeric(data$BldgType) - 1
data$HouseStyle <- as.numeric(data$HouseStyle) - 1
data$RoofStyle <- as.numeric(data$RoofStyle) - 1
data$ExterCond <- as.numeric(data$ExterCond) - 1
data$Foundation <- as.numeric(data$Foundation) - 1
data$BsmtCond <- as.numeric(data$BsmtCond) - 1
data$KitchenQual <- as.numeric(data$KitchenQual) - 1
data$GarageCond <- as.numeric(data$GarageCond) - 1
data$SaleCondition <- as.numeric(data$SaleCondition) - 1
data$HouseCond <- as.numeric(data$HouseCond) - 1

#colSums() returns the number of missing values in each column of the data data frame
colSums(is.na(data))

#Replace missing values with the median value of each column
data$LotFrontage <- impute(data$LotFrontage,median)
data$BsmtCond <- impute(data$BsmtCond, median)
data$MasVnrArea <- impute(data$MasVnrArea, median)
data$GarageCond <- impute(data$GarageCond,median)

########################### 2ND QUESTION- A ####################################
#Load the nnet and caret packages
library(nnet)
library(caret)

# Fit a multinomial logistic regression model to predict HouseCond
# Set a random seed for reproducibility
set.seed(123)

# Create a training and testing set by splitting the data randomly using createDataPartition
train_index <- caret::createDataPartition(data$HouseCond, p = 0.8, list = FALSE)
training <- data[train_index, ]
testing <- data[-train_index, ]

# Fit a multinomial logistic regression model using the multinom function from the nnet package
# The formula HouseCond ~ . specifies that all variables in the data frame except HouseCond are used to predict the outcome
# The family argument specifies that a binomial distribution should be used for the response variable
model <- multinom(HouseCond ~ ., data = training, family = "binomial")

# Print the summary of the model to examine its coefficients and goodness-of-fit statistics
summary(model)

########################### 2ND QUESTION- B ####################################

#Build a decision tree model using the 'tree' function, with SalePrice as the response variable and all other variables as predictors
(tree.Glc_all_data <- tree(SalePrice ~ ., data = data, na.action = na.pass))
#Summarize the decision tree model
summary(tree.Glc_all_data)
#Plot the decision tree model
plot(tree.Glc_all_data)
#Add labels to the plot 
text(tree.Glc_all_data)
#Compute the cross-validated error rate for the decision tree model using the 'errorest' function with bootstrapping
errorest(SalePrice ~ ., data = data, model = rpart, estimator = "boot")
#Compute the cross-validated error rate for the decision tree model using the 'errorest' function with k-fold cross-validation
errorest(SalePrice ~ ., data = data, model = rpart, estimator = "cv")


########################### 3rd  QUESTION- A ####################################

# Load the required package for random forest
require(randomForest)

# Train the random forest model with SalePrice as the response variable and all other variables in the dataset as predictors
(RF <- randomForest(SalePrice ~ ., data = data))

# Use the errorest function to estimate the out-of-sample error of the random forest model using bootstrapping
errorest(SalePrice ~ ., data = data, model = randomForest, estimator = "boot")

# Use the errorest function to estimate the out-of-sample error of the random forest model using cross-validation
errorest(SalePrice ~ ., data = data, model = randomForest, estimator = "cv")


# Load the required package for SVM
library(e1071)

# Split the data into training and testing sets
set.seed(123) # set the seed for reproducibility
library(caret)
train_index <- caret::createDataPartition(data$SalePrice, p = 0.8, list = FALSE) # create indices for training and testing sets
train_data <- data[train_index, ] # subset the training set
test_data <- data[-train_index, ] # subset the testing set

# Fit the SVM regression model with radial kernel, cost = 10, and gamma = 0.01
svm_model <- svm(SalePrice ~ ., data = train_data, kernel = "radial", cost = 10, gamma = 0.01)

# Plot the SVM model
plot(svm_model, data)

# Summarize the SVM model
summary(svm_model)

# Make predictions on the testing set using the SVM model
predictions <- predict(svm_model, test_data)

# Calculate the root mean squared error (RMSE) between predicted and actual SalePrice
rmse <- sqrt(mean((predictions - test_data$SalePrice)^2))
print(paste("RMSE:", rmse))

# Use grid search to find the best combination of cost and gamma parameters for the SVM model
set.seed(1) # set the seed for reproducibility
tune.out = tune(svm, SalePrice ~ ., data = train_data, kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)


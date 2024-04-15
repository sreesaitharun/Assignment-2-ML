# Load necessary libraries
install.packages(c("tidyverse", "caret", "glmnet"))
library(tidyverse)
library(caret)
library(glmnet)

# Set working directory
setwd(r"(C:\Users\peddo\Downloads\)")

# Read the datasets
students <- read.csv("oulad-students.csv")
assessments <- read.csv("oulad-assessments.csv")

# Merge datasets
merged_data <- merge(students, assessments, by = c("id_student", "code_module", "code_presentation"))

# Remove rows with missing values
merged_data <- na.omit(merged_data)

# Convert date columns to Date type
merged_data$date <- as.Date(merged_data$date)
merged_data$date_submitted <- as.Date(merged_data$date_submitted)

# Convert factor columns to factors
merged_data$gender <- as.factor(merged_data$gender)
merged_data$region <- as.factor(merged_data$region)
merged_data$highest_education <- as.factor(merged_data$highest_education)
merged_data$imd_band <- as.factor(merged_data$imd_band)
merged_data$age_band <- as.factor(merged_data$age_band)
merged_data$final_result <- as.factor(merged_data$final_result)
merged_data$assessment_type <- as.factor(merged_data$assessment_type)

# Classification Model (predicting final_result)
# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(merged_data$final_result, p = 0.7, list = FALSE)
train_data <- merged_data[trainIndex, ]
test_data <- merged_data[-trainIndex, ]

# Train a classification model (using logistic regression as an example)
classification_model <- glm(final_result ~ . - id_student - date - date_registration - date_unregistration, data = train_data, family = binomial)

# Predict on test data
predictions_classification <- predict(classification_model, newdata = test_data, type = "response")

# Regression Model (predicting score)
# Train a regression model
regression_model <- glm(score ~ . - id_student - date - date_registration - date_unregistration, data = train_data)

# Predict on test data
predictions_regression <- predict(regression_model, newdata = test_data)

# Model interpretation
# Classification model interpretation
summary(classification_model)

# Regression model interpretation
summary(regression_model)


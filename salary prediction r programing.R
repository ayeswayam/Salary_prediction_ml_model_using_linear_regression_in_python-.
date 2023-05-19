# Load required libraries
library(caret)
library(ggplot2)

# Read the CSV file
data <- read.csv("C:\\Users\\ayesw\\Desktop\\summer training projects\\salary prediction\\Salary.csv")
data

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$Salary, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Create a linear regression model
model <- train(Salary ~ YearsExperience, data = trainData, method = "lm")

# Print the model details
print(model)

# Make predictions on the test set
predictions <- predict(model, newdata = testData)

# Calculate the RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((testData$Salary - predictions)^2))
print(paste("RMSE:", rmse))

# Plot the actual vs. predicted salaries
ggplot(data = testData, aes(x = YearsExperience, y = Salary)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predictions), color = "red") +
  labs(x = "Years of Experience", y = "Salary") +
  ggtitle("Actual vs. Predicted Salary")

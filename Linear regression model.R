
#Part 3 : Linear regression model 

#We split data into training and test sets
library(rsample)
set.seed(123)

split <- initial_split(df, prop = 0.8)  # 80% for training
train_df <- training(split)
test_df <- testing(split)

cat("Training set shape:", dim(train_df), "\n")
cat("Testing set shape:", dim(test_df), "\n")


#We eparate features (X) and target variable (Y) for training set
X_train <- train_df[, !names(train_df) %in% "selling_price"]  # All columns except the target variable
y_train <- train_df$selling_price  # The target variable

#we do the same thing for testing set
X_test <- test_df[, !names(test_df) %in% "selling_price"]  # All columns except the target variable
y_test <- test_df$selling_price  # The target variable

#Fit the linear regression model
linear_model <- lm(selling_price ~ ., data = train_df)


summary(linear_model)

#Make the predictions

predictions <- predict(linear_model, newdata = X_test)

#Model evaluation
mse <- mean((predictions - y_test) ^ 2)
cat("Mean Squared Error:", mse, "\n")

r_squared <- 1 - (sum((predictions - y_test) ^ 2) / sum((y_test - mean(y_test)) ^ 2))
cat("R-squared:", r_squared, "\n")

#R-squared: 0.6138746 is really decent strat but there is still room for improvememnt 
#Now we will plot actual vs predicted values to see if linear regression was actuallya good fit 
library(ggplot2)

results_df <- data.frame(Actual = y_test, Predicted = predictions)

ggplot(results_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Linear Regression",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_minimal()


#according to the graph the The points are not closely aligned with the red dashed line (which represents a perfect fit). 
#There seems to be a curved or spread-out pattern, indicating a potential non-linear relationship between the features and the target variable.
#We will try to solve this by using the stacking method 





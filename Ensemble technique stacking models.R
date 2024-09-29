#Part 4 : Ensemble technique stacking models 

library(caret)
library(randomForest) 
library(rpart) 
library(dplyr)  

#Step 1: Train base models on training data

#Linear Regression
lm_model <- lm(selling_price ~ ., data = train_df)

#Decision Tree
tr_model <- rpart(selling_price ~ ., data = train_df)

#Random Forest
rf_model <- randomForest(selling_price ~ ., data = train_df)

#Step 2: Generate predictions from base models
train_preds <- data.frame(
  linear_pred = predict(linear_model, newdata = train_df),
  tree_pred = predict(tree_model, newdata = train_df),
  rf_pred = predict(rf_model, newdata = train_df)
)
#Step 3: Train meta-model using predictions from base models


lm_predictions <- predict(lm_model, newdata = X_train)  # For training data
tr_model<-predict(tr_model , newdata= X_train)
rf_predictions <- predict(rf_model, newdata = X_train)  # For training data

#Combine the training predictions 
train_preds_df <- data.frame(selling_price = train_df$selling_price, 
                             lm_preds = lm_predictions, 
                             rf_preds = rf_predictions)

#Fit the meta-model
meta_model <- lm(selling_price ~ ., data = train_preds_df)
# View the summary of the meta-model
summary(meta_model)

#Step 4: Make predictions with the meta-model using test predictions
test_preds_df <- data.frame(lm_preds = predict(lm_model, newdata = X_test), 
                            rf_preds = predict(rf_model, newdata = X_test))

#Make predictions using the meta-model
meta_predictions <- predict(meta_model, newdata = test_preds_df)




meta_r_squared <- 1 - (sum((meta_predictions - y_test) ^ 2) / sum((y_test - mean(y_test)) ^ 2))
cat("Meta Model R-squared:", meta_r_squared, "\n")

#Plot actual vs predicted values for the meta-model
results_meta_df <- data.frame(Actual = y_test, Predicted = meta_predictions)

ggplot(results_meta_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted (Meta-Model)",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_minimal()

rmse <- sqrt(mean((meta_predictions - y_test) ^ 2))
cat("Meta Model RMSE:", rmse, "\n")
#Meta Model R-squared: 0.9322857 a significant improvememnt 




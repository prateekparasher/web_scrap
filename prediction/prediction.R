
#Load the data

head(data)



#Lets Plot

plot(data)




#Correlation Test

cor(road_data$accidents, warning_data$month)



#Linear Model

linearMod <- lm(accidents ~ month, data = data)

print(linearMod)

summary(linearMod)



#Polynomial Model

polynomialMod <- lm(accidents ~ month + I(month ^ 2), data = data)

print(polynomialMod)

summary(polynomialMod)



#Sampling

no_of_records <- sample(1:nrow(data), 0.8 * nrow(data))

training_data <- data[no_of_records,]

testing_data <- data[-no_of_records,]



#Training Linear Model

lr_model <- lm(accidents ~ month, data = training_data)

lm_predicted <- predict(lr_model, testing_data)

lm_predicted



lm_actual_preds <- data.frame(cbind(actuals = road_data$accident, predicted = lm_predicted))

lm_actual_preds



#Training Polynomial Model(Second Order)

pl_model <- lm(accidents ~ month + I(month ^ 2), data = training_data)

pl_predicted <- predict(pl_model, testing_data)

pl_predicted



pl_actual_preds <- data.frame(cbind(actuals = road_data$accident, predicted = pl_predicted))

pl_actual_preds



#Lets validate, Compare and Decide which model fits our data

#AIC

AIC(linearMod)

AIC(polynomialMod)



#BIC

BIC(linearMod)

BIC(polynomialMod)



#Correlation Accuracy

lm_correlation_accuracy <- cor(lm_actual_preds)

lm_correlation_accuracy



pl_correlation_accuracy <- cor(pl_actual_preds)

pl_correlation_accuracy



#Min_Max Accuracy

lm_min_max_accuracy <- mean(apply(lm_actual_preds, 1, min) / apply(lm_actual_preds, 1, max))

lm_min_max_accuracy



pl_min_max_accuracy <- mean(apply(pl_actual_preds, 1, min) / apply(pl_actual_preds, 1, max))

pl_min_max_accuracy



#Mape

lm_mape <- mean(abs(lm_actual_preds$predicted - lm_actual_preds$actuals) / lm_actual_preds$actuals)

lm_mape



pl_mape <- mean(abs(pl_actual_preds$predicted - pl_actual_preds$actuals) / pl_actual_preds$actuals)

pl_mape



#Summary

summary(lr_model)

summary(pl_model)




---
title: "Appendixes"
output: pdf_document
---

# Appendix A

## Data Exploration

```{r}
library(ggplot2)
library(ggcorrplot)
library(pROC)
data <- read.csv("Envdata.csv")
attach(data)
head(data)
```

```{r}
colnames(data)
```

```{r}
str(data)
```


```{r}
sapply(data, class)
```

```{r}
summary(data)
```

```{r}
# Histogram for 'length_of_stay_minutes'
hist(data$length_of_stay_minutes, breaks = 20, main = "Distribution of Length of Stay")
```

```{r}
# Bar chart for 'triage'
barplot(table(data$triage), main = "Triage Distribution")

# Bar chart for 'gender'
barplot(table(data$gender), main = "Gender Distribution")

```

```{r}
# Cross-tabulation of 'gender' and 'asthma'
cross_table <- table(data$gender, data$asthma)
print(cross_table)

```

# Appendix B

## Supervised Learning : Multiple Linear Regression

### Research Question: What is the combined influence of air quality parameters (CO, O3, NO2, SO2, PPM10, visibility reduction, AQI), weather conditions (precipitation, relative humidity, vapor pressure, windspeed, wind direction, max windspeed) on the length of stay for patients?

```{r}
data_new <- data[, sapply(data, is.numeric)]
str(data_new)
```


```{r}
cor_data_new <- cor(data_new)
ggcorrplot(cor_data_new)
```

```{r}
cor_data_new
```

Drop ppm10 because strong correlation with aqi.

Drop maxwindspeed because strong correlation with windspeed.

```{r}
model1 <- lm(length_of_stay_minutes ~ postcode + co + o3 + no2 + so2 
             + visibility_reduction + aqi + precipitation + relativehumidity 
             + vapourpressure + windspeed + winddirection, data = data)
summary(model1)
```

```{r}
anova(model1)
```

Drop o3

```{r}
model2 <- lm(length_of_stay_minutes ~ postcode + co + no2 + so2 + visibility_reduction 
             + aqi + precipitation + relativehumidity + vapourpressure + windspeed 
             + winddirection, data = data)
summary(model2)
```

```{r}
anova(model2)
```


Drop windspeed

```{r}
model3 <- lm(length_of_stay_minutes ~ postcode + co + no2 + so2 + visibility_reduction 
             + aqi + precipitation + relativehumidity + vapourpressure 
             + winddirection, data = data)
summary(model3)
```

```{r}
anova(model3)
```


Drop so2

```{r}
model4 <- lm(length_of_stay_minutes ~ postcode + co + no2 + visibility_reduction + aqi 
             + precipitation + relativehumidity + vapourpressure + winddirection, data = data)
summary(model4)
```

Drop vapourpressure

```{r}
model5 <- lm(length_of_stay_minutes ~ postcode + co + no2 + visibility_reduction + aqi 
             + precipitation + relativehumidity + winddirection, data = data)
summary(model5)
```

Drop aqi

```{r}
model6 <- lm(length_of_stay_minutes ~ postcode + co + no2 + visibility_reduction 
             + precipitation + relativehumidity + winddirection, data = data)
summary(model6)
```

Drop precipitation

```{r}
model7 <- lm(length_of_stay_minutes ~ postcode + co + no2 + visibility_reduction 
             + relativehumidity + winddirection, data = data)
summary(model7)
```

Drop winddirection

```{r}
model8 <- lm(length_of_stay_minutes ~ postcode + co + no2 + visibility_reduction 
             + relativehumidity, data = data)
summary(model8)
```

Drop relativehumidity

```{r}
model9 <- lm(length_of_stay_minutes ~ postcode + co + no2 + visibility_reduction, data = data)
summary(model9)
```

Drop postcode

```{r}
model10 <- lm(length_of_stay_minutes ~  co + no2 + visibility_reduction, data = data)
summary(model10)
```

```{r}
anova(model10)
```


Drop co

```{r}
model11 <- lm(length_of_stay_minutes ~ no2 + visibility_reduction, data = data)
summary(model11)
```

```{r}
anova(model11)
```

```{r}
qf(0.95,2,105)
```


```{r}
step(lm(length_of_stay_minutes ~ postcode + co + no2 + so2 + visibility_reduction + aqi 
        + precipitation + relativehumidity + vapourpressure + windspeed + winddirection, 
        data = data), direction = "backward")
```


```{r}
plot(predict(model11),resid(model11), xlab = "Fitted Values", ylab = "Residuals")
```

```{r}
hist(resid(model11), main = paste("Histogram of Residuals"), xlab = "Residuals")
```

```{r}
par(mfrow=c(2,2))
plot(model11)
```

# Appendix C

## Supervised Learning : Multiple Logistic Regression

### Research Question: Can we predict the likelihood of asthma attacks based on patient characteristics, environmental factors, and triage information using logistic regression?

```{r}
data$asthma <- as.factor(data$asthma)
str(data)
```

```{r}
log_model1 <- glm(asthma ~ length_of_stay_minutes + postcode + co + o3 + no2 + so2 
                  + visibility_reduction + aqi + precipitation + relativehumidity 
                  + vapourpressure + windspeed + winddirection, family = binomial, data = data)
summary(log_model1)
```

```{r}
roc_curve <- roc(data$asthma, predict(log_model1, type = "response"))
auc(roc_curve)
```


```{r}
log_model2 <- glm(asthma ~ length_of_stay_minutes + postcode + co + o3 + so2 
                  + visibility_reduction + aqi + precipitation + relativehumidity 
                  + vapourpressure + windspeed + winddirection, family = binomial, data = data)
summary(log_model2)
```

```{r}
roc_curve <- roc(data$asthma, predict(log_model2, type = "response"))
auc(roc_curve)
```

```{r}
glm_prob<-predict(log_model2, type="response")
glm_pred<-rep("No",108)
glm_pred[glm_prob>0.5]="Yes"
table(glm_pred,asthma)
```

```{r}
Misclassification_Rate <- (18+11)/(55+18+11+24)
Misclassification_Rate
False_Positive_Rate <- 11/(55+11)
False_Positive_Rate
False_Negative_Rate <- 18/(18+24)
False_Negative_Rate
```

```{r}
log_model3 <- glm(asthma ~ length_of_stay_minutes + postcode + co + o3 + so2 
                  + visibility_reduction + precipitation + relativehumidity 
                  + vapourpressure + windspeed + winddirection, family = binomial, data = data)
summary(log_model3)
```

```{r}
roc_curve <- roc(data$asthma, predict(log_model3, type = "response"))
auc(roc_curve)
```

```{r}
log_model4 <- glm(asthma ~ length_of_stay_minutes + postcode + co + o3 + so2 
                  + visibility_reduction + precipitation + relativehumidity 
                  + windspeed + winddirection, family = binomial, data = data)
summary(log_model4)
```

```{r}
roc_curve <- roc(data$asthma, predict(log_model4, type = "response"))
auc(roc_curve)
```

```{r}
log_model5 <- glm(asthma ~ length_of_stay_minutes + postcode + co + o3 + so2 
                  + visibility_reduction + precipitation + windspeed 
                  + winddirection, family = binomial, data = data)
summary(log_model5)
```

```{r}
roc_curve <- roc(data$asthma, predict(log_model5, type = "response"))
auc(roc_curve)
```

```{r}
log_model6 <- glm(asthma ~ length_of_stay_minutes + postcode + co + o3 + so2 
                  + visibility_reduction + precipitation + winddirection, 
                  family = binomial, data = data)
summary(log_model6)
```

```{r}
roc_curve <- roc(data$asthma, predict(log_model6, type = "response"))
auc(roc_curve)
```

```{r}
log_model7 <- glm(asthma ~ length_of_stay_minutes + postcode + co + o3 + so2 
                  + precipitation + winddirection, family = binomial, data = data)
summary(log_model7)
```

```{r}
roc_curve <- roc(data$asthma, predict(log_model7, type = "response"))
auc(roc_curve)
```

```{r}
log_model8 <- glm(asthma ~ postcode + co + o3 + so2 + precipitation 
                  + winddirection, family = binomial, data = data)
summary(log_model8)
```

```{r}
roc_curve <- roc(data$asthma, predict(log_model8, type = "response"))
auc(roc_curve)
```

```{r}
log_model9 <- glm(asthma ~ co + o3 + so2 + precipitation + winddirection, 
                  family = binomial, data = data)
summary(log_model9)
```

```{r}
roc_curve <- roc(data$asthma, predict(log_model9, type = "response"))
auc(roc_curve)
```

```{r}
list_of_models <- list(log_model1, log_model2, log_model3, log_model4, 
                       log_model5, log_model6, log_model7, log_model8, log_model9)

aic_values <- lapply(list_of_models, function(model) {
  AIC(model)
})

for (i in 1:length(aic_values)) {
  cat("Model", i, "AIC =", aic_values[[i]], "\n")
}

```

```{r}
auc_values <- lapply(list(log_model1, log_model2, log_model3, log_model4, 
                          log_model5, log_model6, log_model7, log_model8, 
                          log_model9), function(model) {
  roc_obj <- roc(response = data$asthma, predictor = predict(model, type = "response"))
  auc(roc_obj)
})

for (i in 1:length(auc_values)) {
  cat("Model", i, "AUC =", auc_values[[i]], "\n")
}

```

```{r}
auc_values <- lapply(list(log_model1, log_model2, log_model3, log_model4, 
                          log_model5, log_model6, log_model7, log_model8, 
                          log_model9), function(model) {
  roc_obj <- roc(response = data$asthma, predictor = predict(model, type = "response"))
  auc(roc_obj)
})

model_names <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", 
                 "Model 7", "Model 8", "Model 9")

best_model_index <- which.max(auc_values)

best_model_name <- model_names[best_model_index]

cat("The best model is:", best_model_name, "\n")
```

```{r}
# Assuming you have fitted a logistic regression model (log_model)
library(pROC)
roc_obj <- roc(response = data$asthma, predictor = predict(log_model2, type = "response"))
plot(roc_obj)

# Assuming you have fitted a logistic regression model (log_model)
plot(residuals(log_model2), type = "p")
```


# Appendix D

## Unsupervised Learning: K-means Clustering

```{r}
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(data_new, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')
```

```{r}
km <- kmeans(data_new, centers = 4, nstart = 20)
km
```

```{r}
pp = prcomp(data_new)
plot(pp$x[,1:2], col=fitted(km, "classes")+2)
```

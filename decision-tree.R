library(rpart)
library(dplyr)
library(caret)
library(rpart.plot)
library(caTools)

cars <- read.csv("C:/Users/User/Desktop/R/ML/cars.csv")

cars<- select(cars,-name)
str(cars)
cars$origin<- factor(cars$origin, labels = c("USA", "Europe", "Japan"))


cars_split<- sample.split(cars$origin, SplitRatio = 0.8)
cars_split
train_data<- subset(cars, cars_split == TRUE)
test_data<- subset(cars, cars_split == FALSE)

model<- rpart(train_data$origin~., train_data)
model
#vis
prp(model)

predict_mod<- predict(model, test_data, type  ="class")
predict_tab<- table(predict_mod, test_data$origin)

confusionMatrix(predict_tab)


library(randomForest)
rf_model<- randomForest(train_data$origin~., train_data)
importance(rf_model)

predict_rf <- predict(rf_model, test_data)

confusionMatrix(table(predict_rf, test_data$origin))

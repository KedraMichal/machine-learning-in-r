library(rpart)
library(dplyr)
library(caret)
library(rpart.plot)

cars <- read.csv("C:/Users/User/Desktop/R/ML/cars.csv")

cars<- select(cars,-name)
str(cars)
cars$origin<- factor(cars$origin, labels = c("USA", "Europe", "Japan"))


cars_split<- sample(2, nrow(cars), prob = c(0.7, 0.3), replace = TRUE)
cars_split
train_data<- cars[cars_split ==1,]
test_data<- cars[cars_split ==2,]

model<- rpart(train_data$origin~., train_data)

#vis
prp(model)

predict_mod<- predict(model, test_data, type  ="class")
predict_tab<- confusionMatrix(table(predict_mod, test_data$origin))

confusionMatrix(predict_tab)

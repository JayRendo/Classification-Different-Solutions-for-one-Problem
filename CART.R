set.seed(1)

#load necessary library for the data and creat data
library(hhcartr)
data <- letters
data <- cbind(data.frame(data$y), data$X)

#Splitting the data into training and testing set
sample <- sample(seq(1,2), size = nrow(data), replace = TRUE, prob = c(0.8,0.2))
data_train <- data[sample == 1, ]
data_test <- data[sample == 2, ]

#load necessary library for training
library(rpart)

fit <- rpart(data.y ~., data = data_train, method = "class")
y_pred <- predict(fit, newdata = data_test[,2:17], type = "class")
count <- 0
for (i in 1:length(y_pred)){
  if(y_pred[i] == data_test[i,1]){
    count <- count + 1
  }
}
count/length(y_pred)

#prediction accuracy on training set
y_pred <- predict(fit, newdata = data_train[,2:17], type = "class")
count <- 0
for (i in 1:length(y_pred)){
  if(y_pred[i] == data_train[i,1]){
    count <- count + 1
  }
}
count/length(y_pred)

library(MASS)
data <- Aids2
#Splitting the data into training and testing set
sample <- sample(seq(1,2), size = nrow(data), replace = TRUE, prob = c(0.8,0.2))
data_train <- data[sample == 1, ]
data_test <- data[sample == 2, ]

fit <- rpart(status~., data = data_train, method = "class")
y_pred <- predict(fit, newdata = data_test[,c(1,2,3,4,6,7)],type = "class")
count <- 0
for (i in 1:length(y_pred)){
  if(y_pred[i] == data_test[i,5]){
    count <- count + 1
  }
}
count/length(y_pred)

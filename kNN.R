#For reproducibility
set.seed(1)
#Importing the data
library(MASS)
data <- cats

#creating train and test sets
sample <- sample(seq(1,2), size = nrow(data), replace = TRUE, prob = c(0.8,0.2))
data_train <- data[sample == 1, ]
data_test <- data[sample == 2, ]

#testing the performance of the algorithm
library(class)
#Here k = 1
y_pred <- knn(data_train[,c(2,3)], data_test[,c(2,3)], data_train$Sex)
count <- 0
for (i in 1:nrow(data_test)) {
  if(y_pred[i] == data_test$Sex[i]){
    count <- count + 1
  }
}
count/nrow(data_test)

#Here k corresponds to using half of the closest data
y_pred <- knn(data_train[,c(2,3)], data_test[,c(2,3)], data_train$Sex, k = 72)
count <- 0
for (i in 1:nrow(data_test)) {
  if(y_pred[i] == data_test$Sex[i]){
    count <- count + 1
  }
}
count/nrow(data_test)

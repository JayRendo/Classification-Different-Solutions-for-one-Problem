set.seed(1)
#Uploading the data
data <- Titanic 
data <- data.frame(data)
data <- data[,1:4]

#Splitting the data into training and testing set
sample <- sample(seq(1,2), size = nrow(data), replace = TRUE, prob = c(0.5,0.5))
data_train <- data[sample == 1, ]
data_test <- data[sample == 2, ]

#Packages we're going to use
library(bnlearn)


tree <- chow.liu(data_train)
plot(tree)
tree = set.arc(tree, from = "Age", to = "Sex")
tree = set.arc(tree, from = "Age", to = "Class")
tree = set.arc(tree, from = "Class", to = "Survived")
plot(tree)
fit = bn.fit(tree,data)
fit

y_pred <- predict(fit, node = "Survived", data_test[,1:3])
count <- 0
for (i in 1:length(y_pred)){
  if(y_pred[i] == data_test$Survived[i]){
    count <- count + 1
  }
}
count/length(y_pred)

## Other example of dataset
#Uploading the data
library(MASS)
data <- Aids2 
data <- data[,c(1,2,5,6)]

#Splitting the data into training and testing set
sample <- sample(seq(1,2), size = nrow(data), replace = TRUE, prob = c(0.5,0.5))
data_train <- data[sample == 1, ]
data_test <- data[sample == 2, ]

tree <- chow.liu(data_train)
plot(tree)
tree = set.arc(tree, from = "T.categ", to = "status")
tree = set.arc(tree, from = "sex", to = "T.categ")
tree = set.arc(tree, from = "state", to = "T.categ")
plot(tree)
fit = bn.fit(tree,data)
fit

y_pred <- predict(fit, node = "status", data_test[,c(1,2,4)])
count <- 0
for (i in 1:length(y_pred)){
  if(y_pred[i] == data_test$status[i]){
    count <- count + 1
  }
}
count/length(y_pred)

#For reproducibility
set.seed(1)

#Loading the data and the necessary libraries
library(titanic)
library(MASS)
data <- titanic_train[,2:12]
data <- data[, c(1,5)] #take out categories that aren't relevant + sex + age
data <- na.omit(data)

summary(data[which(data$Survived == 1),2])
summary(data[which(data$Survived == 0),2])

#Dividing the data into a train and a test set
sample <- sample(seq(1,2), size = nrow(data), replace = TRUE, prob = c(0.8,0.2))
data_train <- data[sample == 1, ]
data_test <- data[sample == 2, ]


#Training the method
model <- lda(Survived ~., data = data_train)

#Figuring out the accuracy on the test model
#The test set is fairly balanced so this is an 
#accurate measure
y_pred <- predict(model, newdata = data_test)$class
count <- 0
for (i in 1:nrow(data_test)){
  if (y_pred[i] == data_test$Survived[i]){
    count = count + 1
  }
}
count/nrow(data_test)

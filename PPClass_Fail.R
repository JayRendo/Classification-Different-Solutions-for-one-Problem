#loading data and necessary libraries
library(PPtreeViz)
data <- cats

#Showing the fact that the dataset is not well
#linearly separated
plot(data[which(data$Sex == "F"),]$Bwt, data[which(data$Sex == "F"),]$Hwt, col = "red", xlab = "Bodyweight", ylab = "Heart weight")
points(data[which(data$Sex == "M"),]$Bwt, data[which(data$Sex == "M"),]$Hwt, col = "blue")
#Female cats are in red, male cats in blue 

#Splitting the data into train and test set
sample <- sample(seq(1,2), size = nrow(data), replace = TRUE, prob = c(0.7,0.3))
data_train <- data[sample == 1, ]
data_test <- data[sample == 2, ]

#Train the Projection Pursuit Classification procedure
Tree.result <- PPTreeclass(Sex~.,data = data_train,"LDA")
#Plot the splits that it makes
plot(Tree.result)

#Predict and compute accuracy:
y_pred <- predict(Tree.result, newdata = data_test[2:3])
count <- 0
for(i in 1:length(y_pred)){
  if(y_pred[i] == data_test[i,1]){
    count <- count + 1
  }
}
count/length(y_pred)

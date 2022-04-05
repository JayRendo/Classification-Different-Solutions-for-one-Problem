#loading data and necessary libraries
library(PPtreeViz)
data <- iris

#Splitting the data into train and test set
sample <- sample(seq(1,2), size = nrow(data), replace = TRUE, prob = c(0.7,0.3))
data_train <- data[sample == 1, ]
data_test <- data[sample == 2, ]

#Train the Projection Pursuit Classification procedure
Tree.result <- PPTreeclass(Species~.,data = data_train,"LDA")
#Plot the splits that it makes
plot(Tree.result)

#Predict and compute accuracy:
y_pred <- predict(Tree.result, newdata = data_test[1:4])
count <- 0
for(i in 1:length(y_pred)){
  if(y_pred[i] == data_test[i,5]){
    count <- count + 1
  }
}
count/length(y_pred)

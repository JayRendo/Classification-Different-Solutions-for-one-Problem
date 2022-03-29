
#For reproducibility
set.seed(1)
#Importing the data

data <- iris
data <- data[,c(1,5)] #only keep one attribute for simplicity
#Dividing data into train and test set
sample <- sample(seq(1,2), size = nrow(data), replace = TRUE, prob = c(0.7,0.3))
data_train <- data[sample == 1, ]
data_test <- data[sample == 2, ]

acc <- rep(-1,100)
bwvec <- seq(from = 0.01, to = 1, by = 0.01)
for(j in 1:100){

#Computing the densities and using them for classification
denshat1 <- density(data[which(data$Species == "setosa"),1], bw = bwvec[j])
densfct1 <- approxfun(denshat1$x,denshat1$y, rule = 2)
denshat2 <- density(data[which(data$Species == "versicolor"),1], bw = bwvec[j])
densfct2 <- approxfun(denshat2$x, denshat2$y, rule = 2)
denshat3 <- density(data[which(data$Species == "virginica"),1], bw = bwvec[j])
densfct3 <- approxfun(denshat3$x,denshat3$y, rule = 2)

#Computing the predicted value and accuracy
count <- 0
for(i in 1:length(data_test[,1]) ){
  y1 <- densfct1(data_test[i,1])
  y2 <- densfct2(data_test[i,1])
  y3 <- densfct3(data_test[i,1])
  if(y1 == max(c(y1,y2,y3))){
    if(data_test[i,2] == "setosa"){
      count <- count + 1
    }
  }else if(y2 == max(c(y1,y2,y3))){
    if(data_test[i,2] == "versicolor"){
      count <- count + 1
    }
  }else if(y3 == max(c(y1,y2,y3))){
    if(data_test[i,2] == "virginica"){
      count <- count + 1
    }
  }
}
acc[j] <- count/length(data_test[,1])
}
plot(bwvec, acc, xlab = "bandwidth", ylab = "accuracy", type = "l")

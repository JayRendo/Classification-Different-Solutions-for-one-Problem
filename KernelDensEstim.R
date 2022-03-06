##STILL NEED TO FINISH THIS
#For reproducibility
set.seed(1)
#Importing the data
library(PPCI)
data <- optidigits
data <- data[c(1,2)]
data <- data.frame(data)
data$c <- as.factor(data$c)
#Dividing data into train and test set
sample <- sample(seq(1,2), size = nrow(data), replace = TRUE, prob = c(0.9,0.1))
data_train <- data[sample == 1, ]
data_test <- data[sample == 2, ]

#Computing the densities and using them for classification
library(np)
library(MASS)
list_dens = rep(NA, 10)
for (i in 1:10){
  bw <- bandwidth.nrd(data_train[which(data_train$c == i-1),1:64])
  list_dens[i] <- npudens(bw)
}


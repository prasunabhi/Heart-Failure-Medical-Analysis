# Perceptrons

str(HeartFailure)

HeartFailure$DEATH_EVENT <- lapply(HeartFailure$DEATH_EVENT, function(x) {
  if(x == '0')
    HeartFailure$DEATH_EVENT <- -1 # Alive
  else if(x == '1')
    HeartFailure$DEATH_EVENT <- 1 # Dead
  else
    HeartFailure$DEATH_EVENT <- NULL
})

#Converting Death Event Variable to Factor
HeartFailure$DEATH_EVENT <- as.factor(HeartFailure$DEATH_EVENT)

#Creating training and testing data
HeartFailure_index <- sample(nrow(HeartFailure), 0.7*nrow(HeartFailure))
HeartFailure_train <- HeartFailure[HeartFailure_index, ]
HeartFailure_test <- HeartFailure[-HeartFailure_index, ]

#Creating the Perceptron Learning Algorithm
perceptron <- function(X, y, numEpochs) {
  results <- list()
  w <- runif(ncol(X), -10, 10) #Initalize weights
  
  # For loop - number of generations(epochs) - number of times dataset is ran through
  for(j in 1:numEpochs) {
    predictedResult <- numeric(length=100) # Initalize predictedResult vector
    numIncorrect = 0 # Keeps track of # of missclassified points
    
    # For loop - loop throught dataset
    for(i in 1:length(y)) {
      xi = as.numeric(unlist(X[i,])) # Convert dataframe to vector
      predictedResult[i] = sign(w %*% xi) # Predict the point
      
      # If predicted point is incorrect - change weight
      if(predictedResult[i] != y[i]) {
        numIncorrect = numIncorrect + 1 # Add one to # of missclassified points
        w <- w + as.numeric(y[i]) * xi # Update the weight w <- w + WiXi
      }
    }
    # Print results of this generation(epoch)
    cat("\nEpoch #: ", j)
    cat("\nNumber Incorrect: ", numIncorrect)
    cat("\nFinal Weight: ", w)
  }
}

set.seed(7)
#Creating first Perceptron model
X1 <- HeartFailure_train[, c("age","serum_sodium")]
Y1 <- HeartFailure_train$DEATH_EVENT
perceptron(X1,Y1,8)

#Take the weight from last Epoch
weight = c(450.7802, -233.0451)

#Multiply weights to age and serum sodium
HeartFailure_test$Wage <- HeartFailure_test$age*weight[1]
HeartFailure_test$Wserum_sodium <- HeartFailure_test$serum_sodium*weight[2]

#Add all weights and call it predict
HeartFailure_test$predict <- rowSums(HeartFailure_test[,c("Wage","Wserum_sodium")])

#Use confusion matrix to evaluate the model performance
perceptron1predict <- table(HeartFailure_test$DEATH_EVENT == 1, HeartFailure_test$predict > 0) + 
  table(HeartFailure_test$DEATH_EVENT == -1, HeartFailure_test$predict < 0)

perceptron1predict

#Get the accuracy rate
sum(diag(perceptron1predict))/sum(perceptron1predict)
# The accuracy is 58%

#Creating Second Perceptron model
X2 <- HeartFailure_train[, c("age","time")]
Y2 <- HeartFailure_train$DEATH_EVENT
perceptron(X2,Y2,8)

#Take the weight from last Epoch
weight = c(33.31596, -238.605)

#Multiply weights to age and time
HeartFailure_test$Wage <- HeartFailure_test$age*weight[1]
HeartFailure_test$Wtime <- HeartFailure_test$time*weight[2]

#Add all weights and call it predict
HeartFailure_test$predict2 <- rowSums(HeartFailure_test[,c("Wage","Wtime")])

#Use confusion matrix to evaluate the model performance
perceptron2predict <- table(HeartFailure_test$DEATH_EVENT == 1, HeartFailure_test$predict2 > 0) + 
  table(HeartFailure_test$DEATH_EVENT == -1, HeartFailure_test$predict2 <0)

perceptron2predict

#Get the accuracy rate
sum(diag(perceptron2predict))/sum(perceptron2predict)
# The accuracy is 75%

#Creating third Perceptron model
X3 <- HeartFailure_train[, c("age","ejection_fraction")]
Y3 <- HeartFailure_train$DEATH_EVENT
perceptron(X3,Y3,8)

#Take the weight from last Epoch
weight = c(53.54299, -124.1598)

#Multiply weights to age and ejection fraction
HeartFailure_test$Wage <- HeartFailure_test$age*weight[1]
HeartFailure_test$Wejection_fraction <- HeartFailure_test$ejection_fraction*weight[2]

#Add all weights and call it predict
HeartFailure_test$predict3 <- rowSums(HeartFailure_test[,c("Wage","Wejection_fraction")])

#Use confusion matrix to evaluate the model performance
perceptron3predict <- table(HeartFailure_test$DEATH_EVENT == 1, HeartFailure_test$predict3 > 0) + 
  table(HeartFailure_test$DEATH_EVENT == -1, HeartFailure_test$predict3 < 0)

perceptron3predict

#Get the accuracy rate
sum(diag(perceptron3predict))/sum(perceptron3predict)
# The accuracy is 67.8%

#Creating fourth Perceptron model
X4 <- HeartFailure_train[, c("age","platelets")]
Y4 <- HeartFailure_train$DEATH_EVENT
perceptron(X4,Y4,8)

#Take the weight from last Epoch
weight = c(947.8022, -177854.8)

#Multiply weights to age and platelets
HeartFailure_test$Wage <- HeartFailure_test$age*weight[1]
HeartFailure_test$Wplatelets <- HeartFailure_test$platelets*weight[2]

#Add all weights and call it predict
HeartFailure_test$predict4 <- rowSums(HeartFailure_test[,c("Wage","Wplatelets")])

#Use confusion matrix to evaluate the model performance
perceptron4predict <- table(HeartFailure_test$DEATH_EVENT == 1, HeartFailure_test$predict4 > 0) + 
  table(HeartFailure_test$DEATH_EVENT == -1, HeartFailure_test$predict4 < 0)

perceptron4predict

#Get the accuracy rate
sum(diag(perceptron4predict))/sum(perceptron4predict)

#Creating fifth Perceptron model
X5 <- HeartFailure_train[, c("age","ejection_fraction","time")]
Y5 <- HeartFailure_train$DEATH_EVENT
perceptron(X5,Y5,24)

#Take the weight from last Epoch
weight = c(325.214, -586.1598, -201.1988)

#Multiply weights to age, ejection_fraction and time
HeartFailure_test$Wage <- HeartFailure_test$age*weight[1]
HeartFailure_test$Wejection_fraction <- HeartFailure_test$ejection_fraction*weight[2]
HeartFailure_test$Wtime <- HeartFailure_test$time*weight[3]

#Add all weights and call it predict
HeartFailure_test$predict5 <- rowSums(HeartFailure_test[,c("Wage","Wejection_fraction","time")])

#Use confusion matrix to evaluate the model performance
perceptron5predict <- table(HeartFailure_test$DEATH_EVENT == 1, HeartFailure_test$predict5 > 0) + 
  table(HeartFailure_test$DEATH_EVENT == -1, HeartFailure_test$predict5 < 0)

perceptron5predict

#Get the accuracy rate
sum(diag(perceptron5predict))/sum(perceptron5predict)

#Creating sixth Perceptron model
X6 <- HeartFailure_train[, c("age","ejection_fraction","platelets","time","creatinine_phosphokinase")]
Y6 <- HeartFailure_train$DEATH_EVENT
perceptron(X6,Y6,8)

#Take the weight from last Epoch
weight = c(462.2172, -813.6016, 28075.59, -9399.191, 12530.83)

#Multiply weights to age, ejection_fraction, platelets, time and creatinine_phosphokinase
HeartFailure_test$Wage <- HeartFailure_test$age*weight[1]
HeartFailure_test$Wejection_fraction <- HeartFailure_test$ejection_fraction*weight[2]
HeartFailure_test$Wplatelets <- HeartFailure_test$platelets*weight[3]
HeartFailure_test$Wtime <- HeartFailure_test$time*weight[4]
HeartFailure_test$Wcreatinine_phosphokinase <- HeartFailure_test$creatinine_phosphokinase*weight[5]

#Add all weights and call it predict
HeartFailure_test$predict6 <- rowSums(HeartFailure_test[,c("age","ejection_fraction","platelets","time","creatinine_phosphokinase")])

#Use confusion matrix to evaluate the model performance
perceptron6predict <- table(HeartFailure_test$DEATH_EVENT == 1, HeartFailure_test$predict6 > 0) + 
  table(HeartFailure_test$DEATH_EVENT == -1, HeartFailure_test$predict6 < 0)

perceptron6predict

#Get the accuracy rate
sum(diag(perceptron6predict))/sum(perceptron6predict)


#Creating sixth Perceptron model
X6 <- HeartFailure_train[, c("age","ejection_fraction","platelets","time")]
Y6 <- HeartFailure_train$DEATH_EVENT
perceptron(X6,Y6,8)

#Take the weight from last Epoch
weight = c(259.1083, -234.6829, -12458.85, -4192.565)

#Multiply weights to age, ejection_fraction, platelets, time and creatinine_phosphokinase
HeartFailure_test$Wage <- HeartFailure_test$age*weight[1]
HeartFailure_test$Wejection_fraction <- HeartFailure_test$ejection_fraction*weight[2]
HeartFailure_test$Wplatelets <- HeartFailure_test$platelets*weight[3]
HeartFailure_test$Wtime <- HeartFailure_test$time*weight[4]

#Add all weights and call it predict
HeartFailure_test$predict6 <- rowSums(HeartFailure_test[,c("age","ejection_fraction","platelets","time")])

#Use confusion matrix to evaluate the model performance
perceptron6predict <- table(HeartFailure_test$DEATH_EVENT == 1, HeartFailure_test$predict6 > 0) + 
  table(HeartFailure_test$DEATH_EVENT == -1, HeartFailure_test$predict6 < 0)

perceptron6predict

#Get the accuracy rate
sum(diag(perceptron6predict))/sum(perceptron6predict)

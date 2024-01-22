#Final version of assignment1 - MLB

#Converting Death Event Variable to Factor
HeartFailure$DEATH_EVENT <- as.factor(HeartFailure$DEATH_EVENT)

# Using ifelse to create a new variable
# HeartFailure_test$DEATH_EVENT <- ifelse(HeartFailure_test$DEATH_EVENT == 1, "Dead", "Live")

#Viewing the structure of the dataset
str(HeartFailure)

#Visualizing therelationship of Age and Platelets with Death Event
ggplot(HeartFailure, aes(x = age, y = platelets)) +
  geom_point(aes(colour = DEATH_EVENT, shape= DEATH_EVENT), size = 3) +
  xlab("Age") +
  ylab("Platelets") +
  ggtitle("Death Event vs Age and Platelets") +
  theme(plot.title = element_text(hjust = 0.5))

#Visualizing therelationship of Age and Time(Follow-up) with Death Event
ggplot(HeartFailure, aes(x = time, y = age)) +
  geom_point(aes(colour = DEATH_EVENT, shape= DEATH_EVENT), size = 3) +
  xlab("Time(Follow-up)") +
  ylab("Age") +
  ggtitle("Death Event vs Age and Time(Follow-up)") +
  theme(plot.title = element_text(hjust = 0.5))

#Visualizing therelationship of Time(Follow-up) and Platelets with Death Event
ggplot(HeartFailure, aes(x = time, y = platelets)) +
  geom_point(aes(colour = DEATH_EVENT, shape= DEATH_EVENT), size = 3) +
  xlab("Time(Follow-up)") +
  ylab("Platelets") +
  ggtitle("Death Event vs Time(Follow-up) and Platelets") +
  theme(plot.title = element_text(hjust = 0.5))

#Visualizing therelationship of Age and Ejection fraction with Death Event
ggplot(HeartFailure, aes(x = age, y = ejection_fraction)) +
  geom_point(aes(colour = DEATH_EVENT, shape= DEATH_EVENT), size = 3) +
  xlab("Age") +
  ylab("Ejection fraction") +
  ggtitle("Death Event vs Age and Ejection fraction") +
  theme(plot.title = element_text(hjust = 0.5))

# Visualizing the relationship of Age and Serum Creatinine with Death Event
ggplot(HeartFailure, aes(x = age, y = serum_creatinine)) +
  geom_point(aes(colour = DEATH_EVENT, shape= DEATH_EVENT), size = 3) +
  xlab("Age") +
  ylab("Serum Creatinine") +
  ggtitle("Death Event vs Age and Serum Creatinine") +
  theme(plot.title = element_text(hjust = 0.5))

# Visualizing the  relationship of Serum Sodium and Serum Creatinine with Death Event
ggplot(HeartFailure, aes(x = serum_sodium, y = serum_creatinine)) +
  geom_point(aes(colour = DEATH_EVENT, shape= DEATH_EVENT), size = 3) +
  xlab("Serum Sodium") + # Corrected x-axis label
  ylab("Serum Creatinine") + # Corrected y-axis label
  ggtitle("Serum Sodium vs Serum Creatinine") +
  theme(plot.title = element_text(hjust = 0.5))

# Creating training and testing data
HeartFailure_index <- sample(nrow(HeartFailure), 0.7*nrow(HeartFailure))
HeartFailure_train <- HeartFailure[HeartFailure_index, ]
HeartFailure_test <- HeartFailure[-HeartFailure_index, ]

# Perceptrons

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




# SVM models

# Q.3A
#Creating First SVM Model with Age, Serum Sodium as x variables
svm_model1 <- svm(DEATH_EVENT ~ age + serum_sodium, data = HeartFailure_train)

#Printing the model and it's summary
print(svm_model1)
summary(svm_model1)

#Plot the model
d1 <- HeartFailure[,c("DEATH_EVENT","age","serum_sodium")]
plot(svm_model1, d1)

#Predict the model
heartpredict1 <- predict(svm_model1,HeartFailure_test)
predicttable1 <- table(heartpredict1, HeartFailure_test$DEATH_EVENT)
predicttable1

#Calculate the accuracy
Accuracy1 <- sum(diag(predicttable1))/sum(predicttable1)

# Format the accuracy as a percentage with two decimal places
accuracy_formatted1 <- sprintf("Accuracy of the SVM model: %.2f%%", Accuracy1 * 100)

# Displaying the accuracy of svm model1
#cat(formatted_accuracy, "\n")
print(accuracy_formatted1)

#Creating Second SVM Model with Age, Time as x variables
svm_model2 <- svm(DEATH_EVENT ~ age + time, data = HeartFailure)

#Printing the model and it's summary
print(svm_model2)
summary(svm_model2)

#Plot the model
d2 <- HeartFailure[,c("DEATH_EVENT","age", "time")]
plot(svm_model2, d2)

#Predict the model
heartpredict2 <- predict(svm_model2,HeartFailure_test)
predicttable2 <- table(heartpredict2, HeartFailure_test$DEATH_EVENT)
predicttable2

#Calculate the accuracy
Accuracy2 <- sum(diag(predicttable2))/sum(predicttable2)

# Format the accuracy as a percentage with two decimal places
accuracy_formatted2 <- sprintf("Accuracy of the SVM model: %.2f%%", Accuracy2 * 100)

# Displaying the accuracy of svm model1
#cat(formatted_accuracy, "\n")
print(accuracy_formatted2)

#Creating Third SVM Model with Age, ejection fraction as x variables
svm_model3 <- svm(DEATH_EVENT ~ age + ejection_fraction, data = HeartFailure)

#Printing the model and it's summary
print(svm_model3)
summary(svm_model3)

#Plot the model
d3 <- HeartFailure[,c("DEATH_EVENT","age", "ejection_fraction")]
plot(svm_model3, d3)

#Predict the model
heartpredict3 <- predict(svm_model3,HeartFailure_test)
predicttable3 <- table(heartpredict3, HeartFailure_test$DEATH_EVENT)
predicttable3

#Calculate the accuracy
Accuracy3 <- sum(diag(predicttable3))/sum(predicttable3)

# Format the accuracy as a percentage with two decimal places
accuracy_formatted3 <- sprintf("Accuracy of the SVM model: %.2f%%", Accuracy3 * 100)

# Displaying the accuracy of svm model1
#cat(formatted_accuracy, "\n")
print(accuracy_formatted3)

#Creating fourth SVM Model with Age, platelets as x variables
svm_model4 <- svm(DEATH_EVENT ~ age + platelets, data = HeartFailure_train)

#Printing the model and it's summary
print(svm_model4)
summary(svm_model4)

#Plot the model
d4 <- HeartFailure[,c("DEATH_EVENT","age","platelets")]
plot(svm_model4, d4)

#Predict the model
heartpredict4 <- predict(svm_model4,HeartFailure_test)
predicttable4 <- table(heartpredict4, HeartFailure_test$DEATH_EVENT)
predicttable4

#Calculate the accuracy
Accuracy4 <- sum(diag(predicttable4))/sum(predicttable4)

# Format the accuracy as a percentage with two decimal places
accuracy_formatted4 <- sprintf("Accuracy of the SVM model: %.2f%%", Accuracy4 * 100)

# Displaying the accuracy of svm model1
#cat(formatted_accuracy, "\n")
print(accuracy_formatted4)

# Q.3B

#Creating first SVM Model with Age, ejection fraction, time as x variables
svm_model5 <- svm(DEATH_EVENT ~ age + ejection_fraction + time, data = HeartFailure)

#Printing the model and it's summary
print(svm_model5)
summary(svm_model5)

#Plot the model
plot(svm_model5, HeartFailure_train, ejection_fraction ~ time, slice = list(age=50, diabetes=1))

#Predict the model
heartpredict5 <- predict(svm_model5,HeartFailure_test)
predicttable5 <- table(heartpredict5, HeartFailure_test$DEATH_EVENT)
predicttable5

#Calculate the accuracy
Accuracy5 <- sum(diag(predicttable5))/sum(predicttable5)

# Format the accuracy as a percentage with two decimal places
accuracy_formatted5 <- sprintf("Accuracy of the SVM model: %.2f%%", Accuracy5 * 100)

# Displaying the accuracy of svm model1
#cat(formatted_accuracy, "\n")
print(accuracy_formatted5)

#Creating second SVM Model with Age, ejection fraction, platelets, time, creatinine phosphokinase as x variables
svm_model6 <- svm(DEATH_EVENT ~ age + ejection_fraction + platelets + time + creatinine_phosphokinase, data = HeartFailure_train)

#Printing the model and it's summary
print(svm_model6)
summary(svm_model6)

#Plot the model
plot(svm_model6, HeartFailure_train, creatinine_phosphokinase ~ ejection_fraction, slice = list(age=50, diabetes=1))

plot(svm_model6, HeartFailure_train, platelets ~ time, slice = list(age=50, diabetes=1))

#Predict the model
heartpredict6 <- predict(svm_model6,HeartFailure_test)
predicttable6 <- table(heartpredict6, HeartFailure_test$DEATH_EVENT)
predicttable6

#Calculate the accuracy
Accuracy6 <- sum(diag(predicttable6))/sum(predicttable6)

# Format the accuracy as a percentage with two decimal places
accuracy_formatted6 <- sprintf("Accuracy of the SVM model: %.2f%%", Accuracy6 * 100)

# Displaying the accuracy of svm model1
#cat(formatted_accuracy, "\n")
print(accuracy_formatted6)

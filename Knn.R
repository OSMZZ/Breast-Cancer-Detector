#-----------------------------ASSINGMENT----------------------------

#a)
data = read.csv("C:\\Users\\OSM\\Desktop\\project R\\Breast-Cancer-Prediction-master\\Breast Cancer Wisconsin.csv")
dim(data)  #dimension
str(data)  #data type of each variable


#b)Splitting the data

set.seed(123)
test_size = sample(nrow(data), 100)

data_test = data[test_size,]

data_train = data[-test_size, ]


train_features = data_train[, 3:32]
test_features = data_test[, 3:32]

#c) 

library(class)
#scaling the data


knn_model = knn(train_features, test_features, cl = data_train$diagnosis, k = 21)
knn_model

confusion_matrix = table(actual = data_test$diagnosis,predict =  knn_model)

#Accuracy = (TP+TN)/(TP+TN_FP+FN)
accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy

#d) summmarize and note the difference

summary(data$radius_mean)
summary(data$area_mean)
summary(data$smoothness_mean)
        #The units of measurement are too vast

#e) min-max normalization

#FORMULA: (x - min of x)/ (max of x - min of x)
normalized_data = function(data) {
  
  scaled_data = data
  
  for (i in 3:ncol(data)){   #iterating over the columns (there are 30 columns)
    x_min = min(data[,i])  #all the rows in each column
    x_max = max(data[,i])
  
  if (x_max - x_min != 0){
    scaled_data[,i] = (data[,i] - x_min)/(x_max-x_min)
  }
  else {
    scaled_data[,i] = 0
  }
    
    return (scaled_data)
  }
  
}

normalized_data =normalized_data(data)

#f) splitting into train and test

normal_train = normalized_data[1:469, ]

normal_test  = normalized_data[470:569, ]

normal_train_features = normal_train[, 3:32]
normal_test_features = normal_test[, 3:32]

#g) build the model on normalized data

knn_model_normalized = knn(normal_train_features, normal_test_features, cl = normal_train$diagnosis, k = 21)
knn_model_normalized

confusion_matrix = table(actual = normal_test$diagnosis,predict =  knn_model_normalized)

#Accuracy = (TP+TN)/(TP+TN_FP+FN)
accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy



#h) scaling the data according to z-score standardization

train_scale = scale(data_train[,3:32])
test_scale = scale(data_test[, 3:32])

knn_model_scaled = knn(train_scale, test_scale, cl = data_train$diagnosis, k = 21)
knn_model_scaled

confusion_matrix = table(actual = data_test$diagnosis,predict =  knn_model_scaled)

#Accuracy = (TP+TN)/(TP+TN_FP+FN)
accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy


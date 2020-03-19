library(tidyverse)
library(randomForest)
library(rpart.plot)

data <- read_csv("train.csv")
predict_data <- read_csv("test.csv")

# dealing with missing data
data[is.na(data$Age),]$Age <- round(mean(data$Age, na.rm = TRUE))
data[is.na(data$Embarked),]$Embarked <- 'S' 

str(data)

# Processing the structure of data
data_m <- data[,-(1)]
data_m <- data_m[,-(3)]
data_m <- data_m[,-(7)]
data_m <- data_m[,-(8)]
colnames(data_m)

data_m$Survived <- as.factor(data_m$Survived)
data_m$Pclass <- as.factor(data_m$Pclass)
data_m$Sex <- as.factor(data_m$Sex)
data_m$SibSp <- as.factor(data_m$SibSp)
data_m$Embarked <- as.factor(data_m$Embarked)

# seperating into train and test
sample_rows <- sample(nrow(data), 0.75 * nrow(data))
train <- data_m[sample_rows, ]
test <- data_m[-sample_rows, ]

TrueValues <- test[,1]
test <- test[,-(1)]

# Modeling
model <- rpart(Survived~., data = train)
rpart.plot(model)

res <- predict(model,newdata = test,type = "class")
res <- as.data.frame(res)

# Accuracy 
mean(res==TrueValues)

predict_data_m <- predict_data[,-(1)]
predict_data_m <- predict_data_m[,-(2)]
predict_data_m <- predict_data_m[,-(8)]
predict_data_m <- predict_data_m[,-(6)]

colnames(predict_data_m)
colnames(data_m)

predict_data_m$Pclass <- as.factor(predict_data_m$Pclass)
predict_data_m$Sex <- as.factor(predict_data_m$Sex)
predict_data_m$SibSp <- as.factor(predict_data_m$SibSp)
predict_data_m$Embarked <- as.factor(predict_data_m$Embarked)

sol <- predict(model,newdata = predict_data_m,type = "class")
final <- data.frame(predict_data$PassengerId, sol)
write.csv(final, file = 'titanic.csv', row.names = F)

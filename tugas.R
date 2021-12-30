#import data set
myData <- read.csv(file = "Customer_Behaviour.csv", header = TRUE)

#melihat data dari atas
head(myData)

#untuk melihat summary data
summary(myData)

behaviour <- myData[,c(3,4)]
behaviour.class <- myData[,"Age"]

#ploting sebaran data
plot(behaviour, pch=16)


#melihat sebaran data
pairs(myData[, 3:5])

#mengubah data menjadi factor
myData$Gender<-factor(myData$Gender)
myData$Purchased<-factor(myData$Purchased)

#cleaning data : na.omit R function menghapus data yang tidak lengkap
myData = na.omit(myData)
rownames(myData) <- 1:nrow(myData)

#mengubah age menjadi factor dengan ketrangan  <=25 adalah muda, 25 - 45 adalah dewasa, >45 adalah tua
myData$Age[myData$Age <= 25] = "muda"
myData$Age[(myData$Age > 25 & myData$Age <=45)] = "dewasa"
myData$Age[(myData$Age != "muda") & (myData$Age != "dewasa")] = "tua"
myData$Age<-factor(myData$Age)

#mengubah Estimated Salary menjadi factor kemudian dimasukkan dalam 3 kelas penghasilan
myData$EstimatedSalary[myData$EstimatedSalary <= 50000] = "rendah"
myData$EstimatedSalary[(myData$EstimatedSalary > 50000 & myData$EstimatedSalary <=90000)] = "menengah"
myData$EstimatedSalary[(myData$EstimatedSalary != "rendah") & (myData$EstimatedSalary != "menengah")] = "tinggi"
myData$EstimatedSalary<-factor(myData$EstimatedSalary)

#Plotting histogram 2 variabel input:
barplot(table(myData$Purchased), xlab="Purchased", ylab="Frequency", main="Histogram of Purchased")
barplot(table(myData$EstimatedSalary), xlab="Estimated Salary", ylab="Frequency", main="Histogram of Estimated Salary")
barplot(table(myData$Age), xlab="Age", ylab="Frequency", main="Histogram of Age")


#membuat train set dan test set dengan menggunakan 70% sample
library(caret)
set.seed(1)
trainIndex <- createDataPartition(myData$Purchased, p = 0.7)$Resample1
train <- myData[trainIndex, ]
test <- myData[-trainIndex, ]
dim(train)
dim(test)

print(table(train$Purchased))
print(table(test$Purchased))

#memodelkan dengan naive bayes
library(e1071)
modelNaive1 <- naiveBayes(Purchased ~ Age + EstimatedSalary , data = train)
print(modelNaive1)

#aplikasi model ke test set
modelNaive1Pred <- predict(modelNaive1, test)

#gunakan confusion matrix untuk evaluasi
confusionMatrix(test$Purchased, modelNaive1Pred)











# import library
if(!require(caret)){install.packages("caret")} 
if(!require(e1071)){install.packages("e1071")}
if(!require(ROCR)){install.packages("ROCR")}
if(!require(profvis)){install.packages("profvis")}
if(!require(mice)){install.packages("mice")}
if(!require(pROC)){install.packages("pROC")}
if(!require(rpart)){install.packages("rpart")}
if(!require(rpart.plot)){install.packages("rpart.plot")}
if(!require(randomForest)){install.packages("randomForest")}

library(caret) 
library(e1071)
library(ROCR)
library(profvis)
library(mice)
library(pROC)
library(rpart)
require(rpart.plot) 
library(randomForest)

# read data
data_raw<-read.csv("G:/E_Business/Homework/Mid_Project/Chile.csv")
head(data_raw)
data_raw=subset(data_raw,select=-c(1) )

# NA value imputation
data_removed<-data_raw[complete.cases(data_raw$vote),]
data_imputation<-mice(data_removed,m=1,maxit=50,method="cart",seed=87)
data1<-complete(data_imputation,1)
data1

# Split the dataset 
index=createDataPartition(data1$vote,p=0.8,list = F)
train=data1[index,]
validation=data1[-index,]
dim(train)
dim(validation)
names(train) 
head(train); 
head(validation)
levels(train$vote)<-make.names(levels(factor(train$vote)))
levels(validation$vote)<-make.names(levels(factor(validation$vote)))
 


# Build Model
x=trainControl(method="repeatedcv",number=10,repeats=3,classProbs=TRUE)
model_KNN<-train(vote~. ,data=train,method="knn",preProcess=c("center","scale"),trControl=x,tuneLength=10)
model_NaiveBayes<-naiveBayes(vote ~ .,data=train)
model_CartTree<-rpart(vote ~. , data=train,cp=2e-3)
model_RandomForest<-randomForest(vote ~ .,data=train)
model_SVM = svm(formula=vote ~ .,data=train)

model_KNN
model_NaiveBayes
model_CartTree
model_RandomForest
model_SVM

prp(model_CartTree,faclen=0,fallen.leaves=TRUE,shadow.col="gray",extra=2.)  

# Prediction
KNN_pred<-predict(model_KNN,validation)
NaiveBayes_pred<-predict(model_NaiveBayes,validation)
CartTree_pred<-predict(model_CartTree,newdata=validation,type="class")
RandomForest_pred<-predict(model_RandomForest,validation)
SVM_pred=predict(model_SVM,validation)


# Accuracy & Confusion Matrix
matrix<-confus.matrix<-table(real=validation$vote,predict=KNN_pred)
roc_obj <- multiclass.roc(as.numeric(validation$vote),as.numeric(KNN_pred))
accuracy<-sum(diag(matrix))/sum(confus.matrix)
print("KNN accuracy")
print(accuracy)
print(auc(roc_obj))
print(matrix)

matrix<-table(real=validation$vote,predict=NaiveBayes_pred)
roc_obj <- multiclass.roc(as.numeric(validation$vote),as.numeric(NaiveBayes_pred))
accuracy<-sum(diag(matrix))/sum(matrix)
print("NaiveBayes accuracy")
print(accuracy)
print(auc(roc_obj))
print(matrix)

matrix<-table(real=validation$vote,predict=CartTree_pred)
roc_obj <- multiclass.roc(as.numeric(validation$vote),as.numeric(CartTree_pred))
accuracy<-sum(diag(matrix))/sum(matrix)
print("Decision Tree accuracy")
print(accuracy)
print(auc(roc_obj))
print(matrix)

matrix<-table(real=validation$vote,predict=RandomForest_pred)
roc_obj <- multiclass.roc(as.numeric(validation$vote),as.numeric(RandomForest_pred))
accuracy<-sum(diag(matrix))/sum(matrix)
print("Rondom Forest accuracy")
print(accuracy)
print(matrix)

matrix<-table(real=validation$vote,predict=SVM_pred)
roc_obj <- multiclass.roc(as.numeric(validation$vote),as.numeric(SVM_pred))
accuracy<-sum(diag(matrix))/sum(matrix)
print("SVM accuracy")
print(accuracy)
print(auc(roc_obj))
print(matrix)



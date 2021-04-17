
# import library
if(!require(caret)){install.packages("caret")} 
if(!require(e1071)){install.packages("e1071")}
if(!require(ROCR)){install.packages("ROCR")}
if(!require(profvis)){install.packages("profvis")}
if(!require(mice)){install.packages("mice")}
if(!require(pROC)){install.packages("pROC")}


library(caret) 
library(e1071)
library(ROCR)
library(profvis)
library(mice)
library(pROC)

# read data
data_raw<-read.csv("G:/E_Business/Homework/Classification/Chile.csv")
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
 
x=trainControl(method="repeatedcv",number=10,repeats=3,classProbs=TRUE)
model1<-train(vote~. ,data=train,method="knn",preProcess=c("center","scale"),trControl=x,metric="ROC",tuneLength=10)
model1
plot(model1)

# predict
valid_pred<-predict(model1,validation)
valid_pred

# confusion matrix
confusion_matrix<-confusionMatrix(valid_pred,validation$vote)
print(confusion_matrix$table)
print(confusion_matrix$overall)

# ROC curve
roc.multi <-multiclass.roc(as.numeric(valid_pred),as.numeric(validation$vote),curve=TRUE)
rs <- roc.multi[['rocs']]
roc.multi$auc

plot.roc(rs[[1]],col='black')
plot.roc(rs[[2]],add=TRUE,col="goldenrod")
plot.roc(rs[[3]],add=TRUE,col="lightsteelblue")
plot.roc(rs[[4]],add=TRUE,col="salmon")
plot.roc(rs[[5]],add=TRUE,col="purple")
plot.roc(rs[[6]],add=TRUE,col="green")

legend("right",
       legend=c("A&N","A&U","A&Y","N&U","N&Y","U&Y"),
       col=c("black", "goldenrod", "lightsteelblue","salmon","purple","green"),
       lwd=4,cex =0.8, xpd = TRUE, horiz=FALSE)


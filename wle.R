#setwd("C:\\Users\\Timur\\Documents\\GitHub\\coursera-practicalML")

library (caret)
library (kernlab)
library (e1071)
pml_training = read.csv("pml-training.csv", header = T, na.string=c("","NA"));
pml_testing = read.csv("pml-testing.csv", header = T, na.string=c("","NA"));

pml_training = pml_training[ , ! apply( pml_training , 2 , function(x) any(is.na(x)) ) ]
pml_testing = pml_testing[ , ! apply( pml_testing , 2 , function(x) any(is.na(x)) ) ]

nzv = nearZeroVar(pml_training,saveMetrics = TRUE)
pml_training = pml_training[,8:60]
pml_testing = pml_testing[,8:59]

col_names = names(pml_training)
for (col_name in names(pml_training))
{
  jpeg(paste0(col_name, ".jpeg"))
  hist(training[,col_name])
  dev.off() 
}

inTrain = createDataPartition(y=pml_training$classe, p=0.75, list=FALSE)
training = pml_training[inTrain,]
testing = pml_training[-inTrain,]

M = abs(cor(training[,-53]))
diag(M) = 0
which(M>0.9, arr.ind = TRUE)

prComp = prcomp(training[,-53], scale=TRUE)
plot(prComp$x[,1], prComp$x[,2], col=training$classe)


preProc = preProcess(training[,-53], method = "pca", pcaComp=20)
A = as.matrix(training[,-53])  %*% preProc$rotation
A = (A-mean(A))/sd(A)
plot(A[,c(1,2)], col=training$classe)

preProc = preProcess(training[,-53], method = "pca", pcaComp=20)
training_preProcPCA = predict(preProc, training[,-53])
plot(training_preProcPCA[,1], training_preProcPCA[,2], col=training$classe)
training_preProcPCA[,"classe"] = training$classe

testing_PrepPCA = predict(preProc, testing[,-53])
plot(testing_PrepPCA[,1], testing_PrepPCA[,2], col=testing$classe)
testing_PrepPCA[,"classe"] = testing$classe


modelInfo = getModelInfo(model="nnet")
modelInfo$nnet$grid

modelFit = lssvm(classe ~., data=training, preProcess = "pca", kernel="rbfdot")
modelFit = lssvm(classe ~., data=training_preProcPCA, kernel="rbfdot")

trainControl = trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = T)
grid = expand.grid(mtry=27)
modelFit = train(classe ~., data=training, method="rf",preProcess=c("scale","center"), trControl = trainControl)
predictions = predict(modelFit, newdata = testing)
confusionMatrix(predictions, testing$classe)
sum(predictions==rf_predictions)/length(rf_predictions)
save(modelFit, file="rf_repeatedcv")



pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
predictions = predict(modelFit, newdata = pml_testing)
pml_write_files(predictions)


getwd()
setwd("C:\\Users\\Administrator\\Desktop\\Rlessons")
sonar = read.csv('sonar.csv',header=FALSE)
head(sonar)
unique(sonar[,61])
# unique levels are R for rock and M for mine.
names(sonar)[names(sonar) == 'V61'] <- 'Class'
colnames(sonar)

set.seed(101)
sample = sample(1:nrow(sonar),0.8*nrow(sonar))
train = sonar[sample,]
test = sonar[-sample,]
dim(train)
dim(test)
library(e1071)

svm_mod <- svm(Class~ ., data=train, method="C-classification", kernel="linear")
svm_mod

pred_train <-predict(svm_mod,train)
mean(pred_train==train$Class)
table(actual=train[,61],predicted=pred_train)
#accuracy for training set is 93.97 per cent.

#prediction on test set
pred_test <-predict(svm_mod,test)
mean(pred_test==test$Class)
table(actual=test[,61],predicted=pred_test)
#accuracy on testing set is 69.04 per cent.
#The number of support vectors 62.(high which indicates more bias)


#Now we will try to fit a radial kernel
svm_mod2 <- svm(Class~ ., data=train, method="C-classification", kernel="radial")
svm_mod2

trainpred2 = predict(svm_mod2, train)
mean(trainpred2==train$Class)
#accuracy for training is 99.39 per cent

testpred2 = predict(svm_mod2, test)
mean(testpred2==test$Class)
#accuracy for testing is 73.09 per cent
#Number of support vectors is 133

#the radial kernel used in this was with default parameters. Lets tune the svm to
#find good values for all the parameters.

tuning <- tune.svm(x=train[,-61],y=train[,61],gamma=10^(-3:3)
                     ,cost=c(0.01,0.1,1,10,100,1000),kernel="radial")

tuning$best.parameters
#best parameters are gamma=0.01 and cost=10

#We will build a model with these parameters

svm_mod3 <- svm(Class~ ., data=train, method="C-classification",
                kernel="radial",cost=tuning$best.parameters$cost,
                gamma=tuning$best.parameters$gamma)
svm_mod3

trainpred3 <-predict(svm_mod3,train)
mean(trainpred3==train$Class)
#accuracy for training is 100 per cent.

#testing set predictions
testpred3 = predict(svm_mod3, test)
mean(testpred3==test$Class)
#testing accuracy is 78.57 per cent.

#this is the highest accuracy from among the svm models we have tried above.



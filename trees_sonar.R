getwd()
setwd("C:\\Users\\Administrator\\Desktop\\Rlessons")
sonar = read.csv('sonar.csv',header=FALSE)
head(sonar)
unique(sonar[,61])
# unique levels are R for rock and M for mine.
names(sonar)[names(sonar) == 'V61'] <- 'Class'
colnames(sonar)

library(tree)

set.seed(145)
sample = sample(1:nrow(sonar),0.8*nrow(sonar))
train = sonar[sample,]
test = sonar[-sample,]
dim(train)
dim(test)

tree1 = tree(Class~.,train)
tree1
summary(tree1)
plot(tree1)
text(tree1)
tree1.pred = predict(tree1, test, type="class")
tab1=table(actual=test[,61],predicted=tree1.pred)
accuracy1 = sum(diag(tab1))/sum(tab1)
#The accuracy is 71.4
#Lets see if we can improve this by methods like bagging and boosting.

library(MASS)
tree2 = cv.tree(tree1,FUN = prune.misclass)
summary(tree2)
plot(tree2)

tree2prune = prune.misclass(tree1, best = 6)
plot(tree2prune)
text(tree2prune, pretty=0)
summary(tree2prune)
summary(tree1)

#This tree is a bit shallower than the previous one and misclassification error is
#more as compared to revious but negligible.So pruning did not hurt much.

#Lets test this model on the test set now.
tree2prune_pred = predict(tree2prune,test,type="class")
tab2 = table(actual=test[,61],predicted=tree2prune_pred)
accuracy2 = sum(diag(tab2))/sum(tab2)
#accuracy has reduced from 71 to 69 but this will do as the number of terminal nodes
#has become 6 from 12.

##We will apply random forest now

library(randomForest)
tree3 = randomForest(Class~., data= train)
tree3$err.rate[500]

#lets check this tree on test data
tree3_pred = predict(tree3,test)
tab3 = table(actual=test[,61],predicted=tree3_pred)
accuracy3 = sum(diag(tab3))/sum(tab3)
#The accuracy of this tree is 85.71 highest in all the trees we have constructed
#till now.500 trees were grown for this model.No of variables tried at each split 
#was 7.(since there are 60 predictor variables so root 60 rounded off is 7 which is 
#the number of variables considered for each split if it is classification tree i.e
# the default value of mtry)
#Out of bag error for this tree is 18.67 per cent.




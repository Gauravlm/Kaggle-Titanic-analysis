#1 st part
train<- read.csv("train.csv",stringsAsFactors =FALSE)
test<- read.csv("test.csv",stringsAsFactors =FALSE)
table(train$Survived)
prop.table(table(train$Survived)) # 61% dead and 38% survived
# we dont have survived col in test so we adding survived col with 0 values.
test$Survived<-rep(0,418)   

submit<- data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
write.csv(submit,file="done.csv",row.names=FALSE)
table(train$Sex)
prop.table(table(train$Sex))
prop.table(table(train$Sex,train$Survived))
prop.table(table(train$Sex,train$Survived),1)
test$Survived<-0
test$Survived[test$Sex=='female']<- 1
submit<- data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
write.csv(submit,file="done.csv",row.names=FALSE)
summary(train$Age)


# 3rd part
# rpart library use for decision tree model
library(rpart)
# if you predict continuous variable then method="anova"
# if you predict decimal values then use method="class"
fit<-rpart(Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data= train, 
           method = "class")
# now plot fit  
plot(fit)
#insert text in tree
text(fit)
library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
#we called rpart prediction function
prediction<- predict(fit,test,type="class")
# creating data frame of prediction
submit <- data.frame(PassangerId= test$PassengerId,Survived= prediction)
# creating csv file for upload
write.csv(submit,file= "firstTree.csv",row.names= FALSE)


fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfullgrowntree.csv", row.names = FALSE)

# trim the tree
fit<- rpart(Survived~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data=train,
            method="class",
            control= rpart.control(minsplit = 2, cp=0.005))
new.fit<- prp(fit,snip= TRUE)$obj
fancyRpartPlot(new.fit)

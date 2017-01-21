library(rpart)
library(randomForest)
library(party)

#fill NA in age
Agefit<- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
               data= combi[!is.na(combi$Age),],
               method= "anova"
               )

combi$Age[is.na(combi$Age)]<- predict( Agefit, combi[is.na(combi$Age),])
summary(combi)
summary(combi$Embarked)
#which gives blank fields
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = 'S'
combi$Embarked<- factor(combi$Embarked)

summary(combi$Fare)
#to find out location of NA value
which(is.na(combi$Fare))
combi$Fare[1044]<- median(combi$Fare,na.rm=TRUE)
# creating new familyID2
# new factor for Random Forests, only allowed <32 levels, so reduce number
combi$FamilyID2<- combi$FamilyID
combi$FamilyID2<- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize<= 3]<- 'Small'
combi$FamilyID2<- factor(combi$FamilyID2)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]
set.seed(415)
# ntree argument specifies how many trees we want to grow.
# importance=TRUE argument allows us to inspect variable importance
fit<- randomForest(as.factor(Survived)~ Pclass + Sex + Age + SibSp + Parch + Fare +
                     Embarked + Title + FamilySize + FamilyID2,
                   data= train,
                   importance= TRUE,
                   ntree=2000)
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data=train, importance=TRUE, ntree=2000)
varImpPlot(fit)
prediction<- predict(fit,test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)


set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
varImpPlot(fit)
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "ciforest.csv", row.names = FALSE)

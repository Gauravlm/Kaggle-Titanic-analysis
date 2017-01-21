# 4th part
train$Name[1]
test$Survived<- NA
#join together train and test using rbind
combi<- rbind(train,test)

# convert into character
combi$Name<- as.character(combi$Name)
combi$Name[1]

#split for index
strsplit(combi$Name[1],split='[,.]')
strsplit(combi$Name[1],split='[,.]')[[1]]
strsplit(combi$Name[1],split='[,.]')[[1]][[2]]
combi$Title<- strsplit(combi$Name,split= '[,.]')[[1]][[2]]
combi$Title<- sapply(combi$Name,FUN=function(x) {strsplit(x,split= '[,.]')[[1]][[2]]})
combi$Title<- sub(' ','',combi$Title)
table(combi$Title)

#combining the name which are not useful for our model
# %in% to check to see if value is part of vector
combi$Title[combi$Title %in% c('Mme','Mlle')]<- 'Mlle'
combi$Title[combi$Title %in% c('Capt','Don','Dona','Major','Sir')]<- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# convert in factor
combi$Title<- factor(combi$Title)
combi$FamilySize<- combi$SibSp+ combi$Parch+1

# adding surname in combi
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
# giving new family ID

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="" )
combi$FamilyID[combi$FamilySize <= 2]<- 'Samll'
table(combi$FamilyID)
  
famID<- data.frame(table(combi$FamilyID))
famID<- famID[famID$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famID$Var1]<- 'Small'
combi$FamilyID <- factor(combi$FamilyID) 

# split into test and train data set
train<- combi[1:891,]
test<- combi[892:1309,]

# Build a new tree with our new features
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "featurestree.csv", row.names = FALSE)


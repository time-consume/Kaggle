#Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
#read data
train<-read.csv("train.csv",header = T)
test<-read.csv("test.csv",header = T)
attach(train)
names(train)
#extract title
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name)
table(train$Sex, train$Title)
rare_title <-c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
#allocate title
train$Title[train$Title == 'Mlle']        <- 'Miss' 
train$Title[train$Title == 'Ms']          <- 'Miss'
train$Title[train$Title == 'Mme']         <- 'Mrs' 
train$Title[train$Title %in% rare_title]  <- 'Rare Title'
table(train$Sex, train$Title)
#calculate famility size
train$size<-train$SibSp+train$Parch+1
table<-xtabs(~size+Survived,data=train)
summary(table)
#allocate family category
train$fsize[train$size==1]<-'single'
train$fsize[train$size>1&train$size<5]<-'small'
train$fsize[train$size>4]<-'large'
mosaicplot(table(train$fsize, train$Survived), main='Family Size by Survival',shade = T)
#fixing missing value
ggplot(train, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()
train$Embarked[c(62, 830)] <- 'C'
train$Pclass[c(62, 830)] <- 1

sum(is.na(train$Age))
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')
mice_mod <- mice(train[, !names(train) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
mice_output <- complete(mice_mod)
par(mfrow=c(1,2))
hist(train$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
train$Age<-mice_output$Age
sum(is.na(train$age))

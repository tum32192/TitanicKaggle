library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(ROCR)
library(lift)
##----------------------------------------------------------------
## PART 1: Prepare Data
##----------------------------------------------------------------
Tita = read.csv("train.csv", header=T, na.strings="")
view(Tita)

numberOfPassenger <- count(Tita)
Tita$PassengerId <- as.factor(Tita$PassengerId)
Tita$Survived <- as.factor(Tita$Survived)
Tita$Pclass <- as.factor(Tita$Pclass)
Tita$Name <- as.factor(Tita$Name)
Tita$Sex <- as.factor(Tita$Sex)
Tita$Age <- as.numeric(Tita$Age)
Tita$SibSp <- as.numeric(Tita$SibSp)
Tita$Parch <- as.numeric(Tita$Parch)
Tita$Ticket <- as.factor(Tita$Ticket)
Tita$Fare <- as.numeric(Tita$Fare)
Tita$Cabin <- as.character(Tita$Cabin)
Tita$Embarked <- as.factor(Tita$Embarked)

missRateID <- sum(is.na(Tita$PassengerId))/numberOfPassenger
missRateSurvived <- sum(is.na(Tita$Survived))/numberOfPassenger
missRatePclass <- sum(is.na(Tita$Pclass))/numberOfPassenger
missRateName <- sum(is.na(Tita$Name))/numberOfPassenger
missRateSex <- sum(is.na(Tita$Sex))/numberOfPassenger
missRateAge <- sum(is.na(Tita$Age))/numberOfPassenger
missRateSip <- sum(is.na(Tita$SibSp))/numberOfPassenger
missRateParch <- sum(is.na(Tita$Parch))/numberOfPassenger
missRateTicket <- sum(is.na(Tita$Ticket))/numberOfPassenger
missRateFare <- sum(is.na(Tita$Fare))/numberOfPassenger
missRateCabin <- sum(is.na(Tita$Cabin))/numberOfPassenger
missRateEmbarked <- sum(is.na(Tita$Embarked))/numberOfPassenger
missRateAge
missRateEmbarked
missRateCabin

## So we have 3 column that sone datas are missing (Age,Embarked,Cabin)
##----------------------------------------------------------------
## PART 2: Fix Missing Value
##----------------------------------------------------------------

Tita %>%
  ggplot(aes(x = Embarked)) +
  geom_bar()

##If we see the missing embarked data and most data is embarked "S" but I think it have good way to deal it better then mode 
##after looking in data we see that NA data in embarked come form some one who paid for fare 80
Tita %>%
  ggplot(aes(x = Embarked,y=Fare)) +
  geom_boxplot(aes(fill = Pclass))

Tita %>%
  ggplot(aes(x = Embarked,y=Fare)) +
  geom_boxplot()
##########
## We will see relation with Embarked and Fare That Embaeked C have hightest fare 
## S are moderate and Q have lowest Fare
##########
## after plot we can guess someone who paid fare 80 should come from embarked C
##replace Na data with C value
for(i in 1:891){
  if (is.na(Tita$Embarked[i]) ){
    Tita$Embarked[i] <- "C"
  }
}
## I think room number can show something so I fix room number too get only frist letter that may helpful in analys
for(i in 1:891){
    Tita$Cabin[i] <- substr(Tita$Cabin[i],1,1)
}
## plot data to look for relation
Tita %>%
  ggplot(aes(x = Cabin,y=Fare)) +
  geom_boxplot()

## Then we know each cabin fare 
Tita %>%
  group_by(Cabin) %>%
  summarise(mean = mean(Fare))

## Then we can predict what cabin they are by fare
for(i in 1:891){
  if (is.na(Tita$Cabin[i]) ){
    fareNow = Tita$Fare[i]
    if(fareNow<=16.2){
      Tita$Cabin[i] <- "G"
    }
    else if(fareNow<=27.1){
      Tita$Cabin[i] <- "F"
    }
    else if(fareNow<=37.6){
      Tita$Cabin[i] <- "T"
    }
    else if(fareNow<=42.8){
      Tita$Cabin[i] <- "A"
    }
    else if(fareNow<=51.6){
      Tita$Cabin[i] <- "E"
    }
    else if(fareNow<=78.6){
      Tita$Cabin[i] <- "D"
    }
    else if(fareNow<=107){
      Tita$Cabin[i] <- "C"
    }
    else{
      Tita$Cabin[i] <- "B"
    }
  }
}


##----------------------------------------------------------------
## PART 3: Visualization and Feature Relations
##----------------------------------------------------------------

## Visual Fare data
Tita %>%
  ggplot(aes(x = Fare)) +
  geom_density() +
  ggtitle("How much fare customer paid")

Tita %>%
  ggplot(aes(x = Pclass,y= Fare)) +
  geom_boxplot() +
  ggtitle("How much fare customer paid compare to eact class",subtitle ="Is it much difference ?")

Tita %>%
  group_by(Pclass) %>%
  summarise(Q1 = quantile(Fare,0.25),
            mean = mean(Fare),
            median = median(Fare),
            Q3 = quantile(Fare,0.75),
            IQR = IQR(Fare))
  
Tita %>%
  summarise(Q1 = quantile(Fare,0.25),
            mean = mean(Fare),
            median = median(Fare),
            Q3 = quantile(Fare,0.75),
            IQR = IQR(Fare))

chisq.test(
  table(Tita$Fare,Tita$Pclass)
)       

################
##Fare and Class have strong relationship (From above) 
################

Tita %>%
  ggplot(aes(x = Cabin,y=Fare,fill=Pclass)) +
  geom_boxplot() +
  ggtitle("How much fare customer paid compare to eact cabin ",subtitle ="Is it much difference ?")

Tita %>%
  ggplot() +
  geom_bar(aes(x = Cabin,fill=Survived),color = "black",position = "fill") +
  ggtitle("Survived compare to eact cabin ",subtitle ="Is it much difference ?")

chisq.test(
  table(Tita$Fare,Tita$Cabin)
)     

chisq.test(
  table(Tita$Cabin,Tita$Survived)
)     

###########
## We can see that each cabin zone classify by fare and cabin with higher fare 
## have more survived rate
###########

##Gender 
Tita %>%
  ggplot() + 
  geom_bar(aes(x=Survived, fill=Sex),color = "black") +
  ggtitle("Survival compare to Sex",subtitle ="How difference ?")

Tita %>%
  ggplot() + 
  geom_bar(aes(x=Survived, fill=Sex),color = "black",position = "fill") +
  ggtitle("Survival compare to Sex",subtitle ="How difference ?")

chisq.test(
  table(Tita$Survived,Tita$Sex)
)                                                           

#############
## We can see that female have more survived rate
#############

##Age
ggplot() + 
geom_density(aes(x=filter(Tita,Survived==0)$Age,col= "Non Survived")) +
geom_density(aes(x=filter(Tita,Survived==1)$Age,col= "Survived")) +
ggtitle("Survival compare to Age",subtitle ="Red = Survived ,Blue = Non")

chisq.test(
  table(Tita$Survived,Tita$Age)
)   
#######
## We cansee that age not have relation to survived that much but we can notice
## that young age(0-10 year) have more survived rate and teenage to middle age(18-30 year)
## have lower survived rate 
#######

Tita%>%
  ggplot() + 
  geom_boxplot(aes(x=Cabin,y=Age)) +
  ggtitle("Cabin compare to Age",subtitle ="How difference ?")

chisq.test(
  table(Tita$Cabin,Tita$Age)
)                                                           

########
## This show us that it may have some relation with cabin because F and G cabin have
## a bit lower age then other and Cabin T have much more lower age then other
########

##Survival compare to class 
Tita %>%
  ggplot() + 
  geom_bar(aes(x=Survived, fill=Pclass),color = "black") +
  ggtitle("Survival compare to their class",subtitle ="How difference ?")

## Better for see how difference in each class
Tita %>%
  ggplot() + 
  geom_bar(aes(x=Pclass, fill=Survived),color = "black",position = "fill") +
  ggtitle("Survival compare to their class",subtitle ="How difference ?")

chisq.test(
  table(Tita$Survived,Tita$Pclass)
)                                                           

################
## We can conclude that high class have more survived rate
################

##Embarked 
Tita%>%
  ggplot() + 
  geom_bar(aes(x=Embarked,fill = Survived),color = "black",position = "fill") +
  ggtitle("Survival compare to Embarked",subtitle ="How difference ?")

chisq.test(
  table(Tita$Survived,Tita$Embarked)
)  

################
## we can see some relationship between embarked and Survived but form above(Part 2)
## That embarked and Fare are have relation and people who have higher Fare will have 
## more survived rate 
################

##########################################
## Form every data we get we can conclude that Embarked Cabin Pclass have relation with
## Fare and We know higher fare have more survived rate and if we looking deeper that
## every Embarked Cabin and Pclass that have higher fare will get more survived rate too
##########################################

##SibSp
Tita%>%
  ggplot() + 
  geom_bar(aes(x=SibSp,fill = Survived),color = "black",position = "fill") +
  ggtitle("Survival compare to SibSp",subtitle ="How difference ?")

chisq.test(
  table(Tita$Survived,Tita$SibSp)
)  

##Parch
Tita%>%
  ggplot() + 
  geom_bar(aes(x=Parch,fill = Survived),color = "black",position = "fill") +
  ggtitle("Survival compare to Parch",subtitle ="How difference ?")

chisq.test(
  table(Tita$Survived,Tita$Parch)
)  

###########
## We can conclue that SibSp and Parch have some relationship with survived 
## From data we can see that people who come in small group have more survived rate
###########

##----------------------------------------------------------------
## PART 4: Modeling
##----------------------------------------------------------------

# Predict Survival from Cabin Zone
sample_modelCabin <- glm(Survived~Cabin,data = Tita,family = binomial,na.action = "na.exclude")
Predicted_resultCabin <- predict(sample_modelCabin,Tita,type ='response')
newresultCabin <- factor(ifelse(Predicted_resultCabin>0.5,"1","0"))
confusionMatrix(newresultCabin,Tita$Survived,mode='prec_recall',positive='1')
newresultCabin

# Predict Survival from Gender
sample_modelGender <- glm(Survived~Sex,data = Tita,family = binomial,na.action = "na.exclude")
Predicted_resultGender <- predict(sample_modelGender,Tita,type ='response')
newresultGender <- factor(ifelse(Predicted_resultGender>0.5,"1","0"))
confusionMatrix(newresultGender,Tita$Survived,mode='prec_recall',positive='1')
newresultGender

# Predict Survival from Age
sample_modelAge <- glm(Survived~Age,data = Tita,family = binomial,na.action = "na.exclude")
Predicted_resultAge <- predict(sample_modelAge,Tita,type ='response')
newresultAge <- factor(ifelse(Predicted_resultAge>0.5,"1","0"))
confusionMatrix(newresultAge,Tita$Survived,mode='prec_recall',positive='1')
newresultAge

# Predict Survival from Fare
sample_modelFare <- glm(Survived~Fare,data = Tita,family = binomial,na.action = "na.exclude")
Predicted_resultFare <- predict(sample_modelFare,Tita,type ='response')
newresultFare <- factor(ifelse(Predicted_resultFare>0.5,"1","0"))
confusionMatrix(newresultFare,Tita$Survived,mode='prec_recall',positive='1')
newresultFare

# Predict Survival from Pclass
sample_modelPclass <- glm(Survived~Pclass,data = Tita,family = binomial,na.action = "na.exclude")
Predicted_resultPclass <- predict(sample_modelPclass,Tita,type ='response')
newresultPclass <- factor(ifelse(Predicted_resultPclass>0.5,"1","0"))
confusionMatrix(newresultPclass,Tita$Survived,mode='prec_recall',positive='1')
newresultPclass

# Predict Survival from Every Class
sample_model <- glm(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Cabin + Embarked ,data = Tita,family = binomial,na.action = "na.exclude")
Predicted_result <- predict(sample_model,Tita,type ='response')
newresult <- factor(ifelse(Predicted_result>0.5,"1","0"))
confusionMatrix(newresult,Tita$Survived,mode='prec_recall',positive='1')
newresult

# Predict using decision tree
test_ind <- sample(nrow(Tita),0.2*nrow(Tita))
titanic_training <- Tita[-test_ind,]
titanic_testing <- Tita[test_ind,]
tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Cabin + Embarked ,data = titanic_training)
rpart.plot(tree)
tree$variable.importance
head(predict(tree,titanic_testing))
res <- predict(tree,titanic_testing,type = "class")
head(res)
confusionMatrix(res,titanic_testing$Survived,mode = "prec_recall",positive = "1")

#######
## From basic decision tree show us a good model with 82.58% Accuracy and 93.62% Precision
## But have low Recall (61.11%) So it not good model now because we predict someone 
## who should survived wrong so it mean we will miss some valueable person 
#######

##lift
res.p <- predict(tree,titanic_testing)[,"1"]
lift_result <- data.frame(
    prob = res.p,
    y = titanic_testing$Survived)
lift_obj <- lift(y~prob,data = lift_result, class = "1")
plot(lift_obj,values =50)

pred <- prediction(res.p,titanic_testing$Survived,label.ordering = c("0","1"))
perf_lift <- performance(pred,"lift","rpp")
plot(perf_lift)

TopDecileLift(res.p,as.integer(titanic_testing$Survived)-1)
#########
## As you can see form lift we have good lift value that we can get 50% sample found by
## 20% sample test
#########

train_control <- trainControl(method = "cv",number = 5)
model <- train(Survived~Pclass + Sex + SibSp + Parch + Fare + Cabin + Embarked,data = titanic_training,trControl = train_control,method="rpart")
model
view(titanic_training)

########
## Because cross validation every column can't NA but our data have NA in age column so
## we decide to not use cv method in our model
########

##----------------------------------------------------------------
## PART 5: Evaluation
##----------------------------------------------------------------

## So we do the same in PART 1 & 2 but with test data
Tita2 = read.csv("test.csv", header=T, na.strings="")
view(Tita2)

numberOfPassenger2 <- count(Tita2)
Tita2$PassengerId <- as.factor(Tita2$PassengerId)
Tita2$Pclass <- as.factor(Tita2$Pclass)
Tita2$Name <- as.factor(Tita2$Name)
Tita2$Sex <- as.factor(Tita2$Sex)
Tita2$Age <- as.numeric(Tita2$Age)
Tita2$SibSp <- as.numeric(Tita2$SibSp)
Tita2$Parch <- as.numeric(Tita2$Parch)
Tita2$Ticket <- as.factor(Tita2$Ticket)
Tita2$Fare <- as.numeric(Tita2$Fare)
Tita2$Cabin <- as.character(Tita2$Cabin)
Tita2$Embarked <- as.factor(Tita2$Embarked)

missRateID2 <- sum(is.na(Tita2$PassengerId))/numberOfPassenger2
missRatePclass2 <- sum(is.na(Tita2$Pclass))/numberOfPassenger2
missRateName2 <- sum(is.na(Tita2$Name))/numberOfPassenger2
missRateSex2 <- sum(is.na(Tita2$Sex))/numberOfPassenger2
missRateAge2 <- sum(is.na(Tita2$Age))/numberOfPassenger2
missRateSip2 <- sum(is.na(Tita2$SibSp))/numberOfPassenger2
missRateParch2 <- sum(is.na(Tita2$Parch))/numberOfPassenger2
missRateTicket2 <- sum(is.na(Tita2$Ticket))/numberOfPassenger2
missRateFare2 <- sum(is.na(Tita2$Fare))/numberOfPassenger2
missRateCabin2 <- sum(is.na(Tita2$Cabin))/numberOfPassenger2
missRateEmbarked2 <- sum(is.na(Tita2$Embarked))/numberOfPassenger2
missRateAge2
missRateEmbarked2
missRateCabin2
missRateFare2
## So we have 3 column that sone datas are missing (Age,Fare,Cabin)

## We want to combine Tita and Tita2 so first we have to make it have same colume 
Tita2$Survived <- NA
Tita2$Survived <- as.factor(Tita2$Survived)

TitaMerge <- rbind(Tita,Tita2)
view(TitaMerge)

## Then fix missing data Fare by use mean of pclass and embarked to predict missing fare
TitaMerge %>%
  filter(!is.na(Fare)) %>%
  group_by(Embarked,Pclass) %>%
  summarise(mean=mean(Fare))

## So our missing Fare data come from Embarked S and Class 3 so it mean is 14.4
for(i in 1:418){
  if (is.na(Tita2$Fare[i]) ){
    Tita2$Fare[i] <- 14.4
  }
}


## Then fix with cabin data
for(i in 1:418){
  Tita2$Cabin[i] <- substr(Tita2$Cabin[i],1,1)
}

## Then we can predict what cabin they are by fare
for(i in 1:418){
  if (is.na(Tita2$Cabin[i]) ){
    fareNow = Tita2$Fare[i]
    if(fareNow<=16.2){
      Tita2$Cabin[i] <- "G"
    }
    else if(fareNow<=27.1){
      Tita2$Cabin[i] <- "F"
    }
    else if(fareNow<=37.6){
      Tita2$Cabin[i] <- "T"
    }
    else if(fareNow<=42.8){
      Tita2$Cabin[i] <- "A"
    }
    else if(fareNow<=51.6){
      Tita2$Cabin[i] <- "E"
    }
    else if(fareNow<=78.6){
      Tita2$Cabin[i] <- "D"
    }
    else if(fareNow<=107){
      Tita2$Cabin[i] <- "C"
    }
    else{
      Tita2$Cabin[i] <- "B"
    }
  }
}

##Predict test data by decision tree
resAns <- predict(tree,Tita2,type = "class")
resAns

##Make csv file and make Format to submit in kaggle 
Ans <-data.frame(resAns)
Ans$PassengerId <- c(892:1309)
Ans <- Ans[,c(2,1)]
names(Ans) <- c("PassengerId","Survived")
Ans
Ans$Survived <- as.integer(Ans$Survived)-1
Ans$PassengerId <- as.integer(Ans$PassengerId)

write.csv(Ans,file="gender_submission2.csv",row.names = FALSE)

TitaTib <- as.tbl(Tita)
table(TitaTib$Sex)

view(iris)

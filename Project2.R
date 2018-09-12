#Remove all stored object
rm(list = ls())
#Set working directory
setwd("E:/")

#Load the libraries
x = c("plyr","dplyr","ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm","corrplot","cowplot","gmodels", "ROSE", 'sampling', 'DataCombine', 'inTrees')
lapply(x, require, character.only = TRUE)
rm(x)
#Load the Data
train=read.csv("Train_data.csv",header = T,na.strings = c(" ", "", "NA"))
test=read.csv("Test_data.csv",header = T,na.strings = c(" ", "", "NA"))

#Understanding the data
dim(train)
dim(test)

colnames(train)
colnames(test)

str(train)

str(test)
##################EXPLORATORY DATA ANALYSIS###############################################
##################UNIVARIATE ANALYSIS#####################################################

train$state=as.factor(train$state)
train$phone.number=as.factor(train$phone.number)
train$international.plan=as.factor(train$international.plan)
train$voice.mail.plan=as.factor(train$voice.mail.plan)
train$Churn=as.factor(train$Churn)

test$state=as.factor(test$state)
test$phone.number=as.factor(test$phone.number)
test$international.plan=as.factor(test$international.plan)
test$voice.mail.plan=as.factor(test$voice.mail.plan)
test$Churn=as.factor(test$Churn)

prop.table(table(train$Churn))
prop.table(table(test$Churn))

ggplot(train %>% group_by(Churn) %>% summarise(Count = n())) + 
  geom_bar(aes(Churn, Count), stat = "identity", fill = "coral1") +
  xlab("") +
  geom_label(aes(Churn, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Churn")

# plot for state
p1 = ggplot(train %>% group_by(state) %>% summarise(Count = n())) + 
  geom_bar(aes(state, Count), stat = "identity", fill = "coral1") +
  xlab("") +
  geom_label(aes(state, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("State")
# plot for international plan
p2 = ggplot(train %>% group_by(international.plan) %>% summarise(Count = n())) + 
  geom_bar(aes(international.plan, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(international.plan, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("International Plan")

# plot for voice mail plan
p3 = ggplot(train %>% group_by(voice.mail.plan) %>% summarise(Count = n())) + 
  geom_bar(aes(voice.mail.plan, Count), stat = "identity", fill = "coral1") +
  xlab("") +
  geom_label(aes(voice.mail.plan, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Voice Mail Plan")
second_row = plot_grid(p2, p3, nrow = 1)
plot_grid(p1, second_row, ncol = 1)

p4 = ggplot(train) + geom_histogram(aes(account.length), binwidth = 10, fill = "blue")
p5 = ggplot(train) + geom_histogram(aes(number.vmail.messages), binwidth = 5, fill = "blue")
p6 = ggplot(train) + geom_histogram(aes(total.day.minutes), binwidth = 10, fill = "blue")
p7 = ggplot(train) + geom_histogram(aes(total.day.calls), binwidth = 10, fill = "blue")
plot_grid(p4, p5, p6,p7, nrow = 2) # plot_grid() from cowplot package

p8 = ggplot(train) + geom_histogram(aes(total.day.charge), binwidth = 5, fill = "blue")
p9 = ggplot(train) + geom_histogram(aes(total.eve.minutes), binwidth = 10, fill = "blue")
p10 = ggplot(train) + geom_histogram(aes(total.eve.calls), binwidth = 10, fill = "blue")
p11 = ggplot(train) + geom_histogram(aes(total.eve.charge), binwidth = 1, fill = "blue")
plot_grid(p8, p9, p10,p11, nrow = 2) # plot_grid() from cowplot package

p12 = ggplot(train) + geom_histogram(aes(number.customer.service.calls), binwidth = 1, fill = "blue")
p13 = ggplot(train) + geom_histogram(aes(total.night.minutes), binwidth = 10, fill = "blue")
p14 = ggplot(train) + geom_histogram(aes(total.night.calls), binwidth = 10, fill = "blue")
p15 = ggplot(train) + geom_histogram(aes(total.night.charge), binwidth = 1, fill = "blue")
plot_grid(p12, p13, p14,p15, nrow = 2) # plot_grid() from cowplot package

p16 = ggplot(train) + geom_histogram(aes(total.intl.minutes), binwidth = 1, fill = "blue")
p17 = ggplot(train) + geom_histogram(aes(total.intl.calls), binwidth = 1, fill = "blue")
p18 = ggplot(train) + geom_histogram(aes(total.intl.charge), binwidth = 0.5, fill = "blue")
plot_grid(p16, p17, p18, nrow = 2) # plot_grid() from cowplot package

#########################MULTIVARIATE ANALYSIS##############################
#USING crosstable function from gmodels
CrossTable(train$state,train$Churn)
#plot a stacked barcharts
ggplot(train,aes(state,fill=Churn))+geom_bar()+labs(title="State vs Churn",x="state",y="count")+theme_bw()
ggplot(train,aes(international.plan,fill=Churn))+geom_bar()+labs(title="Intl plan vs Churn",
                                                                   x="international plan",y="count")+theme_bw()
ggplot(train,aes(voice.mail.plan,fill=Churn))+geom_bar()+labs(title="Voice plan vs Churn",
                                                                x="Voice mail plan",y="count")+theme_bw()

##################Missing Value###############
colSums(is.na(train))
colSums(is.na(train))
sum(is.na(train))
sum(is.na(test))

############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(train,is.numeric) #selecting only numeric

numeric_data = train[,numeric_index]

cnames = colnames(numeric_data)

for(i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(train))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Churn")+
           ggtitle(paste("Box plot of Churn for",cnames[i])))
}

# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
gridExtra::grid.arrange(gn13,gn14,gn15,ncol=3)
gridExtra::grid.arrange(gn16,ncol=1)

##################################Feature Selection################################################
## Correlation Plot 
corrgram(train[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Chi-squared Test of Independence
factor_index = sapply(train,is.factor)
factor_data = train[,factor_index]
for (i in 1:5)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data[,i])))
}
test1=test
## Dimension Reduction
train = subset(train, 
                         select = -c(area.code,total.day.charge,total.eve.charge,total.night.charge,total.intl.charge,
                                     phone.number))

test = subset(test, 
               select = -c(area.code,total.day.charge,total.eve.charge,total.night.charge,total.intl.charge,
                           phone.number))
##Data Manupulation; convert string categories into factor numeric
for(i in 1:ncol(train)){
  
  if(class(train[,i]) == 'factor'){
    
    train[,i] = factor(train[,i], labels=(1:length(levels(factor(train[,i])))))
    
  }
}
for(i in 1:ncol(test)){
  
  if(class(test[,i]) == 'factor'){
    
    test[,i] = factor(test[,i], labels=(1:length(levels(factor(test[,i])))))
    
  }
}

###################################Model Development#######################################

#Divide data into train and test using stratified sampling method
set.seed(1234)
train.index = createDataPartition(train$Churn, p = .80, list = FALSE)
train = train[ train.index,]
validation  = train[-train.index,]
##Decision tree for classification
#Develop Model on training data
C50_model = C5.0(Churn ~., train, trials = 100, rules = TRUE)
#Summary of DT model
summary(C50_model)
#Lets predict for validation cases
C50_Predictions = predict(C50_model, validation[,-15], type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(validation$Churn, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

#Accuracy=98.9%
#FNR=7.7%

#Lets Predict for test case
test_D = predict(C50_model, test[,-15], type = "class")

##Evaluate the performance of classification model
ConfMatrix_test_D = table(test$Churn, test_D)
confusionMatrix(ConfMatrix_test_D)

#Accuracy=95.4
#FNR=29.4


###Random Forest

RF_model = randomForest(Churn ~ .,train, importance = TRUE, ntree = 500)

#Extract rules fromn random forest
#transform rf object to an inTrees' format
treeList = RF2List(RF_model)  
# 
# #Extract rules
exec = extractRules(treeList, train[,-15])  # R-executable conditions
# 
# #Visualize some rules
exec[1:2,]
# 
# #Make rules more readable:
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]
# 
# #Get rule metrics
ruleMetric = getRuleMetric(exec, train[,-15], train$Churn)  # get rule metrics
# 
# #evaulate few rules
ruleMetric[1:2,]

#Presdict validation data using random forest model
RF_Predictions = predict(RF_model, validation[,-15])
##Evaluate the performance of classification model
ConfMatrix_RF = table(validation$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF)

#Accuracy=100%
#FNR=0%

#Random forest for test case
RF_Test=predict(RF_model,test[,-15])
##Evaluate the performance of classification model
ConfMatrix_RF_Test = table(test$Churn, RF_Test)
confusionMatrix(ConfMatrix_RF_Test)

#Accuracy=94.5
#FNR=30.4

#Logistic Regression
logit_model = glm(Churn ~ ., data = train, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = validation, type = "response")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)


##Evaluate the performance of classification model
ConfMatrix_RF = table(validation$Churn, logit_Predictions)

#Accuracy
sum(diag(ConfMatrix_RF))/nrow(validation)

#Accuracy: 86.5
#FNR: 72.8

#prediction for test cases
logit_Predictions_Test = predict(logit_model, newdata = test, type = "response")

#convert prob
logit_Predictions_Test = ifelse(logit_Predictions_Test > 0.5, 1, 0)

##Evaluate the performance of classification model
ConfMatrix_RF_T = table(test$Churn, logit_Predictions_Test)

#Accuracy
sum(diag(ConfMatrix_RF_T))/nrow(test)

#Accuracy: 86.7
#FNR: 75.4
##KNN Implementation
library(class)

#Predict test data
KNN_Predictions = knn(train[, 1:14], validation[, 1:14], train$Churn, k = 7)

#Confusion matrix
Conf_matrix = table(KNN_Predictions, validation$Churn)

#Accuracy
sum(diag(Conf_matrix))/nrow(validation)

#Accuracy = 90.4
#FNR = 6.25

#Prediction for test case
KNN_Predictions_T = knn(train[, 1:14], test[, 1:14], train$Churn, k = 7)
Conf_matrix_T = table(KNN_Predictions_T,test$Churn)
#Accuracy
sum(diag(Conf_matrix_T))/nrow(test)

#Accuracy=89.2
#FNR=18.6

#naive Bayes
library(e1071)

#Develop model
NB_model = naiveBayes(Churn ~ ., data = train)

#predict on test cases #raw
NB_Predictions = predict(NB_model, validation[,1:14], type = 'class')

#Look at confusion matrix
Conf_matrix_V = table(observed = validation[,15], predicted = NB_Predictions)
confusionMatrix(Conf_matrix_V)
#Accuracy = 89.1
#FNR = 59.3

#Prediction for test case
NB_Predictions_T = predict(NB_model, test[,1:14], type = 'class')
#Look at confusion matrix
Conf_matrix_Te = table(observed = test[,15], predicted = NB_Predictions_T)
confusionMatrix(Conf_matrix_Te)
#Accuracy = 87.7
#FNR = 69.6

#K-folda cross-validation for Decision Tree
# define training control
train_control<- trainControl(method="cv", number=10)

# train the model 
model<- C5.0(Churn~., data=train, trControl=train_control, method="class")

# make predictions
predictions<- predict(model,validation)

# append predictions
validation_D<- cbind(validation,predictions)

# summarize results
confusionMatrix<- confusionMatrix(validation_D$predictions,validation$Churn)
confusionMatrix
#Accuracy=95.1
#FNR=9.1%

# make predictions for test cases
predictions_T<- predict(model,test)

# append predictions
test_D<- cbind(test,predictions_T)

# summarize results
confusionMatrix_T<- confusionMatrix(test_D$predictions,test$Churn)
confusionMatrix_T

#Accuracy=95.1
#FNR=7.74

#K-folda cross-validation for Random Forest
# define training control
train_control1<- trainControl(method="cv", number=10)

# train the model 
model1<- randomForest(Churn~., data=train, trControl1=train_control1, method="class")

# make predictions
predictions_R<- predict(model1,validation)

# append predictions
validation_R<- cbind(validation,predictions_R)

# summarize results
confusionMatrix_R<- confusionMatrix(validation_R$predictions_R,validation$Churn)
confusionMatrix_R
#Accuracy=100
#FNR=0%

# make predictions for test cases
predictions_RF<- predict(model1,test)

# append predictions
test_R<- cbind(test,predictions_RF)

# summarize results
confusionMatrix_DT<- confusionMatrix(test_R$predictions_RF,test$Churn)
confusionMatrix_DT

#Accuracy=94
#FNR=17.4

test1$Churn=predictions_T
test1$Churn=as.numeric(test1$Churn)
test1$Churn[test1$Churn==1]="False"
test1$Churn[test1$Churn==2]="True"
write.csv(test1,file = "Sample.csv",row.names = F)

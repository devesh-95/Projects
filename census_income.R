census <- read.csv("G:/R/census-income.csv")

sum(is.na(census))       #here the missing values are represented by '?'

str(census)

census$workclass[28]  #has a whitespace infront of '?'

###################################################Replace ? with NA

census[census==' ?'] <- NA

##########################################################Removing NA values
census <- na.omit(census)


############################################################converting to character type

census$workclass <- as.character(census$workclass)
census$education <- as.character(census$education)
census$marital.status <- as.character(census$marital.status)
census$occupation <- as.character(census$occupation)
census$relationship <- as.character(census$relationship)
census$race <- as.character(census$race)
census$sex <- as.character(census$sex)
census$native.country <- as.character(census$native.country)
census$X <- as.character(census$X)


#######################################################remove whitespaces

library(dplyr)
library(stringr)

census %>% mutate_if(is.character,str_trim) -> census
View(census)
census$workclass[3]


#####################################converting back to factor
census$workclass <- as.factor(census$workclass)
census$education <- as.factor(census$education)
census$marital.status <- as.factor(census$marital.status)
census$occupation <- as.factor(census$occupation)
census$relationship <- as.factor(census$relationship)
census$race <- as.factor(census$race)
census$sex <- as.factor(census$sex)
census$native.country <- as.factor(census$native.country)
census$X <- as.factor(census$X)

str(census)



##########################################################Data Manipulation

census_ed <- census$education
census %>% select(age:relationship) ->  census_seq   
census %>% select(c(5,8,11)) -> census_col     #census[c(5,8,11)]
census %>% select(sex,workclass) %>% filter(sex=="Male" & workclass=='State-gov') -> male_gov
census %>% select(age,education,native.country) %>% filter(age==39 & (education=="Bachelors" | native.country == "United-States")) -> census_us
census %>% sample_n(200) -> census_200

census %>% count(workclass)
census %>% group_by(workclass) %>% summarise(mean(capital.gain))





#######################################################Data Visualization

library(ggplot2)



ggplot(census,aes(relationship,fill=race))+
  geom_bar(position = 'dodge')+
  labs(x="Categories of Relationship",y='Count of Categories',title = "Distribution of Relationships by Race")


ggplot(census,aes(relationship,fill=sex))+
  geom_bar(position = 'dodge')+
  labs(x='categories of Relationship',y='Count of Categories',title='Distribution of Relationships by Sex')


ggplot(census,aes(age,fill=X))+
 geom_histogram(bins = 50)+
 labs(title = 'Distribution of Age',fill='Yearly Income')+
  theme(plot.background = element_rect("black","white"))


ggplot(census,aes(y=hours.per.week, x=capital.gain,col=X))+
  geom_point(alpha=0.6,size=2)+ #apha -> transparency
  labs(x="Capital Gain",y='Hours per week',title='Capital gain vs Hours per week',col='Yearly Income')  


ggplot(census,aes(y=age, x=education, fill=sex)) +
  geom_boxplot()+
  labs(title ='Box-Plot of Age by Education & Sex')



#######################################Linear Regression

install.packages('caTools')
library(caTools)


set.seed(123)

sample.split(census$education.num,SplitRatio = 0.7) -> split_tag

subset(census,split_tag==T) -> train
subset(census,split_tag==F) -> test

lmodel <- lm(hours.per.week ~ education.num, data = train)
summary(lmodel)

pred_val <- predict(lmodel,newdata = test)
head(pred_val)

cbind(Actual= test$hours.per.week, Predicted = pred_val) -> final_data
as.data.frame(final_data) -> final_data

final_data$Actual - final_data$Predicted -> error
cbind(final_data,error) -> final_data

sqrt(mean((final_data$error)^2))

plot(census$education.num, census$hours.per.week)
abline(lmodel)
 
#################################################logistic regression

set.seed(123)

sample.split(census$X, SplitRatio = 0.65) -> split_tag
subset(census,split_tag==T) -> train
subset(census,split_tag==F) -> test

logmod <- glm(X~occupation,data=train,family = 'binomial')
summary(logmod)

pred_val <- predict(logmod,newdata = test, type='response')
head(pred_val)
range(pred_val)

install.packages('ROCR')
library(ROCR)

prediction(pred_val,test$X) -> logmod_roc

performance(logmod_roc,'acc') ->acc
plot(acc)


table(census$X)

pred <- ifelse(pred_val > 0.48, '>50k','<=50k')
table(pred,test$X) -> tab_logmod

sum(diag(tab_logmod))/sum(tab_logmod) -> accuracy
accuracy

install.packages('caret')
library(caret)

confusionMatrix(factor(pred),test$X)

performance(logmod_roc,'tpr','fpr') -> roc 
plot(roc)

performance(logmod_roc,'auc') -> auc
auc <- auc@y.values[[1]]
auc


#####################################################multiple logistic regression

set.seed(123)

sample.split(census$X,SplitRatio = 0.8) -> split_tag
subset(census, split_tag==T) -> train
subset(census, split_tag==F) -> test

mlog <- glm(X~age+workclass+education, data = train, family = 'binomial')
summary(mlog)

pred_val <- predict(mlog,newdata = test,type = 'response')
head(pred_val)
range(pred_val)

library(ROCR)

prediction(pred_val,test$X) -> pred_mlog_roc

performance(pred_mlog_roc,'acc') -> acc
plot(acc)       

mlog_pred <- ifelse(pred_val > 0.45,'>50k','<=50k')

table(mlog_pred,test$X) -> tab
sum(diag(tab))/sum(tab)    #higher accuracy

performance(pred_mlog_roc,'tpr','fpr') -> auc
plot(auc)

auc_ROCR <- performance(pred_mlog_roc,'auc')
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

predict(mlog,newdata = data.frame(age=52,workclass="State-gov",education = "HS-grad"),type = 'response')



############################################### Decision Tree
set.seed(444)


sample.split(census$X,SplitRatio = 0.7) -> split_tag
subset(census,split_tag==T) -> train
subset(census,split_tag==F) -> test


library(rpart)
install.packages('rpart.plot')
library(rpart.plot)

census_model <- rpart(formula = X ~ ., data = train, method = 'class')
rpart.plot(x=census_model,type = 5,extra = 0,tweak = 1.2)

class_predict <- predict(object = census_model,newdata = test, type='class')

tab <- table(class_predict,test$X)

sum(diag(tab))/sum(tab)

confusionMatrix(class_predict,test$X)


#########################################################Random Forest

set.seed(333)

sample.split(census$X,SplitRatio = 0.8) -> split_tag
subset(census, split_tag==T) -> train
subset(census,split_tag==F) -> test

install.packages('randomForest')
library(randomForest)

set.seed(222)
census_model <- randomForest(formula = X ~ .,data = train,ntree=300)

print(census_model)
plot(census_model)

class_predict <- predict(object = census_model, newdata = test,type = 'class')

tab <- table(Prediction=class_predict,Actual=test$X)
tab

sum(diag(tab))/sum(tab)  #highest accuracy

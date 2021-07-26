library(tidyverse) 
library(MASS) 
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(lubridate)
library(ROCR)


#importing data sets

gen_data = read.csv("D:/project/isi, partha sir/new data/general_data.csv", stringsAsFactors = FALSE)
emp_survey = read.csv("D:/project/isi, partha sir/new data/employee_survey_data.csv", stringsAsFactors = FALSE)
manager_survey = read.csv("D:/project/isi, partha sir/new data/manager_survey_data.csv", stringsAsFactors = FALSE)
in_time = read.csv("D:/project/isi, partha sir/new data/in_time.csv", stringsAsFactors = F)
out_time = read.csv("D:/project/isi, partha sir/new data/out_time.csv", stringsAsFactors = F)



#data informations

str(gen_data)
str(emp_survey)
str(manager_survey)

length(unique(tolower(gen_data$EmployeeID)))   #4410, confirming EmployeeID is unique key. It also has Target Variable (Attrition)
length(unique(tolower(emp_survey$EmployeeID)))     #4410, confirming EmployeeID is unique key
length(unique(tolower(manager_survey$EmployeeID))) #4410, confirming EmployeeID is unique key
length(unique(tolower(in_time$EmployeeID)))        #4410, confirming EmployeeID is unique key
length(unique(tolower(out_time$EmployeeID)))        #4410, confirming EmployeeID is unique key


setdiff(gen_data$EmployeeID,emp_survey$EmployeeID)  #Identical EmployeeID across these datasets
setdiff(gen_data$EmployeeID,manager_survey$EmployeeID) #Identical EmployeeID across these datasets
setdiff(gen_data$EmployeeID,in_time$EmployeeID)   #Identical EmployeeID across these datasets
setdiff(gen_data$EmployeeID,out_time$EmployeeID)   #Identical EmployeeID across these datasets


#merging all data
hrdata<-merge(gen_data,emp_survey, by="EmployeeID", all = FALSE)
hrdata<- merge(hrdata,manager_survey, by="EmployeeID", all = FALSE)

dim(hrdata)
head(hrdata)

#Missing values 
sum(is.na(hrdata))
(colSums(is.na(hrdata))/nrow(hrdata))*100  #it is 2.5% of total so computing these NA Values


#NA value computation
hrdata$NumCompaniesWorked[which(is.na(hrdata$NumCompaniesWorked))]<-median(hrdata$NumCompaniesWorked, na.rm = TRUE)
hrdata$TotalWorkingYears[which(is.na(hrdata$TotalWorkingYears))]<-median(hrdata$TotalWorkingYears, na.rm = TRUE)
hrdata$EnvironmentSatisfaction[which(is.na(hrdata$EnvironmentSatisfaction))]<-median(hrdata$EnvironmentSatisfaction, na.rm = TRUE)
hrdata$JobSatisfaction[which(is.na(hrdata$JobSatisfaction))]<-median(hrdata$JobSatisfaction, na.rm = TRUE)
hrdata$WorkLifeBalance[which(is.na(hrdata$WorkLifeBalance))]<-median(hrdata$WorkLifeBalance, na.rm = TRUE)


sum(is.na(hrdata)) #no missing values



#removing columns which are having only single type value
hrdata<-hrdata[, -c(9,16,18)]

head(hrdata)
dim(hrdata)


# As per data dictionary, bringing few variables into correct format 
# we are converting the numeric values to categorical variables
hrdata$Education[which(hrdata$Education==1)]<-'Below College'
hrdata$Education[which(hrdata$Education==2)]<-'College'
hrdata$Education[which(hrdata$Education==3)]<-'Bachelor'
hrdata$Education[which(hrdata$Education==4)]<-'Master'
hrdata$Education[which(hrdata$Education==5)]<-'Doctor'


hrdata$EnvironmentSatisfaction[which(hrdata$EnvironmentSatisfaction==1)]<-'Low'
hrdata$EnvironmentSatisfaction[which(hrdata$EnvironmentSatisfaction==2)]<-'Medium'
hrdata$EnvironmentSatisfaction[which(hrdata$EnvironmentSatisfaction==3)]<-'High'
hrdata$EnvironmentSatisfaction[which(hrdata$EnvironmentSatisfaction==4)]<-'Very High'


hrdata$JobInvolvement[which(hrdata$JobInvolvement==1)]<-'Low'
hrdata$JobInvolvement[which(hrdata$JobInvolvement==2)]<-'Medium'
hrdata$JobInvolvement[which(hrdata$JobInvolvement==3)]<-'High'
hrdata$JobInvolvement[which(hrdata$JobInvolvement==4)]<-'Very High'

hrdata$JobSatisfaction[which(hrdata$JobSatisfaction==1)]<-'Low'
hrdata$JobSatisfaction[which(hrdata$JobSatisfaction==2)]<-'Medium'
hrdata$JobSatisfaction[which(hrdata$JobSatisfaction==3)]<-'High'
hrdata$JobSatisfaction[which(hrdata$JobSatisfaction==4)]<-'Very High'

hrdata$WorkLifeBalance[which(hrdata$WorkLifeBalance==1)]<-'Bad'
hrdata$WorkLifeBalance[which(hrdata$WorkLifeBalance==2)]<-'Good'
hrdata$WorkLifeBalance[which(hrdata$WorkLifeBalance==3)]<-'Better'
hrdata$WorkLifeBalance[which(hrdata$WorkLifeBalance==4)]<-'Best'

hrdata$PerformanceRating[which(hrdata$PerformanceRating==1)]<-'Low'
hrdata$PerformanceRating[which(hrdata$PerformanceRating==2)]<-'Good'
hrdata$PerformanceRating[which(hrdata$PerformanceRating==3)]<-'Excellent'
hrdata$PerformanceRating[which(hrdata$PerformanceRating==4)]<-'Outstanding'

head(hrdata)
dim(hrdata)

library(gganimate)
library(ggpubr)
l1=ggplot(hrdata,aes(x=BusinessTravel,fill=Attrition))+geom_bar(stat='count')+theme(axis.text.x=element_text(angle = 90,hjust=1))+scale_y_continuous(labels=scales::percent)
l2=ggplot(hrdata,aes(x=Gender,fill=Attrition))+geom_bar(stat='count')+theme(axis.text.x=element_text(angle = 90,hjust=1))+scale_y_continuous(labels=scales::percent)
l3=ggplot(hrdata,aes(x=Education,fill=Attrition))+geom_bar(stat='count')+theme(axis.text.x=element_text(angle = 90,hjust=1))+scale_y_continuous(labels=scales::percent)
l4=ggplot(hrdata,aes(x=JobSatisfaction,fill=Attrition))+geom_bar(stat='count')+theme(axis.text.x=element_text(angle = 90,hjust=1))+scale_y_continuous(labels=scales::percent)
l5=ggplot(hrdata,aes(x=YearsSinceLastPromotion,fill=Attrition))+geom_bar(stat='count')+theme(axis.text.x=element_text(angle = 90,hjust=1))+scale_y_continuous(labels=scales::percent)
l6=ggplot(hrdata,aes(x=JobInvolvement,fill=Attrition))+geom_bar(stat='count')+theme(axis.text.x=element_text(angle = 90,hjust=1))
fig=ggarrange(l1,l2,l3,l4,l5,l6,labels=c("A","B","c","D","E","F"),ncol=3,nrow=2)
fig



#boxplot
A=ggplot(hrdata,aes(y=MonthlyIncome,x=Education,fill=Attrition))+stat_boxplot(geom='errorbar',coef=c(5,95))+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
B=ggplot(hrdata,aes(y=MonthlyIncome,x=BusinessTravel,fill=Attrition))+stat_boxplot(geom='errorbar',coef=c(5,95))+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
C=ggplot(hrdata,aes(y=MonthlyIncome,x=Gender,fill=Attrition))+stat_boxplot(geom='errorbar',coef=c(5,95))+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
D=ggplot(hrdata,aes(y=MonthlyIncome,x=JobSatisfaction,fill=Attrition))+stat_boxplot(geom='errorbar',coef=c(5,95))+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
E=ggplot(hrdata,aes(y=MonthlyIncome,x=YearsSinceLastPromotion,fill=Attrition))+stat_boxplot(geom='errorbar',coef=c(5,95))+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
F=ggplot(hrdata,aes(y=MonthlyIncome,x=JobInvolvement,fill=Attrition))+stat_boxplot(geom='errorbar',coef=c(5,95))+geom_boxplot()+theme(axis.text.x=element_text(angle = 90,hjust=1))
fig2=ggarrange(A,B,C,D,E,F,labels=c("A","B","c","D","E","F"),ncol=2,nrow=3)
fig2

library(sjPlot)

library(ggmosaic)
library(reprex)
library(gridExtra)
library(data.table)


library(ggplot2)
library(plotly)




#mosaic
M1=ggplot(hrdata)+geom_mosaic(aes(x=product(Attrition,Education),fill=Attrition),na.rm=TRUE)+labs(x='Education',y='Attrition')
M1
tab1=hrdata%>%select(Attrition,Education)%>%sjtab(fun='xtab',var.labels=c("Education","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
tab1

M2=ggplot(hrdata)+geom_mosaic(aes(x=product(Attrition,DistanceFromHome),fill=Attrition),na.rm=TRUE)+labs(x='DistanceFromHome',y='Attrition')
M2
tab2=hrdata%>%select(Attrition,DistanceFromHome)%>%sjtab(fun='xtab',var.labels=c("DistanceFromHome","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
tab2


M3=ggplot(hrdata)+geom_mosaic(aes(x=product(Attrition,BusinessTravel),fill=Attrition),na.rm=TRUE)+labs(x='BusinessTravel',y='Attrition')
M3
tab3=hrdata%>%select(Attrition,BusinessTravel)%>%sjtab(fun='xtab',var.labels=c("BusinessTravel","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
tab3


M4=ggplot(hrdata)+geom_mosaic(aes(x=product(Attrition,Gender),fill=Attrition),na.rm=TRUE)+labs(x='Gender',y='Attrition')
M4
tab4=hrdata%>%select(Attrition,Gender)%>%sjtab(fun='xtab',var.labels=c("Gender","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
tab4

M5=ggplot(hrdata)+geom_mosaic(aes(x=product(Attrition,JobSatisfaction),fill=Attrition),na.rm=TRUE)+labs(x='Gender',y='Attrition')
M5
tab5=hrdata%>%select(Attrition,JobSatisfaction)%>%sjtab(fun='xtab',var.labels=c("JobSatisfaction","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
tab5

tab6=hrdata%>%select(Attrition,YearsSinceLastPromotion)%>%sjtab(fun='xtab',var.labels=c("YearsSinceLastPromotion","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
tab6



tab8=hrdata%>%select(Attrition,JobInvolvement)%>%sjtab(fun='xtab',var.labels=c("JobInvolvement","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
tab8

tab7=hrdata%>%select(Attrition,PerformanceRating)%>%sjtab(fun='xtab',var.labels=c("PerformanceRating","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
tab7

tab9=hrdata%>%select(Attrition,YearsAtCompany)%>%sjtab(fun='xtab',var.labels=c("YearsAtCompany","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
 tab9

 tab10=hrdata%>%select(Attrition,Age)%>%sjtab(fun='xtab',var.labels=c("Age","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
 tab10
 
 
 
 tab11=hrdata%>%select(Attrition,Department)%>%sjtab(fun='xtab',var.labels=c("Department","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
 tab11
 
 
 tab12=hrdata%>%select(Attrition,EducationField)%>%sjtab(fun='xtab',var.labels=c("EducationField","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
 tab12
 
 
 tab13=hrdata%>%select(Attrition,JobRole)%>%sjtab(fun='xtab',var.labels=c("JobRole","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
 tab13
 
 tab14=hrdata%>%select(Attrition,MaritalStatus)%>%sjtab(fun='xtab',var.labels=c("MaritalStatus","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
 tab14
 
 tab15=hrdata%>%select(Attrition,EnvironmentSatisfaction)%>%sjtab(fun='xtab',var.labels=c("EnvironmentSatisfaction","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
 tab15
 
 
 tab16=hrdata%>%select(Attrition,WorkLifeBalance)%>%sjtab(fun='xtab',var.labels=c("WorkLifeBalance","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
 tab16
 
 
 tab17=hrdata%>%select(Attrition,PerformanceRating)%>%sjtab(fun='xtab',var.labels=c("PerformanceRating","Attrition"),show.row.prc=T,show.col.prc=T,show.summary=T,show.exp=T)
 tab17
 
 

 
 
 
 
 
dim(hrdata)


#Converting specific columns to factors just for templorary purpose
hrdata$EmployeeID<-as.factor(hrdata$EmployeeID)


#Feature scaling
num_var<-sapply(hrdata, is.numeric)
hrdata[num_var]<-lapply(hrdata[num_var], scale)
hrdata$EmployeeID<-as.numeric(hrdata$EmployeeID)

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
hrdata$Attrition<- ifelse(hrdata$Attrition=="Yes",1,0)

# Checking Attrition rate of prospect employee
Attritionrate <- sum(hrdata$Attrition)/nrow(hrdata)
Attritionrate   #15.70% Attrition rate.

str(hrdata)

#Creating categorical subset from the dataset
Cat_var<- hrdata[, c("Education","BusinessTravel", "EnvironmentSatisfaction", "Department", "EducationField","Gender","JobRole","MaritalStatus", "JobSatisfaction","WorkLifeBalance", "JobInvolvement", "PerformanceRating" )]



# converting categorical attributes to factor
hrdata_factor<- data.frame(sapply(Cat_var, function(x) factor(x)))
str(hrdata_factor)

#creating dummy variables for factor attributes
dummies<- data.frame(sapply(hrdata_factor, function(x) data.frame(model.matrix(~x-1,data =hrdata_factor))))

#combining the final dataset
hrdata<-hrdata[, !(names(hrdata)) %in% c( "Education","BusinessTravel", "EnvironmentSatisfaction", "Department", "EducationField","Gender","JobRole","MaritalStatus",  "JobSatisfaction","WorkLifeBalance", "JobInvolvement", "PerformanceRating", "EmployeeID")]

hrdata_final<-cbind(hrdata,dummies)
str(hrdata_final) #2228 obs. of 66 variables

# splitting the data between train and test
set.seed(100)
indices = sample.split(hrdata_final$Attrition, SplitRatio = 0.7)
train = hrdata_final[indices,]
test = hrdata_final[!(indices),]



###logistic regression

#Initial model
ml1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(ml1) #AIC: 963.3 ....Residual deviance: 857.3



# Stepwise selection
library("MASS")
model_2<- stepAIC(ml1, direction="both")
summary(model_2)  


library(car)
vif(ml1)

model_3<-glm(formula = Attrition ~ Age + DistanceFromHome + StockOptionLevel + NumCompaniesWorked + TrainingTimesLastYear + TotalWorkingYears +YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + EnvironmentSatisfaction.xHigh + EnvironmentSatisfaction.xLow + Department.xHuman.Resources + EducationField.xHuman.Resources + Gender.xFemale +  JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xDivorced + MaritalStatus.xMarried +  JobSatisfaction.xMedium + WorkLifeBalance.xBad + JobInvolvement.xHigh + JobInvolvement.xLow + JobInvolvement.xMedium , family = "binomial", data = train)
summary(model_3) #AIC: 939.64  ....Residual deviance:  873.64  
vif(model_3)

#excluding num comp worked

model_4<-glm(formula = Attrition ~ Age + JobLevel +  PercentSalaryHike + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently  + Gender.xFemale  + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + JobInvolvement.xHigh + JobInvolvement.xLow + JobInvolvement.xMedium , family = "binomial", data = train)
summary(model_4) #AIC: 939.64  ....Residual deviance:  873.64  
vif(model_4)

#excluding percent salary hike

model_5<-glm(formula = Attrition ~ Age + JobLevel +  PercentSalaryHike + TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently  + Gender.xFemale  + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium + JobInvolvement.xHigh + JobInvolvement.xLow + JobInvolvement.xMedium , family = "binomial", data = train)
summary(model_5) #AIC: 939.64  ....Residual deviance:  873.64  
vif(model_4)



model_6<-glm(formula = Attrition ~ Age  + NumCompaniesWorked +  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently  + Gender.xFemale  + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium  , family = "binomial", data = train)
summary(model_6) #AIC: 939.64  ....Residual deviance:  873.64  
vif(model_6)

#excluding total working years
model_7<-glm(formula = Attrition ~ Age  + NumCompaniesWorked  + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently  + Gender.xFemale  + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium  , family = "binomial", data = train)
summary(model_7) #AIC: 939.64  ....Residual deviance:  873.64  
vif(model_7)


head(hrdata)

# With 11 significant variables in the model
#All are highly significant and having VIF value less than 2
final_model<- model_7

#######################################################################
### Model Evaluation
### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", newdata = test[,-2])


test_pred
# Let's see the summary 
summary(test_pred)
test$prob <- test_pred
#View(test)


# Let's use the probability cutoff of 50%.
test_pred_Attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_Attrition,test_pred_Attrition) 

#######################################################################

#Let's check with different cut-off values , i.e at 40% instead 50%
test_pred_Attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

#install.packages("e1071")
library(e1071)
test_conf <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")
test_conf  
#Accuracy=85%
#Attrition Accuracy(Sensitivity)= 46%
#Non Attrition Accuracy(Specificity)=92%


library(ISLR)
#LDA

model_7lda<-lda(formula = Attrition ~ Age  + NumCompaniesWorked +  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently  + Gender.xFemale  + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium  , data = train)
summary(model_7lda) 
print(model_7lda)

plot(model_7lda)

predictions_LDA = data.frame(predict(model_7lda, test))
names(predictions_LDA)


predictions_LDA_2 = cbind(test, predictions_LDA)

predictions_LDA_2 %>%
  count(class, Attrition)

predictions_LDA_2 %>%
  summarize(score = mean(class == Attrition))


##Predicting training results.
predmodel.train.lda = predict(lda.model, data=train)
table(Predicted=predmodel.train.lda$class, Survived=Survived)

attach(test)
predmodel.test.lda = predict(model_7lda, newdata=test)
table(Predicted=predmodel.test.lda$class, Attrition=test$Attrition)


# Logistic model, for comparison
model_7<-glm(formula = Attrition ~ Age  + NumCompaniesWorked +  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently  + Gender.xFemale  + JobSatisfaction.xHigh + JobSatisfaction.xLow + JobSatisfaction.xMedium  , family = "binomial", data = train)

logistic_probs = data.frame(probs = predict(model_6, test, type="response"))

predictions_logistic = logistic_probs %>%
  mutate(class = ifelse(probs>.5, "1", "0"))

predictions_logistic = cbind(test, predictions_logistic)

predictions_logistic %>%
  count(class, Attrition)

predictions_logistic %>%
  summarize(score = mean(class == Attrition))

attach(test)
predmodel.test.lda = predict(lda.model, newdata=test)
table(Predicted=predmodel.test.lda$class, Survived=test$Survived)

library(GGally)
ggpairs(hrdata[, c("Age", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike","TotalWorkingYears", "YearsAtCompany", "YearsSinceLastPromotion", "YearsWithCurrManager","JobLevel","StockOptionLevel","TrainingTimesLastYear")])


head(hrdata)
dim(train)
dim(test)
library(pROC)
library(rpart)
library(credibilityImpactCode)
load("C:/Users/Davis/Documents/rProjects/losAtAdmission/trainData.rda")
##regression model example
class = "LOSDays"
predVars<-c("MSDRGName",  
            "PatientCity",
            "AttendingProviderName",
            "DiagnosisGroup"  ,
            "PrimaryDiagnosis")
##split the data into training and eval sets
train_data<-na.omit(rd[rd$DischargeDate <  as.Date('2016-09-30'),c(predVars, class)])
eval_data<-na.omit(rd[as.Date('2016-12-31') > rd$DischargeDate &
                rd$DischargeDate >=  as.Date('2016-09-30'),c(predVars, class)])



### learn the impact coding scheme on the training data
impFit<-impactCodeFit(train_data, catCols = predVars, classCol = class, fam = 'gaussian')

train_dataX = impactCodeTransform(impFit, train_data)
train_data.ready<-data.frame(train_dataX,class = train_data[,class])

eval_dataX = impactCodeTransform(impFit, eval_data)
eval_data.ready<-data.frame(eval_dataX, class = eval_data[,class])

fit<-rpart(class~., data =  train_data.ready)
preds<-predict(fit, eval_data.ready)
obs<-eval_data.ready$class
caret::postResample(preds, obs)



## example with classification model 
library(pROC)
library(rpart)
library(credibilityImpactCode)
load("C:/Users/Davis/Documents/rProjects/losAtAdmission/trainData.rda")
class = "UHC_ReadmissionFlag"
predVars<-c("MSDRGName",  
            "PatientCity",
            "AttendingProviderName",
            "DiagnosisGroup"  ,
            "PrimaryDiagnosis")
##split the data into training and eval sets
train_data<-na.omit(rd[rd$DischargeDate <  as.Date('2016-09-30'),c(predVars, class)])
eval_data<-na.omit(rd[as.Date('2016-12-31') > rd$DischargeDate &
                rd$DischargeDate >=  as.Date('2016-09-30'),c(predVars, class)])



### learn the impact coding scheme on the training data
impFit<-impactCodeFit(train_data, catCols = predVars, classCol = class, fam = 'binomial')

train_dataX = impactCodeTransform(impFit, train_data)
train_data.ready<-data.frame(train_dataX,class = train_data[,class])

eval_dataX = impactCodeTransform(impFit, eval_data)
eval_data.ready<-data.frame(eval_dataX, class = eval_data[,class])

fit<-glm(class~., data =  train_data.ready, family = 'binomial')
preds<-predict(fit, eval_data.ready, type =  "response")
obs<-eval_data.ready$class
r<-pROC::roc(obs, preds)
plot(r)


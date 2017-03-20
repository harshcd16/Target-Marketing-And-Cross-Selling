rm(list = ls(all = TRUE))

setwd("D:/INSOFE/Class PPTs/Project/Target_Marketing_and_cross_selling__Data")
source("D:/INSOFE/Class PPTs/Project/Target_Marketing_and_cross_selling__Data/Functions.R")

#Importing xls data file
library(XLConnect)
df <- readWorksheetFromFile("Target Marketing and cross selling - Data.xls", sheet=1)

############################################Pre-Processing############################################
#Data Summary
summary(df)
str(df)
head(df)

#Recognize required attributes.
required_Variables <- c("Customer.Type", "Customer.ID", "Job.Code", "Rev.Code", "Call.Date","Setup.Date","Last.Service.Date","Complete.Date","Schedule.Date","Dispatch.Date")

df <- subset(df, select = required_Variables)

#Data Summary
summary(df)
str(df)

#Check for NA, NULL, Empty Cells.
sum(is.na(df))
sum(is.null(df))

#Recognize Factor Attributes
df$Rev.Code <- as.factor(df$Rev.Code)
df$Customer.Type <- as.factor(df$Customer.Type)
df$Job.Code <- as.factor(df$Job.Code)

#Data Summary
summary(df)
str(df)

#Group all the customer ids together in ordered way.
library(plyr)
df_OrderedByCustID <- arrange(df,df$Customer.ID,df$Call.Date)
rm(df)

#Check the number of times each customer gets back. If a customer doesn't has enough historical data we drop that customer. Threshold -> Number of Transactions for a customer must not be less than 3.
cust_frequency <- table(df_OrderedByCustID$Customer.ID)
cust_frequency <- as.data.frame(cust_frequency)
sub.data <- cust_frequency[cust_frequency[,2] < 3 ,]
x <- as.character(sub.data$Var1)
df_OrderedByCustID <- df_OrderedByCustID[df_OrderedByCustID[,2] != x[1],]
df_OrderedByCustID <- df_OrderedByCustID[df_OrderedByCustID[,2] != x[2],]

###########################################Feature Engineering########################################

#Create an attribute to capture number of days after which a client calls again for next service.
Number.Of.Days.Until.Next.Call <- numeric()

#Get the unique Customer-ids
unique_ids <- unique(df_OrderedByCustID$Customer.ID)

Number.Of.Days.Until.Next.Call <- calculateNumOfDaysUntilNextCall(Number.Of.Days.Until.Next.Call,unique_ids)
class(Number.Of.Days.Until.Next.Call)

df_OrderedByCustID$Number.Of.Days.Until.Next.Call <- as.numeric(Number.Of.Days.Until.Next.Call)

#Calculate Average number of days during which a client comes back for each customer-id
mean_days <- tapply(df_OrderedByCustID$Number.Of.Days.Until.Next.Call, df_OrderedByCustID$Customer.ID, mean)
class(mean_days)

#For each customer-id add a new column to indicate the average number of days for churn calculation.
Mean.Days <- vector(mode = "numeric", length = nrow(df_OrderedByCustID))
df_OrderedByCustID$Mean.Days <- Mean.Days
customerID <- dimnames(mean_days)[[1]]
class(customerID)
df_OrderedByCustID$Mean.Days <- createNewColumnForAverageReturnDays(mean_days,customerID,df_OrderedByCustID$Customer.ID,Mean.Days)

#Add a new feature to calculate number of days between Scheduled and Dispatch dates. Assumption being that if Dispatch date is after Scheduled date the customer will churn.
df_OrderedByCustID$Dispatched.OnOrBefore.Scheduled.Date <- df_OrderedByCustID$Dispatch.Date <= df_OrderedByCustID$Schedule.Date

#Add a new feature to check if the Completion Date is on or before the Scheduled Date. Assumption being that if Completion date is after Scheduled date the customer is likely to churn.
df_OrderedByCustID$Completed.OnOrBefore.Scheduled.Date <- df_OrderedByCustID$Complete.Date <= df_OrderedByCustID$Schedule.Date

# Number of days taken to complete since call date
df_OrderedByCustID$Number.Of.Days.TO.Complete.Since.Call.Date <- as.numeric(as.Date(df_OrderedByCustID$Complete.Date)  - as.Date(df_OrderedByCustID$Call.Date))

#If Complete.Date is more than 2 days of Call.Date. The customer is likely to churn. 
df_OrderedByCustID$Completed.Within.2Days.Of.Call.Date <- ifelse(df_OrderedByCustID$Number.Of.Days.TO.Complete.Since.Call.Date > 2, FALSE, TRUE)

#Check the structure of the data
str(df_OrderedByCustID)

#Add a new attribute for churn
df_OrderedByCustID$Churn <- ((df_OrderedByCustID$Number.Of.Days.Until.Next.Call > df_OrderedByCustID$Mean.Days) | (df_OrderedByCustID$Dispatched.OnOrBefore.Scheduled.Date == FALSE)
| (df_OrderedByCustID$Completed.OnOrBefore.Scheduled.Date == FALSE)
| (df_OrderedByCustID$Completed.Within.2Days.Of.Call.Date== FALSE))

#Drop the first transaction records for each customer-id for which Number.Of.Days.Until.Next.Call is 0
df_OrderedByCustID <- subset(x = df_OrderedByCustID, df_OrderedByCustID$Number.Of.Days.Until.Next.Call != 0)

#Add a new column which will indicate 0 if the customer has churned or indicate the Rev.Code if the 
#customer has not churned.
Rev.Code.For.Non.Churn <- ifelse(df_OrderedByCustID$Churn == TRUE,0,df_OrderedByCustID$Rev.Code)
df_OrderedByCustID$Rev.Code.For.Non.Churn <- Rev.Code.For.Non.Churn

df_OrderedByCustID$Churn <- as.factor(df_OrderedByCustID$Churn)
df_OrderedByCustID$Rev.Code.For.Non.Churn <- as.factor(df_OrderedByCustID$Rev.Code.For.Non.Churn)

str(df_OrderedByCustID)

##############################################Split Data for Train/Test##############################
# Move last transaction for each customer id to test data

row_names_test <- getTestRows(df_OrderedByCustID)

row_names_train <- setdiff(rownames(df_OrderedByCustID), row_names_test)
  
#Extract test and train data
df_TestData <- df_OrderedByCustID[row_names_test,]
df_TrainData <- df_OrderedByCustID[row_names_train,]

str(df_TrainData)

###############################################Model Building################################################

################Building Logistic model for Churn - START############
#Building Logistic model on all attributes
df_TrainDataGLM <- subset(x = df_TrainData, select = -c(Rev.Code.For.Non.Churn))

#Used maxit = 100 because the model was not converging and returning error
LogReg <- glm(formula = Churn ~ ., family = binomial("logit"), data = df_TrainDataGLM, maxit = 100)

#Check for Alias Co-efficients
ld.vars <- attributes(alias(LogReg)$Complete)$dimnames[[1]]
ld.vars
class(ld.vars)
rm(LogReg)
#Since the co-efficients mentioned in ld.vars are infinitely collinear we drop them and again build a model. Along with the above attributes VIF throws error
LogReg1 <- glm(formula = Churn ~ .-Customer.ID-Setup.Date-Mean.Days-Number.Of.Days.TO.Complete.Since.Call.Date-Last.Service.Date, family = binomial("logit"), data = df_TrainDataGLM, maxit = 100)

library(car)
vif(LogReg1) #No Collinear Attributes exist

summary(LogReg1)
rm(LogReg1)

#Drop Last.Service.Date and Number.Of.Days.Until.Next.Call and rebuild the model since they are least significant
LogReg2 <- glm(formula = Churn ~ .-Customer.ID-Setup.Date-Mean.Days-Number.Of.Days.TO.Complete.Since.Call.Date-Last.Service.Date-Number.Of.Days.Until.Next.Call, family = binomial("logit"), data = df_TrainDataGLM, maxit = 100)

summary(LogReg2)

######################Model- Evaluation#################
df_TestDataGLM <- subset(x = df_TestData, select = -c(Rev.Code.For.Non.Churn))

# train results
prob<-predict(LogReg2, type="response")
pred_class <- ifelse(prob> 0.5, 1, 0)
table(df_TrainDataGLM$Churn,pred_class)

# Test results 
fitted.results <- predict(LogReg2,df_TestDataGLM,type='response')
fitted.class <- ifelse(fitted.results > 0.5,1,0)
table(df_TestDataGLM$Churn,fitted.class)

#Calculate Recall for Train data
conf.mat.Train = table(df_TrainDataGLM$Churn,pred_class)
accuracy_Train = sum(diag(conf.mat.Train))/sum(conf.mat.Train)
#precision = conf.mat[2,2]/sum(conf.mat[,2])
recall_Train = conf.mat.Train[2,2]/sum(conf.mat.Train[2,])

#Calculate Recall for Test data
conf.mat.Test = table(df_TestDataGLM$Churn,fitted.class)
accuracy_Test = sum(diag(conf.mat.Test))/sum(conf.mat.Test)
#precision = conf.mat[2,2]/sum(conf.mat[,2])
recall_Test = conf.mat.Test[2,2]/sum(conf.mat.Test[2,])

recall_Train #67.64%
recall_Test  #86.35%

accuracy_Train #65.56%
accuracy_Test  #58.80%

library(ROCR)
p <- predict(LogReg2,df_TestDataGLM, type="response")
pr <- prediction(p, df_TestDataGLM$Churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0, b= 1)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc  # 61.37%

################Building Logistic model for Churn - END##############################################

################Building CART Decision Tree Model for Churn - START #####################################

df_TrainDataDT <- subset(x = df_TrainData, select = -c(Rev.Code.For.Non.Churn))

library(rpart)
dtCart = rpart(Churn~., data=df_TrainDataDT, method="class")    
plot(dtCart, main="Classification Tree for loan Class",
     margin=0.15, uniform=TRUE)
text(dtCart,use.n=T)
summary(dtCart)

########################Model Evaluation########################################
df_TestDataDT <- subset(x = df_TestData, select = -c(Rev.Code.For.Non.Churn))

#Recall for Train data
conf.mat.DT=table(df_TrainDataDT$Churn, predict(dtCart, newdata=df_TrainDataDT, type="class"))
accuracy_Train = sum(diag(conf.mat.DT))/sum(conf.mat.DT)
#precision = conf.mat[2,2]/sum(conf.mat[,2])
recall_Train = conf.mat.DT[2,2]/sum(conf.mat.DT[2,])*100

#Recall for Test data
conf.mat.DT=table(df_TestDataDT$Churn, predict(dtCart, newdata=df_TestDataDT, type="class"))
accuracy_Test = sum(diag(conf.mat.DT))/sum(conf.mat.DT)
#precision = conf.mat[2,2]/sum(conf.mat[,2])
recall_Test = conf.mat.DT[2,2]/sum(conf.mat.DT[2,])*100

accuracy_Train #96.81%
accuracy_Test  #83.52%

recall_Train   #96.24%
recall_Test    #87.47%

library(ROCR)
p <- predict(dtCart,df_TestDataDT, type = "prob")
pr <- prediction(p[,2],df_TestDataDT$Churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(a=0, b= 1)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc  # 82.78%

################Building CART Decision Tree Model for Churn - END#####################################


################Building C5.0 Decision Tree Model for Rev.Code.For.Non.Churn - START#####################################
######################################Preprocess data##################################
df_C5.0 <- subset(x = df_OrderedByCustID, select = -c(Churn))
str(df_C5.0)

#Convert Date attributes to char because C5.0 doesn't operate on 'POSIXct', 'POSIXt' attributes
df_C5.0$Call.Date <- as.character.Date(df_C5.0$Call.Date)
df_C5.0$Setup.Date <- as.character.Date(df_C5.0$Setup.Date)
df_C5.0$Last.Service.Date <- as.character.Date(df_C5.0$Last.Service.Date)
df_C5.0$Complete.Date <- as.character.Date(df_C5.0$Complete.Date)
df_C5.0$Schedule.Date <- as.character.Date(df_C5.0$Schedule.Date)
df_C5.0$Dispatch.Date <- as.character.Date(df_C5.0$Dispatch.Date)

#Convert logical attributes to factor because C5.0 doesn't operate on logical attributes
df_C5.0$Dispatched.OnOrBefore.Scheduled.Date <- as.factor(df_C5.0$Dispatched.OnOrBefore.Scheduled.Date)
df_C5.0$Completed.OnOrBefore.Scheduled.Date <- as.factor(df_C5.0$Completed.OnOrBefore.Scheduled.Date)
df_C5.0$Completed.Within.2Days.Of.Call.Date <- as.factor(df_C5.0$Completed.Within.2Days.Of.Call.Date)
str(df_C5.0)

########################################Split Test And Train Data####################################
row_names_test <- getTestRows(df_C5.0)

row_names_train <- setdiff(rownames(df_C5.0), row_names_test)

#Extract test and train data
df_TestDataDT1 <- df_C5.0[row_names_test,]
df_TrainDataDT1 <- df_C5.0[row_names_train,]
str(df_TrainDataDT1)

library(C50)
dtC50= C5.0(Rev.Code.For.Non.Churn ~ ., data = df_TrainDataDT1, rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)

########################Model Evaluation########################################
#Recall for Train data
conf.mat.DT=table(df_TrainDataDT1$Rev.Code.For.Non.Churn, predict(dtC50, newdata=df_TrainDataDT1, type="class"))
accuracy_Train = sum(diag(conf.mat.DT))/sum(conf.mat.DT)
#precision = conf.mat[2,2]/sum(conf.mat[,2])
recall_Train = conf.mat.DT[2,2]/sum(conf.mat.DT[2,])*100

#Recall for Test data
conf.mat.DT=table(df_TrainDataDT1$Rev.Code.For.Non.Churn, predict(dtC50, newdata=df_TestDataDT1, type="class"))
accuracy_Test = sum(diag(conf.mat.DT))/sum(conf.mat.DT)
#precision = conf.mat[2,2]/sum(conf.mat[,2])
recall_Test = conf.mat.DT[2,2]/sum(conf.mat.DT[2,])*100

accuracy_Train #86.71%
accuracy_Test #86.71%

recall_Train #97.25%
recall_Test #97.25%

################Building C5.0 Decision Tree Model for Rev.Code.For.Non.Churn - END#####################################

################Building SVM Model for Rev.Code.For.Non.Churn - START#####################################
# Load required libraries
library(vegan)
library(dummies)
library(e1071)
#Drop Churn Variable
df_SVM <- subset(x = df_OrderedByCustID, select = -c(Churn))
df_SVM_Temp <- df_SVM
str(df_SVM)

###########################################PreProcessing#########################################
#Drop POSIXct date attributes as these can't be converted to numeric. The POSIXct attribute itself converts to a long literal which can be treated as id and not useful in prediction.
df_SVM <- subset(x = df_SVM, select = -c(Call.Date,Setup.Date,Last.Service.Date,Complete.Date, Schedule.Date, Dispatch.Date ))
str(df_SVM)

attr = c('Customer.Type', 'Job.Code','Rev.Code', 'Number.Of.Days.Until.Next.Call', 'Mean.Days', 'Dispatched.OnOrBefore.Scheduled.Date', 'Completed.OnOrBefore.Scheduled.Date', 'Number.Of.Days.TO.Complete.Since.Call.Date', 'Completed.Within.2Days.Of.Call.Date', 'Rev.Code.For.Non.Churn')

cat_attr <- c('Customer.Type', 'Job.Code', 'Rev.Code','Dispatched.OnOrBefore.Scheduled.Date','Completed.OnOrBefore.Scheduled.Date','Completed.Within.2Days.Of.Call.Date', 'Rev.Code.For.Non.Churn')

num_attr <- setdiff(attr,cat_attr)

cat_Data <- data.frame(sapply(df_SVM[,cat_attr], as.factor))
num_Data <- data.frame(sapply(df_SVM[,num_attr], as.numeric))

df_SVM = cbind(num_Data, cat_Data)
rm(cat_Data, num_Data)

# Do the summary statistics and check for missing values and outliers.
summary(df_SVM)
str(df_SVM)
#View(df_SVM)

# Build the classification model.
ind_Num_Attr = num_attr
rm(num_attr)
ind_Cat_Attr = setdiff(cat_attr, "Rev.Code.For.Non.Churn")
rm(cat_attr)

# Standardizing the numeric data
cla_Data = decostand(df_SVM[,ind_Num_Attr], "range") 
str(cla_Data)
rm(ind_Num_Attr)

# Convert all categorical attributes to numeric 
# 1. Using dummy function, convert attributes into numeric attributes 
customer_Type = dummy(df_SVM$Customer.Type)
job_code = dummy(df_SVM$Job.Code)
rev_code <- dummy(df_SVM$Rev.Code)
Dispatched.OnOrBefore.Scheduled.Date <- dummy(df_SVM$Dispatched.OnOrBefore.Scheduled.Date)
Completed.OnOrBefore.Scheduled.Date <- dummy(df_SVM$Completed.OnOrBefore.Scheduled.Date)
Completed.Within.2Days.Of.Call.Date <- dummy(df_SVM$Completed.Within.2Days.Of.Call.Date)

cla_Data = cbind(cla_Data, customer_Type, job_code, rev_code, Dispatched.OnOrBefore.Scheduled.Date, Completed.OnOrBefore.Scheduled.Date, Completed.Within.2Days.Of.Call.Date)
str(cla_Data)

rm(ind_Cat_Attr)
ind_Attr = names(cla_Data)

# Append the Target attribute. Customer-ID is added so that we can split the data with last transactions for each customer-id in test data.

cla_Data = cbind(cla_Data, Rev.Code.For.Non.Churn=df_SVM_Temp[,"Rev.Code.For.Non.Churn"],  Customer.ID= df_SVM_Temp[,"Customer.ID"]) 
rm(df_SVM_Temp)

str(cla_Data)
#View(cla_Data)

##################################Split data into Train/Test########################################
cla_Data$Customer.ID <- as.character(cla_Data$Customer.ID)
str(cla_Data$Customer.ID)
# Move last transaction for each customer id to test data

row_names_test <- getTestRows(cla_Data)

row_names_train <- setdiff(rownames(cla_Data), row_names_test)

#Extract test and train data
df_TestDataSVM <- cla_Data[row_names_test,]
df_TrainDataSVM <- cla_Data[row_names_train,]

#Drop "Customer_ID" from Train and Test Data
df_TestDataSVM <- subset(x = df_TestDataSVM, select = -c(Customer.ID))
df_TrainDataSVM <- subset(x = df_TrainDataSVM, select = -c(Customer.ID))
str(df_TrainDataSVM)

################################################# Build best SVM model ###################################
svm_model = svm(x = df_TrainDataSVM[,ind_Attr], 
            y = df_TrainDataSVM$Rev.Code.For.Non.Churn, 
            type = "C-classification", 
            kernel = "linear", cost = 0.01, gamma = 0.02) 

# Look at the model summary
summary(svm_model)

svm_model$index

plot(cmdscale(dist(df_TrainDataSVM[,ind_Attr])),
     col = as.integer(df_TrainDataSVM$Rev.Code.For.Non.Churn),
     pch = c("o","+")[1:nrow(df_TrainDataSVM) %in% svm_model$index + 1])

# Predict on train data  
pred_Train  =  predict(svm_model, df_TrainDataSVM[,ind_Attr])  

# Build confusion matrix and find accuracy   
cm_Train = table(df_TrainDataSVM$Rev.Code.For.Non.Churn, pred_Train)
accu_Train= sum(diag(cm_Train))/sum(cm_Train)
recall_Train = cm_Train[2,2]/sum(cm_Train[2,])*100
rm(pred_Train, cm_Train)

# Predict on test data
pred_Test = predict(svm_model, df_TestDataSVM[,ind_Attr]) 

# Build confusion matrix and find accuracy   
cm_Test = table(df_TestDataSVM$Rev.Code.For.Non.Churn, pred_Test)
accu_Test= sum(diag(cm_Test))/sum(cm_Test)
recall_Test = cm_Test[2,2]/sum(cm_Test[2,])*100
rm(pred_Test, cm_Test)

recall_Train #98.70%
recall_Test  #97.72%

accu_Train #91.40%
accu_Test  #89.20%

################Building SVM Model for Rev.Code.For.Non.Churn - END#####################################

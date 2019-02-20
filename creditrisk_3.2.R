# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()
setwd("~/Dropbox/7 miao/SPL-WS1819-20181017/p2p_lender")
setwd("C:/Users/srq04/Dropbox/7 miao/SPL-WS1819-20181017/p2p_lender")
#install.packages("MASS")
#library("MASS")
#install and load packages
packs = c("caret", "rpart", "hmeasure","rpart.plot","ggplot2","gmodels","MASS")
library(caret)
library(rpart)
library(hmeasure)
library(rpart.plot)
library(ggplot2)
library(gmodels)
library(MASS)
library(lubridate)

options(scipen=200) ##do not use scientific notation
##load data
dataset= read.csv("newdata_1415.csv")
#dataset= subset(dataset, select=c(2:16))

##===============================================================explore the dataset==========================================================
##convert the dependent variable into binary: bad and good
levels(dataset$loan_status)[levels(dataset$loan_status) %in% c("Charged Off","Default")] = "bad"  #loans charged off or default are defined as bad
levels(dataset$loan_status)[levels(dataset$loan_status)%in% c( "Fully Paid")]= "good" #others are defined as good

#summary(dataset$loan_status)
#distribution of default and non-default sample
summary(dataset$loan_status) #get the number of bad and good loans
n= nrow(dataset)
table(dataset$loan_status)/n  # get the proportion of bad and good loans

#=========independent variables
str(dataset)
#convert the datatype of explanatory variables
#creating estimated credit history: issue month-The month the borrower's earliest reported credit line was opened; the number of date since the earliest reported credit line was opened
dataset$issue_d=as.Date(paste("01-", dataset$issue_d, sep = ""), format='%d-%b-%Y')
dataset$earliest_cr_line= as.Date(paste("01-",dataset$earliest_cr_line, sep = ""), format='%d-%b-%Y')

dataset$loan_amnt =as.numeric(dataset$loan_amnt) 
dataset$annual_inc =as.numeric(dataset$annual_inc)
dataset$delinq_2yrs=as.numeric(dataset$delinq_2yrs)
dataset$inq_last_6mths= as.numeric(dataset$inq_last_6mths)

#two borrower indebtedness variables are not directly given
# creating loan amount to annual income
dataset$loan_inc = dataset$loan_amnt/ dataset$annual_inc
boxplot(loan_inc~loan_status, data = dataset, main="boxplot of grouped interest rate") ##the average interest rate for bad borrowers tend to be higher

#creating annual instalment to income, the variable instalment is monthly
dataset$ann_instal_inc= dataset$installment * 12 / dataset$annual_inc
boxplot(ann_instal_inc~loan_status, data = dataset, main="boxplot of grouped interest rate") ##the average interest rate for bad borrowers tend to be higher

#the variable credit length
dataset$length_credit= difftime(dataset$issue_d, dataset$earliest_cr_line, units = "auto")
dataset$length_credit=as.numeric(dataset$length_credit)
dataset$length_credit_year= dataset$length_credit/365
summary(dataset$length_credit_year) 

summary(dataset) #mths_since_last_delinq has around one half missing values
#==============explore the relationship between xi and y
#boxplot of loan amount grouped by variable loan_status
boxplot(loan_amnt~loan_status, data = dataset, main="boxplot of grouped loan amount") ##the average loan amount of bad borrowers tend to be higher 

boxplot(int_rate~loan_status, data = dataset, main="boxplot of grouped interest rate") ##the average interest rate for bad borrowers tend to be higher

boxplot(annual_inc~loan_status, data = dataset)
summary(dataset$annual_inc) ##for variable annual income, many outliers exist
#average annual income of bad and good borrowers, good borrowers have a higher average annual income
#convert numeric annual income into categories
#dataset$inc_cat = rep(NA, length(dataset$annual_inc)) #creat a null column
#dataset$inc_cat[which(dataset$annual_inc <= 25000 & dataset$annual_inc>=0)] = "0-25k"
#dataset$inc_cat[which(25000<dataset$annual_inc & dataset$annual_inc <= 50000)] = "25k-50k"
#dataset$inc_cat[which(50000<dataset$annual_inc & dataset$annual_inc <=75000)] = "50k-75k"
#dataset$inc_cat[which(75000<dataset$annual_inc& dataset$annual_inc<=100000)] = "75k-100k"
#dataset$inc_cat[which(dataset$annual_inc>100000 & dataset$annual_inc<= 125000)] = "100k-125k"
#dataset$inc_cat[which(125000 <dataset$annual_inc)] = ">125k"
dataset$inc_cat= cut(dataset$annual_inc, breaks = c(-Inf, 30000,40000,50000,60000,70000,80000,90000,100000,Inf))  #a small number of borrowers with an annual income less than 30k and more than 125k
#the distribution of bad and good customers with annual income larger than 100k is similar to that with 125k
dataset$inc_cat= as.factor(dataset$inc_cat) #convert into categories
prop.table(xtabs(~inc_cat+loan_status, data=dataset),1) #
sort(table(dataset$inc_cat))
#aggregate(dataset[,8], by=list(class=dataset$loan_status), mean) 

prop.table(xtabs(~grade+loan_status, data=dataset),1) #from grade A to G, the proportion of bad borrowers increases
prop.table(xtabs(~sub_grade+loan_status, data=dataset),1) 

prop.table(xtabs(~purpose+loan_status, data=dataset),1)# the credits used for house, moving and small_business are more risky
sort(table(dataset$purpose))#one borrower applicate for a credit for education or wedding, and the credit is good. It would be problematic after splitting the data into train and test set.
#So I deleted these two observations
library(data.table)
dataset=setDT(dataset)[, if(.N>1) .SD, by=.(purpose)]  #clear the categories with only one observation
dataset$purpose=as.factor(dataset$purpose)

#debt to income
boxplot(dti~loan_status, data = dataset)
summary(dataset$dti)  # one outlier is extremly large: 672.5
dataset$dti[dataset$dti>500]= 17.56
dataset$dti[is.na(dataset$dti)]=17.56# replace the outlier with median value 
boxplot(dti~loan_status, data = dataset) # bad borrowers tend to have a higher dti, debt to income ratio 

boxplot(delinq_2yrs~loan_status, data = dataset)  
summary(dataset$delinq_2yrs)
#convert the variable delinq_2yrs into catagories, because the most borrowers have deliquency less than 5 times
dataset$delinq_2yrs_cat=cut(dataset$delinq_2yrs, breaks = c(-Inf,0,1,2,3,5,Inf))  #581/108428 % borrowers have delinquency more than 5 time in last 2 years  
dataset$delinq_2yrs_cat= as.factor(dataset$delinq_2yrs_cat)
sort(table(dataset$delinq_2yrs_cat))
prop.table(xtabs(~delinq_2yrs_cat+loan_status, data=dataset),1) 

summary(dataset$inq_last_6mths)
prop.table(xtabs(~inq_last_6mths+loan_status, data = dataset),1)
#dataset$inq_last_6mths= as.factor(dataset$inq_last_6mths)

summary(dataset$revol_util)
dataset$revol_util[is.na(dataset$revol_util)]= 0.5300 #replce NAs with meadian value
boxplot(revol_util~loan_status, data = dataset) #bad borrowers tend to have a higher revolving rate

prop.table(xtabs(~home_ownership+loan_status, data = dataset),1) #borrowers holding a house with mortgage are best, people who rent a house tend to be bad borrowers
#months since last delinquency
summary(dataset$mths_since_last_delinq) #nas
dataset$mths_since_last_delinq=  ifelse(is.na(dataset$mths_since_last_delinq),-100, dataset$mths_since_last_delinq) #replace NA with negative value, than convert the numeric variable into categories
dataset$mths_since_last_delinq_cat= cut(dataset$mths_since_last_delinq, breaks = c(-Inf, 0,12, 24,36, 48, 60, Inf))
sort(table(dataset$mths_since_last_delinq_cat))
prop.table(xtabs(~mths_since_last_delinq_cat+loan_status, data=dataset),1) 

#the variable open account
summary(dataset$open_acc)
boxplot(open_acc~loan_status, data=dataset) 

#the variable public record
summary(dataset$pub_rec)
sort(table(dataset$pub_rec))  # most popurlar value are 0,1,2,3, so convert this variable into categories
dataset$pub_rec_cat= cut(dataset$pub_rec, breaks = c(-Inf, 0, 1,2, Inf))
sort(table(dataset$pub_rec_cat))  
prop.table(xtabs(~pub_rec_cat+loan_status, data=dataset),1) 

summary(dataset)# no NA

#Calculate information value
library(InformationValue)
#WOETable(X= dataset$, Y=dataset$loan_status)

#ggplot(dataset, aes(x=dti, y=revol_bal, color=loan_status))+ geom_point() +labs(main="")+facet_grid(.~loan_status)
#ggplot(dataset, aes(x=dti, y=revol_bal, color=loan_status))+ geom_point() +labs(main="",na.rm=T)+facet_grid(.~grade)
#ggplot(dataset, aes(x=delinq_2yrs, y=inq_last_6mths, color=loan_status))+ geom_point()+labs(main="",na.rm=T)
#ggplot(dataset, aes(x=revol_util, y=revol_bal, col=loan_status))+ geom_point()+facet_grid(.~loan_status)+labs(main="",na.rm=T)
write.csv(dataset, file = "dataset_all")
#=====================================================================Data Preparation is finished=============================================================
dataset_cat= subset(dataset, select= c("purpose", "int_rate","grade","sub_grade", "home_ownership","delinq_2yrs_cat","dti","inq_last_6mths","loan_status","loan_amnt","open_acc","pub_rec_cat","revol_util","loan_inc",
                                       "ann_instal_inc","length_credit_year","inc_cat","mths_since_last_delinq_cat"))
dataset_2=subset(dataset, select= c("purpose", "int_rate","grade","sub_grade", "home_ownership","delinq_2yrs","dti","inq_last_6mths","loan_status","loan_amnt","open_acc","pub_rec_cat","revol_util","loan_inc",
                                    "ann_instal_inc","length_credit_year","annual_inc","mths_since_last_delinq_cat"))
##split-sample
set.seed(1234)
idx.train_cat = createDataPartition(dataset_cat$loan_status, p=0.6, list=FALSE)
##60% default and non-default sample will be arranged into train set
train= dataset_cat[idx.train_cat, ]  
test= dataset_cat[-idx.train_cat, ]

##60% default and non-default sample will be arranged into train set
train_2= dataset_2[idx.train_cat, ]  
test_2= dataset_2[-idx.train_cat, ]
##structure models

set.seed(123)
lr_m1 = glm(loan_status~., data=train, family= binomial(link="logit"))
summary(lr_m1)
#negative sign of coefficient means more likely to be considered as bad
# 1-pchisq(43470-40330, 53045-51966): reject null hypothesis
install.packages("corrplot")
library(corrplot)
correlations= cor(dataset[,1:18])


lr_m1_2 = glm(loan_status~., data=train_2, family= binomial(link="logit"))
summary(lr_m1_2)

##since the p-values of chageoff_within_12_mths& purpose&delinq_2yrs&revol_util  are greater than 0.9
lr_m3 = glm(loan_status~.-purpose-delinq_2yrs_cat-revol_util-loan_amnt-loan_inc-length_credit_year-ann_instal_inc, data=train, family= binomial(link="logit")) ##AIC:30609
##using stepwise, with glm_stepwise the steps of model selection could be seen
#basic= glm(loan_status~1, data=train, family = binomial(link="logit"))
#glm_stepwise= stepAIC(basic, scope=list(lower=basic, upper=lr_m1), direction="forward", trace=TRUE, steps=100)
#stepvar= glm_stepwise$coefficients
lr_m4= logit.fit.reduced = step(lr_m1) ##AIC: 30600,14
#prp(dt_m1)
#test of the model of training samples
ytrain.lr_m1= predict(lr_m1, newdata=train, type = "response")
ytrain.lr_m3= predict(lr_m3, newdata=train, type = "response")
ytrain.lr_m4= predict(lr_m4, newdata=train, type = "response")
tau=0.5
ytrain.lr.class1= factor(ytrain.lr_m1>tau, labels = c("bad", "good"))
ytrain.lr.class3= factor(ytrain.lr_m3>tau, labels = c("bad", "good"))
ytrain.lr.class4= factor(ytrain.lr_m4>tau, labels = c("bad", "good"))

#confusionmatrix for training samples
cm.lr_train=confusionMatrix(data=ytrain.lr.class1, reference =train$loan_status)
cm.lr3_train=confusionMatrix(data=ytrain.lr.class3, reference =train$loan_status)
cm.lr4_train=confusionMatrix(data=ytrain.lr.class4, reference =train$loan_status)

##predicting probability of default in test samples
yhat.lr_m1= predict(lr_m1, newdata=test, type = "response")
yhat.lr_m3= predict(lr_m3, newdata=test, type = "response")
yhat.lr_m4= predict(lr_m4, newdata=test, type = "response")
yhat.lr_m1_2= predict(lr_m1_2, newdata=test_2, type = "response")

#evaluate the logistic model result
yhat.lr.class1= factor(yhat.lr_m1>tau, labels = c("bad", "good"))
yhat.lr.class3= factor(yhat.lr_m3>tau, labels = c("bad", "good"))
yhat.lr.class4= factor(yhat.lr_m4>tau, labels = c("bad", "good"))

tau= 0.6
##predicting probability of default in test samples
yhat.lr_m11= predict(lr_m1, newdata=test, type = "response")
yhat.lr_m31= predict(lr_m3, newdata=test, type = "response")
yhat.lr_m41= predict(lr_m4, newdata=test, type = "response")

#evaluate the logistic model result
yhat.lr.class11= factor(yhat.lr_m11>tau, labels = c("bad", "good"))
yhat.lr.class31= factor(yhat.lr_m31>tau, labels = c("bad", "good"))
yhat.lr.class41= factor(yhat.lr_m41>tau, labels = c("bad", "good"))
#confusionmatrix
cm.lr_test1= confusionMatrix(data=yhat.lr.class11, reference =test$loan_status)
cm.lr3_test1= confusionMatrix(data=yhat.lr.class31, reference =test$loan_status, positive = "bad")
cm.lr4_test1= confusionMatrix(data=yhat.lr.class41, reference =test$loan_status, positive = "bad")

#comparing the result of test and train sample, the tree decision work worsely in test sample
#ROC
prediction.roc_lr = data.frame(LR= yhat.lr_m1,LR3= yhat.lr_m3,LR4= yhat.lr_m4,LR_2=yhat.lr_m1_2)
h_lr= HMeasure(true.class = as.numeric(test$loan_status=="good"), scores = prediction.roc_lr)
plotROC(h_lr,which = 1)
h_lr$metrics["AUC"] ##LR3 is the best:0.68166

exp(coef(lr_m1)) #odds ratios


#the AUC of stepwise is 0.6815

##CARTS 
prior_candidates = data.frame("prior"=seq(0.05,1,0.05), "AUC"=NA)
for(x in 1:nrow(prior_candidates)){
  dt_train =rpart(loan_status~.,, data = train, method="class", parms=list(prior=c(prior_candidates$prior[x], 1-prior_candidates$prior[x])),
                   control=rpart.control(cp = 0.001))
  yhat_train = as.vector(predict(dt_train, newdata=test, type="prob")[,2])
  prior_candidates[x,"AUC"] = ModelMetrics::auc(as.numeric(test$loan_status=="good"), yhat_train)
}
optimal =prior_candidates[which.max(prior_candidates$AUC),]  ##--> parms=list(prior=c(0.6,0.4)
dt_m1 = rpart(loan_status~., data = train, method="class", parms=list(prior=c(0.5,0.5)), control = rpart.control(cp=0.001)) 
##unbalanced, since more than 85% data are good. Including the argument parms and changing the proportion of good to x  and bad to 1-x. using loops to find a paar of proportion 
#with highest AUC
#plot(dt_m1, uniform = T) 
#text(dt_m1) #prp(dt_m1)  #plot the trees
#plotcp(dt_m1) #plot the cp
#printcp(dt_m1)
cp_min= dt_m1$cptable[which.min(dt_m1$cptable[,"xerror"]), "CP"]  #gettinge the smallest xerror cross-validation error
dt_m2 = rpart(loan_status~., data = train, method="class", parms=list(prior=c(0.5,0.5)), control = rpart.control(cp=cp_min)) 
prp(dt_m2)
#ytrain.dt_m1= predict(dt_m1, newdata=train, type = "prob")[,2]  
#ytrain.dt.class1= factor(ytrain.dt_m1>tau, labels = c("bad", "good"))
#cm.dt_train= confusionMatrix(data=ytrain.dt.class1, reference = train$loan_status)
yhat.dt_m1= predict(dt_m1, newdata=test, type = "prob")[,2]
yhat.dt_m1.class= factor(yhat.dt_m1>0.5, labels = c("bad","good"))##gettinge the results of prediction
cm.dt_test=confusionMatrix(data=yhat.dt_m1.class, reference = test$loan_status)
yhat.dt_m2= predict(dt_m2, newdata=test, type = "prob")[,2]
yhat.dt_m2.class= factor(yhat.dt_m2>0.5, labels = c("bad","good"))##gettinge the results of prediction
cm.dt2_test=confusionMatrix(data=yhat.dt_m2.class, reference = test$loan_status)

yhat.dt_m1.class_0.3= factor(yhat.dt_m1>0.3, labels = c("bad","good"))##gettinge the results of prediction
cm.dt_test_0.3=confusionMatrix(data=yhat.dt_m1.class_0.3, reference = test$loan_status)
yhat.dt_m2.class_0.3= factor(yhat.dt_m2>0.3, labels = c("bad","good"))##gettinge the results of prediction
cm.dt2_test_0.3=confusionMatrix(data=yhat.dt_m1.class_0.3, reference = test$loan_status)

yhat.dt_m1.class_0.2= factor(yhat.dt_m1>0.2, labels = c("bad","good"))##gettinge the results of prediction
cm.dt_test_0.2=confusionMatrix(data=yhat.dt_m1.class_0.2, reference = test$loan_status)
yhat.dt_m2.class_0.2= factor(yhat.dt_m2>0.2, labels = c("bad","good"))##gettinge the results of prediction
cm.dt_test_0.2=confusionMatrix(data=yhat.dt_m1.class_0.2, reference = test$loan_status)
#yhat.dt_m1.class_0.15= ifelse(yhat.dt_m1>0.15, "good","bad")
#yhat.dt_m1.class_0.15= factor(yhat.dt_m1.class_0.15, levels=c("bad","good"))
#cm.dt_test_0.15=confusionMatrix(data=yhat.dt_m1.class_0.15, reference = test$loan_status)  ##if cutoff is 0.15, all samples are good
##minbucket=5
dt_m1.3= rpart(loan_status~., data = train, method="class", parms=list(prior=c(0.7,0.3)), control = rpart.control(cp=cp_min,minbucket=5) )
prp(dt_m1.3, extra = 104, border.col = 0)
yhat.dt_m3= predict(dt_m1.3, newdata=test, type = "prob")[,2]

dt_m4= dt_m1 = rpart(loan_status~., data = train, method="class",parms=list(loss=matrix(c(0, 10, 1,0), ncol=2)), control = rpart.control(cp=0.001)) 
yhat.dt_m4= predict(dt_m1.3, newdata=test, type = "prob")[,2]
prp(dt_m4) ##including a loss matrix does not work
#evaluate the models with AUC
prediction.roc = data.frame(DT1=yhat.dt_m1, DT2=yhat.dt_m2, DT3=yhat.dt_m3)
DThh= HMeasure(true.class = as.numeric(test$loan_status=="good"), scores = prediction.roc)
plotROC(DThh,which = 1)
DThh$metrics["AUC"] ##LR3 is the best:0.6688

prediction.roc = data.frame(LR= yhat.lr_m1,LR3= yhat.lr_m3,LR4= yhat.lr_m4, DT1=yhat.dt_m1, DT2=yhat.dt_m2, DT3=yhat.dt_m3)
htot= HMeasure(true.class = as.numeric(test$loan_status=="good"), scores = prediction.roc)
plotROC(htot,which = 1)
htot$metrics["AUC"] ##LR3 is the best:0.68166

###===========================combating imblanced data with undersampling or oversampling============================================
# Standardize the data. This is important for distance calculations and for numeric optimization later on.
# SMOTE finds the nearest neigbors to minority sampling based on their distance
std.values <- caret::preProcess(train, method = c("center", "scale"))
tr <- predict(std.values, newdata = train)
ts <- predict(std.values, newdata = test)
modelLib <- list()
pred <- list()
# Build a default logit model on the training data
modelLib[["standard"]] <- glm(loan_status~., data = tr, family = binomial(link = "logit"))
pred[["standard"]] <- predict(modelLib[["standard"]], newdata = ts, type = "response")
# Compare predicted probabilities to class ratio
table(ts$loan_status)/nrow(ts)
quantile(pred[["standard"]], probs = seq(0,1,0.1))

#### Undersampling the majority class ####
majority_samples <- sample(which(tr$loan_status == "good"), size = 2870, replace = FALSE)
tr.undersampling <- tr[c( which(tr$loan_status == "bad"), majority_samples),]
table(tr.undersampling$loan_status)

# Train a logit model on the undersampled data
modelLib[["undersampling"]] <- glm(loan_status~., data = tr.undersampling, family = binomial(link = "logit"))
pred[["undersampling"]] <- predict(modelLib[["undersampling"]], newdata = ts, type = "response")

#### Oversampling the minority class ####
# In this case, we oversample on top of the undersampling to further emphasize class 'loan_status'
#Be careful with oversampling! It might lead to overfitting. Do not do it before cross-validation, it would defy the whle purpose if you have twins in train and test sets.
minority_samples <- sample(which(tr.undersampling$loan_status == "bad"), size = 2870, replace = FALSE)
tr.over_and_under <- tr.undersampling[c(1:nrow(tr.undersampling), minority_samples),]
table(tr.over_and_under$loan_status)

modelLib[["under_and_over"]] <- glm(loan_status~., data = tr.over_and_under, family = binomial(link = "logit"))
pred[["under_and_over"]] <- predict(modelLib[["under_and_over"]], newdata = ts, type = "response")

# Compare predicted probabilities for loan_statusing
library(reshape2)
pred.frame <- melt(data.frame(pred))  
library(ggplot2)
ggplot(pred.frame) + geom_density(aes(x = value, col = variable), stat = "density") + xlab("Prob. predictions") + ylab("Density")

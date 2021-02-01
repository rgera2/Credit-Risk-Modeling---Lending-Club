#Assignment 2
###Rahul Gera
###Robin Beura
###Namrata Shindalya

library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(caret)
# The lcData4m.csv file contains data on 3 year loans issues in the first 4 months of 2015, which we will use for this analyses

lcdf <- read.csv('C:/Users/rahul/Documents/572/assignment1/lcData4m.csv')
#Explore the data
str(lcdf)

#Finding proportion of nulls for each variable
nulls_df <- as.data.frame(sapply(lcdf,function(x){round(sum(is.na(x))*100/NROW(x),2)}))
nulls_df <- cbind(variable = row.names(nulls_df), nulls_df)
colnames(nulls_df) <- c("variable","cnt_null")

#Removed nulls at the starting so that dataframe size reduces for future calculations
# Removed variables which have nulls over 60% of their data
nulls_df <- filter(nulls_df,cnt_null < 60)
col <- as.character(nulls_df$variable)
lcdf <- subset(lcdf, select = col)

###interest rate varying with grades
lcdf$int_rate <- gsub("%", "", lcdf$int_rate)
lcdf$int_rate <- as.numeric(lcdf$int_rate)

#calculate the annualized percentage return
lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100

#Some loans are paid back early - find out the actual loan term in months
#  Since last_pymnt_d is a chr variable, we need to covert it to a date var
lcdf$last_pymnt_d<-paste(lcdf$last_pymnt_d, "-01", sep = "")
lcdf$last_pymnt_d<-parse_date_time(lcdf$last_pymnt_d,  "myd")

lcdf$actualTerm <- ifelse(lcdf$loan_status=="Fully Paid", as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)/dyears(1), 3)
#summarize by grade
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), avgRet=mean(annRet), stdRet=sd(annRet), minRet=min(annRet), maxRet=max(annRet))

##check actualReturn again
#Then, considering this actual term, we can calculate the actual annual return
### Assuming if actual term is less than 1, than actual term is 1
lcdf$actualReturn <- ifelse(lcdf$actualTerm >0 ,((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(1/lcdf$actualTerm),((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt))
summary(lcdf$actualReturn)

#sum(ifelse(lcdf$total_pymnt==0,1,0))
## 19 people have total_pymnt = 0
#For cost-based performance, we want to see the average interest rate, and the average of proportion of loan amount paid back, grouped by loan_status
lcdf %>% group_by(loan_status) %>% summarise(avgret = mean(actualReturn),intRate=mean(int_rate), totRet=mean((total_pymnt-funded_amnt)/funded_amnt))

# Notice that the totRet on Charged Off loans as -0.366, so, for every dollar invested, there is a loss of .366 cents.   For Fully Paid loans, the totRet seems less than what may be  expected from intRate -- how do you explain this?
# because duration is not always a year
#you may like to look at some of these variables
#lcdf %>% select(loan_status, loan_amnt, funded_amnt, total_pymnt, int_rate, actualTerm, actualReturn ) %>% view()

#some more summaries
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), defaultRate=defaults/nLoans, avgret = mean(actualReturn),intRate=mean(int_rate), totRet=mean((total_pymnt-funded_amnt)/funded_amnt) ,term = median(actualTerm))
### based on avg ret, invest in D (till now)

#convert emp_length to factor -- can order the factors in  a meaningful way
lcdf$emp_length <- factor(lcdf$emp_length, levels=c("n/a", "< 1 year","1 year","2 years", "3 years" ,  "4 years",   "5 years",   "6 years",   "7 years" ,  "8 years", "9 years", "10+ years" ))

lcdf$purpose <- fct_recode(as.factor(lcdf$purpose), other="wedding", other="renewable_energy")


#Note - character variables can cause a problem with some model packages, so better to convert all of these to factors

lcdf= lcdf %>% mutate_if(is.character, as.factor)

#Some derived attributes

#Derived attribute: proportion of satisfactory bankcard accounts 
lcdf$propSatisBankcardAccts <- ifelse(lcdf$num_bc_tl>0, lcdf$num_bc_sats/lcdf$num_bc_tl, 0)

#Another one - lets calculate the length of borrower's history with LC
#  i.e time between earliest_cr_line and issue_d
lcdf$earliest_cr_line<-paste(lcdf$earliest_cr_line, "-01", sep = "")
lcdf$earliest_cr_line<-parse_date_time(lcdf$earliest_cr_line, "myd")

#or we can use the lubridate functions to precidely handle date-times durations
lcdf$borrHistory <- as.duration(lcdf$earliest_cr_line %--% lcdf$issue_d  ) / dyears(1)

#Another new attribute: ratio of openAccounts to totalAccounts

### proportion of revolving accounts
lcdf$propRevAccts <- ifelse(lcdf$num_rev_accts>0, lcdf$num_op_rev_tl/lcdf$num_rev_accts, 0)
#### drop parent variables
lcdf <- lcdf %>% dplyr :: select(-c(num_rev_accts,num_op_rev_tl,num_bc_sats))

### proportion of active bankcard accounts
lcdf$propActiveBankcardAccts <- ifelse(lcdf$num_bc_tl>0, lcdf$num_actv_bc_tl/lcdf$num_bc_tl, 0)
#### drop parent variable
lcdf <- lcdf %>% select(-c(num_bc_tl,num_actv_bc_tl))

####fico ranges to mean fico score
lcdf$fico_avg <- (lcdf$fico_range_high + lcdf$fico_range_low)/ 2
summary(lcdf$fico_avg)

##### dropping the range now as average is present
lcdf <- lcdf %>% select(-c(fico_range_low, fico_range_high))

####using grade as grade_rank variable
### AS grade is an ordinal variable so we can replace characters to rank
### It would be easier for my decision tree to pick it
lcdf$grade_rank <- as.factor(as.numeric(lcdf$grade))

#### dropping grades now
lcdf <- lcdf %>% select(-grade)
#Drop some other columns which are not useful and those which will cause 'leakage'
fut_leak <- c("collection_recovery_fee","last_fico_range_high","last_fico_range_low","funded_amnt","funded_amnt_inv","issue_d","last_credit_pull_d","last_pymnt_amnt","last_pymnt_d","out_prncp","out_prncp_inv","pymnt_plan","recoveries","total_pymnt","total_pymnt_inv","total_rec_int","total_rec_late_fee","total_rec_prncp","total_rev_hi_lim")
lcdf <- lcdf %>% select(-fut_leak)

#some additional vars to drop
varsToRemove=c("earliest_cr_line","emp_title","int_rate", "num_accts_ever_120_pd", "application_type", "policy_code", "pub_rec", "pub_rec_bankruptcies","sub_grade","term","title","tot_coll_amt","zip_code")
lcdf <- lcdf %>% select(-varsToRemove)

#### removing some variables on the basis of analysis after creating decision trees.
varsToRemove_an <- c("open_acc",	"acc_now_delinq",	"chargeoff_within_12_mths",	"mo_sin_rcnt_rev_tl_op",	"mort_acc",	"num_tl_120dpd_2m",	"num_tl_90g_dpd_24m",	"pct_tl_nvr_dlq",	"tax_liens",	"total_bal_ex_mort",	"debt_settlement_flag",	"annRet",	"delinq_2yrs",	"collections_12_mths_ex_med",	"tot_cur_bal",	"delinq_amnt",	"num_tl_30dpd",	"tot_hi_cred_lim",	"hardship_flag")
lcdf <- lcdf %>% select(-varsToRemove_an)

nm<- names(lcdf)[colSums(is.na(lcdf))>0]
summary(lcdf[, nm])

###### Missing values picked above, replacing them now 
### used boxplots and summary for the table

lcdf$revol_util <- as.numeric(gsub("%","",lcdf$revol_util))
lcdf <- lcdf %>% replace_na(list(mths_since_last_delinq=640, revol_util=median(lcdf$revol_util, na.rm=TRUE),
                                bc_open_to_buy=median(lcdf$bc_open_to_buy, na.rm=TRUE),
                                bc_util=mean(lcdf$bc_util, na.rm=TRUE),mo_sin_old_il_acct=max(lcdf$mo_sin_old_il_acct, na.rm=TRUE),
                                mths_since_recent_bc=max(lcdf$mths_since_recent_bc, na.rm=TRUE),
                                mths_since_recent_inq=max(lcdf$mths_since_recent_inq, na.rm=TRUE),
                                num_tl_120dpd_2m=0,percent_bc_gt_75 = mean(lcdf$percent_bc_gt_75, na.rm=TRUE)))

### check once again
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]

#Next we will build some  models
#library(rpart)

#It can be useful to convert the target variable, loan_status to  a factor variable
lcdf$loan_status <- factor(lcdf$loan_status, levels=c("Fully Paid", "Charged Off"))
lcdf <- lcdf %>% select(-c(initial_list_status,home_ownership,verification_status,
                           inq_last_6mths,num_actv_rev_tl,num_sats,num_tl_op_past_12m,
                           percent_bc_gt_75,propSatisBankcardAccts,propActiveBankcardAccts))
#split the data into trn, tst subsets
lcdf_ann4 <- lcdf
lcdf_ann2 <- lcdf
########### above 2 df will be used later
lcdf <- lcdf %>% select(-c(actualReturn,actualTerm))
nr<-nrow(lcdf)

set.seed(22)
trnIndex<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
lcdfTrn <- lcdf[trnIndex, ]
lcdfTst <- lcdf[-trnIndex, ]
dim(lcdfTrn)
dim(lcdfTst)

library(ROSE)
lcdf_rose <- ROSE(loan_status ~., data = lcdfTrn, seed = 22)$data
table(lcdf$loan_status)
table(lcdf_rose$loan_status)

####
#lcdf_over <- ovun.sample(loan_status ~., data = lcdfTrn, method = "over", N = 110564, seed = 22)$data
#table(lcdf_over$loan_status)
#lcdf_both <- ovun.sample(loan_status ~., data = lcdfTrn, method = "both",p=0.5, seed = 22)$data
#table(lcdf_both$loan_status)
#lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", parms = list(split = "information"),control = rpart.control(minsplit=10, minbucket = 3, cp=0.0003))
#lcDT1 <- rpart(loan_status ~., data=lcdf_rose, method="class", parms = list(split = "information"),control = rpart.control(minsplit=30, cp=0.0003))
#lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", parms = list(split = "information"), control = rpart.control(cp = 0.0003, minsplit = 50))

######### Random forest from previous ###############
library('randomForest')
library('ranger')
#for reproducible results, set a specific value for the random number seed
#removed after exploring performance on DT
lcdf_rf <- lcdf_rose %>% select(-c(mths_since_last_delinq,total_acc,
                              mo_sin_old_il_acct,num_il_tl))

### Choosing best 13 variables
lcdf_rf2 <- lcdf_rf %>% select(c(addr_state,grade_rank,annual_inc,purpose,
                                 emp_length,bc_open_to_buy,avg_cur_bal,
                                 fico_avg,acc_open_past_24mths,revol_bal,
                                 mo_sin_rcnt_tl,total_bc_limit,dti,loan_status))

table(lcdf_rf2$loan_status)

##### for test
lcdfTst_rf2 <- lcdfTst %>% select(colnames(lcdf_rf2))


###lcdf_rose_x <- lcdf_rose_rf %>% select(-c(loan_status))
#rfModel = randomForest(loan_status ~., data=lcdf_rose_rf, mtry= 10,ntree=3, importance=TRUE )
#rfModel1 <- randomForest(loan_status ~., data=lcdf_rose_rf, mtry= 10,ntree=501, importance=TRUE )
rfModel2 <- randomForest(loan_status ~., data=lcdf_rf2,nodesize=10,ntree=201,maxnodes=3,importance=TRUE)
### Did cross-valrf_cv <- rfcv(lcdf_rose_x,lcdf_rose_rf$loan_status,cv.fold = 10,ntree=25)
#check the model -- see what OOB error rate it gives
##0.26 min for 500 but no improvement beyond 200
#rfModel1$err.rate
rfModel2$err.rate
#rf_cv$error.cv
#Variable importance
importance(rfModel2)
varImpPlot(rfModel2)

importance(rfModel2, type=1)  #is there a difference -- WHY?

#Evaluate performance
predTrn=predict(rfModel2,lcdf_rf2)
#predTrn=predict(rfModel2,lcdf_rose_rf)
table(pred = predTrn, true=lcdf_rf2$loan_status)
mean(predTrn == lcdf_rf2$loan_status)
###0.645
### test
table(pred = predict(rfModel2,lcdfTst_rf2), true=lcdfTst_rf2$loan_status)
mean(predict(rfModel2,lcdfTst_rf2) ==lcdfTst_rf2$loan_status)
###0.63
##CM
#pred          Fully Paid Charged Off
#Fully Paid       15026        1554
#Charged Off      8664        2543

#### function to find perfect threshold

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])}, perf@x.values, perf@y.values, pred@cutoffs)}

#aucPerf <-performance(pred, "prec", "rec")
#cutoffs <- data.frame(cut=aucPerf@alpha.values[[1]], rec=aucPerf@x.values[[1]],
             #prec=aucPerf@y.values[[1]])
#cutoffs <- cutoffs %>% mutate(f1_score = ((2*prec*rec)/(prec+rec)))


CTHRESH=0.442
predProbTrn=predict(rfModel2,lcdf_rf2, type='prob')
predTrnCT = ifelse(predProbTrn[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
predTrnCT <- as.factor(predTrnCT)
table(predTrnCT , true=lcdf_rf2$loan_status)
mean(predTrnCT == lcdf_rf2$loan_status)
##0.645 opt - 0.648
CTHRESH=0.442
predProbTst=predict(rfModel2,lcdfTst_rf2, type='prob')
predTstCT = ifelse(predProbTst[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
predTstCT <- as.factor(predTstCT)
table(predTstCT , true=lcdfTst_rf2$loan_status)
mean(predTstCT == lcdfTst_rf2$loan_status)
#0.615 - opt
library(ROCR)
score=predict(rfModel2,lcdfTst_rf2,type = "prob")[,"Charged Off"]
pred=prediction(score,lcdfTst_rf2$loan_status, label.ordering = c("Fully Paid", "Charged Off"))
#label.ordering here specifies the 'negative', 'positive' class labels  
#ROC curve
aucPerf <-performance(pred, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)
print(opt.cut(aucPerf, pred))

#AUC value
aucPerf=performance(pred, "auc")
aucPerf@y.values
###0.675

## sen - 0.65
## spec -0.60
## cutoff - 0.44
###########################RF model ends##################################



lcdf_rose_glm <- lcdf_rf
lcdf_rose_glm2 <- lcdf_rf2
### set charged off as positive values
lcdf_rose_glm2 <- lcdf_rose_glm2 %>% mutate(loan_status = ifelse(loan_status == "Charged Off",1,0))

lcdfTst_glm <- lcdfTst %>% select(-c(mths_since_last_delinq,total_acc,
                                   mo_sin_old_il_acct,num_il_tl))

lcdfTst_glm <- lcdfTst_glm %>% mutate(loan_status = ifelse(loan_status == "Charged Off",1,0))
lcdfTst_glm2 <- lcdfTst_glm %>% select(c(colnames(lcdf_rose_glm2)))
#1.b1 Developing a GLM model.
lcdf_glm <- glm(loan_status~.,data = lcdf_rose_glm2,family=binomial(logit))
imp_glm <- varImp(lcdf_glm)
#With a different classsification threshold
CTHRESH=0.5
predProbTrn_glm = predict(lcdf_glm,lcdf_rose_glm2, type='response')
predTrnCT_glm = ifelse(predProbTrn_glm > CTHRESH, 1, 0)
predTrnCT_glm <- as.factor(predTrnCT_glm)
table(predTrnCT_glm , true=lcdf_rose_glm2$loan_status)
mean(predTrnCT_glm == lcdf_rose_glm2$loan_status)
##0.647 0.64(14 var)
CTHRESH=0.5
predProbTst_glm=predict(lcdf_glm,lcdfTst_glm2, type='response')
predTstCT_glm = ifelse(predProbTst_glm > CTHRESH, 1, 0)
table(predTstCT_glm,true=lcdfTst_glm2$loan_status)
mean(predTstCT_glm == lcdfTst_glm2$loan_status)
##0.62
predTstCT_glm <- as.factor(predTstCT_glm)

#Or you can use the confusionMatrix fuction from the caret package

library(ROCR)
pred_glm=prediction(predProbTst_glm, lcdfTst_glm2$loan_status, label.ordering = c(0,1))
#ROC curve
roc_glm <-performance(pred_glm, "tpr", "fpr")
print(opt.cut(roc_glm, pred_glm))
##sensitivity 0.66
##specificity 0.61
##cutoff      0.499
plot(roc_glm)
abline(a=0, b= 1)
#AUC value
auc_glm=performance(pred_glm, "auc")
auc_glm@y.values
##0.687
#PR Curve
RP.glm <-performance(pred_glm,"prec", "rec")
plot(RP.glm)

##### glm grade-wise analysis
cbind(lcdfTst_glm2,predTstCT_glm) %>% group_by(grade_rank) %>% summarize(nLoans = n(), 
              defaults = sum(loan_status=="1"), 
              def_rate = defaults/nLoans*100, 
              pred = sum(predTstCT_glm=="1"),
              pred_rate = pred/nLoans*100)


#########glm ends #######################

###Trying Ridge Regression
#install.packages("glmnet")
library(glmnet)
xd <-lcdf_rose_glm2 %>% dplyr :: select(-c(loan_status))
yd <- lcdf_rose_glm2$loan_status
test_xd <- lcdfTst_glm2 %>% dplyr :: select(-c(loan_status))
test_yd <- lcdfTst_glm2$loan_status
dim(xd)
#performing logistic regrssion so using family as binomial
#by default glmnet() uses 10 k fold validation method to determine best lamda value. thus we are using cv.glmnet.
#Alpha = 0, Ridge regression. Alpha = 1, Lasso regression.
#The default is type.measure="deviance", which uses squared-error for gaussian models (a.k.a type.measure="mse" there), deviance for logistic
#and poisson regression, and partial-likelihood for the Cox model. type.measure="class" applies to binomial and multinomial logistic regression only, and gives misclassification error. type.measure="auc" is for two-class logistic regression only,
#and gives area under the ROC curve
lcdf_rg <- cv.glmnet(data.matrix(xd),yd,alpha =0 ,type.measure = c("auc"), nfolds = 10, family="binomial")
print(lcdf_rg)
# lamda which gives minimum cross-validated error
lcdf_rg$lambda.min
#lamda which gives the most regularized model having error within 1 std error of the min error
lcdf_rg$lambda.1se  

lcdf_rg1 <- glmnet(data.matrix(xd),yd,alpha =0 ,lambda = lcdf_rg$lambda.min,type.measure = c("auc"), nfolds = 10, family="binomial")
PredTrain.F = predict(lcdf_rg1, data.matrix(xd), type="class")
PredTrain.F <- as.factor(PredTrain.F)
yd <- as.factor(yd)
confusionMatrix(PredTrain.F,yd, positive = "1")
##0.63 sen-0.6448
#####need to resolve error
PredTest.F <- predict(lcdf_rg1, data.matrix(test_xd), type="class")
PredTest.F <- as.factor(PredTest.F)
test_yd <- as.factor(test_yd)
confusionMatrix(PredTest.F,test_yd, positive="1")
##0.63 sen-0.64
plot(lcdf_rg) #need to understand this
assess.glmnet(lcdf_rg1, data.matrix(xd),yd, family = c("binomial"))
assess.glmnet(lcdf_rg1, data.matrix(test_xd),test_yd, family = c("binomial"))
###AUC - 0.688
confusion.glmnet(lcdf_rg1, data.matrix(test_xd),test_yd, family = c("binomial"))
plot(roc.glmnet(lcdf_rg1, data.matrix(test_xd),test_yd, family = c("binomial")))
abline(a=0, b= 1)

###Trying Lasso Regression
#install.packages("glmnet")
xd_la <-lcdf_rose_glm2 %>% select(-c(loan_status))
yd_la <- lcdf_rose_glm2$loan_status
lcdf_la <- cv.glmnet(data.matrix(xd_la),yd_la,alpha =1 ,type.measure = ("auc"), nfolds = 10, family="binomial")
print(lcdf_la)
# lamda which gives minimum cross-validated error
lcdf_la$lambda.min
#lamda which gives the most regularized model having error within 1 std error of the min error
lcdf_la$lambda.1se  
lcdf_la1 <- glmnet(data.matrix(xd),yd,alpha =0 ,lambda = lcdf_la$lambda.min,type.measure = c("auc"), nfolds = 10, family="binomial")
plot(lcdf_la) #need to understand this
assess.glmnet(lcdf_la1, data.matrix(xd_la),yd_la, family = c("binomial"))
assess.glmnet(lcdf_la1, data.matrix(test_xd),test_yd, family = c("binomial"))
##AUC - 0.688
confusion.glmnet(lcdf_la, data.matrix(xd_la),yd_la, family = c("binomial"))
confusion.glmnet(lcdf_la, data.matrix(test_xd),test_yd, family = c("binomial"))
plot(roc.glmnet(lcdf_la, data.matrix(xd_la),yd_la, family = c("binomial")))

###Trying Generalized boosted regression model - vanilla case
#install.packages("gbm")
library(gbm)

######################################################################################
#Vanilla case
#install.packages("xgboost")
library('xgboost')

boostTrn<-model.matrix(~.+0, data=lcdf_rose_glm2)
boostTst<-model.matrix(~.+0, data=lcdfTst_glm2)
dim(boostTrn)
dim(boostTst)
#train-error:0.242886 when nrounds = 1000
#train-error:0.271157 when maxdepth = 4
#train-error:0.211901 when maxdepth = 5
#train-error:0.169101 when maxdepth = 6(default)
#train-error:0.229036 when eta = 0.07

btrain<-xgb.DMatrix(subset(boostTrn,select=-c(loan_status)), label=boostTrn[,"loan_status"])
btest<-xgb.DMatrix(subset(boostTst,select=-c(loan_status)), label=boostTst[,"loan_status"])

xg<-xgboost( data = btrain, 
             nrounds= 100, 
             max.depth=6, 
             nfold = 5, 
             eta=0.3, 
             subsample = 1,
             eval_metric = "auc",
             objective="binary:logistic")
#With a different classsification threshold
CTHRESH=0.5
predTrn_boost1 = predict(xg,btrain, type='response')
predTrnCT_boost1 = ifelse(predTrn_boost1 > CTHRESH, 1,0)
predTrnCT_boost1 <- as.factor(predTrnCT_boost1)
table(predTrnCT_boost1 , true=lcdf_rose_glm2$loan_status)
mean(predTrnCT_boost1 == lcdf_rose_glm2$loan_status)
##0.772
CTHRESH=0.5
predTst_boost1=predict(xg,btest, type='response')
predTstCT_boost1 = ifelse(predTst_boost1 > CTHRESH, 1, 0)
predTstCT_boost1 <- as.factor(predTstCT_boost1)
table(predTstCT_boost1 , true=lcdfTst_glm2$loan_status)
mean(predTstCT_boost1 == lcdfTst_glm2$loan_status)
##0.59


boost_pred1=prediction(predTst_boost1, lcdfTst_glm2$loan_status, label.ordering = c(0,1))

#ROC curve
aucboostPerf1 <-performance(boost_pred1, "tpr", "fpr")
plot(aucboostPerf1)
abline(a=0, b= 1)

print(opt.cut(aucboostPerf1, boost_pred1))

#AUC value
aucboostPerf1=performance(boost_pred1, "auc")
aucboostPerf1@y.values
#0.672

#Lift curve
liftboostPerf <-performance(boost_pred1, "lift", "rpp")
plot(liftboostPerf)


############################################################################################################################
#Performing cross validation to get the parameters to do xgboost
btrain1<-xgb.DMatrix(subset(boostTrn,select=-c(loan_status)), label=boostTrn[,"loan_status"])

params <- expand.grid(
  booster = "gbtree",
  subsample=c(0.7,1), #subsample fraction for training data
  #for trees
  nrounds = c(900),
  nfold = c(5),
  max.depth=c(4,5,6),
  eta = c(0.01,0.03), #learning rate
  eval_metric = c("auc"),
  best_tree = 0,
  best_auc = 0
)


for(i in 1:nrow(params)){
  xgbcv <- xgb.cv(data = btrain1, 
                  nrounds = params$nrounds[i], nfold = params$nfold[i], 
                  booster = params$booster[i],
                  subsample = params$subsample[i],
                  max.depth = params$max.depth[i],
                  eta = params$eta[i],
                  eval_metric = params$eval_metric[i],
                  objective="binary:logistic", 
                  showsd = T
                  ,stratified = T  #stratify=T, we'll ensure that distribution of target class is maintained in the resampled data sets.
                  ,print_every_n = 1
                  ,early_stop_round = 20 #Will train until test_error hasn't improved in 20 rounds
                  ,maximize = F)
  
  params$best_tree[i] <-which.max(xgbcv$evaluation_log$test_auc_mean)
  params$best_auc[i] <-max(xgbcv$evaluation_log$test_auc_mean)
}

#Performing xgboost after providing input to parameters from cross validation.
### cv gives us 730 as best nrounds
xgbParams<-expand.grid(
  nrounds = 730,
  subsample=0.8,  #subsample fraction for training data
  #for trees
  nfold = 5, 
  max.depth=c(3),
  eta = c(0.01), #learning rate
  eval_metric = c("auc")
)

for(i in 1:nrow(xgbParams)){
  xg_1<-xgboost( data = btrain1, 
                 nrounds= xgbParams$nrounds[i], 
                 max.depth=xgbParams$max.depth[i], 
                 nfold = xgbParams$nfold, 
                 eta=xgbParams$eta[i], 
                 subsample = xgbParams$subsample[i],
                 eval_metric = xgbParams$eval_metric[i],
                 objective="binary:logistic")
}
summary(xg_1)

# Get the feature real names
names <- dimnames(btrain1)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xg_1)
View(importance_matrix)

# Nice graph
xgb.plot.importance(importance_matrix[1:40,])


#With a different classsification threshold
CTHRESH=0.5
predTrn_boost = predict(xg_1,btrain1, type='response')
predTrnCT_boost = ifelse(predTrn_boost > CTHRESH, 1,0)
predTrnCT_boost <- as.factor(predTrnCT_boost)
table(predTrnCT_boost , true=lcdf_rose_glm2$loan_status)
mean(predTrnCT_boost == lcdf_rose_glm2$loan_status)
#0.69
CTHRESH=0.5
predTst_boost=predict(xg_1,btest, type='response')
predTstCT_boost = ifelse(predTst_boost > CTHRESH, 1, 0)
predTstCT_boost <- as.factor(predTstCT_boost)
table(predTstCT_boost , true=lcdfTst_glm2$loan_status)
mean(predTstCT_boost == lcdfTst_glm2$loan_status)
##0.64

boost_pred=prediction(predTst_boost, lcdfTst_glm2$loan_status, label.ordering = c(0,1))


#ROC curve
aucboostPerf <-performance(boost_pred, "tpr", "fpr")
plot(aucboostPerf)
abline(a=0, b= 1)

print(opt.cut(aucboostPerf, boost_pred))

#AUC value
aucboostPerf=performance(boost_pred, "auc")
aucboostPerf@y.values
#0.69
########## grad-wise analysis ########

cbind(lcdfTst_glm2,predTstCT_boost) %>% 
  group_by(grade_rank) %>% 
  summarize(nLoans = n(), defaults = sum(loan_status=="1"),
            def_rate = defaults/nLoans*100, 
            pred = sum(predTstCT_boost=="1"),
            pred_rate = pred/nLoans*100)
#Lift curve
liftboostPerf <-performance(boost_pred, "lift", "rpp")
plot(liftboostPerf)

##########deciles for gbm
predTstCT_boost_gbm <- (as.numeric(predTstCT_boost1))-1
pred_Tst_tile1 <- lcdfTst_glm2 %>% select(grade_rank, loan_status) %>%  mutate(predTstCT_boost_gbm)
pred_Tst_tile1 <- pred_Tst_tile1 %>% mutate(tile=ntile(-predTstCT_boost_gbm, 10))
decile_boost_test<- pred_Tst_tile1 %>% group_by(tile) %>% summarise(nloans = n(), pred_defaults=sum(predTstCT_boost_gbm=="1"), 
                                                                    act_defaults= sum(loan_status == "1"), pred_def_rate = (pred_defaults/nloans)*100, 
                                                                    act_def_rate =(act_defaults/nloans)*100, Fully_paid_rate = 100-pred_def_rate, 
                                                                    Fully_paid_rate_act = 100-act_def_rate, totA=sum(grade_rank=="1"),
                                                                    totB=sum(grade_rank=="2" ), totC=sum(grade_rank=="3"), totD=sum(grade_rank=="4"), totE=sum(grade_rank=="5"),
                                                                    totF=sum(grade_rank=="6"), totG=sum(grade_rank=="7"))


###########deciles for glm
predTstCT_glm_new <- (as.numeric(predTstCT_glm))-1
pred_Tst_tile <- lcdfTst_glm2 %>% select(grade_rank, loan_status) %>%  mutate(predTstCT_glm_new)
pred_Tst_tile <- pred_Tst_tile %>% mutate(tile=ntile(-predTstCT_glm_new, 10))
decile_glm_test<- pred_Tst_tile %>% group_by(tile) %>% summarise(nloans = n(), pred_defaults=sum(predTstCT_glm_new=="1"), 
                                                                 act_defaults= sum(loan_status == "1"), pred_def_rate = (pred_defaults/nloans)*100, 
                                                                 act_def_rate =(act_defaults/nloans)*100, Fully_paid_rate = 100-pred_def_rate, 
                                                                 Fully_paid_rate_act = 100-act_def_rate, totA=sum(grade_rank=="1"),
                                                                 totB=sum(grade_rank=="2" ), totC=sum(grade_rank=="3"), totD=sum(grade_rank=="4"), totE=sum(grade_rank=="5"),
                                                                 totF=sum(grade_rank=="6"), totG=sum(grade_rank=="7"))


###################### 2nd Question starts here##############################

library(ranger)
dim(lcdf_ann2)
str(lcdf_ann2)
### return dataframe
lcdf_ann2 <- lcdf_ann2 %>% select(-c(actualTerm))
######## 14 variables #####
lcdf_ann2_temp <- lcdf_ann2 %>% select(c(colnames(lcdf_rf2)))
lcdf_ann2 <- cbind(lcdf_ann2_temp,actualReturn = lcdf_ann2$actualReturn)

###exploring data
lcdf_ann_num <- lcdf_ann2 %>% select_if(is.numeric)
summary(lcdf_ann_num)

#### outlier treatment
lcdf_ann_num_x <- lcdf_ann_num %>% select(-c(actualReturn))
lcdf_ann_num_y <- lcdf_ann_num$actualReturn

### Tukey's method using IQR
for(i in 1:ncol(lcdf_ann_num_x)){
  Q3<-quantile(lcdf_ann_num_x[,i],0.75)
  Q1<-quantile(lcdf_ann_num_x[,i],0.25)
  IQR<-(Q3-Q1)
  left<- (Q1-(1.5*IQR))
  right<- (Q3+(1.5*IQR))
  lcdf_ann_num_x[,i] <- ifelse(lcdf_ann_num_x[,i]<left,left,
                               ifelse(lcdf_ann_num_x[,i]>right,right,
                                      lcdf_ann_num_x[,i]))
}

corr <- as.data.frame(cor(lcdf_ann_num_x))

lcdf_ann_cat <- lcdf_ann2 %>% select(-c(colnames(lcdf_ann_num)))
lcdf_ann_new <- as.data.frame(cbind(lcdf_ann_num_x,lcdf_ann_cat,actualReturn =lcdf_ann_num_y))

#### checking anova
summary(aov(actualReturn~grade_rank+addr_state+purpose+emp_length,lcdf_ann_new))
#### categorical variables are important

nr_ann<-nrow(lcdf_ann_new)
set.seed(22)
trnIndex_ann<- sample(1:nr_ann, size = round(0.70*nr_ann), replace=FALSE)
lcdfTrn_ann <- lcdf_ann_new[trnIndex, ]
lcdfTst_ann <- lcdf_ann_new[-trnIndex, ]
dim(lcdfTrn_ann)
dim(lcdfTst_ann)

###############glm model ###############

lcdf_ann_glm <- glm(actualReturn~.,data=subset(lcdfTrn_ann,select = -c(loan_status)),family = gaussian)
#lcdf_glml <- glm(actualReturn~.,data=lcdf_ann,family = gaussian)
#lcdf_glml <- glm(actualReturn~.,data=lcdf_ann,family = gaussian)

##Results
sqrt(mean((lcdf_ann_glm$linear.predictors - lcdfTrn_ann$actualReturn)^2))
#train rmse 0.0839
sqrt(mean(((predict(lcdf_ann_glm, lcdfTst_ann)) - lcdfTst_ann$actualReturn)^2))
##test rmse 0.0849 
plot(predict(lcdf_ann_glm, lcdfTrn_ann),lcdfTrn_ann$actualReturn)
plot(predict(lcdf_ann_glm, lcdfTst_ann),lcdfTst_ann$actualReturn)

#####Ridge#####
xd_ann <-lcdfTrn_ann %>% dplyr :: select(-c(actualReturn,loan_status))
yd_ann <- lcdfTrn_ann$actualReturn
test_xd_ann <- lcdfTst_ann %>% dplyr :: select(-c(actualReturn,loan_status))
test_yd_ann <- lcdfTst_ann$actualReturn
##### cross-validated
lcdf_ann_rg_cv <- cv.glmnet(data.matrix(xd_ann),yd_ann,alpha =0 ,type.measure = c("mse"), nfolds = 10, family="gaussian")
opt_lambda <- lcdf_ann_rg_cv$lambda.min
lcdf_ann_rg <- lcdf_ann_rg_cv$glmnet.fit

##Results
sqrt(mean((predict(lcdf_ann_rg, s = opt_lambda, newx = data.matrix(xd_ann)) - yd_ann)^2))
#train rmse 0.084
sqrt(mean((predict(lcdf_ann_rg, s = opt_lambda, newx = data.matrix(test_xd_ann)) - test_yd_ann)^2))
##test rmse 0.085 
plot(predict(lcdf_ann_rg, s = opt_lambda, newx = data.matrix(xd_ann)),yd_ann)
plot(predict(lcdf_ann_rg, s = opt_lambda, newx = data.matrix(test_xd_ann)),test_yd_ann)

#####Lasso#####
##### cross-validated
lcdf_ann_la_cv <- cv.glmnet(data.matrix(xd_ann),yd_ann,alpha =1 ,type.measure = c("mse"), nfolds = 10, family="gaussian")
opt_lambda_la <- lcdf_ann_la_cv$lambda.min
lcdf_ann_la <- lcdf_ann_la_cv$glmnet.fit

##Results
sqrt(mean((predict(lcdf_ann_la, s = opt_lambda_la, newx = data.matrix(xd_ann)) - yd_ann)^2))
#train rmse 0.084
sqrt(mean((predict(lcdf_ann_la, s = opt_lambda_la, newx = data.matrix(test_xd_ann)) - test_yd_ann)^2))
##test rmse 0.085 
plot(predict(lcdf_ann_rg, s = opt_lambda, newx = data.matrix(xd_ann)),yd_ann)
plot(predict(lcdf_ann_rg, s = opt_lambda, newx = data.matrix(test_xd_ann)),test_yd_ann)


##### Random Forest
lcdf_ann_rf <- ranger(actualReturn~.,data = subset(lcdfTrn_ann,select = -c(loan_status)),num.trees=401,importance='permutation')

##Results
sqrt(mean((lcdf_ann_rf$predictions - lcdfTrn_ann$actualReturn)^2))
#train rmse 0.0845(501) 0.0849(201) 0.0846(401)
sqrt(mean(((predict(lcdf_ann_rf, lcdfTst_ann))$predictions - lcdfTst_ann$actualReturn)^2))
##test rmse 0.085(501) 0.0855(201) 0.0854(401)
plot(predict(lcdf_ann_rf, lcdfTrn_ann)$predictions,lcdfTrn_ann$actualReturn)
plot(predict(lcdf_ann_rf, lcdfTst_ann)$predictions,lcdfTst_ann$actualReturn)

#### GBM

library('xgboost')

boostTrn_ann<-model.matrix(~.+0, data=lcdfTrn_ann%>% select(-c('loan_status')))
boostTst_ann<-model.matrix(~.+0, data=lcdfTst_ann%>% select(-c('loan_status')))
dim(boostTrn_ann)
dim(boostTst_ann)

#Performing xgboost after providing input to parameters from cross validation.
btrain_ann<-xgb.DMatrix(subset(boostTrn_ann,select = -c(actualReturn)),label=boostTrn_ann[,"actualReturn"])
btest_ann<-xgb.DMatrix(subset(boostTst_ann,select = -c(actualReturn)),label=boostTst_ann[,"actualReturn"])

xgbParams_ann<-expand.grid(
  nrounds = 930,
  nfold = 5, 
  max.depth=c(3),
  eta = c(0.01) #learning rate
)

for(i in 1:nrow(xgbParams_ann)){
  xg_1<-xgboost( data = btrain_ann, 
                 nrounds= xgbParams_ann$nrounds[i], 
                 max.depth=xgbParams_ann$max.depth[i], 
                 nfold = xgbParams_ann$nfold, 
                 eta=xgbParams_ann$eta[i], 
                 objective="reg:squarederror")
}
summary(xg_1)

##Results
sqrt(mean((predict(xg_1,btrain_ann)- subset(boostTrn_ann,select = c(actualReturn)))^2))
#train rmse 0.0834(930) 0.0832(1010) 0.0836(510)
sqrt(mean((predict(xg_1,btest_ann)- subset(boostTst_ann,select = c(actualReturn)))^2))
##test rmse 0.0848(930) 0.0849(1010) 0.085(510)
plot(predict(xg_1,btrain_ann),subset(boostTrn_ann,select = c(actualReturn)))
plot(predict(xg_1,btest_ann),subset(boostTst_ann,select = c(actualReturn)))

#xgb_Lin_Mcv <- xgb.cv(data = btrain_ann, nrounds = 5000, nfold = 5, eta=0.3, subsample=1, early_stopping_rounds=10, booster="gblinear", alpha=0.0001)

#xgbParams_ann2<-expand.grid(
# nrounds = 3021,
#nfold = 5, 
#max.depth=c(6),
#eta = c(0.3) #learning rate
#)


#for(i in 1:nrow(xgbParams_ann2)){
#  xg_2<-xgboost( data = btrain_ann, 
#                nrounds= xgbParams_ann2$nrounds[i], 
#               max.depth=xgbParams_ann2$max.depth[i], 
#              nfold = xgbParams_ann2$nfold, 
#             eta=xgbParams_ann2$eta[i], 
#            objective="reg:squarederror")
#}
#summary(xg_1)

##Results
#sqrt(mean((predict(xg_2,btrain_ann)- subset(boostTrn_ann,select = c(actualReturn)))^2))
#train rmse 1.02
#sqrt(mean((predict(xg_2,btest_ann)- subset(boostTst_ann,select = c(actualReturn)))^2))
##test rmse 9.4


###########Decile tables ##################
####gbm ####
######train#######

predXgbRet_Trn <- lcdfTrn_ann %>% select(grade_rank, loan_status, actualReturn) %>%
  mutate(predXgbRet=predict(xg_1,btrain_ann))
predXgbRet_Trn <- predXgbRet_Trn %>% mutate(tile=ntile(-predXgbRet, 10))
decile_gbm<- predXgbRet_Trn %>% group_by(tile) %>% summarise(count=n(), avgPredRet=mean(predXgbRet), numDefaults=sum(loan_status=="Charged Off"),
                                                             avgActRet=mean(actualReturn), minRet=min(actualReturn), maxRet=max(actualReturn), totA=sum(grade_rank=="1"),
                                                             totB=sum(grade_rank=="2" ), totC=sum(grade_rank=="3"), totD=sum(grade_rank=="4"), totE=sum(grade_rank=="5"),
                                                             totF=sum(grade_rank=="6"), totG=sum(grade_rank=="7"))

######test ######
predXgbRet_Tst <- lcdfTst_ann %>% select(grade_rank, loan_status, actualReturn) %>%
  mutate(predXgbRet=predict(xg_1,btest_ann))
predXgbRet_Tst <- predXgbRet_Tst %>% mutate(tile=ntile(-predXgbRet, 10))
decile_gbm_tst_ann<- predXgbRet_Tst %>% group_by(tile) %>% summarise(count=n(), avgPredRet=mean(predXgbRet), numDefaults=sum(loan_status=="Charged Off"),
                                                                 avgActRet=mean(actualReturn), minRet=min(actualReturn), maxRet=max(actualReturn), totA=sum(grade_rank=="1"),
                                                                 totB=sum(grade_rank=="2" ), totC=sum(grade_rank=="3"), totD=sum(grade_rank=="4"), totE=sum(grade_rank=="5"),
                                                                 totF=sum(grade_rank=="6"), totG=sum(grade_rank=="7"))

########glm###########
######train#########
predglmRet_Trn <- lcdfTrn_ann %>% select(grade_rank, loan_status, actualReturn) %>%
  mutate(predglmRet=predict(lcdf_ann_glm, lcdfTrn_ann))
predglmRet_Trn <- predglmRet_Trn %>% mutate(tile=ntile(-predglmRet, 10))
decile_glm<- predglmRet_Trn %>% group_by(tile) %>% summarise(count=n(), avgPredRet=mean(predglmRet), numDefaults=sum(loan_status=="Charged Off"),
                                                             avgActRet=mean(actualReturn), minRet=min(actualReturn), maxRet=max(actualReturn), totA=sum(grade_rank=="1"),
                                                             totB=sum(grade_rank=="2" ), totC=sum(grade_rank=="3"), totD=sum(grade_rank=="4"), totE=sum(grade_rank=="5"),
                                                             totF=sum(grade_rank=="6"), totG=sum(grade_rank=="7"))
############# test ########
predglmRet_Tst <- lcdfTst_ann %>% select(grade_rank, loan_status, actualReturn) %>%
  mutate(predglmRet=predict(lcdf_ann_glm, lcdfTst_ann))
predglmRet_Tst <- predglmRet_Tst %>% mutate(tile=ntile(-predglmRet, 10))
decile_glm_test_ann<- predglmRet_Tst %>% group_by(tile) %>% summarise(count=n(), avgPredRet=mean(predglmRet), numDefaults=sum(loan_status=="Charged Off"),
                                                                  avgActRet=mean(actualReturn), minRet=min(actualReturn), maxRet=max(actualReturn), totA=sum(grade_rank=="1"),
                                                                  totB=sum(grade_rank=="2" ), totC=sum(grade_rank=="3"), totD=sum(grade_rank=="4"), totE=sum(grade_rank=="5"),
                                                                  totF=sum(grade_rank=="6"), totG=sum(grade_rank=="7"))

###### rf ################
######train#############
predrfRet_Trn <- lcdfTrn_ann %>% select(grade_rank, loan_status, actualReturn) %>%
  mutate(predrfRet=predict(lcdf_ann_rf, lcdfTrn_ann)$predictions)
predrfRet_Trn <- predrfRet_Trn %>% mutate(tile=ntile(-predrfRet, 10))
decile_rf<- predrfRet_Trn %>% group_by(tile) %>% summarise(count=n(), avgPredRet=mean(predrfRet), numDefaults=sum(loan_status=="Charged Off"),
                                                           avgActRet=mean(actualReturn), minRet=min(actualReturn), maxRet=max(actualReturn), totA=sum(grade_rank=="1"),
                                                           totB=sum(grade_rank=="2" ), totC=sum(grade_rank=="3"), totD=sum(grade_rank=="4"), totE=sum(grade_rank=="5"),
                                                           totF=sum(grade_rank=="6"), totG=sum(grade_rank=="7"))
############# test ########
predrfRet_Tst <- lcdfTst_ann %>% select(grade_rank, loan_status, actualReturn) %>%
  mutate(predrfRet=predict(lcdf_ann_rf, lcdfTst_ann)$predictions)
predrfRet_Tst <- predrfRet_Tst %>% mutate(tile=ntile(-predrfRet, 10))
decile_rf_test<- predrfRet_Tst %>% group_by(tile) %>% summarise(count=n(), avgPredRet=mean(predrfRet), numDefaults=sum(loan_status=="Charged Off"),
                                                                avgActRet=mean(actualReturn), minRet=min(actualReturn), maxRet=max(actualReturn), totA=sum(grade_rank=="1"),
                                                                totB=sum(grade_rank=="2" ), totC=sum(grade_rank=="3"), totD=sum(grade_rank=="4"), totE=sum(grade_rank=="5"),
                                                                totF=sum(grade_rank=="6"), totG=sum(grade_rank=="7"))


################ question 4 ###############################
######## developing model for 4th
lcdf_4<-subset(lcdf, grade_rank!="1" & grade_rank!="2")
dim(lcdf_4)
nr4<-nrow(lcdf_4)

set.seed(22)
trnIndex4<- sample(1:nr4, size = round(0.70*nr4), replace=FALSE)
lcdf_4Trn <- lcdf_4[trnIndex4, ]
lcdf_4Tst <- lcdf_4[-trnIndex4, ]
dim(lcdf_4Trn)
dim(lcdf_4Tst)

library(ROSE)
lcdf_4_rose <- ROSE(loan_status ~., data = lcdf_4Trn, seed = 22)$data
table(lcdf_4$loan_status)
table(lcdf_4_rose$loan_status)
#removed after exploring performance on DT
lcdf_4_rf <- lcdf_4_rose %>% select(-c(mths_since_last_delinq,total_acc,
                                       mo_sin_old_il_acct,num_il_tl))

### Choosing best 13 variables
lcdf_4_rf2 <- lcdf_4_rf %>% select(c(addr_state,grade_rank,annual_inc,purpose,
                                     emp_length,bc_open_to_buy,avg_cur_bal,
                                     fico_avg,acc_open_past_24mths,revol_bal,
                                     mo_sin_rcnt_tl,total_bc_limit,dti,loan_status))

table(lcdf_4_rf2$loan_status)
#############################
lcdf_4_rose_glm2 <- lcdf_4_rf2
### set charged off as positive values
lcdf_4_rose_glm2 <- lcdf_4_rose_glm2 %>% mutate(loan_status = ifelse(loan_status == "Charged Off",1,0))

lcdf_4Tst_glm <- lcdf_4Tst %>% select(-c(mths_since_last_delinq,total_acc,
                                         mo_sin_old_il_acct,num_il_tl))

lcdf_4Tst_glm <- lcdf_4Tst_glm %>% mutate(loan_status = ifelse(loan_status == "Charged Off",1,0))
lcdf_4Tst_glm2 <- lcdf_4Tst_glm %>% select(c(colnames(lcdf_4_rose_glm2)))
lcdf_4Tst_glm2 <- subset(lcdf_4Tst_glm2, addr_state!="ME")

###### Developing a GLM model.
lcdf_4_glm <- glm(loan_status~.,data = lcdf_4_rose_glm2,family=binomial(logit))
imp_glm <- varImp(lcdf_4_glm)
#With a different classsification threshold
CTHRESH=0.48
predProbTrn_glm4 = predict(lcdf_4_glm,lcdf_4_rose_glm2, type='response')
predTrnCT_glm4 = ifelse(predProbTrn_glm4 > CTHRESH, 1, 0)
predTrnCT_glm4 <- as.factor(predTrnCT_glm4)
table(predTrnCT_glm4 , true=lcdf_4_rose_glm2$loan_status)
mean(predTrnCT_glm4 == lcdf_4_rose_glm2$loan_status)
#####0.577
CTHRESH=0.48
predProbTst_glm4=predict(lcdf_4_glm,lcdf_4Tst_glm2, type='response')
predTstCT_glm4 = ifelse(predProbTst_glm4 > CTHRESH, 1, 0)
table(predTstCT_glm4,true=lcdf_4Tst_glm2$loan_status)
mean(predTstCT_glm4 == lcdf_4Tst_glm2$loan_status)
#####0.59
predTstCT_glm4 <- as.factor(predTstCT_glm4)

pred_glm4=prediction(predProbTst_glm4, lcdf_4Tst_glm2$loan_status, label.ordering = c(0,1))
#ROC curve
roc_glm4 <-performance(pred_glm4, "tpr", "fpr")
print(opt.cut(roc_glm4, pred_glm4))
##sensitivity 0.61
##specificity 0.55
##cutoff      0.485
plot(roc_glm4)
abline(a=0, b= 1)
#AUC value
auc_glm4=performance(pred_glm4, "auc")
auc_glm4@y.values
###0.61

#PR Curve
RP.glm4 <-performance(pred_glm4,"prec", "rec")
plot(RP.glm4)

###########xgboost ###############
boostTrn4<-model.matrix(~.+0, data=lcdf_4_rose_glm2)
boostTst4<-model.matrix(~.+0, data=lcdf_4Tst_glm2)
dim(boostTrn4)
dim(boostTst4)

btrain4<-xgb.DMatrix(subset(boostTrn4,select=-c(loan_status)), label=boostTrn4[,"loan_status"])
btest4<-xgb.DMatrix(subset(boostTst4,select=-c(loan_status)), label=boostTst4[,"loan_status"])

#### model ######
xg4<-xgboost( data = btrain4, 
              nrounds= 850, 
              max.depth=3, 
              nfold = 5, 
              eta=0.01, 
              subsample = 0.8 ,
              eval_metric = "auc",
              objective="binary:logistic")
CTHRESH=0.41
predTrn4_boost1 = predict(xg4,btrain4, type='response')
predTrnCT4_boost1 = ifelse(predTrn4_boost1 > CTHRESH, 1,0)
predTrnCT4_boost1 <- as.factor(predTrnCT4_boost1)
#lcdf_4_rose$loan_status<-(as.numeric(lcdf_4_rose$loan_status)-1)
table(predTrnCT4_boost1 , true=lcdf_4_rose_glm2$loan_status)
mean(predTrnCT4_boost1 == lcdf_4_rose_glm2$loan_status)
#####0.69
####0.69 eta- 0.01 0.66 thres
CTHRESH=0.38
predTst4_boost1=predict(xg4,btest4, type='response')
predTstCT4_boost1 = ifelse(predTst4_boost1 > CTHRESH, 1, 0)
predTstCT4_boost1 <- as.factor(predTstCT4_boost1)
#lcdf_4Tst$loan_status<-(as.numeric(lcdf_4Tst$loan_status)-1)
table(predTstCT4_boost1 , true=lcdf_4Tst_glm2$loan_status)
mean(predTstCT4_boost1 == lcdf_4Tst_glm2$loan_status)
####0.70 0.54 thres 0.587 sen
####0.73 - eta 0.01
boost4_pred1=prediction(predTst4_boost1, lcdf_4Tst_glm2$loan_status, label.ordering = c(0,1))

#ROC curve
aucboostPerf41 <-performance(boost4_pred1, "tpr", "fpr")
plot(aucboostPerf41)
abline(a=0, b= 1)

#### function to find perfect threshold
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])}, perf@x.values, perf@y.values, pred@cutoffs)}

print(opt.cut(aucboostPerf41, boost4_pred1))

#AUC value
aucboostPerf1=performance(boost4_pred1, "auc")
aucboostPerf1@y.values
#0.586

#Lift curve
liftboostPerf <-performance(boost4_pred1, "lift", "rpp")
plot(liftboostPerf)


########### Decile ###########
predTstCT4_boost_gbm <- (as.numeric(predTstCT4_boost1))-1
pred4_Tst_tile1 <- lcdf_4Tst_glm2 %>% select(grade_rank, loan_status) %>%  mutate(predTstCT4_boost_gbm)

pred4_Tst_tile1 <- pred4_Tst_tile1 %>% mutate(tile=ntile(-predTstCT4_boost_gbm, 10))
decile4_boost_test<- pred4_Tst_tile1 %>% group_by(tile) %>%
  summarise(nloans = n(), pred_defaults=sum(predTstCT4_boost_gbm=="1"),
            act_defaults= sum(loan_status == "1"),
            pred_def_rate = (pred_defaults/nloans)*100,
            act_def_rate =(act_defaults/nloans)*100,
            Fully_paid_rate = 100-pred_def_rate, 
            Fully_paid_rate_act = 100-act_def_rate,
            totC=sum(grade_rank=="3"),totD=sum(grade_rank=="4"),
            totE=sum(grade_rank=="5"),                                                                    totF=sum(grade_rank=="6"), totG=sum(grade_rank=="7"))

predTstCT4_glm <- (as.numeric(predTstCT_glm4))-1
pred4_Tst_tile2 <- lcdf_4Tst_glm2 %>% select(grade_rank, loan_status) %>%  mutate(predTstCT4_glm)

##### glm grade-wise analysis
cbind(lcdf_4Tst_glm2,predTstCT_glm4) %>% group_by(grade_rank) %>% summarize(nLoans = n(), 
                                                                         defaults = sum(loan_status=="1"), 
                                                                         def_rate = defaults/nLoans*100, 
                                                                         pred = sum(predTstCT_glm4=="1"),
                                                                         pred_rate = pred/nLoans*100)


pred4_Tst_tile2 <- pred4_Tst_tile2 %>% mutate(tile=ntile(-predTstCT4_glm, 10))
decile4_glm_test<- pred4_Tst_tile2 %>% group_by(tile) %>% summarise(nloans = n(), pred_defaults=sum(predTstCT4_glm=="1"), 
                                                                    act_defaults= sum(loan_status == "1"), pred_def_rate = (pred_defaults/nloans)*100, 
                                                                    act_def_rate =(act_defaults/nloans)*100, Fully_paid_rate = 100-pred_def_rate, 
                                                                    Fully_paid_rate_act = 100-act_def_rate,totC=sum(grade_rank=="3"),totD=sum(grade_rank=="4"), totE=sum(grade_rank=="5"),
                                                                    totF=sum(grade_rank=="6"), totG=sum(grade_rank=="7"))


################## Linear model for 4th question ###############################
dim(lcdf_ann4)
str(lcdf_ann4)
### return dataframe
lcdf_ann4 <- subset(lcdf_ann4, grade_rank!="1" & grade_rank!="2")
lcdf_ann4 <- lcdf_ann4 %>% select(-c(actualTerm))
######## 14 variables #####
lcdf_ann4_temp <- lcdf_ann4 %>% select(c(colnames(lcdf_rf2)))
lcdf_ann4 <- cbind(lcdf_ann4_temp,actualReturn = lcdf_ann4$actualReturn)

###exploring data
lcdf_ann_num4 <- lcdf_ann4 %>% select_if(is.numeric)
summary(lcdf_ann_num4)

#### outlier treatment
lcdf_ann_num_x4 <- lcdf_ann_num4 %>% select(-c(actualReturn))
lcdf_ann_num_y4 <- lcdf_ann_num4$actualReturn

### Tukey's method using IQR
for(i in 1:ncol(lcdf_ann_num_x4)){
  Q3<-quantile(lcdf_ann_num_x4[,i],0.75)
  Q1<-quantile(lcdf_ann_num_x4[,i],0.25)
  IQR<-(Q3-Q1)
  left<- (Q1-(1.5*IQR))
  right<- (Q3+(1.5*IQR))
  lcdf_ann_num_x4[,i] <- ifelse(lcdf_ann_num_x4[,i]<left,left,
                               ifelse(lcdf_ann_num_x4[,i]>right,right,
                                      lcdf_ann_num_x4[,i]))
}

corr4 <- as.data.frame(cor(lcdf_ann_num_x4))

lcdf_ann_cat4 <- lcdf_ann4 %>% select(-c(colnames(lcdf_ann_num4)))
lcdf_ann_new4 <- as.data.frame(cbind(lcdf_ann_num_x4,lcdf_ann_cat4,actualReturn =lcdf_ann_num_y4))

#### checking anova
summary(aov(actualReturn~grade_rank+addr_state+purpose+emp_length,lcdf_ann_new4))
#### categorical variables are important

nr_ann4<-nrow(lcdf_ann_new4)
set.seed(22)
trnIndex_ann4<- sample(1:nr_ann4, size = round(0.70*nr_ann4), replace=FALSE)
lcdfTrn_ann4 <- lcdf_ann_new4[trnIndex_ann4, ]
lcdfTst_ann4 <- lcdf_ann_new4[-trnIndex_ann4, ]
dim(lcdfTrn_ann4)
dim(lcdfTst_ann4)

###############glm model ###############

lcdf_ann_glm4 <- glm(actualReturn~.,data=subset(lcdfTrn_ann4,select = -c(loan_status)),family = gaussian)
#lcdf_glml <- glm(actualReturn~.,data=lcdf_ann,family = gaussian)
#lcdf_glml <- glm(actualReturn~.,data=lcdf_ann,family = gaussian)

##Results
sqrt(mean((lcdf_ann_glm4$linear.predictors - lcdfTrn_ann4$actualReturn)^2))
#train rmse 0.109
sqrt(mean(((predict(lcdf_ann_glm4, lcdfTst_ann4)) - lcdfTst_ann4$actualReturn)^2))
### test rmse 0.108

######## decile test ##################
predglmRet_Tst4 <- lcdfTst_ann4 %>% select(grade_rank, loan_status, actualReturn) %>%
  mutate(predglmRet=predict(lcdf_ann_glm4, lcdfTst_ann4))
predglmRet_Tst4 <- predglmRet_Tst4 %>% mutate(tile=ntile(-predglmRet, 10))
decile_glm_test_ann4<- predglmRet_Tst4 %>% group_by(tile) %>% summarise(count=n(), avgPredRet=mean(predglmRet), numDefaults=sum(loan_status=="Charged Off"),
                                                                      avgActRet=mean(actualReturn), minRet=min(actualReturn), maxRet=max(actualReturn),
                                                                      totC=sum(grade_rank=="3"), totD=sum(grade_rank=="4"), totE=sum(grade_rank=="5"),
                                                                      totF=sum(grade_rank=="6"), totG=sum(grade_rank=="7"))


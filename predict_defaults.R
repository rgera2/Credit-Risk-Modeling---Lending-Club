library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)

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

### proportion of defaults Q2a1
lcdf %>% group_by(loan_status) %>% summarise(n=n()) %>% mutate(Proportion=n/sum(n)*100)
lcdf %>% group_by(loan_status,grade) %>% summarise(n=n()) %>% mutate(Proportion=n/sum(n)*100)
#How does loan status vary by loan grade
lcdf %>% group_by(loan_status, grade) %>% tally()
#or, using table
table(lcdf$loan_status, lcdf$grade)

####proportion according to grades
### Added some analysis here - proportion of defaults according to grades.
lcdf %>% group_by(grade) %>% summarize(defaults=sum(loan_status=="Charged Off"), nloans = n(), default_rate = defaults/nloans *100)
#### Output is expected
###percentage of default loans as per grade as expected.

### within grades
lcdf %>% group_by(sub_grade) %>% summarize(defaults=sum(loan_status=="Charged Off"), nloans = n(), default_rate = defaults/nloans *100) %>% print(n=40)

###### Expected output till D grade after that it's more or less similar
#### we can also analyze through charts
ggplot(lcdf, aes( x = loan_status)) + geom_bar(aes(fill=grade))
ggplot(lcdf, aes( x = loan_status)) + geom_bar(aes(fill=sub_grade))

#How does number of loans, loan amount, interest rate vary by grade

lcdf %>% group_by(grade) %>% summarise(sum(loan_amnt))
#and/or what is the mean loan_amnt by grade?
lcdf %>% group_by(grade) %>% summarise(mean(loan_amnt))
ggplot(lcdf, aes( x = loan_amnt)) + geom_histogram(aes(fill=grade))
ggplot(lcdf, aes( x = loan_amnt)) + geom_histogram() + facet_wrap(~loan_status)


######Q2a2 interest rate varying with grades
lcdf$int_rate <- gsub("%", "", lcdf$int_rate)
lcdf$int_rate <- as.numeric(lcdf$int_rate)

lcdf %>% group_by(grade) %>% summarise(mean(int_rate))
lcdf %>% group_by(sub_grade) %>% summarise(mean(int_rate)) %>% print(n=40)

#Or plot these..
ggplot(lcdf, aes( x = int_rate)) + geom_histogram()
ggplot(lcdf, aes( x = int_rate)) + geom_histogram(aes(fill=grade))
ggplot(lcdf, aes( x = int_rate)) + geom_histogram(aes(fill=sub_grade))

##### This is what we expect as higher grades should have lower interest rates

########### Q2a3
lcdf %>% group_by(purpose) %>% tally()
lcdf %>% group_by(purpose) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), default_rate = defaults/nLoans *100, avgInterest= mean(int_rate), avgLoanAmt=mean(loan_amnt), avgPmnt=mean(total_pymnt))
lcdf %>% group_by(purpose) %>% summarise(sum(loan_amnt))
lcdf %>% group_by(purpose) %>% summarise(mean(loan_amnt))
lcdf %>% group_by(purpose,grade) %>% tally() ## not much information here
ggplot(lcdf, aes( x = purpose)) + geom_bar(aes(fill=grade))
#.....

#Some summarized info on loans by grade
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt))

####Q2a4
#calculate the annualized percentage return
lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100

#summarize by grade
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), avgRet=mean(annRet), stdRet=sd(annRet), minRet=min(annRet), maxRet=max(annRet))


#Some loans are paid back early - find out the actual loan term in months
#  Since last_pymnt_d is a chr variable, we need to covert it to a date var
lcdf$last_pymnt_d<-paste(lcdf$last_pymnt_d, "-01", sep = "")
lcdf$last_pymnt_d<-parse_date_time(lcdf$last_pymnt_d,  "myd")

lcdf$actualTerm <- ifelse(lcdf$loan_status=="Fully Paid", as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)/dyears(1), 3)

####Q4a4
##check actualReturn again
#Then, considering this actual term, we can calculate the actual annual return
### Assuming if actual term is less than 1, than actual term is 1
lcdf$actualReturn <- ifelse(lcdf$actualTerm >=1 ,((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(1/lcdf$actualTerm)*100,((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*100)
summary(lcdf$actualReturn)

#For cost-based performance, we want to see the average interest rate, and the average of proportion of loan amount paid back, grouped by loan_status
lcdf %>% group_by(loan_status) %>% summarise(avgret = mean(actualReturn),intRate=mean(int_rate), totRet=mean((total_pymnt-funded_amnt)/funded_amnt))

# Notice that the totRet on Charged Off loans as -0.366, so, for every dollar invested, there is a loss of .366 cents.   For Fully Paid loans, the totRet seems less than what may be  expected from intRate -- how do you explain this?
# because duration is not always a year
#you may like to look at some of these variables
lcdf %>% select(loan_status, loan_amnt, funded_amnt, total_pymnt, int_rate, actualTerm, actualReturn ) %>% view()

#some more summaries
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), defaultRate=defaults/nLoans, avgret = mean(actualReturn),intRate=mean(int_rate), totRet=mean((total_pymnt-funded_amnt)/funded_amnt) ,term = median(actualTerm))
### based on avg ret, invest in D

#convert emp_length to factor -- can order the factors in  a meaningful way
lcdf$emp_length <- factor(lcdf$emp_length, levels=c("n/a", "< 1 year","1 year","2 years", "3 years" ,  "4 years",   "5 years",   "6 years",   "7 years" ,  "8 years", "9 years", "10+ years" ))

#Look at loan purpose
lcdf %>% group_by(purpose) %>% tally()
  # do you want to recode some categories with very few cases to "other"
lcdf$purpose <- fct_recode(as.factor(lcdf$purpose), other="wedding", other="renewable_energy")


#Note - character variables can cause a problem with some model packages, so better to convert all of these to factors

lcdf= lcdf %>% mutate_if(is.character, as.factor) ###unsure about this, need to ask TA

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
lcdf <- lcdf %>% select(-c(num_rev_accts,num_op_rev_tl,num_bc_sats))

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

#### ratio of delinquent accounts over total accounts
#look up for it ##lcdf$propDelqAccts <- ifelse(lcdf$acc_open_past_24mths>0, lcdf$acc_now_delinq/lcdf$acc_open_past_24mths, 0)

########bookmark#########
#Drop some variables for potential leakage, others

#Drop some other columns which are not useful and those which will cause 'leakage'
fut_leak <- c("collection_recovery_fee","last_fico_range_high","last_fico_range_low","funded_amnt","funded_amnt_inv","issue_d","last_credit_pull_d","last_pymnt_amnt","last_pymnt_d","out_prncp","out_prncp_inv","pymnt_plan","recoveries","total_pymnt","total_pymnt_inv","total_rec_int","total_rec_late_fee","total_rec_prncp","total_rev_hi_lim")
lcdf <- lcdf %>% select(-fut_leak)

#some additional vars to drop
varsToRemove=c("earliest_cr_line","emp_title","int_rate", "num_accts_ever_120_pd", "application_type", "policy_code", "pub_rec", "pub_rec_bankruptcies","sub_grade","term","title","tot_coll_amt","zip_code")
lcdf <- lcdf %>% select(-varsToRemove)

#### removing some variables on the basis of analysis after creating decision trees.
varsToRemove_an <- c("open_acc",	"acc_now_delinq",	"chargeoff_within_12_mths",	"mo_sin_rcnt_rev_tl_op",	"mort_acc",	"num_tl_120dpd_2m",	"num_tl_90g_dpd_24m",	"pct_tl_nvr_dlq",	"tax_liens",	"total_bal_ex_mort",	"debt_settlement_flag",	"actualReturn",	"delinq_2yrs",	"collections_12_mths_ex_med",	"tot_cur_bal",	"delinq_amnt",	"num_tl_30dpd",	"tot_hi_cred_lim",	"hardship_flag",	"actualTerm",	"annRet")
lcdf <- lcdf %>% select(-varsToRemove_an)
#.....


#Missing values

#Drop vars with all empty values
##### already done
####lcdf <- lcdf %>% select_if(function(x){!all(is.na(x))})


#missing value proportions in each column
#colMeans(is.na(lcdf))
# or, get only those columns where there are missing values
#colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]

#remove variables which have more than, for example, 60% missing values
#nm<-names(lcdf)[colMeans(is.na(lcdf))>0.6]
#lcdf <- lcdf %>% select(-nm)

#Impute missing values - first get the columns with missing values
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]
#summary of data in these columns
nm<- names(lcdf)[colSums(is.na(lcdf))>0]
summary(lcdf[, nm])
####verification
#mths_since_last_delinq: has 48% missings, these pertain to no delinquincy, so replace by max value (176) or a value higher than the max (500) -- we will try this out on a temporary dataset lcx with the attributes that have misisng values
#lcx<-lcdf[, c(nm)]
#colMeans(is.na(lcx))[colMeans(is.na(lcx))>0]
#lcx<- lcx %>% replace_na(list(mths_since_last_delinq = 640))
#For revol_util, suppose we want to replace the missing values by the median
#lcx$revol_util <- as.numeric(gsub("%","",lcx$revol_util))
#lcx<- lcx %>% replace_na(list(revol_util=median(lcx$revol_util, na.rm=TRUE)))
##### bc_open_to_buy median makes sense
#lcx<- lcx %>% replace_na(list(bc_open_to_buy=median(lcx$bc_open_to_buy, na.rm=TRUE)))
#### bc_util - mean makes more sense
#lcx<- lcx %>% replace_na(list(bc_util=mean(lcx$bc_util, na.rm=TRUE)))
###### mo_sin_old_il_acct - maximum 
#lcx<- lcx %>% replace_na(list(mo_sin_old_il_acct=max(lcx$mo_sin_old_il_acct, na.rm=TRUE)))
##### mths_since_recent_bc - maximum
#lcx<- lcx %>% replace_na(list(mths_since_recent_bc=max(lcx$mths_since_recent_bc, na.rm=TRUE)))
###### mths_since_recent_inq - maximum
#lcx<- lcx %>% replace_na(list(mths_since_recent_inq=max(lcx$mths_since_recent_inq, na.rm=TRUE)))
####### num_tl_120dpd_2m- 0
#lcx<- lcx %>% replace_na(list(num_tl_120dpd_2m=0))
##### percent_bc_gt_75 - mean makes more sense
#lcx <- lcx %>% replace_na(list(percent_bc_gt_75 = mean(lcx$percent_bc_gt_75, na.rm=TRUE)))

#Similarly for the other variables
#If we are sure this is working and what we want, can replace the missing values on the lcdf dataset

###### Missing values picked above, replacing them now 
### used boxplots and summary for the table

lcdf$revol_util <- as.numeric(gsub("%","",lcdf$revol_util))
lcdf<- lcdf %>% replace_na(list(mths_since_last_delinq=640, revol_util=median(lcdf$revol_util, na.rm=TRUE),
                                bc_open_to_buy=median(lcdf$bc_open_to_buy, na.rm=TRUE),
                                bc_util=mean(lcdf$bc_util, na.rm=TRUE),mo_sin_old_il_acct=max(lcdf$mo_sin_old_il_acct, na.rm=TRUE),
                                mths_since_recent_bc=max(lcdf$mths_since_recent_bc, na.rm=TRUE),
                                mths_since_recent_inq=max(lcdf$mths_since_recent_inq, na.rm=TRUE),
                                num_tl_120dpd_2m=0,percent_bc_gt_75 = mean(lcdf$percent_bc_gt_75, na.rm=TRUE)))

### check once again
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]



#Next we will build some  models

library(rpart)

#It can be useful to convert the target variable, loan_status to  a factor variable
lcdf$loan_status <- factor(lcdf$loan_status, levels=c("Fully Paid", "Charged Off"))
lcdf <- lcdf %>% select(-c(initial_list_status,home_ownership,verification_status,
                           inq_last_6mths,num_actv_rev_tl,num_sats,num_tl_op_past_12m,
                           percent_bc_gt_75,propSatisBankcardAccts,propActiveBankcardAccts))
#split the data into trn, tst subsets
nr<-nrow(lcdf)

set.seed(22)
trnIndex<- sample(1:nr, size = round(0.70*nr), replace=FALSE)
lcdfTrn <- lcdf[trnIndex, ]
lcdfTst <- lcdf[-trnIndex, ]
dim(lcdfTrn)
dim(lcdfTst)
library(ROSE)
lcdf_rose <- ROSE(loan_status ~., data = lcdfTrn, seed = 22)$data
table(lcdf_rose$loan_status)

####
#lcdf_over <- ovun.sample(loan_status ~., data = lcdfTrn, method = "over", N = 110564, seed = 22)$data
#table(lcdf_over$loan_status)
#lcdf_both <- ovun.sample(loan_status ~., data = lcdfTrn, method = "both",p=0.5, seed = 22)$data
#table(lcdf_both$loan_status)
#lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", parms = list(split = "information"),control = rpart.control(minsplit=10, minbucket = 3, cp=0.0003))
#lcDT1 <- rpart(loan_status ~., data=lcdf_rose, method="class", parms = list(split = "information"),control = rpart.control(minsplit=30, cp=0.0003))
#lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", parms = list(split = "information"), control = rpart.control(cp = 0.0003, minsplit = 50))

#### We checked rpart at all cp = 0.0 and then basis of plot cp we pruned our tree
lcDT1_r <- rpart(loan_status ~., data=lcdf_rose, method="class", parms = list(split = "gini"), control = rpart.control(cp = 0.0, minsplit = 50))


#Do we want to prune the tree -- check for performance with dfferent cp levels
printcp(lcDT1_r)
plotcp(lcDT1_r)
#lcDT1p_r<- prune.rpart(lcDT1_r, cp=lcDT1_r$cptable[which.min(lcDT1_r$cptable[,"xerror"]),"CP"])
lcDT1p_r<- prune.rpart(lcDT1_r, cp=0.0005)
#lcDT1p_b<- prune.rpart(lcDT1_b, cp=lcDT1_b$cptable[which.min(lcDT1_b$cptable[,"xerror"]),"CP"])
#lcDT1p_o<- prune.rpart(lcDT1_o, cp=lcDT1_o$cptable[which.min(lcDT1_o$cptable[,"xerror"]),"CP"])
#summary(lcDT1_r)
#......

#variable importance
lcDT1p_r$variable.importance

#Performance evaluation
#Evaluate performance
predTrn=predict(lcDT1p_r,lcdf_rose, type='class')
table(pred = predTrn, true=lcdf_rose$loan_status)
mean(predTrn == lcdf_rose$loan_status)

table(pred = predict(lcDT1p_r,lcdfTst, type='class'), true=lcdfTst$loan_status)
mean(predict(lcDT1p_r,lcdfTst, type='class') ==lcdfTst$loan_status)

#With a different classsification threshold
CTHRESH=0.614
predProbTrn=predict(lcDT1p_r,lcdf_rose, type='prob')
predTrnCT = ifelse(predProbTrn[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
predTrnCT <- as.factor(predTrnCT)
table(predTrnCT , true=lcdf_rose$loan_status)
mean(predTrnCT == lcdf_rose$loan_status)

CTHRESH=0.614
predProbTst=predict(lcDT1p_r,lcdfTst, type='prob')
predTstCT = ifelse(predProbTst[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
predTstCT <- as.factor(predTstCT)
table(predTstCT , true=lcdfTst$loan_status)
mean(predTstCT == lcdfTst$loan_status)

#Or you can use the confusionMatrix fuction from the caret package
library(caret)
confusionMatrix(predTrnCT, lcdf_rose$loan_status)
confusionMatrix(predTstCT, lcdfTst$loan_status)
#if you get an error saying that the 'e1071' package is required,
# you should install and load that too
#Notice that the output says
#   'Positive' class: Fully Paid
#So,the confusionMatrix based performance measures are based
#  on the "Fully Paid" class as the class of interest.
# If you want to get performance measure for "Charged Off", use
#    the positive- paremeter
confusionMatrix(predTrn, lcdfTrn$loan_status, positive="Charged Off")


#ROC plot
library(ROCR)

score=predict(lcDT1p_r,lcdfTst, type="prob")[,"Charged Off"]
pred=prediction(score, lcdfTst$loan_status, label.ordering = c("Fully Paid", "Charged Off"))
#label.ordering here specifies the 'negative', 'positive' class labels  

#ROC curve
aucPerf <-performance(pred, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)

#### function to find perfect threshold
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
  d = (x - 0)^2 + (y-1)^2
  ind = which(d == min(d))
  c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
  cutoff = p[[ind]])}, perf@x.values, perf@y.values, pred@cutoffs)}

print(opt.cut(aucPerf, pred))

#AUC value
aucPerf=performance(pred, "auc")
aucPerf@y.values


#Lift curve
liftPerf <-performance(pred, "lift", "rpp")
plot(liftPerf)

#### C50
library(C50)
cntrl = C5.0Control(CF=0.08,minCases = 5)
lcDT1_r <- C5.0(loan_status ~., data=lcdf_rose, control = cntrl)

lcDT1p_r <- lcDT1_r

#Performance evaluation
#Evaluate performance
predTrn=predict(lcDT1p_r,lcdf_rose, type='class')
table(pred = predTrn, true=lcdf_rose$loan_status)
mean(predTrn == lcdf_rose$loan_status)

table(pred = predict(lcDT1p_r,lcdfTst, type='class'), true=lcdfTst$loan_status)
mean(predict(lcDT1p_r,lcdfTst, type='class') ==lcdfTst$loan_status)

#With a different classsification threshold
CTHRESH=0.62
predProbTrn=predict(lcDT1p_r,lcdf_rose, type='prob')
predTrnCT = ifelse(predProbTrn[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
predTrnCT <- as.factor(predTrnCT)
table(predTrnCT , true=lcdf_rose$loan_status)
mean(predTrnCT == lcdf_rose$loan_status)

CTHRESH=0.62
predProbTst=predict(lcDT1p_r,lcdfTst, type='prob')
predTstCT = ifelse(predProbTst[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
predTstCT <- as.factor(predTstCT)
table(predTstCT , true=lcdfTst$loan_status)
mean(predTstCT == lcdfTst$loan_status)

#Or you can use the confusionMatrix fuction from the caret package
library(caret)
confusionMatrix(predTrnCT, lcdf_rose$loan_status)
confusionMatrix(predTstCT, lcdfTst$loan_status)
#if you get an error saying that the 'e1071' package is required,
# you should install and load that too
#Notice that the output says
#   'Positive' class: Fully Paid
#So,the confusionMatrix based performance measures are based
#  on the "Fully Paid" class as the class of interest.
# If you want to get performance measure for "Charged Off", use
#    the positive- paremeter
confusionMatrix(predTrn, lcdfTrn$loan_status, positive="Charged Off")

##### randomforest 

library('randomForest')

#for reproducible results, set a specific value for the random number seed
set.seed(22)

#develop a model with 200 trees, and obtain variable importance
lcdf_rf <- lcdf %>% select(-c(mths_since_last_delinq,total_acc,
                              mo_sin_old_il_acct,num_il_tl))

nr<-nrow(lcdf_rf)

set.seed(22)
trnIndex<- sample(1:nr, size = round(0.85*nr), replace=FALSE)
lcdfTrn_rf <- lcdf_rf[trnIndex, ]
lcdfTst_rf <- lcdf_rf[-trnIndex, ]
dim(lcdfTrn_rf)
dim(lcdfTst_rf)
lcdf_rose_rf <- ROSE(loan_status ~., data = lcdfTrn_rf, seed = 22)$data
table(lcdf_rose_rf$loan_status)

### Choosing best 12 variables
lcdf_rf2 <- lcdf_rf %>% select(c(addr_state,grade_rank,annual_inc,
                                 emp_length,bc_open_to_buy,avg_cur_bal,
                                 fico_avg,acc_open_past_24mths,revol_bal,
                                 mo_sin_rcnt_tl,total_bc_limit,dti,loan_status))
nr<-nrow(lcdf_rf2)

set.seed(22)
trnIndex<- sample(1:nr, size = round(0.85*nr), replace=FALSE)
lcdfTrn_rf2 <- lcdf_rf2[trnIndex, ]
lcdfTst_rf2 <- lcdf_rf2[-trnIndex, ]
dim(lcdfTrn_rf2)
dim(lcdfTst_rf2)
lcdf_rose_rf2 <- ROSE(loan_status ~., data = lcdfTrn_rf2, seed = 22)$data
table(lcdf_rose_rf2$loan_status)


lcdf_rose_x <- lcdf_rose_rf %>% select(-c(loan_status))
rfModel = randomForest(loan_status ~., data=lcdf_rose_rf, mtry= 10,ntree=3, importance=TRUE )
rfModel1 <- randomForest(loan_status ~., data=lcdf_rose_rf, mtry= 10,ntree=21, importance=TRUE )
rfModel2 <- randomForest(loan_status ~., data=lcdf_rose_rf2, mtry= 10,ntree=21, importance=TRUE)
rf_cv <- rfcv(lcdf_rose_x,lcdf_rose_rf$loan_status,cv.fold = 10)
#check the model -- see what OOB error rate it gives


#Variable importance
importance(rfModel1)
varImpPlot(rfModel1)

importance(rfModel, type=2)  #is there a difference -- WHY?

#Evaluate performance

predTrn=predict(rfModel1,lcdf_rose_rf)
table(pred = predTrn, true=lcdf_rose_rf$loan_status)

mean(predTrn == lcdf_rose_rf$loan_status)

table(pred = predict(rfModel1,lcdfTst_rf), true=lcdfTst_rf$loan_status)
mean(predict(rfModel1,lcdfTst_rf) ==lcdfTst_rf$loan_status)

CTHRESH=0.57
predProbTrn=predict(rfModel1,lcdf_rose_rf, type='prob')
predTrnCT = ifelse(predProbTrn[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
predTrnCT <- as.factor(predTrnCT)
table(predTrnCT , true=lcdf_rose_rf$loan_status)
mean(predTrnCT == lcdf_rose_rf$loan_status)

CTHRESH=0.57
predProbTst=predict(rfModel1,lcdfTst_rf, type='prob')
predTstCT = ifelse(predProbTst[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
predTstCT <- as.factor(predTstCT)
table(predTstCT , true=lcdfTst_rf$loan_status)
mean(predTstCT == lcdfTst_rf$loan_status)

score=predict(rfModel1,lcdfTst_rf, type="prob")[,"Charged Off"]
pred=prediction(score, lcdfTst_rf$loan_status, label.ordering = c("Fully Paid", "Charged Off"))
#label.ordering here specifies the 'negative', 'positive' class labels  

#ROC curve
aucPerf <-performance(pred, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)

#AUC value
aucPerf=performance(pred, "auc")
aucPerf@y.values
##print(opt.cut(aucPerf, pred))

confusionMatrix(predTrnCT, lcdf_rose_rf$loan_status)
confusionMatrix(predTstCT, lcdfTst_rf$loan_status)

#Draw the lift curve fr teh random forest model
perfLift_rfTst=performance(pred, "lift", "rpp")
plot(perfLift_rfTst)

#Performance with profit.loss

#Incorporating profits & costs
PROFITVAL <- 10*6.7*3 #profit (on $100) from accurately identifying Fully_paid loans
COSTVAL <- -25*12.3*3  # loss (on $100) from incorrectly predicting a Charged_Off loan as Full_paid
scoreTst <- predict(lcDT1p_r,lcdfTst, type="prob")[,"Fully Paid"]  
scoreTst_rf <- predict(rfModel1,lcdfTst_rf,type="prob")[,"Fully Paid"]
#Note- we want to identify those loans wth high prob for being FullyPaid
prPerf <- data.frame(scoreTst)
prPerf <- cbind(prPerf, status=lcdfTst$loan_status)
prPerf <- prPerf[order(-scoreTst) ,]  #sort in desc order of  prob(fully_paid)
prPerf$profit <- ifelse(prPerf$status == 'Fully Paid', PROFITVAL, COSTVAL)
prPerf$cumProfit <- cumsum(prPerf$profit)

### for random forest
#Note- we want to identify those loans wth high prob for being FullyPaid
prPerf_rf <- data.frame(scoreTst_rf)
prPerf_rf <- cbind(prPerf_rf, status=lcdfTst_rf$loan_status)
prPerf_rf <- prPerf_rf[order(-scoreTst_rf) ,]  #sort in desc order of  prob(fully_paid)
prPerf_rf$profit <- ifelse(prPerf_rf$status == 'Fully Paid', PROFITVAL, COSTVAL)
prPerf_rf$cumProfit <- cumsum(prPerf_rf$profit)

#to compare against the default approach of investing in CD with 2% int (i.e. $6 profit out of $100 in 3 years)
prPerf$cdRet <- 6
prPerf$cumCDRet <- cumsum(prPerf$cdRet)
plot(prPerf$cumProfit, col='red')
lines(prPerf_rf$cumProfit, col='blue', lwd=5)
lines(prPerf$cumCDRet, col='green',lwd=3)

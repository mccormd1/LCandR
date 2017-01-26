#LCcleandatafortrain.r -takes in and cleans past data 

getwd()
dir()
setwd("Downloads/")
my_data<-read_csv('LoanStats3b.csv',skip=1,n_max=188179,col_types = "n-nnnccncc-ccncccc--c--cnn-nnnnnnnncnnnnnnnnncnc-nn-cnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn") #set max to max -4 

summary(my_data)
names(my_data)

# Select variables to keep and subset the data

variables <- c("id","loan_amnt","term","int_rate","sub_grade","emp_length","home_ownership","annual_inc",
               "loan_status","purpose","addr_state","dti","delinq_2yrs","mths_since_last_record",
               "open_acc","pub_rec","revol_bal","revol_util","total_acc","initial_list_status","collections_12_mths_ex_med","mths_since_last_major_derog")

train <- my_data[variables]


# Reduce loan status to binary "Performing" and "NonPerforming" Measures:
# train$new_status <- (factor(ifelse(train$loan_status %in% c("Current", "Fully Paid"), 
#                                   "Performing", "NonPerforming")))
train$new_status <- as.numeric(factor(ifelse(train$loan_status %in% c("Current", "Fully Paid"), 
                                   0, 1)))-1 ##1,0 means 1=performing, 0= non performing. 0,1 0=performing, 1=non performing
# Convert a subset of the numeric variables to factors
# train$delinq_2yrs <- factor(train$delinq_2yrs)
# train$open_acc <- factor(train$open_acc)
# train$pub_rec <- factor(train$pub_rec)
# train$total_acc <- factor(train$total_acc)
# train$purpose <- factor(train$purpose)
# Convert interest rate numbers to numeric (strip percent signs)
train$int_rate <- as.numeric(sub("%", "", train$int_rate))
train$revol_util <- as.numeric(sub("%", "", train$revol_util))

train$term <- as.numeric(sub(" months","",train$term))

train$emp_length <- sub("< 1","0",train$emp_length)
train$emp_length <- sub("[+]","",train$emp_length)
train$emp_length <- as.numeric(gsub("yea(r|rs)","",train$emp_length))
#train$sub_grade <- as.numeric(chartr('ABCDEFG',"1234567",train$sub_grade))

train$purpose<-as.numeric(factor(train$purpose,levels=c("car","credit_card","debt_consolidation","home_improvement","house","major_purchase","medical","moving","moving","other","renewable_energy","small_business","vacation","wedding")))

train$home_ownership <- as.numeric(factor(train$home_ownership,levels=c("NONE","OTHER","RENT","MORTGAGE","OWN"),ordered=TRUE))
  #sort(unique((train$home_ownership)))
  #select(my_data,id,loan_amnt,term,int_rate,installment,emp_length,home_ownership,annual_inc,purpose,acc_now_delinq,bc_open_to_buy,dti,delinq_2yrs,delinq_amnt,inq_last_6mths,starts_with("mths_since"),mort_acc,starts_with("revol"),starts_with("total"),pub_rec_bankruptcies,num_accts_ever_120_pd,chargeoff_within_12_mths,collections_12_mths_ex_med,tax_liens,num_sats,starts_with("mo_sin"),starts_with("tot_"),starts_with("num_"),pct_tl_nvr_dlq,application_type,max_bal_bc,il_util,all_util,inq_fi,inq_last_12m,starts_with("out"),recoveries,starts_with("collection"),last_pymnt_d)->train_data
  #select(my_data,id,loan_amnt,term,int_rate,installment,emp_length,home_ownership,annual_inc,purpose,acc_now_delinq,bc_open_to_buy,dti,delinq_2yrs,delinq_amnt,inq_last_6mths,starts_with("mths_since"),mort_acc,starts_with("revol"),starts_with("total"),-total_cu_tl,pub_rec_bankruptcies,num_accts_ever_120_pd,chargeoff_within_12_mths,collections_12_mths_ex_med,tax_liens,num_sats,starts_with("mo_sin"),starts_with("tot_"),starts_with("num_"),pct_tl_nvr_dlq,application_type,starts_with("out"),recoveries,starts_with("collection"),last_pymnt_d)->train_data
  
  #train_data %>%
    #transmute(home_owner=as.numeric(as.factor(home_ownership)))
# impcolums<-select(train,id,loan_amnt,term,int_rate,sub_grade,emp_length,home_ownership,annual_inc,
#                           new_status,purpose,dti,delinq_2yrs,mths_since_last_record,
#                           open_acc,pub_rec,revol_bal,revol_util,total_acc,initial_list_status,collections_12_mths_ex_med,
#                           mths_since_last_major_derog)

trainnorm<-train %>%
  mutate_each(funs(mynormalize(.)),-int_rate,-new_status,-sub_grade,-id,-purpose,-loan_status,-addr_state,-initial_list_status)


smpsize<-floor(.75*nrow(train))
splitind<-sample(seq_len(nrow(train)),size=smpsize)

mytrain<-trainnorm[splitind, ]
mytest<-trainnorm[-splitind, ]

myreg<-glm(new_status~loan_amnt+emp_length+home_ownership+annual_inc+purpose+dti+delinq_2yrs+
             open_acc+pub_rec+revol_bal+revol_util+total_acc,data=mytrain,family="binomial")
summary(myreg)

traincoef<-predict(myreg,mytrain,type="response")
trainerr<--1/nrow(mytrain)*sum(log(traincoef)*mytrain$new_status+log(1-traincoef)*(1-mytrain$new_status),na.rm=TRUE)

testcoef<-predict(myreg,mytest,type="response")

mytest$fit<-testcoef
testerr<--1/nrow(mytest)*sum(log(testcoef)*mytest$new_status+log(1-testcoef)*(1-mytest$new_status),na.rm=TRUE)

mytest %>%
arrange((fit)) -> ranked_data

ranked_data %>%
  select(id,sub_grade,int_rate,fit,new_status)->aucanalysis

aucanalysis$notdefault<-1-aucanalysis$new_status
costFN<-25 #

costFP<-7.5#7.5 #this is ~10% return over 3 years on a $25 bid. (actually 7.425)

cutoffs<-seq(from =0,to=.6,by=.001)
tpr<-vector(mode="numeric",length=length(cutoffs))
fpr<-tpr
mymincost<-tpr
truepos<-tpr
falsepos<-tpr

count<-1
for (ii in cutoffs){
  # mymincost[count]<-sum(aucanalysis[1:ii,"new_status"]*costdefault)+sum(aucanalysis[ii:nrow(aucanalysis),"notdefault"]*7.5)
  truepos[count]<-sum(aucanalysis[aucanalysis$fit>=ii, "new_status"],na.rm = TRUE)
  falsepos[count]<-sum(aucanalysis[aucanalysis$fit>=ii, "notdefault"],na.rm = TRUE)
  trueneg<-sum(aucanalysis[aucanalysis$fit<=ii, "notdefault"],na.rm = TRUE)
  falseneg<-sum(aucanalysis[aucanalysis$fit<=ii, "new_status"],na.rm = TRUE)
  mymincost[count]<-(falseneg*costFN+falsepos[count]*costFP)
  tpr[count]<-truepos[count]/(truepos[count]+falseneg)
  fpr[count]<-falsepos[count]/(falsepos[count]+trueneg)
  count<-count+1
}
condpos<-sum(aucanalysis$new_status)
condneg<-sum(aucanalysis$notdefault)
plot(fpr,tpr)
plot(mymincost/nrow(aucanalysis))
mycut<-cutoffs[which.min(mymincost)]
  #write.csv(train_data,"train_data")
cutoffs[100]
aucanalysis %>%
  filter(fit<mycut/5) %>%
  mutate(totalreturn3yr=notdefault*(int_rate*1.7425)) %>% #.7425=25 dollars * .99 cut *3 for 3 year loan /100 to percentile 
  group_by(sub_grade) %>%
  summarise(avgreturn3yr=mean(totalreturn3yr))  ->bysubgrade

aucanalysis %>%
  filter(grepl("D",sub_grade,fixed=TRUE)) %>%
  mutate(totalreturn3yr=notdefault*(int_rate*1.7425)) %>%
  #arrange(desc(totalreturn3yr)) ->rankedDbyreturn
  arrange(fit) ->rankedDbyreturn

plot(bysubgrade$avgreturn3yr)
head(bysubgrade,20)
getwd()
options(scipen = 999)

#Importing the Dataset
bank_data<-read.csv("Train_nyOWmfK.csv")
View(bank_data)
dim(bank_data)  
str(bank_data)
View(sapply(bank_data , class))
names(bank_data)

#Data Cleaning.Removing business unnecessary variables
bank_data$ID<-NULL
bank_data$DOB<-NULL
bank_data$Lead_Creation_Date<-NULL
bank_data$City<-NULL
bank_data$Var1 <- NULL
bank_data$Var2 <- NULL
bank_data$Var5<- NULL
bank_data$Var4 <- NULL
bank_data$Source <- NULL
bank_data$Employer_Name<-NULL
bank_data$Salary_Account<-NULL  #it is categorical var with high cardinality so we're dropping this



require(dplyr)
cat_var<-names(dplyr::select_if(bank_data,is.character))
View(bank_data[cat_var])
num_var<-names(dplyr::select_if(bank_data,is.numeric))
View(bank_data[num_var])

bank_data$Gender<-as.factor(bank_data$Gender)
bank_data$Mobile_Verified<-as.factor(bank_data$Mobile_Verified)
bank_data$Filled_Form<-as.factor(bank_data$Filled_Form)
bank_data$Device_Type<-as.factor(bank_data$Device_Type)


#User Defined Function for descriptive statistics
mystats<-function(x){
  nmiss<-sum(is.na(x))
  a<-x[!is.na(x)]
  m<-mean(a)
  n<-length(a)
  s<-sd(a)
  min<-min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max<-max(a)
  UC<-m+3*s
  LC<-m-3*s
  outlier_flag<-max>UC|min<LC
  return(c(n=n,nmiss=nmiss,outlier_flag=outlier_flag,mean=m,stdev=s,min=min,p1=p1,p5=p5,p10=p10,
           q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,UC=UC,LC=LC))
}

diag_stats<-t(data.frame(sapply(bank_data[,num_var], mystats)))
View(diag_stats)
write.csv(diag_stats,file="diag_stats_info_BankingCaseStudy.csv")



#Check for Missing Values
View(sapply(bank_data,function(x) sum(is.na(x))))
#-We have a large number of missing values in some variales but we cannot drop these variables because they seem important
# from the business point of view.So we'll impute these.

bank_data$Loan_Amount_Applied[is.na(bank_data$Loan_Amount_Applied)] <- median(bank_data$Loan_Amount_Applied,na.rm = T)
bank_data$Loan_Tenure_Applied[is.na(bank_data$Loan_Tenure_Applied)] <- median(bank_data$Loan_Tenure_Applied,na.rm = T)
bank_data$Existing_EMI[is.na(bank_data$Existing_EMI)] <- mean(bank_data$Existing_EMI,na.rm = T)
bank_data$Loan_Amount_Submitted[is.na(bank_data$Loan_Amount_Submitted)] <- median(bank_data$Loan_Amount_Submitted,na.rm = T)
bank_data$Loan_Tenure_Submitted[is.na(bank_data$Loan_Tenure_Submitted)] <- median(bank_data$Loan_Tenure_Submitted,na.rm = T)
bank_data$Interest_Rate[is.na(bank_data$Interest_Rate)] <- median(bank_data$Interest_Rate,na.rm = T)
bank_data$Processing_Fee[is.na(bank_data$Processing_Fee)] <- median(bank_data$Processing_Fee,na.rm = T)
bank_data$EMI_Loan_Submitted[is.na(bank_data$EMI_Loan_Submitted)] <- median(bank_data$EMI_Loan_Submitted,na.rm = T)




#Outlier Treatment
outlier_treat <- function(x){
  UC1 = quantile(x, p=0.99,na.rm=T)
  LC1 = quantile(x, p=0.01,na.rm=T)
  x=ifelse(x>UC1, UC1, x)
  x=ifelse(x<LC1, LC1, x)
  return(x)
}
bank_data[,num_var]<- data.frame(apply(bank_data[,num_var],2,FUN=outlier_treat))

#corelation
cor_mat <- cor(bank_data[num_var])
View(cor_mat)
write.csv(cor_mat,"cor_mat_BanklOans.csv")


bank_data<-cbind(bank_data[,num_var],bank_data[,cat_var])
View(bank_data)

#Splitting the data into Training ,Validation and Testing Dataset
train_ind<-sample(1:nrow(bank_data),size = floor(0.70*nrow(bank_data)))
training<-bank_data[train_ind,]
testing<-bank_data[-train_ind,]

#Building model for training dataset
fit<-glm(Disbursed~Monthly_Income+Loan_Amount_Applied+Loan_Tenure_Applied+Existing_EMI+Loan_Amount_Submitted
         +Loan_Tenure_Submitted+Interest_Rate+Processing_Fee+EMI_Loan_Submitted+LoggedIn+Gender+
           Mobile_Verified+Filled_Form+Device_Type,data=training,family=binomial(logit))
#Output of Logistic Regression
summary(fit)
ls(fit)
fit$model



Concordance = function(GLM.binomial) {
  outcome_and_fitted_col = cbind(GLM.binomial$y, GLM.binomial$fitted.values)
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
  else {zeros = zeros[1:length(ones[,1]),]}
  ones_and_zeros = data.frame(ones, zeros)
  conc = rep(NA, length(ones_and_zeros[,1]))
  disc = rep(NA, length(ones_and_zeros[,1]))
  ties = rep(NA, length(ones_and_zeros[,1]))
  for (i in 1:length(ones_and_zeros[,1])) {
    if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
    {conc[i] = 1
    disc[i] = 0
    ties[i] = 0}
    else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 0
      ties[i] = 1
    }
    else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 1
      ties[i] = 0
    }
  }

  conc_rate = mean(conc, na.rm=TRUE)
  disc_rate = mean(disc, na.rm=TRUE)
  tie_rate = mean(ties, na.rm=TRUE)
  Somers_D<-conc_rate - disc_rate
  gamma<- (conc_rate - disc_rate)/(conc_rate + disc_rate)
  return(list(concordance=conc_rate, num_concordant=sum(conc), discordance=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties),
              somers_D=Somers_D, Gamma=gamma))
  
}

Concordance(fit)

#Stepwise Regression
step1=step(fit,direction ='both')

fit2<-glm(Disbursed ~ Monthly_Income + Loan_Amount_Applied + Loan_Tenure_Applied + 
    Existing_EMI + Loan_Amount_Submitted + Loan_Tenure_Submitted + 
    Interest_Rate + LoggedIn + Filled_Form,data=training,family=binomial(logit))

summary(fit2)

Concordance(fit2)

#Predicting for Training Dataset
train1<-cbind(training,Prob=predict(fit2,type="response"))
View(train1)



#Creating Deciles
decLocations<-quantile(train1$Prob,probs=seq(0.1,0.9,by=0.1))
train1$decile<-findInterval(train1$Prob,c(-Inf,decLocations,Inf))
View(train1)

#Decile Analysis Report
decile_grp<-group_by(train1,decile)
decile_sum_train<-summarize(decile_grp,total_cnt=n(),min_prob=min(p=Prob),
                            max_prob=max(Prob),disbursed_cnt=sum(Disbursed),
                            non_disbursed_cnt=total_cnt-disbursed_cnt)
decile_sum_train<-arrange(decile_sum_train,desc(decile)) 
View(decile_sum_train)
write.csv(decile_sum_train,"Bankdata_FitTrain.csv",row.names = F)



test1<-cbind(testing,Prob=predict(fit2,testing,type="response"))
View(test1)



decLocations<-quantile(test1$Prob,probs=seq(0.1,0.9,by=0.1))
test1$decile<-findInterval(test1$Prob,c(-Inf,decLocations,Inf))
View(test1)

#Decile Analysis Report
decile_grp<-group_by(test1,decile)
decile_sum_test<-summarize(decile_grp,total_cnt=n(),min_prob=min(p=Prob),
                            max_prob=max(Prob),disbursed_cnt=sum(Disbursed),
                            non_disbursed_cnt=total_cnt-disbursed_cnt)
decile_sum_test<-arrange(decile_sum_test,desc(decile)) 
View(decile_sum_test)

install.packages("InformationValue")
library(InformationValue)
cut1 <- optimalCutoff(train1$Disbursed,train1$Prob, optimiseFor = "Both", returnDiagnostics = T)
cut1
ROCTable<-data.frame(cut1$sensitivityTable)
View(ROCTable)  
train1$pred_Y <- ifelse(train1$Prob>  0.1335348,1,0)
confusionMatrix(train1$Disbursed,train1$pred_Y)


confusionMatrix(train1$Disbursed,train1$Prob, threshold =  0.1335348)

sum(train1$Disbursed)
plotROC(train1$Disbursed,train1$Prob, Show.labels = F)

install.packages("ROCR")
require(ROCR)
pred_train_fit2 <- prediction(train1$Prob, train1$Disbursed)
perf_fit2 <- performance(pred_train_fit2, "tpr", "fpr")
plot(perf_fit2)
abline(0, 1)
performance(pred_train_fit2, "auc")@y.values


#Hence we have made the above Logistic Model for Loan Disbursement.
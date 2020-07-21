options(scipen = 999)
install.packages("dplyr")
library("dplyr")

#Importing the dataset
loans_data<-read.csv("LoansData.csv")

View(loans_data)
dim(loans_data)
str(loans_data)
summary(loans_data)
View(sapply(loans_data , class))
names(loans_data)



loans_data$Interest.Rate=as.numeric(gsub("\\%", "", loans_data$Interest.Rate))
loans_data$Debt.To.Income.Ratio=as.numeric(gsub("\\%", "", loans_data$Debt.To.Income.Ratio))
loans_data$FICO.Score <- as.numeric(substring(loans_data$FICO.Range, 1, 3))+2
loans_data$FICO.Range<-NULL

loans_data<-loans_data[!loans_data$Employment.Length=='n/a',]
loans_data$Employment.Length[loans_data$Employment.Length=='< 1 year']<-0
loans_data$Employment.Length[loans_data$Employment.Length=='10+ years']<-10
loans_data$Employment.Length <- as.numeric(gsub(' years| year','',loans_data$Employment.Length))


cat_var<-names(dplyr::select_if(loans_data,is.character))
View(loans_data[cat_var])
num_var<-names(dplyr::select_if(loans_data,is.numeric))
View(loans_data[num_var])


loans_data$Loan.Length<-as.factor(loans_data$Loan.Length)
loans_data$Loan.Purpose<-as.factor(loans_data$Loan.Purpose)
loans_data$State<-as.factor(loans_data$State)
loans_data$Home.Ownership<-as.factor(loans_data$Home.Ownership)


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

diag_stats<-t(data.frame(sapply(loans_data[num_var], mystats)))
View(diag_stats)
write.csv(diag_stats,file="diag_stats_info.csv")


loans_data$Monthly.Income[loans_data$Monthly.Income>17712.487855]<-17712.487855
loans_data$FICO.Score[loans_data$FICO.Score>812.944328]<-813
loans_data$Open.CREDIT.Lines[loans_data$Open.CREDIT.Lines>24]<-23.622611
loans_data$Revolving.CREDIT.Balance[loans_data$Revolving.CREDIT.Balance>70069.546046]<-70069.546046
loans_data$Inquiries.in.the.Last.6.Months[loans_data$Inquiries.in.the.Last.6.Months>4]<-4

#Missing value treatment for continuous &categorical variables
require(Hmisc)
loans_data[num_var]<-data.frame(sapply(loans_data[num_var],function(x) impute(x,mean)))
loans_data[,cat_var] <- apply(data.frame(loans_data[,cat_var]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})
loans_data<-cbind(loans_data[,num_var],loans_data[,cat_var])


#Data Visualization
hist(loans_data$Interest.Rate,col="pink")
boxplot(loans_data$Interest.Rate, col = "orange", xlab = "loans_data$Interest.Rate",  main = "Boxplot")

boxplot(loans_data$Interest.Rate ~ loans_data$Loan.Length, col = "blue", varwidth = TRUE)
boxplot(loans_data$Interest.Rate ~ loans_data$Loan.Purpose, col = "orange", varwidth = TRUE)
boxplot(loans_data$Interest.Rate ~ loans_data$Home.Ownership, col = "purple", varwidth = TRUE)

plot(loans_data$Interest.Rate,loans_data$Monthly.Income, pch = 19, col = "pink")
plot(loans_data$Revolving.CREDIT.Balance, loans_data$Interest.Rate, pch = 19, col = "Orange")
plot(loans_data$Interest.Rate ~ loans_data$FICO.Score, col = "brown", varwidth = TRUE)
boxplot(loans_data$Interest.Rate ~ loans_data$State, col = "red", varwidth = TRUE)


#Correlation Matrix
corrm<- cor(loans_data[,num_var])
View(corrm)
write.csv(corrm, file = "correlation_matrix.csv")
install.packages("corrplot")
require(corrplot)
corrplot(cor(loans_data[,num_var],use="pairwise.complete.obs"))


#Hypothesis testing

# Compare the interest rate between the 36 months and 60 months individual
# H0 = There is no interest rate difference in interest rate between 36 and 60 months
# H1 = There exists an interest rate difference between 36 and 60 months
t.test(Interest.Rate ~ Loan.Length, data = loans_data)

# P-value (< 0.00000000000000022) < 0.05, we reject the null hypothesis




set.seed(12345)
#Splitting the dataset into Training,Validation and Testing Set
train_ind<-sample(1:nrow(loans_data),size = floor(0.70*nrow(loans_data)))

training<-loans_data[train_ind,]
testing<-loans_data[-train_ind,]

#Building Linear Regression Model
fit<-lm(Interest.Rate~Amount.Requested+Amount.Funded.By.Investors+Debt.To.Income.Ratio         
        +Monthly.Income+FICO.Score+Open.CREDIT.Lines+Revolving.CREDIT.Balance+Inquiries.in.the.Last.6.Months+
          Loan.Purpose+Loan.Length+State+Home.Ownership,data=training)
View(fit)
summary(fit)

#VIF Variance Inflation Factor
install.packages("car",dependencies = TRUE)
library("car")
vif(fit)


require(MASS)
step3<-stepAIC(fit,direction = "both")

fit2<-lm(Interest.Rate ~  Amount.Requested + Amount.Funded.By.Investors + 
            Monthly.Income + FICO.Score + Open.CREDIT.Lines + Revolving.CREDIT.Balance + 
            Inquiries.in.the.Last.6.Months + Loan.Purpose + Loan.Length,data=training)
summary(fit2)
vif(fit2)


fit3<-lm(Interest.Rate ~  Amount.Requested +  
           Monthly.Income + FICO.Score + Open.CREDIT.Lines + Revolving.CREDIT.Balance + 
           Inquiries.in.the.Last.6.Months + Loan.Purpose + Loan.Length,data=training)
summary(fit3)
vif(fit3)


#Diaganostic Plots
layout(matrix(c(1,2,3,4),2,2))
plot(fit3)  

hist(fit3$residuals) 
mean(fit3$residuals)
# The graph shows-   
# a) Residuals are normally distributed
# b) Mean of residuals~0
# c) Residuals are randomly distributed
# d) Influential observations can be seen from Cook's Distance plot


#Scoring using Predict function
t1<-cbind(training,pred_InterestRate=predict(fit3,training))
View(t1)


t2<-cbind(testing,pred_InterestRate=predict(fit3,testing))
View(t2)

#Confidence Interval
confint(fit3,level = 0.95)
confint(fit3,level = 0.99)
confint(fit3,level = 0.90)

#Decile Analysis Report---t1(training)
deLocations<-quantile(t1$pred_InterestRate,probs = seq(0.1,0.9,by=0.1))

# Use Find interval with -inf and inf as upper and lower bounds
t1$decile<-findInterval(t1$pred_InterestRate,c(-Inf,deLocations,Inf))
t1$decile<-as.factor(t1$decile)

t1_DA1<-dplyr::group_by(t1,decile)%>% summarise(count=length(decile),
                                avgPred_InterestRate=mean(pred_InterestRate),
                                avgActual_InterestRate=mean(Interest.Rate))
View(t1_DA1)


t2$decile<-findInterval(t2$pred_InterestRate,c(-Inf,deLocations,Inf))
t2$decile<-as.factor(t2$decile)

t2_DA1<-dplyr::group_by(t2,decile)%>% summarise(count=length(decile),
                                                avgPred_InterestRate=mean(pred_InterestRate),
                                                avgActual_InterestRate=mean(Interest.Rate))
View(t2_DA1)

#Using Decile Analysis we saw that both Actual and Predicted Interest Rate follow the same pattern


#Hence We have built and verified the Linear Regression Model 

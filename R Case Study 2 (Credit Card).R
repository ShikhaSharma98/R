getwd()
setwd("C:\\Users\\Desktop\\AnalytixLabs\\R case study 2 (Credit card)")

sessionInfo()
customer_acqusition=read.csv("Customer Acqusition.csv")
repayment=read.csv("Repayment.csv")
spend=read.csv("spend.csv")

library(dplyr)
library(lubridate)
library(ggplot2)

View(customer_acqusition)
View(repayment)
View(spend)

repayment$X <- NULL
repayment$SL.No. <- NULL
customer_acqusition$No <- NULL
spend$Sl.No.<- NULL
repayment$Month = as.Date(repayment$Month , format = "%d-%b-%y")
spend$Month = as.Date(spend$Month , format = "%d-%b-%y")


tran_data = merge(x = spend,y = customer_acqusition,by.x ="Customer",by.y="Customer" )
tran_data = merge(x= tran_data,y = repayment,by.x= "Customer" ,by.y = "Customer")

spnd_data = merge(x = spend,y = customer_acqusition,by.x ="Customer",by.y="Customer" )
repay_data = merge(x= repayment,y = customer_acqusition,by.x= "Customer" ,by.y = "Customer")

View(tran_data)
View(spnd_data)
View(repay_data)
options(scipen = 999)

# Q1. In the above dataset,

# a. Incase age is less than 18, replace it with mean of age values.

customer_acqusition$Age[customer_acqusition$Age <18] <- mean(customer_acqusition$Age,na.rm = T)


# b. Incase spend amount is more than the limit, replace it with 50% of that customer's limit.

spnd_data$Amount = ifelse(spnd_data$Amount > spnd_data$Limit,spnd_data$Limit*0.5,spnd_data$Amount)

# c. Incase the repayment amount is more than the limit, replace the repayment with the limit. 

repay_data$Amount= ifelse(repay_data$Amount > repay_data$Limit , repay_data$Limit, repay_data$Amount)



# Q2.From the above dataset create the following summaries: 

# a.How many distinct customers exist? 

total_uni_cus <- length(unique(tran_data$Customer))

# b.How many distinct categories exist?

uni_categories <- length(unique(tran_data$Type))

# c. What is the average monthly spend by customers?

monthly_spend <-spnd_data %>%
group_by(lubridate::month(Month)) %>%
summarise(month_average_spend = mean(Amount))


# d. What is the average monthly repayment by customers?

monthly_repay <- repay_data %>%
group_by(lubridate::month(Month)) %>%
summarise(month_average = mean(Amount))

# e. if the monthly rate of interest is 2.9%, what is the profit for the bank for each month?

monthly_profit = monthly_repay[2] - monthly_spend[2]
profit_inc_rate = monthly_profit+ 2.9*monthly_profit/100
profit = round(profit_inc_rate -  monthly_profit)


# f. What are the top 5 product types? 

top5 <- spnd_data%>% group_by(Type) %>% summarize(spnd = sum(Amount, na.rm=T)) %>% arrange(desc(spnd))
head(top5, 5)


# g. Which city is having maximum spend?

city_vise_spnd <- spnd_data %>%
group_by(City) %>%
summarise(max_spend = sum(Amount))
city_spnd <- city_vise_spnd[order(-city_vise_spnd$max_spend),]
max_spndng_city <- head(city_spnd,1)
max_spndng_city 

# h. Which age group is spending more money? 


spnd_data$AgeGrp[spnd_data$Age>=60] <- 'Senior'
spnd_data$AgeGrp[spnd_data$Age>=25 & spnd_data$Age<60] <- 'Adult'
spnd_data$AgeGrp[spnd_data$Age>=18 & spnd_data$Age<25] <- 'youth'
spnd_data$AgeGrp[spnd_data$Age<18] <- 'Kids'
agegrp_vise_spnd <- spnd_data %>%
group_by(AgeGrp) %>%
summarise(max_spend = sum(Amount))
agegrp_vise_spnd <- agegrp_vise_spnd[order(-agegrp_vise_spnd$max_spend),]
agegrp_vise_spnd

# i. Who are the top 10 customers in terms of repayment?

repy_amount <- repay_data %>% group_by(Customer) %>% summarise(repay_amt = sum(Amount))
top_cust = repy_amount[order(-repy_amount$repay_amt),] 
top_10_cust = head(top_cust,10)
top_10_cust

# 3. Calculate the city wise spend on each product on yearly basis. Also include a graphical representation for the same. 

city_vise_spnding <- spnd_data %>%group_by(City,year(Month),Type) %>% summarise(spnd_amt = sum(Amount))
ggplot(data = city_vise_spnding,aes(`year(Month)`,spnd_amt)) + geom_point(aes(colour = Type)) + facet_grid(. ~ City)



# 4. Create graphs for

# a. Monthly comparison of total spends, city wise 

monthly_spnd_city <- spnd_data %>% group_by(month(Month),City) %>% summarise(spnd_amt = sum(Amount))
ggplot(data = monthly_spnd_city,aes(`month(Month)`,spnd_amt)) + geom_line(aes(colour = monthly_spnd_city$City)) 

# b. Comparison of yearly spend on air tickets 

yearly_spnd_AT <- spnd_data %>% group_by(year(Month),Type) %>%filter(Type == 'AIR TICKET') %>% summarise(spnd_amt = sum(Amount)) 
plot(yearly_spnd_AT$`year(Month)`,yearly_spnd_AT$spnd_amt,xlab= "amount spend",ylab = "years",main = "yearly spending on air tickets", col = "orange", type = "o")

# c. Comparison of monthly spend for each product

monthly_spnd_prdct <- spnd_data %>% group_by(month(Month),Type) %>% summarise(spnding = sum(Amount))
ggplot(data = monthly_spnd_prdct,aes(`month(Month)`,spnding)) + geom_line(aes(colour = Type))



# 5. Write user defined R function to perform the following analysis:
#    You need to find top 10 customers for each city in terms of their repayment
#    amount by different products and by different time periods i.e. year or
#    month. The user should be able to specify the product
#    (Gold/Silver/Platinum) and time period (yearly or monthly) and the function
#    should automatically take these inputs while identifying the top 10 customers


R_udf <- function(){
  n <- readline(prompt="Enter the product category(Gold/Silver/Platinum): ")
  p <- readline(prompt="Enter time period(month/year): ")
  if (n=='Gold' & p=='year'){
    t <- repay_data %>% group_by(City,Customer, Product,  year= lubridate::year(repay_data$Month)) %>% summarize(reamt= sum(Amount, na.rm=T)) %>% top_n( n= 10, wt= reamt) %>% filter(Product=='Gold')
    print(t)
  } else if (n=='Silver' & p=='year'){
    c <- repay_data %>% group_by(City, Customer, Product,  year= lubridate::year(repay_data$Month)) %>% summarize(reamt= sum(Amount, na.rm=T)) %>% top_n( n= 10, wt= reamt) %>% filter(Product=='Silver')
    print(c)
  } else if (n=='Platinum' & p=='year'){
    k <- repay_data%>% group_by(City, Customer, Product,  year= lubridate::year(repay_data$Month)) %>% summarize(reamt= sum(Amount, na.rm=T)) %>% top_n( n= 10, wt= reamt) %>% filter(Product=='Platinum') 
    print(k)
  } else if (n=='Gold' & p=='month'){
    i <- repay_data %>% group_by(City, Customer, Product, month= lubridate::month(repay_data$Month, label= TRUE, abb=TRUE), year= lubridate::year(repay_data$Month)) %>% summarize(reamt= sum(Amount, na.rm=T)) %>% top_n( n= 10, wt= reamt) %>% filter(Product=='Gold') 
    print(i)  
  } else if (n=='Silver' & p=='month'){
    l <- repay_data %>% group_by(City, Customer, Product, month= lubridate::month(repay_data$Month, label= TRUE, abb=TRUE), year= lubridate::year(repay_data$Month)) %>% summarize(reamt= sum(Amount, na.rm=T)) %>% top_n( n= 10, wt= reamt) %>% filter(Product=='Silver') 
    print(l)
  } else if(n=='Platinum'& p=='month'){
    d <- repay_data %>% group_by(City, Customer, Product, month= lubridate::month(repay_data$Month, label= TRUE, abb=TRUE), year= lubridate::year(repay_data$Month)) %>% summarize(reamt= sum(Amount, na.rm=T)) %>% top_n( n= 10, wt= reamt) %>% filter(Product=='Platinum') 
    print(d)
  }
}

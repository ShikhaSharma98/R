getwd()
setwd("C:\\Users\\Desktop\\AnalytixLabs\\R case study 3 (Visualization)")
data <- read.csv(file="SalesData.csv")
str(data)
View(data)
install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("purrr")
library(dplyr)
library(ggplot2)
library(reshape2)
options(scipen = 999)

#Q1) compare sales by region for 2016 and 2015 using bar chart

t1 <- data %>% group_by(Region) %>% summarize(sales2015= sum(Sales2015, na.rm = T), sales2016= sum(Sales2016, na.rm=T))
t2 <- melt(t1, id.vars= 'Region')
p1 <- ggplot(t2, aes(x= Region, y= value, fill= variable))+ geom_bar(stat = 'identity', position = 'dodge')
p1

#Q2) pie charts for sales for each region in 2016

a <- data %>% group_by(Region) %>% summarize(Sales16= sum(Sales2016, na.rm=T))
b<- ggplot(a, aes(x='', y= Sales16, fill=Region)) +geom_bar(stat='identity')+ coord_polar('y', start=0)
b

#Q3) compare sales of 2015, 2016 with region and tiers
n1 <- data%>% group_by(Region, Tier) %>% summarize(sales15= sum(Sales2015, na.rm = T), sales16= sum(Sales2016, na.rm=T))
n2 <- melt(n1, id.vars= c('Region','Tier'))
n3 <- ggplot(data= n2, aes(x= Tier, y= value, fill= variable))+ geom_bar(stat = 'identity', position='dodge')+ facet_grid(.~Region)
n3


#Q4) in east region, which state registered a decline in 2016 as compared to 2015
p1 <- data %>% group_by(Region, State) %>% summarize(sales15= sum(Sales2015, na.rm = T), sales16= sum(Sales2016, na.rm=T)) %>% 
  filter(Region=='East')
p2 <- melt(p1, id.vars = c('Region','State'))
p3 <- ggplot(data= p2, aes(x=State, y=value, group=variable, colour= variable))+ geom_line()+geom_point()
p3
#NY state has a decline in 2016 as compared to 2015


#Q5) in all the high tier, which division saw a decline in no. of units sold in 2016 as compared to 2015?
k1 <- data %>% group_by(Tier, Division) %>% summarize(units15= sum(Units2015, na.rm = T), units16= sum(Units2016, na.rm=T)) %>% 
  filter(Tier=='High')
k2 <- melt(k1, id.vars= c('Tier','Division'))
k3 <- ggplot(data= k2, aes(x=Division, y=value, group=variable, colour= variable))+ geom_line() + geom_point()
k3
#no division saw any decline in units sold in 2016 as compared to 2015


#Q6) create a new column Qtr,
# Jan- Mar: Q1
# Apr- Jun: Q2
# Jul- Sep: Q3
# Oct- Dec: Q4
data <- data %>% mutate(Qtr= case_when((Month=='Jan' | Month=='Feb' | Month=='Mar') ~ 'Q1',
                                           (Month=='Apr' | Month=='May' | Month=='Jun') ~ 'Q2',
                                           (Month=='Jul' | Month=='Aug' | Month=='Sep') ~ 'Q3',
                                           (Month=='Oct' | Month=='Nov' | Month=='Dec')~ 'Q4'))
View(data)


#Q7) compare quarter wise sales in 2015 and 2016 in a bar plot
b1 <- data %>% group_by(Qtr) %>% summarize(Sale15= sum(Sales2015, na.rm=T), Sale16=sum(Sales2016, na.rm=T))
b2 <- melt(b1, id.vars = 'Qtr' )
b3 <- ggplot(b2, aes(x= Qtr, y= value, fill= variable))+ geom_bar(stat = 'identity', position = 'dodge')
b3



#Q8) determine the composition of Qtr wise sales in 2015 with regards to all the tiers in a pie chart.
# Draw 4 pie charts representing a quarter for each year
x1 <- data %>% group_by(Qtr, Tier) %>%  summarize(sales15= sum(Sales2015, na.rm=T)) %>% filter(Qtr=='Q1')
x2 <- data %>% group_by(Qtr, Tier) %>%  summarize(sales15= sum(Sales2015, na.rm=T)) %>% filter(Qtr=='Q2')
x3 <- data %>% group_by(Qtr, Tier) %>%  summarize(sales15= sum(Sales2015, na.rm=T)) %>% filter(Qtr=='Q3')
x4 <- data %>% group_by(Qtr, Tier) %>%  summarize(sales15= sum(Sales2015, na.rm=T)) %>% filter(Qtr=='Q4')

py1 <- pie(x1$sales15, labels= x1$Tier, main= 'Q1')
py2 <- pie(x2$sales15, labels= x2$Tier, main= 'Q2')
py3 <- pie(x3$sales15, labels= x3$Tier, main= 'Q3')
py4 <- pie(x4$sales15, labels= x4$Tier, main= 'Q4')

 
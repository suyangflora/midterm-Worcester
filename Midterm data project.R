
## I chose Leading Cause of Death to be my dataset
## from which I am trying to find patterns between different causes of deaths and regions
## Since lots of variables inside are of great amount of missing values (equal -1111 or -2222), 
## I picked 65+ years old White with cancer(F_Wh_Cancer), 65+ years old White with Heart Disease(F_Wh_HearDis),
## 45-64 years old white with Heart Disease(E_Wh_HearDis), 45-64 years old white with Cancer(E_Wh_Cancer)


LCOD<-read.csv("C:/Users/Yangsu/Desktop/Homework/R/midterm project/chsi_dataset/LEADINGCAUSESOFDEATH.csv")
attach(LCOD)
dim(LCOD)

## Build up a new data frame which includes variables I need to use
data<-data.frame(State_FIPS_Code,County_FIPS_Code,CHSI_County_Name,CHSI_State_Name,F_Wh_Cancer,F_Wh_HeartDis,E_Wh_HeartDis,E_Wh_Cancer)
attach(data)
dim(data)

library(plyr)
count(F_Wh_Cancer)
## we find 18 missing values for F_Wh_Cancer

count(F_Wh_HeartDis)
## we find 18 missing values for F_Wh_HeartDis

count(E_Wh_HeartDis)
## we find 72 missing values for E_Wh_HeartDis

count(E_Wh_Cancer)
## we find 71 missing values for E_Wh_Cancer

## all variables above have quite low percentage of missing values so I assume they should work fine.


## delete missing values, codes below credits to JiaYuan.Shi
delete1=which(F_Wh_Cancer=="-1111"|F_Wh_Cancer=="-2222"|F_Wh_HeartDis=="-1111"|
                F_Wh_HeartDis=="-2222"|E_Wh_Cancer=="-1111"|E_Wh_Cancer=="-2222"|
                E_Wh_HeartDis=="-1111"|E_Wh_HeartDis=="-2222")
data=data[-c(delete1),]
dim(data)

attach(data)
count(F_Wh_Cancer)
count(F_Wh_HeartDis)
count(E_Wh_Cancer)
count(E_Wh_HeartDis)
## so far we have no more extreme and missing values, so we can start do the analysis.

summary(data)

# mean value for F_Wh_Cancer is 21.04
# mean value for F_Wh_HeartDis is 32.7
# mean value for E_Wh_HeartDis is 25.83
# mean value for E_Wh_Cancer is 35.02

library(ggplot2)
ggplot(data,aes(x=F_Wh_Cancer))+geom_histogram(aes(y=..density..),binwidth=2,colour="black",fill="white")+
  geom_density(alpha=.2,fill="#FF6666")
ggplot(data,aes(x=F_Wh_HeartDis))+geom_histogram(aes(y=..density..),binwidth=2,colour="black",fill="white")+
  geom_density(alpha=.2,fill="#FF6666")
ggplot(data,aes(x=E_Wh_Cancer))+geom_histogram(aes(y=..density..),binwidth=2,colour="black",fill="white")+
  geom_density(alpha=.2,fill="#FF6666")
ggplot(data,aes(x=E_Wh_HeartDis))+geom_histogram(aes(y=..density..),binwidth=2,colour="black",fill="white")+
  geom_density(alpha=.2,fill="#FF6666")


boxplot(F_Wh_Cancer,F_Wh_HeartDis)
t.test(F_Wh_Cancer,F_Wh_HeartDis,alternative="two.sided")
# Those above 65 years old die of Heart disease significantly more than Cancer

boxplot(E_Wh_HeartDis,E_Wh_Cancer)
t.test(E_Wh_Cancer,E_Wh_HeartDis,alternative="two.sided")
# Those 45-64 years old die of Cancer significantly more than Heart disease

boxplot(F_Wh_Cancer,E_Wh_Cancer)
t.test(F_Wh_Cancer,E_Wh_Cancer,alternative="two.sided")
# Those who die of cancer by 45-64 years old are significantly more than above 65 years old

boxplot(F_Wh_HeartDis,E_Wh_HeartDis)
t.test(F_Wh_HeartDis,E_Wh_HeartDis,alternative="two.sided")
# Those who die of Heart Disease at the age above 65 years old are significantly more than 45-64 years old



## compare Worcester data with other counties in US

wor<-data[data$CHSI_County_Name=="Worcester" & data$CHSI_State_Name=="Massachusetts", ]
wor
# Worcester value for F_wh_Cancer is 22
# Worcester value for F_wh_HeartDis is 29
# Worcester value for E_wh_Cancer is 36
# Worcester value for E_wh_HeartDis is 21

## from above, we can see that for Worcester, two age groups whites are having higher death rate of cancer
## than mean of the whole country, but for heart disease, it shows lower rate.



## next we do kmeans

data1<-data[c(5:8)]
head(data1)
set.seed(10000)
datakmean <- kmeans(x = data1, centers = 5)
datakmean

## from which we got clusters:
## K-means clustering with 5 clusters of sizes 585, 428, 510, 897, 576

## Cluster means:
##  F_Wh_Cancer F_Wh_HeartDis E_Wh_HeartDis E_Wh_Cancer
##    20.98632      29.10940      22.52308    31.71282
##    19.67757      39.74065      28.40654    35.74766
##    20.34706      34.86471      31.57647    29.56863
##    21.42698      32.51951      26.52843    36.24638
##    22.06424      29.50347      21.07986    40.67882

require(useful)
plot(datakmean,data=data1)

## Worcester belongs to the first cluster.
## It shows that Worcester do have higher rate in death of Cancer for middle age whites.
## But lower rate in death of Heart disease.


## to find best fit cluster numbers
dataBest <- FitKMeans(data1, max.clusters=20, seed=10000)
dataBest
PlotHartigan(dataBest)
## I think clusters of 10 works the best because after 10, Hartigan do not drop a lot


## Conclusion: I would strongly suggest leader of Worcester in Massachusetts put more effort on 
## caring about cancer risk for middle age whites and try to find out the reasons for causing the cancer.
# CreditCardSeg
Segmentation of Credit Card users

#Packages:
require(psych)
require(GPArotation)
require(corrgram)
require(dplyr)
require(tables)


#importing data:
ccdata<- read.csv(file = "C:/Study/SAS&R/Final Case-studies and study/Credit Card Segmentation/CC GENERAL.csv")
str(ccdata)

ccdatacheck<- ccdata[, -1]

#Missing values: Credit Limit(1), Minimum_Payments(313)
blank<- function(x){sum(is.na(x))}
apply(ccdata, 2, blank)

mystats<- function(x){
    nmiss<- sum(is.na(x))
    a<- x[!is.na(x)]
    m<- mean(a)
    sdv<- sd(a)
    mi<- min(a)
    qtrs<- quantile(a, probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
    q<- quantile(a, probs = 0.99)
    UL1<- m+(2*sdv)
    UL2<- m+(3*sdv)
    ma<- max(a)
    outl1<- 0
    outl2<- 0
    outl1[ma>UL2]<- 1
    outl2[ma>q]<- 1
    nout<- sum(a>q)
    return(c(nmiss=nmiss, mean=m, sdv=sdv, min=mi, qtrs=qtrs, UL2=UL2, max=ma, outl1=outl1, outl2=outl2, nout=nout))
}

check<- data.matrix(t(apply(ccdatacheck, 2, mystats)))

write.csv(check, file = "C:/Study/SAS&R/Final Case-studies and study/Credit Card Segmentation/Soultion_CC/datacleaning.csv")

#Outlier capping

ccdata$BALANCE[ccdata$BALANCE>5909.11180785]<- 5909.11180785
ccdata$PURCHASES[ccdata$PURCHASES>3998.6195]<- 3998.6195
ccdata$ONEOFF_PURCHASES[ccdata$ONEOFF_PURCHASES>2671.09399999999]<- 2671.09399999999
ccdata$INSTALLMENTS_PURCHASES[ccdata$INSTALLMENTS_PURCHASES>1750.0875]<- 1750.0875
ccdata$CASH_ADVANCE[ccdata$CASH_ADVANCE>4647.16912199999]<- 4647.16912199999
ccdata$CASH_ADVANCE_TRX[ccdata$CASH_ADVANCE_TRX>15]<- 15
ccdata$PURCHASES_TRX[ccdata$PURCHASES_TRX>57]<- 57
ccdata$CREDIT_LIMIT[ccdata$CREDIT_LIMIT>12000]<- 12000
ccdata$PAYMENTS[ccdata$PAYMENTS>6082.09059525]<- 6082.09059525
ccdata$MINIMUM_PAYMENTS[ccdata$MINIMUM_PAYMENTS>2766.56331]<- 2766.56331

#Missing calue treatment
miss<- ccdata[is.na(ccdata$MINIMUM_PAYMENTS), ]
ccdata$MINIMUM_PAYMENTS[ccdata$BALANCE==0]<- 0
replace(ccdata$MINIMUM_PAYMENTS, list = is.na(ccdata$MINIMUM_PAYMENTS), (ccdata$BALANCE/3))

ccdata[is.na(ccdata$CREDIT_LIMIT), ]
check<- ccdata[ccdata$BALANCE==0,]
ccdata$CREDIT_LIMIT[is.na(ccdata$CREDIT_LIMIT)]<-  median(ccdata$CREDIT_LIMIT, na.rm = T)

#Calculating KPIs:

#Monthly Average Purchase= Purchases/Tenure
ccdata$Mon_Avg_Purchase<- ccdata$PURCHASES/ccdata$TENURE

#Monthly Average Cash Advance amount= Cash Advance/Tenure
ccdata$Mon_Avg_Cash<- ccdata$CASH_ADVANCE/ccdata$TENURE

#Limit Usage= Balance/Credit_limit
ccdata$Limit_Usage<- ccdata$BALANCE/ccdata$CREDIT_LIMIT

#Average Amount/Purchase
ccdata$Avg_Amt_purchase<- ccdata$PURCHASES/ccdata$PURCHASES_TRX
ccdata$Avg_Amt_purchase[ccdata$Avg_Amt_purchase=="NaN"]<- 0
ccdata$Avg_Amt_purchase[ccdata$PURCHASES_TRX==0]<- 0

#Average Amount/Cash Advance
ccdata$Avg_Amt_Cash<- ccdata$CASH_ADVANCE/ccdata$CASH_ADVANCE_TRX
ccdata$Avg_Amt_Cash[ccdata$Avg_Amt_Cash=="NaN"]<- 0
ccdata$Avg_Amt_Cash[ccdata$CASH_ADVANCE_TRX==0]<- 0

#Payments/Min Payments
ccdata$Pay_to_minpay<- ccdata$PAYMENTS/ccdata$MINIMUM_PAYMENTS
ccdata$Pay_to_minpay[ccdata$MINIMUM_PAYMENTS==0]<- 0

#Factor analysis to reduce colliniarity
ccdatacheck<- ccdata[, -1]
corrl<- cor(ccdatacheck)
corrgram(ccdatacheck, order = T, lower.panel = panel.shade, upper.panel = NULL,
         text.panel = panel.txt)

#find the no. of factors
scree(corrl, factors = T, pc= T, main = "Scree Plot", hline = NULL, add = F)
eigen(corrl)$values
eigen_values <- mutate(data.frame(eigen(corrl)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrl..values)
                       , pct_var=eigen.corrl..values/sum(eigen.corrl..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrl..values)) 

#7 or 8 factors seems to be good
F_Analysis<- fa(r= corrl, 8, rotate = "varimax", fm = "ml")
F_Analysis<- fa(r= corrl, 7, rotate = "varimax", fm = "ml")
F_Analysis<- fa(r= corrl, 6, rotate = "varimax", fm = "ml")
F_Analysis<- fa(r= corrl, 5, rotate = "varimax", fm = "ml")
F_Analysis<- fa(r= corrl, 9, rotate = "varimax", fm = "ml")
F_Analysis<- fa(r= corrl, 4, rotate = "varimax", fm = "ml")
F_Sort<- fa.sort(F_Analysis)
F_Sort$loadings
F_Loading<- data.frame(F_Sort$loadings[1:ncol(ccdatacheck),])
write.csv(F_Loading, file = "C:/Study/SAS&R/Final Case-studies and study/Karthik Sol/Credit Card Segmentation/FactorCC4.csv")


#Variables selected:
#ONEOFF_PURCHASES
#Mon_Avg_Purchase
#PAYMENTS
#Avg_Amt_purchase
#PURCHASES_FREQUENCY
#INSTALLMENTS_PURCHASES
#BALANCE
#Limit_Usage
#Mon_Avg_Cash
#Avg_Amt_Cash
#TENURE

CC_Seg<- ccdata[ ,c("BALANCE", "Mon_Avg_Purchase", "Avg_Amt_purchase",
                    "PURCHASES_FREQUENCY", "PAYMENTS", "ONEOFF_PURCHASES",
                    "INSTALLMENTS_PURCHASES", "Mon_Avg_Cash", "Avg_Amt_Cash",
                    "Limit_Usage", "TENURE")]

CC_Seg1<- ccdata[ ,c("BALANCE", "Mon_Avg_Purchase", "Avg_Amt_purchase",
                    "PURCHASES_FREQUENCY", "PAYMENTS", "ONEOFF_PURCHASES",
                    "INSTALLMENTS_PURCHASES", "Mon_Avg_Cash", "Avg_Amt_Cash",
                    "Limit_Usage", "TENURE")]

#Standardizing
CC_Seg<- scale(CC_Seg)

#Clusters using K-Means
Cl_four<- kmeans(CC_Seg, 4)
Cl_five<- kmeans(CC_Seg, 5)
Cl_six<-  kmeans(CC_Seg, 6)

CC_Seg1<- cbind(CC_Seg1, Cluster4=Cl_four$cluster, Cluster5=Cl_five$cluster, Cluster6=Cl_six$cluster)
CC_Seg1$Cluster4<- factor(CC_Seg1$Cluster4)
CC_Seg1$Cluster5<- factor(CC_Seg1$Cluster5)
CC_Seg1$Cluster6<- factor(CC_Seg1$Cluster6)

#profiling:
profile1<- tabular(1+BALANCE+Mon_Avg_Purchase+Avg_Amt_purchase+PURCHASES_FREQUENCY+PAYMENTS+
                     ONEOFF_PURCHASES+INSTALLMENTS_PURCHASES+Mon_Avg_Cash+Avg_Amt_Cash+
                     Limit_Usage+TENURE ~ mean + (mean*Cluster4)+(mean*Cluster5)+(mean*Cluster6), data = CC_Seg1)
profile1<- data.frame(as.matrix(profile1))
profile2<- tabular(1 ~ length+(length*Cluster4)+(length*Cluster5)+(length*Cluster6), data = CC_Seg1)
profile2<- data.frame(as.matrix(profile2))

write.csv(profile1, file = "C:/Study/SAS&R/Final Case-studies and study/Karthik Sol/Credit Card Segmentation/profile1.csv")
write.csv(profile2, file = "C:/Study/SAS&R/Final Case-studies and study/Karthik Sol/Credit Card Segmentation/profile2.csv")

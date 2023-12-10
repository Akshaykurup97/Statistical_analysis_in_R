#Few of libraries used in our Project


> library("tidyverse")
> library("dplyr")
> library("tidyr")
> library("ggplot2")
> library("missMethods")
> library("Hmisc")
> library("BiocManager")
> library("mice")
> library("naniar")
>library("ggthemes)
>library("ggpubr"")
>library("broom)
>library("rms")



#Reading the data 
>data<-read.csv("C:/Users/skyalkond/Desktop/572 project/data.txt")
> View(data)


#Summary of Credit_Limit in our data
>summary(data$Credit_Limit)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1438    2555    4549    8632   11068   34516 
#Assumptions
#Checking for normality of data
> attrited<-subset(data,data$Attrition_Flag=="Attrited Customer")
> existing<-subset(data,data$Attrition_Flag=="Existing Customer")
> shapiro.test(attrited$Credit_Limit)

Shapiro-Wilk normality test

data:  attrited$Credit_Limit
W = 0.71476, p-value < 2.2e-16
> ad.test(existing$Credit_Limit)

Anderson-Darling normality test

data:  existing$Credit_Limit
A = 779.17, p-value < 2.2e-16
#To Visualize the normality
> ggdensity(attrited$Credit_Limit, main = "Normality", xlab = "Credit Limit Attrited Customer")
> ggdensity(existing$Credit_Limit, main = "Normality", xlab = "Credit Limit Existing Customer")

#Wilcoxon Rank Sum Test
> wilcox.test(attrited$Credit_Limit,existing$Credit_Limit)

Wilcoxon rank sum test with continuity correction

data:  attrited$Credit_Limit and existing$Credit_Limit
W = 6361348, p-value = 3.008e-07
alternative hypothesis: true location shift is not equal to 0

#MCAR for Hypothesis1
#Creating missing data
>data_mcar1- delete_MCAR(data, 0.2,"Credit_Limit")
#Checking for no.of N/A values 
> sapply(data_mcar1,function(x) sum(is.na(x)))
#Imputing missing values
> imputed_Data <- mice(data_mcar1, m=5, maxit = 50, method = 'cart', seed = 500)
> completeData1 <- complete(imputed_Data,2)
#Confirming no missing values in our data
> sapply(completeData1,function(x) sum(is.na(x)))
#Summary after imputing values
> summary(completeData1$Credit_Limit)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1438    2554    4555    8637   11094   34516 
#WIlcoxon Rank Sum Test for Hypothesis on Mean
> attrited3<-subset(completeData1,completeData1$Attrition_Flag=="Attrited Customer")
> existing3<-subset(completeData1,completeData1$Attrition_Flag=="Existing Customer")
> wilcox.test(attrited3$Credit_Limit,existing3$Credit_Limit)

Wilcoxon rank sum test with continuity correction

data:  attrited3$Credit_Limit and existing3$Credit_Limit
W = 6266213, p-value = 3.595e-07
alternative hypothesis: true location shift is not equal to 0

#MNAR for Hypothesis 1
#Creating missing data
> data_mnar1<-delete_MNAR_censoring(data,0.2,"Credit_Limit")
#Checking for no.of N/A values 
> sapply(data_mnar1,function(x) sum(is.na(x)))
#Imputing missing values
> imputed_Data <- mice(data_mnar1, m=5, maxit = 50, method = 'rp', seed = 500)
> completeData2 <- complete(imputed_Data,2)
#Confirming no missing values in our data
> sapply(completeData2,function(x) sum(is.na(x)))
#WIlcoxon Rank Sum Test for Hypothesis on Mean
> attrited2<-subset(completeData2,completeData2$Attrition_Flag=="Attrited Customer")
> existing2<-subset(completeData2,completeData2$Attrition_Flag=="Existing Customer")
> wilcox.test(attrited2$Credit_Limit,existing2$Credit_Limit)

Wilcoxon rank sum test with continuity correction

data:  attrited2$Credit_Limit and existing2$Credit_Limit
W = 7397592, p-value = 7.839e-06
alternative hypothesis: true location shift is not equal to 0
# We Performed Several Data Visualizations as part of Exploratory Data Analysis.
# We devised to use Logistic Regression.
data <- data %>% 
  mutate(Dummy_Attrition = ifelse(Attrition_Flag == "Attrited Customer", 1, 0))

model_ONLY_transAmt_transCt <- glm(data= data, formula = Dummy_Attrition ~ Total_Trans_Ct * Total_Trans_Amt , 
                                   family = "binomial" (link = "logit"))

broom::tidy(model_ONLY_transAmt_transCt)

summary(model_ONLY_transAmt_transCt)


model_All <- glm(data= data, formula = Dummy_Attrition ~ Customer_Age + Gender + 
                   Dependent_count + Education_Level + Marital_Status + Income_Category + 
                   Card_Category + Months_on_book + Total_Relationship_Count + 
                   Months_Inactive_12_mon + Contacts_Count_12_mon + Credit_Limit + 
                   Total_Revolving_Bal + Avg_Open_To_Buy + Total_Amt_Chng_Q4_Q1 + 
                   Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1 + Avg_Utilization_Ratio + Total_Trans_Ct * Total_Trans_Amt , 
                 family = "binomial" (link = "logit"))



summary(model_All)


model_final <- glm(data= data, formula = Dummy_Attrition ~ Gender +
                     Dependent_count + Income_Category  + 
                     Total_Relationship_Count + 
                     Months_Inactive_12_mon + Contacts_Count_12_mon + 
                     Total_Revolving_Bal + Total_Amt_Chng_Q4_Q1 + 
                     Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1 + Total_Trans_Ct * Total_Trans_Amt , 
                   family = "binomial" (link = "logit"))
summary(model_final)

# Missing Values Analysis for Hypothesis #2
# MCAR for Hypothesis 2
> data_mcar2<- delete_MCAR(data, 0.2, c("Total_Trans_Amt","Months_Inactive_12_mon","Total_Revolving_Bal"))
> summary(data_mcar2)
   CLIENTNUM         Attrition_Flag      Customer_Age      Gender          Dependent_count Education_Level   
 Min.   :708082083   Length:10127       Min.   :26.00   Length:10127       Min.   :0.000   Length:10127      
 1st Qu.:713036770   Class :character   1st Qu.:41.00   Class :character   1st Qu.:1.000   Class :character  
 Median :717926358   Mode  :character   Median :46.00   Mode  :character   Median :2.000   Mode  :character  
 Mean   :739177606                      Mean   :46.33                      Mean   :2.346                     
 3rd Qu.:773143533                      3rd Qu.:52.00                      3rd Qu.:3.000                     
 Max.   :828343083                      Max.   :73.00                      Max.   :5.000                     
                                                                                                             
 Marital_Status     Income_Category    Card_Category      Months_on_book  Total_Relationship_Count Months_Inactive_12_mon
 Length:10127       Length:10127       Length:10127       Min.   :13.00   Min.   :1.000            Min.   :0.000         
 Class :character   Class :character   Class :character   1st Qu.:31.00   1st Qu.:3.000            1st Qu.:2.000         
 Mode  :character   Mode  :character   Mode  :character   Median :36.00   Median :4.000            Median :2.000         
                                                          Mean   :35.93   Mean   :3.813            Mean   :2.339         
                                                          3rd Qu.:40.00   3rd Qu.:5.000            3rd Qu.:3.000         
                                                          Max.   :56.00   Max.   :6.000            Max.   :6.000         
                                                                                                   NA's   :2025          
 Contacts_Count_12_mon  Credit_Limit   Total_Revolving_Bal Avg_Open_To_Buy Total_Amt_Chng_Q4_Q1 Total_Trans_Amt
 Min.   :0.000         Min.   : 1438   Min.   :   0.0      Min.   :    3   Min.   :0.0000       Min.   :  563  
 1st Qu.:2.000         1st Qu.: 2555   1st Qu.: 397.8      1st Qu.: 1324   1st Qu.:0.6310       1st Qu.: 2148  
 Median :2.000         Median : 4549   Median :1285.0      Median : 3474   Median :0.7360       Median : 3889  
 Mean   :2.455         Mean   : 8632   Mean   :1166.5      Mean   : 7469   Mean   :0.7599       Mean   : 4390  
 3rd Qu.:3.000         3rd Qu.:11068   3rd Qu.:1788.0      3rd Qu.: 9859   3rd Qu.:0.8590       3rd Qu.: 4732  
 Max.   :6.000         Max.   :34516   Max.   :2517.0      Max.   :34516   Max.   :3.3970       Max.   :18484  
                                       NA's   :2025                                             NA's   :2025   
 Total_Trans_Ct   Total_Ct_Chng_Q4_Q1 Avg_Utilization_Ratio
 Min.   : 10.00   Min.   :0.0000      Min.   :0.0000       
 1st Qu.: 45.00   1st Qu.:0.5820      1st Qu.:0.0230       
 Median : 67.00   Median :0.7020      Median :0.1760       
 Mean   : 64.86   Mean   :0.7122      Mean   :0.2749       
 3rd Qu.: 81.00   3rd Qu.:0.8180      3rd Qu.:0.5030       
 Max.   :139.00   Max.   :3.7140      Max.   :0.9990       
                                                           
 Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1
 Min.   :0.0000077                                                                                                                 
 1st Qu.:0.0000990                                                                                                                 
 Median :0.0001815                                                                                                                 
 Mean   :0.1599975                                                                                                                 
 3rd Qu.:0.0003373                                                                                                                 
 Max.   :0.9995800                                                                                                                 
                                                                                                                                   
 Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2
 Min.   :0.00042                                                                                                                   
 1st Qu.:0.99966                                                                                                                   
 Median :0.99982                                                                                                                   
 Mean   :0.84000                                                                                                                   
 3rd Qu.:0.99990                                                                                                                   
 Max.   :0.99999                                                                                                                   
                                                                                                                                   
> imputed_data_mcar2 <- mice(data_mcar2,m=5,maxit=50,method='rf',seed=500)

 iter imp variable
  1   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  1   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  1   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  1   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  1   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  2   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  2   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  2   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  2   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  2   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  3   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  3   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  3   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  3   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  3   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  4   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  4   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  4   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  4   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  4   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  5   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  5   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  5   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  5   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  5   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  6   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  6   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  6   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  6   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  6   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  7   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  7   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  7   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  7   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  7   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  8   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  8   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  8   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  8   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  8   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  9   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  9   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  9   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  9   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  9   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  10   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  10   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  10   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  10   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  10   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  11   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  11   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  11   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  11   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  11   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  12   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  12   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  12   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  12   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  12   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  13   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  13   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  13   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  13   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  13   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  14   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  14   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  14   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  14   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  14   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  15   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  15   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  15   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  15   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  15   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  16   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  16   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  16   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  16   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  16   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  17   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  17   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  17   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  17   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  17   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  18   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  18   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  18   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  18   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  18   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  19   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  19   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  19   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  19   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  19   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  20   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  20   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  20   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  20   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  20   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  21   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  21   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  21   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  21   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  21   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  22   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  22   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  22   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  22   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  22   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  23   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  23   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  23   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  23   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  23   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  24   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  24   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  24   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  24   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  24   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  25   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  25   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  25   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  25   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  25   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  26   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  26   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  26   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  26   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  26   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  27   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  27   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  27   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  27   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  27   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  28   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  28   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  28   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  28   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  28   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  29   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  29   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  29   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  29   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  29   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  30   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  30   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  30   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  30   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  30   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  31   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  31   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  31   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  31   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  31   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  32   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  32   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  32   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  32   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  32   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  33   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  33   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  33   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  33   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  33   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  34   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  34   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  34   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  34   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  34   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  35   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  35   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  35   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  35   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  35   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  36   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  36   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  36   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  36   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  36   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  37   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  37   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  37   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  37   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  37   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  38   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  38   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  38   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  38   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  38   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  39   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  39   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  39   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  39   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  39   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  40   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  40   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  40   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  40   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  40   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  41   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  41   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  41   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  41   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  41   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  42   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  42   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  42   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  42   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  42   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  43   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  43   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  43   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  43   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  43   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  44   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  44   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  44   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  44   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  44   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  45   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  45   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  45   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  45   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  45   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  46   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  46   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  46   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  46   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  46   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  47   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  47   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  47   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  47   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  47   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  48   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  48   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  48   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  48   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  48   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  49   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  49   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  49   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  49   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  49   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  50   1  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  50   2  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  50   3  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  50   4  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
  50   5  Months_Inactive_12_mon  Total_Revolving_Bal  Total_Trans_Amt
Warning message:
Number of logged events: 502 
> completeData1 <- complete(imputed_data_mcar2,2)
> summary(completeData1)
   CLIENTNUM         Attrition_Flag      Customer_Age      Gender          Dependent_count Education_Level   
 Min.   :708082083   Length:10127       Min.   :26.00   Length:10127       Min.   :0.000   Length:10127      
 1st Qu.:713036770   Class :character   1st Qu.:41.00   Class :character   1st Qu.:1.000   Class :character  
 Median :717926358   Mode  :character   Median :46.00   Mode  :character   Median :2.000   Mode  :character  
 Mean   :739177606                      Mean   :46.33                      Mean   :2.346                     
 3rd Qu.:773143533                      3rd Qu.:52.00                      3rd Qu.:3.000                     
 Max.   :828343083                      Max.   :73.00                      Max.   :5.000                     
 Marital_Status     Income_Category    Card_Category      Months_on_book  Total_Relationship_Count Months_Inactive_12_mon
 Length:10127       Length:10127       Length:10127       Min.   :13.00   Min.   :1.000            Min.   :0.000         
 Class :character   Class :character   Class :character   1st Qu.:31.00   1st Qu.:3.000            1st Qu.:2.000         
 Mode  :character   Mode  :character   Mode  :character   Median :36.00   Median :4.000            Median :2.000         
                                                          Mean   :35.93   Mean   :3.813            Mean   :2.329         
                                                          3rd Qu.:40.00   3rd Qu.:5.000            3rd Qu.:3.000         
                                                          Max.   :56.00   Max.   :6.000            Max.   :6.000         
 Contacts_Count_12_mon  Credit_Limit   Total_Revolving_Bal Avg_Open_To_Buy Total_Amt_Chng_Q4_Q1 Total_Trans_Amt
 Min.   :0.000         Min.   : 1438   Min.   :   0        Min.   :    3   Min.   :0.0000       Min.   :  563  
 1st Qu.:2.000         1st Qu.: 2555   1st Qu.: 243        1st Qu.: 1324   1st Qu.:0.6310       1st Qu.: 2148  
 Median :2.000         Median : 4549   Median :1276        Median : 3474   Median :0.7360       Median : 3894  
 Mean   :2.455         Mean   : 8632   Mean   :1161        Mean   : 7469   Mean   :0.7599       Mean   : 4403  
 3rd Qu.:3.000         3rd Qu.:11068   3rd Qu.:1786        3rd Qu.: 9859   3rd Qu.:0.8590       3rd Qu.: 4739  
 Max.   :6.000         Max.   :34516   Max.   :2517        Max.   :34516   Max.   :3.3970       Max.   :18484  
 Total_Trans_Ct   Total_Ct_Chng_Q4_Q1 Avg_Utilization_Ratio
 Min.   : 10.00   Min.   :0.0000      Min.   :0.0000       
 1st Qu.: 45.00   1st Qu.:0.5820      1st Qu.:0.0230       
 Median : 67.00   Median :0.7020      Median :0.1760       
 Mean   : 64.86   Mean   :0.7122      Mean   :0.2749       
 3rd Qu.: 81.00   3rd Qu.:0.8180      3rd Qu.:0.5030       
 Max.   :139.00   Max.   :3.7140      Max.   :0.9990       
 Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1
 Min.   :0.0000077                                                                                                                 
 1st Qu.:0.0000990                                                                                                                 
 Median :0.0001815                                                                                                                 
 Mean   :0.1599975                                                                                                                 
 3rd Qu.:0.0003373                                                                                                                 
 Max.   :0.9995800                                                                                                                 
 Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2
 Min.   :0.00042                                                                                                                   
 1st Qu.:0.99966                                                                                                                   
 Median :0.99982                                                                                                                   
 Mean   :0.84000                                                                                                                   
 3rd Qu.:0.99990                                                                                                                   
 Max.   :0.99999                                                                                                                   
> completeData1 <- completeData1 %>% 
+ +   mutate(Dummy_Attrition = ifelse(Attrition_Flag == "Attrited Customer", 1, 0))
Error in ifelse(Attrition_Flag == "Attrited Customer", 1, 0) : 
  object 'Attrition_Flag' not found
> completeData1 <- completeData1 %>% mutate(Dummy_Attrition = ifelse(Attrition_Flag == "Attrited Customer", 1, 0))
> model_final <- glm(data= completeData1, formula = Dummy_Attrition ~ Gender +
+                      Dependent_count + Income_Category  + 
+                      Total_Relationship_Count + 
+                      Months_Inactive_12_mon + Contacts_Count_12_mon + 
+                      Total_Revolving_Bal + Total_Amt_Chng_Q4_Q1 + 
+                      Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1 + Total_Trans_Ct * Total_Trans_Amt , 
+                    family = "binomial" (link = "logit"))
> summary(model_final)

Call:
glm(formula = Dummy_Attrition ~ Gender + Dependent_count + Income_Category + 
    Total_Relationship_Count + Months_Inactive_12_mon + Contacts_Count_12_mon + 
    Total_Revolving_Bal + Total_Amt_Chng_Q4_Q1 + Total_Trans_Amt + 
    Total_Trans_Ct + Total_Ct_Chng_Q4_Q1 + Total_Trans_Ct * Total_Trans_Amt, 
    family = binomial(link = "logit"), data = completeData1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-6.5245  -0.3365  -0.1374  -0.0341   3.8170  

Coefficients:
                                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)                     3.121e+00  3.643e-01   8.566  < 2e-16 ***
GenderM                        -8.692e-01  1.504e-01  -5.780 7.49e-09 ***
Dependent_count                 1.186e-01  3.058e-02   3.878 0.000105 ***
Income_Category$40K - $60K     -6.241e-01  1.920e-01  -3.250 0.001155 ** 
Income_Category$60K - $80K     -3.632e-01  1.773e-01  -2.048 0.040517 *  
Income_Category$80K - $120K    -2.282e-01  1.724e-01  -1.323 0.185670    
Income_CategoryLess than $40K  -5.018e-01  2.080e-01  -2.412 0.015858 *  
Income_CategoryUnknown         -6.941e-01  2.338e-01  -2.969 0.002992 ** 
Total_Relationship_Count       -4.537e-01  2.795e-02 -16.233  < 2e-16 ***
Months_Inactive_12_mon          5.439e-01  3.984e-02  13.654  < 2e-16 ***
Contacts_Count_12_mon           4.749e-01  3.730e-02  12.730  < 2e-16 ***
Total_Revolving_Bal            -9.284e-04  4.723e-05 -19.657  < 2e-16 ***
Total_Amt_Chng_Q4_Q1           -9.749e-01  1.984e-01  -4.914 8.94e-07 ***
Total_Trans_Amt                 2.272e-03  1.103e-04  20.594  < 2e-16 ***
Total_Trans_Ct                 -8.721e-02  4.213e-03 -20.703  < 2e-16 ***
Total_Ct_Chng_Q4_Q1            -3.016e+00  2.027e-01 -14.875  < 2e-16 ***
Total_Trans_Amt:Total_Trans_Ct -2.075e-05  1.290e-06 -16.082  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 8927.2  on 10126  degrees of freedom
Residual deviance: 4387.7  on 10110  degrees of freedom
AIC: 4421.7

Number of Fisher Scoring iterations: 8

# MNAR for Hypothesis #2
> data.mis<-delete_MNAR_censoring(data,0.15,"Total_Revolving_Bal")
> sapply(data.mis,function(x) sum(is.na(x)))
                                                                                                                         CLIENTNUM 
                                                                                                                                 0 
                                                                                                                    Attrition_Flag 
                                                                                                                                 0 
                                                                                                                      Customer_Age 
                                                                                                                                 0 
                                                                                                                            Gender 
                                                                                                                                 0 
                                                                                                                   Dependent_count 
                                                                                                                                 0 
                                                                                                                   Education_Level 
                                                                                                                                 0 
                                                                                                                    Marital_Status 
                                                                                                                                 0 
                                                                                                                   Income_Category 
                                                                                                                                 0 
                                                                                                                     Card_Category 
                                                                                                                                 0 
                                                                                                                    Months_on_book 
                                                                                                                                 0 
                                                                                                          Total_Relationship_Count 
                                                                                                                                 0 
                                                                                                            Months_Inactive_12_mon 
                                                                                                                                 0 
                                                                                                             Contacts_Count_12_mon 
                                                                                                                                 0 
                                                                                                                      Credit_Limit 
                                                                                                                                 0 
                                                                                                               Total_Revolving_Bal 
                                                                                                                              1519 
                                                                                                                   Avg_Open_To_Buy 
                                                                                                                                 0 
                                                                                                              Total_Amt_Chng_Q4_Q1 
                                                                                                                                 0 
                                                                                                                   Total_Trans_Amt 
                                                                                                                                 0 
                                                                                                                    Total_Trans_Ct 
                                                                                                                                 0 
                                                                                                               Total_Ct_Chng_Q4_Q1 
                                                                                                                                 0 
                                                                                                             Avg_Utilization_Ratio 
                                                                                                                                 0 
Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1 
                                                                                                                                 0 
Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2 
                                                                                                                                 0 
                                                                                                                                 
> imputed_Data <- mice(data.mis, m=5, maxit = 50, method = 'cart', seed = 500)

 iter imp variable
  1   1  Total_Revolving_Bal
  1   2  Total_Revolving_Bal
  1   3  Total_Revolving_Bal
  1   4  Total_Revolving_Bal
  1   5  Total_Revolving_Bal
  2   1  Total_Revolving_Bal
  2   2  Total_Revolving_Bal
  2   3  Total_Revolving_Bal
  2   4  Total_Revolving_Bal
  2   5  Total_Revolving_Bal
  3   1  Total_Revolving_Bal
  3   2  Total_Revolving_Bal
  3   3  Total_Revolving_Bal
  3   4  Total_Revolving_Bal
  3   5  Total_Revolving_Bal
  4   1  Total_Revolving_Bal
  4   2  Total_Revolving_Bal
  4   3  Total_Revolving_Bal
  4   4  Total_Revolving_Bal
  4   5  Total_Revolving_Bal
  5   1  Total_Revolving_Bal
  5   2  Total_Revolving_Bal
  5   3  Total_Revolving_Bal
  5   4  Total_Revolving_Bal
  5   5  Total_Revolving_Bal
  6   1  Total_Revolving_Bal
  6   2  Total_Revolving_Bal
  6   3  Total_Revolving_Bal
  6   4  Total_Revolving_Bal
  6   5  Total_Revolving_Bal
  7   1  Total_Revolving_Bal
  7   2  Total_Revolving_Bal
  7   3  Total_Revolving_Bal
  7   4  Total_Revolving_Bal
  7   5  Total_Revolving_Bal
  8   1  Total_Revolving_Bal
  8   2  Total_Revolving_Bal
  8   3  Total_Revolving_Bal
  8   4  Total_Revolving_Bal
  8   5  Total_Revolving_Bal
  9   1  Total_Revolving_Bal
  9   2  Total_Revolving_Bal
  9   3  Total_Revolving_Bal
  9   4  Total_Revolving_Bal
  9   5  Total_Revolving_Bal
  10   1  Total_Revolving_Bal
  10   2  Total_Revolving_Bal
  10   3  Total_Revolving_Bal
  10   4  Total_Revolving_Bal
  10   5  Total_Revolving_Bal
  11   1  Total_Revolving_Bal
  11   2  Total_Revolving_Bal
  11   3  Total_Revolving_Bal
  11   4  Total_Revolving_Bal
  11   5  Total_Revolving_Bal
  12   1  Total_Revolving_Bal
  12   2  Total_Revolving_Bal
  12   3  Total_Revolving_Bal
  12   4  Total_Revolving_Bal
  12   5  Total_Revolving_Bal
  13   1  Total_Revolving_Bal
  13   2  Total_Revolving_Bal
  13   3  Total_Revolving_Bal
  13   4  Total_Revolving_Bal
  13   5  Total_Revolving_Bal
  14   1  Total_Revolving_Bal
  14   2  Total_Revolving_Bal
  14   3  Total_Revolving_Bal
  14   4  Total_Revolving_Bal
  14   5  Total_Revolving_Bal
  15   1  Total_Revolving_Bal
  15   2  Total_Revolving_Bal
  15   3  Total_Revolving_Bal
  15   4  Total_Revolving_Bal
  15   5  Total_Revolving_Bal
  16   1  Total_Revolving_Bal
  16   2  Total_Revolving_Bal
  16   3  Total_Revolving_Bal
  16   4  Total_Revolving_Bal
  16   5  Total_Revolving_Bal
  17   1  Total_Revolving_Bal
  17   2  Total_Revolving_Bal
  17   3  Total_Revolving_Bal
  17   4  Total_Revolving_Bal
  17   5  Total_Revolving_Bal
  18   1  Total_Revolving_Bal
  18   2  Total_Revolving_Bal
  18   3  Total_Revolving_Bal
  18   4  Total_Revolving_Bal
  18   5  Total_Revolving_Bal
  19   1  Total_Revolving_Bal
  19   2  Total_Revolving_Bal
  19   3  Total_Revolving_Bal
  19   4  Total_Revolving_Bal
  19   5  Total_Revolving_Bal
  20   1  Total_Revolving_Bal
  20   2  Total_Revolving_Bal
  20   3  Total_Revolving_Bal
  20   4  Total_Revolving_Bal
  20   5  Total_Revolving_Bal
  21   1  Total_Revolving_Bal
  21   2  Total_Revolving_Bal
  21   3  Total_Revolving_Bal
  21   4  Total_Revolving_Bal
  21   5  Total_Revolving_Bal
  22   1  Total_Revolving_Bal
  22   2  Total_Revolving_Bal
  22   3  Total_Revolving_Bal
  22   4  Total_Revolving_Bal
  22   5  Total_Revolving_Bal
  23   1  Total_Revolving_Bal
  23   2  Total_Revolving_Bal
  23   3  Total_Revolving_Bal
  23   4  Total_Revolving_Bal
  23   5  Total_Revolving_Bal
  24   1  Total_Revolving_Bal
  24   2  Total_Revolving_Bal
  24   3  Total_Revolving_Bal
  24   4  Total_Revolving_Bal
  24   5  Total_Revolving_Bal
  25   1  Total_Revolving_Bal
  25   2  Total_Revolving_Bal
  25   3  Total_Revolving_Bal
  25   4  Total_Revolving_Bal
  25   5  Total_Revolving_Bal
  26   1  Total_Revolving_Bal
  26   2  Total_Revolving_Bal
  26   3  Total_Revolving_Bal
  26   4  Total_Revolving_Bal
  26   5  Total_Revolving_Bal
  27   1  Total_Revolving_Bal
  27   2  Total_Revolving_Bal
  27   3  Total_Revolving_Bal
  27   4  Total_Revolving_Bal
  27   5  Total_Revolving_Bal
  28   1  Total_Revolving_Bal
  28   2  Total_Revolving_Bal
  28   3  Total_Revolving_Bal
  28   4  Total_Revolving_Bal
  28   5  Total_Revolving_Bal
  29   1  Total_Revolving_Bal
  29   2  Total_Revolving_Bal
  29   3  Total_Revolving_Bal
  29   4  Total_Revolving_Bal
  29   5  Total_Revolving_Bal
  30   1  Total_Revolving_Bal
  30   2  Total_Revolving_Bal
  30   3  Total_Revolving_Bal
  30   4  Total_Revolving_Bal
  30   5  Total_Revolving_Bal
  31   1  Total_Revolving_Bal
  31   2  Total_Revolving_Bal
  31   3  Total_Revolving_Bal
  31   4  Total_Revolving_Bal
  31   5  Total_Revolving_Bal
  32   1  Total_Revolving_Bal
  32   2  Total_Revolving_Bal
  32   3  Total_Revolving_Bal
  32   4  Total_Revolving_Bal
  32   5  Total_Revolving_Bal
  33   1  Total_Revolving_Bal
  33   2  Total_Revolving_Bal
  33   3  Total_Revolving_Bal
  33   4  Total_Revolving_Bal
  33   5  Total_Revolving_Bal
  34   1  Total_Revolving_Bal
  34   2  Total_Revolving_Bal
  34   3  Total_Revolving_Bal
  34   4  Total_Revolving_Bal
  34   5  Total_Revolving_Bal
  35   1  Total_Revolving_Bal
  35   2  Total_Revolving_Bal
  35   3  Total_Revolving_Bal
  35   4  Total_Revolving_Bal
  35   5  Total_Revolving_Bal
  36   1  Total_Revolving_Bal
  36   2  Total_Revolving_Bal
  36   3  Total_Revolving_Bal
  36   4  Total_Revolving_Bal
  36   5  Total_Revolving_Bal
  37   1  Total_Revolving_Bal
  37   2  Total_Revolving_Bal
  37   3  Total_Revolving_Bal
  37   4  Total_Revolving_Bal
  37   5  Total_Revolving_Bal
  38   1  Total_Revolving_Bal
  38   2  Total_Revolving_Bal
  38   3  Total_Revolving_Bal
  38   4  Total_Revolving_Bal
  38   5  Total_Revolving_Bal
  39   1  Total_Revolving_Bal
  39   2  Total_Revolving_Bal
  39   3  Total_Revolving_Bal
  39   4  Total_Revolving_Bal
  39   5  Total_Revolving_Bal
  40   1  Total_Revolving_Bal
  40   2  Total_Revolving_Bal
  40   3  Total_Revolving_Bal
  40   4  Total_Revolving_Bal
  40   5  Total_Revolving_Bal
  41   1  Total_Revolving_Bal
  41   2  Total_Revolving_Bal
  41   3  Total_Revolving_Bal
  41   4  Total_Revolving_Bal
  41   5  Total_Revolving_Bal
  42   1  Total_Revolving_Bal
  42   2  Total_Revolving_Bal
  42   3  Total_Revolving_Bal
  42   4  Total_Revolving_Bal
  42   5  Total_Revolving_Bal
  43   1  Total_Revolving_Bal
  43   2  Total_Revolving_Bal
  43   3  Total_Revolving_Bal
  43   4  Total_Revolving_Bal
  43   5  Total_Revolving_Bal
  44   1  Total_Revolving_Bal
  44   2  Total_Revolving_Bal
  44   3  Total_Revolving_Bal
  44   4  Total_Revolving_Bal
  44   5  Total_Revolving_Bal
  45   1  Total_Revolving_Bal
  45   2  Total_Revolving_Bal
  45   3  Total_Revolving_Bal
  45   4  Total_Revolving_Bal
  45   5  Total_Revolving_Bal
  46   1  Total_Revolving_Bal
  46   2  Total_Revolving_Bal
  46   3  Total_Revolving_Bal
  46   4  Total_Revolving_Bal
  46   5  Total_Revolving_Bal
  47   1  Total_Revolving_Bal
  47   2  Total_Revolving_Bal
  47   3  Total_Revolving_Bal
  47   4  Total_Revolving_Bal
  47   5  Total_Revolving_Bal
  48   1  Total_Revolving_Bal
  48   2  Total_Revolving_Bal
  48   3  Total_Revolving_Bal
  48   4  Total_Revolving_Bal
  48   5  Total_Revolving_Bal
  49   1  Total_Revolving_Bal
  49   2  Total_Revolving_Bal
  49   3  Total_Revolving_Bal
  49   4  Total_Revolving_Bal
  49   5  Total_Revolving_Bal
  50   1  Total_Revolving_Bal
  50   2  Total_Revolving_Bal
  50   3  Total_Revolving_Bal
  50   4  Total_Revolving_Bal
  50   5  Total_Revolving_Bal
Warning message:
Number of logged events: 7 

> completeData1 <- completeData1 %>% 
+   mutate(Dummy_Attrition = ifelse(Attrition_Flag == "Attrited Customer", 1, 0))
> model_final <- glm(data= completeData1, formula = Dummy_Attrition ~ Gender +
+                      Dependent_count + Income_Category  + 
+                      Total_Relationship_Count + 
+                      Months_Inactive_12_mon + Contacts_Count_12_mon + 
+                      Total_Revolving_Bal + Total_Amt_Chng_Q4_Q1 + 
+                      Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1 + Total_Trans_Ct * Total_Trans_Amt , 
+                    family = "binomial" (link = "logit"))
> summary(model_final)

Call:
glm(formula = Dummy_Attrition ~ Gender + Dependent_count + Income_Category + 
    Total_Relationship_Count + Months_Inactive_12_mon + Contacts_Count_12_mon + 
    Total_Revolving_Bal + Total_Amt_Chng_Q4_Q1 + Total_Trans_Amt + 
    Total_Trans_Ct + Total_Ct_Chng_Q4_Q1 + Total_Trans_Ct * Total_Trans_Amt, 
    family = binomial(link = "logit"), data = completeData1)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.4151  -0.3142  -0.1134  -0.0227   3.9381  

Coefficients:
                                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)                     3.190e+00  3.747e-01   8.515  < 2e-16 ***
GenderM                        -8.580e-01  1.531e-01  -5.603 2.10e-08 ***
Dependent_count                 1.284e-01  3.141e-02   4.087 4.36e-05 ***
Income_Category$40K - $60K     -6.568e-01  1.965e-01  -3.342 0.000832 ***
Income_Category$60K - $80K     -4.496e-01  1.816e-01  -2.476 0.013299 *  
Income_Category$80K - $120K    -2.810e-01  1.761e-01  -1.596 0.110576    
Income_CategoryLess than $40K  -5.655e-01  2.118e-01  -2.670 0.007590 ** 
Income_CategoryUnknown         -7.003e-01  2.382e-01  -2.940 0.003286 ** 
Total_Relationship_Count       -4.864e-01  2.892e-02 -16.822  < 2e-16 ***
Months_Inactive_12_mon          4.880e-01  4.045e-02  12.063  < 2e-16 ***
Contacts_Count_12_mon           4.936e-01  3.869e-02  12.758  < 2e-16 ***
Total_Revolving_Bal            -9.191e-04  4.869e-05 -18.876  < 2e-16 ***
Total_Amt_Chng_Q4_Q1           -1.193e+00  2.066e-01  -5.773 7.78e-09 ***
Total_Trans_Amt                 2.972e-03  1.281e-04  23.204  < 2e-16 ***
Total_Trans_Ct                 -9.926e-02  4.557e-03 -21.781  < 2e-16 ***
Total_Ct_Chng_Q4_Q1            -3.098e+00  2.084e-01 -14.870  < 2e-16 ***
Total_Trans_Amt:Total_Trans_Ct -2.729e-05  1.478e-06 -18.468  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 8927.2  on 10126  degrees of freedom
Residual deviance: 4132.3  on 10110  degrees of freedom
AIC: 4166.3

Number of Fisher Scoring iterations: 8



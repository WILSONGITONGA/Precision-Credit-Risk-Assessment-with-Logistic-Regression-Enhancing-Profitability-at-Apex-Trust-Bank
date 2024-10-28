# install packages
install.packages("tidymodels")
install.packages("rsample")
install.packages("gridExtra")
install.packages("corrplot")
install.packages("Amelia")
install.packages("vip")
install.packages("caret")




#load libraries
library(tidyverse) # for data manipulation and visualization
library(tidymodels) # for creating linear and logistic regression models
library(gridExtra) # for arranging visualizations
library(Amelia) # for dealing with missing data
library(psych) # for data visualization and descriptive statistics
library(vip) # for visualizing variables importance in models
library(extrafont) # for extra font customization
library(caret) # for additional model analysis
library(rsample) # for creating data splits and resamples
library(corrplot)


# import data(from the environment import data by navigating where your data is located and readr)

# Duplicate the data for EDA
eda_data= Apex_Trust_Dataset

# assign all the categorical column names to an object
cat_cols<- c("status","credit_history","purpose",
            "savings","employment_duration","installment_rate","other_debtors",
            "present_residence","property","other_installment_plans",
            "housing","number_credits","job","people_liable",
            "telephone","foreign_worker","credit_risk")

# convert columns to a factor variable
eda_data[,cat_cols]<- lapply(eda_data[,cat_cols],factor)

# rename variable values to something more meaningful for EDA
eda_data= eda_data |>mutate(credit_risk = ifelse(credit_risk==0,'bad','good'))

eda_data$status =ifelse(eda_data$status==2,'no checking account',
                        ifelse(eda_data$status==2,'<0 USD',
                               ifelse(eda_data$status==3,'0 USD >==&< 200 USD','>200 USD')))

eda_data$status=factor(eda_data$status,levels= c('no checking account','<0 USD','0 USD >=& < 200 USD',
                                                '>=200 USD'))

eda_data$savings =ifelse(eda_data$savings==1,'unknown/no savings account',
                        ifelse(eda_data$savings==2,'<100 USD',
                               ifelse(eda_data$savings==3,'100 >= &< 500 USD',
                                      ifelse(eda_data$savings== 4,'500> & <1000 USD','>1000 USD'))))

eda_data$savings=factor(eda_data$savings,levels= c('unknown/no savings account',
                                                   '<100 USD','100 >=& < 500 USD',
                                                 '500>= & <1000 USD','>=1000 USD'))
eda_data$credit_risk=as.factor(eda_data$credit_risk)

# check the transformed data types
str(eda_data)

# explore the data
summary(eda_data)

# Visual EDA for categorical variables
# determine the response variable distribution
credit_risk_dist<-ggplot(eda_data,aes(x=credit_risk))+
  geom_bar(width = 0.25,fill='blue')+
  theme_minimal()+
  labs(x='Credit Risk',y='Count',title = "Distribution of Response Variable")+
  theme(plot.title = element_text(size = 17,family = "Arial",hjust = 0.5),
        plot.subtitle = element_text(size = 12,family = "Arial",hjust = 0.5),
        plot.background = element_rect(fill='#FBFBFB'))
credit_risk_dist

#  Visualizing distributions of individual variables by credit risk using Histograms
df2=Apex_Trust_Dataset|>
  mutate(credit_risk=as.factor(ifelse(credit_risk==0,'bad','good')))

# status

status<- ggplot(data = df2,aes(status))+
  geom_histogram(breaks=seq(0,5,by=1),
  col="black",
  aes(fill=after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count",low= "lightgreen",high = "darkgreen")+
  theme_minimal()

status

# Amount
amount<- ggplot(data = df2,aes(amount))+
  geom_histogram(breaks=seq(0,20000,by=1000),
                 col="black",
                 aes(fill=after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count",low= "lightgreen",high = "darkgreen")+
  theme_minimal()

amount

# Savings
savings<- ggplot(data = df2,aes(savings))+
  geom_histogram(breaks=seq(0,5,by=1),
                 col="black",
                 aes(fill=after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count",low= "lightgreen",high = "darkgreen")+
  theme_minimal()

savings

# Duration
# 
duration<- ggplot(data = df2,aes(duration))+
  geom_histogram(breaks=seq(0,80,by=5),
                 col="black",
                 aes(fill=after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count",low= "lightgreen",high = "darkgreen")+
  theme_minimal()

duration

# Age
age<- ggplot(data = df2,aes(age))+
  geom_histogram(breaks=seq(0,80,by=5),
                 col="black",
                 aes(fill=after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count",low= "lightgreen",high = "darkgreen")+
  theme_minimal()

age

# number_credits
number_credits<- ggplot(data = df2,aes(number_credits))+
  geom_histogram(breaks=seq(0,5,by=1),
                 col="black",
                 aes(fill=after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count",low= "lightgreen",high = "darkgreen")+
  theme_minimal()

number_credits

# other_installment-plans
other_installment_plans<- ggplot(data = df2,aes(other_installment_plans))+
  geom_histogram(breaks=seq(0,5,by=1),
                 col="black",
                 aes(fill=after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count",low= "lightgreen",high = "darkgreen")+
  theme_minimal()

other_installment_plans
# Credit history

credit_history<- ggplot(data = df2,aes(credit_history))+
  geom_histogram(breaks=seq(0,5,by=1),
                 col="black",
                 aes(fill=after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count",low= "lightgreen",high = "darkgreen")+
  theme_minimal()


credit_history

# job

job<- ggplot(data = df2,aes(job))+
  geom_histogram(breaks=seq(0,5,by=1),
                 col="black",
                 aes(fill=after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count",low= "lightgreen",high = "darkgreen")+
  theme_minimal()


job

## View all the plots

grid.arrange(status,duration,age,credit_history,job,
             number_credits,other_installment_plans,savings,amount,ncol=2)

# boxplot for numerical variables
df_num=Apex_Trust_Dataset|>select(age,amount,duration)
df_num=cbind(df_num,eda_data$credit_risk)
colnames(df_num)=c('age','amount','duration','credit_risk')

bp1<-ggplot(df_num,aes(age))+
  geom_boxplot(fill= 'lightgreen',color='black',alpha=0.3,width=0.5)+
  facet_wrap(~credit_risk)+coord_flip()+
  theme_minimal()

bp1


bp2<-ggplot(df_num,aes(duration))+
  geom_boxplot(fill= 'lightgreen',color='black',alpha=0.3,width=0.5)+
  facet_wrap(~credit_risk)+coord_flip()+
  theme_minimal()

bp2

bp3<-ggplot(df_num,aes(amount))+
  geom_boxplot(fill= 'lightgreen',color='black',alpha=0.3,width=0.5)+
  facet_wrap(~credit_risk)+coord_flip()+
  theme_minimal()

bp3

##  Visualizing all the plot in one grid
grid.arrange(bp1,bp2,bp3)


## Checking for correlations within variables or multicollinearity
# Duplicate the data and transform all factor variables to numeric
cor_data= Apex_Trust_Dataset|>mutate_if(is.factor,as.numeric)

# correlation plot
corPlot(cor_data,alpha=0.7)

# Data Preprocessing
# missing values
missmap(Apex_Trust_Dataset)

# Model Development
# set a seed for random number generation to ensure repeatability
set.seed(1234)

#randomly partition data into training (70%) and test(30%) sets
ind<- sample(2,nrow(Apex_Trust_Dataset),replace = TRUE,prob = c(0.7,0.3))
train<- Apex_Trust_Dataset[ind==1,]
test<- Apex_Trust_Dataset[ind==2,]

# logistic Regression model
# fit a logistic regression model to predict the credit risk using all other variables
m1<- glm(credit_risk~.,data=train,family = 'binomial')
summary(m1)

# Visualize the most important features using the vip package
m1%>%
  vip(num_features=20,
      geom="point",
      aesthetics=list(
        size=2,
        color="#3C6255"
      ))+
theme_minimal(base_size = 18)+
labs(title = "Logistic Regression:Feature Importance")

# Predictions on the training data
p1<- predict(m1,train,type='response')
pred1 <- ifelse(p1>0.5,1,0)

# prediction on the test data
p2<- predict(m1,test,type = 'response')
pred2<- ifelse(p2>0.5,1,0)

# calculate confusion matrix for training data

cmlg1 <- confusionMatrix(factor(pred1),factor(train$credit_risk),positive='1')

# print
cmlg1

# Calculate the confusion matrix for test data
clmg2 <- confusionMatrix(factor(pred2),factor(test$credit_risk),positive='1')

# print
clmg2

      

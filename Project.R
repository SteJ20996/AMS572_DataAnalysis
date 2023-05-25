#Hui Chen, Yizhen Jia, Yuhua Qiao and Dawei Ye
#Group 10
#AMS572-Group Project

#Graduate Earnings
# Source: http://new.time.com/money/best-colleges/rankings/best-colleges/
# https://dasl.datadescription.com/datafile/graduate-earnings/?_sfm_cases=500+10000&sf_paged=3

set.seed(10) #setting seed for reproducible results

#checks to see if all neccessary libraries are installed
list.of.packages <- c('car', 'glmnet', 'MASS', 'knitr', 'dplyr', 'mice', 'leaps', 'corrplot')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library('car')
library('glmnet')
library('MASS')
library('knitr')
library('dplyr') 
library('mice')
library('leaps')

library(corrplot)
library(RColorBrewer)

# Set the working directory
setwd("C:\\Users\\jason\\Documents\\SBU_2021_FALL\\AMS572\\Project")
df <- read.csv(file = "graduate_earnings.csv", sep = ',', header = TRUE)
head(df)

#adding in the state attribute
df$State <- substring(df$Location,unlist(gregexpr(",", df$Location))+2)

#Missing values for 61 missing values for merit_aided(8.64% of data)
sum(is.na(df$merit_aided))

100*sum(is.na(df$merit_aided))/nrow(df)
#Missing values for 19 missing values for need_fraction(2.69% of data)
sum(is.na(df$need_fraction))

100*sum(is.na(df$need_fraction))/nrow(df)
#quick view to see if theres a relationship between missing value based on other columns
#seems like we have more missing values for private schools
df[is.na(df$merit_aided),]

df[is.na(df$need_fraction),]

x1 <- nrow(filter(df, is.na(merit_aided), Public==0))
n1 <- nrow(filter(df, Public==0))
x2 <- nrow(filter(df, is.na(merit_aided), Public==1))
n2 <- nrow(filter(df, Public==1))

#let p1=true proportion of missing values for merit_aided based for public schools
#let p2=true proportion of missing values for merit_aided based for private schools
#H0: p1 = p2
#HA: p1 != p2
prop.test(x = c(x1, x2), n = c(n1,n2), alternative = 'two.sided', conf.level = .95)
#By the two sample proportion test, failed to reject H0 at  alpha=0.05. The p-value = 0.1185 > alpha=0.05.
#Not enough evidence to show the two true proportions of missing-ness are not equal.


x1 <- nrow(filter(df, is.na(need_fraction), Public==0))
n1 <- nrow(filter(df, Public==0))
x2 <- nrow(filter(df, is.na(need_fraction), Public==1))
n2 <- nrow(filter(df, Public==1))

#let p1=true proportion of missing values for need_fraction based for public schools
#let p2=true proportion of missing values need_fraction based for private schools
#H0: p1 = p2
#HA: p1 != p2
prop.test(x = c(x1, x2), n = c(n1,n2), alternative = 'two.sided', conf.level = .95)
#By the two sample proportion test, failed to reject H0 at  alpha=0.05. The p-value = 0.07522 > alpha=0.05.
#Not enough evidence to show the two true proportions of missing-ness are not equal.

#Based on the previous 2 tests, it seems like the missing values for merit_aided and need_fraction are random
#We claim that our missing values for merit_aided and need_fraction are MCAR type of missing-ness

#---------------------------------------------------------------------------------

#Hypothesis 1:
#Is the need base aid met?

#define Aid_amount = Price-Price_with_aid
#define Aid_percent = Aid_amount/Price
df$Aid_amount <- df$Price-df$Price_with_aid
df$Aid_percent <- df$Aid_amount/df$Price

#let u1 = the true mean of Aid_percent
#let u2 = the true mean of need_fraction
#H0: u1>=u2
#HA: u1<u2

#dealing way MCAR Way #1:
#dropping the missing values
x<-df[!is.na(df$need_fraction),]$Aid_percent
y<-df[!is.na(df$need_fraction),]$need_fraction
d<- x-y
t.test(d, alternative='less')

#dealing way MCAR Way #2:
#filling in NAs with mean of the column
x<-df$Aid_percent
y<-df$need_fraction
y[is.na(y)]<-mean(y,na.rm=TRUE)
d<- x-y
t.test(d, alternative='less')

#dealing way MCAR Way #3:
#filling NAs with multiple imputation
#predictive mean matching method from MICE(Imputing Missing Data) to deal with missing values

temp_df <- mice(data = df, m = 5, method = "pmm", maxit = 50, seed = 500)
df_fill <- complete(temp_df,1)
x<-df_fill$Aid_percent
y<-df_fill$need_fraction
d<- x-y
t.test(d, alternative='less')

#---------------------------------------------------------------------------------

#Hypothesis 2:

#Setting some values of Earn to NA for Private schools to simulate the effect of
# Missing Not At Random (MNAR)
df[df$School %in% sample_n(df[df$Public==1,], .2*nrow(df))$School,]$Earn <- NA
df$Earn

#looking at correlations matrix and seeing high correlations between SAT&ACT, Price&Public, Price&Price_with_aid etc
df_fit <- subset(df, select=c("Earn", "Public", "State", "SAT", "ACT", "Price", "Price_with_aid", "need_fraction", "merit_aided", "Aid_amount", "Aid_percent"))
df_fit$State <- unclass(factor(df_fit$State))
df_fit <- sapply( df_fit, as.numeric )
M<-cor(df_fit, use="complete.obs")

#plotting the correlation matrix as a heat map
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


#LASSO Regression to model Earning
#selecting the neccessary columns for the 
df_fit <- subset(df, select=c("Earn", "Public", "State", "SAT", "ACT", "Price", "Price_with_aid", "need_fraction", "merit_aided", "Aid_amount", "Aid_percent"))

df_fit$State <- factor(df_fit$State)

#dealing with MNAR
#predictive mean matching method from MICE(Imputing Missing Data) to deal with missing values
temp_df <- mice(data = df_fit, m = 5, method = "pmm", maxit = 50, seed = 500)
df_fit <- complete(temp_df,1)

#setting the dependent and independent variables
y<-df_fit$Earn
x<-cbind(df_fit$Public,df_fit$State,df_fit$SAT,df_fit$ACT,df_fit$Price, df$Price_with_aid, df_fit$need_fraction,df_fit$merit_aided,df_fit$Aid_amount,df_fit$Aid_percent)
#plotting lambda(penalty term from 1000 to 0.001)
lamb <- 10^seq(2,-3, length=10)
fit_lasso <- glmnet(x,y,alpha = 1, lambda = lamb)
#plotting the coefficients with the respective penalty terms
plot(fit_lasso, xvar = "lambda", xlim=c(-5,4))
#outputting the possible coefficients with it respective penalty terms
coef(fit_lasso)

#using cross validation to find the optimal lambda value for LASSO
cv.out <- cv.glmnet(x,y, alpha=1, nfolds=10)
cv.out
fit_lasso_best <- glmnet(x,y,alpha = 1, lambda = cv.out$lambda.min)
coef(fit_lasso_best)

#coefficient of public seems high, we are interpreting our model that
#public school students make more money than private school students

#2 sample t test for checking earnings after model:
x<-filter(df_fit, Public==1)$Earn
y<-filter(df_fit, Public==0)$Earn

var.test(x,y)
#Reject H0, the variances are unequal. Using unpooled variance t test
#H0: mean(X) >= mean(Y)
#HA: mean(X) < mean(Y)
t.test(x,y, alternative = 'less', var.equal = FALSE)
               
               
#-------------------------------------------------------------------------------------------------------------------
# #Looking at the results of multiple regression. for comparison(not part of report)
# df_fit$State <- factor(df_fit$State)
# 
# #Multiple Linear Regression with all variables
# multiple_linear_all <- lm(Earn ~ ., data= df_fit)
# 
# #model selection with AIC criterion
# summary(stepAIC(multiple_linear_all))
# 
# #exhaustive model selection
# best<-regsubsets(Earn ~ ., data= df_fit,nbest=1, intercept = TRUE, really.big=T)
# summary(best)
# 
# #similar results compared to LASSO regression
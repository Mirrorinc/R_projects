#ATTACH THE DATA FOR ANALYSIS
library(readxl)
BBL_DATA <- read_excel("C:/Users/Mirror/Desktop/BBL_DATA.xlsx")
View(BBL_DATA)
attach(BBL_DATA)
#EXTRACT COLUMN FOR THE ANALYSIS 
data<-BBL_DATA[2:4]
#ESTIMATE THE SUMMARY STATISTICS
# THE MEAN,MAX,MIN,MEDIAN AND QUARTILE VALUES
summary(data)
library(lessR)
cor(data)
plot(data)
model<-Regression(GDP~EXPENDITURE+INCOME,data = data)
model
####HYPOTHESIS FOR THE PARAMETERS USING T STATISTIC
#Ho : Bo==0 i.e. the intercept is equal to zero
#H1 : B0 =/ 0 i.e. the intercept is not equal to zero
#Decision
## if the p value is < 0.05 accept H0
## if the p value is > 0.05 reject H0*****
####HYPOTHESIS FOR THE PARAMETERS
#Ho : B1==0 i.e. the slope is equal to zero
#H1 : B1 =/ 0 i.e. the slope is not equal to zero
#Decision
## if the p value is < 0.05 reject H0
## if the p value is > 0.05 accept H0
####HYPOTHESIS FOR THE PARAMETERS
#Ho : B2==0 i.e. the slope is equal to zero
#H1 : B2 =/ 0 i.e. the slope is not equal to zero
#Decision
## if the p value is < 0.05 accept H0
## if the p value is > 0.05 reject H0 ******
###HYPOTHESIS OF GOODNESS OF FIT USING F STATISTIC
### H0 : The model is not a good fit ///accept if p value > 0.05
### H1 : The model is a good fit     /// accept if p value < 0.05****
#TEST FOR MULTICOLLINEARITY
####COLLINEARITY (WITHIN INDEPENDENT VARIABLES i.e EXPENDITURE & INCOME)
####If the VIF =1 the variables are not correlated****
####If the VIF is btwn 1 and 5 i.e the variables are moderately correlated
####If the VIF is > 5 i.e the variables are highly correlated
##REGRESSION MODEL
## GDP =28.1355 - -0.337101031*EXPENDITURE + 0.0000000000001136 *INCOME
model

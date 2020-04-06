library(data.table) 
library(DBI)
library(RSQLite)
con <- dbConnect(RSQLite::SQLite(),'wooldridge2.db')
dbListTables(con)
mb <- dbReadTable(con,'mlb1')
mb

# part1: Our null hypotheses states that: H0:B13 = 0
# Our alternative null hypotheses states that: H0:B13 != 0 
# On testing the model, it can be observed that the t-value is '1.931', which states that the variable is statistically significant at the 10% significance level. Therefore, we reject the null hypotheses.
# Catchers and Outfielders, on average, do not earn the same amount. 
# Catchers earn approximately 28.86% more than the outfielders.

# part2: Our null hypotheses states that: H0:B9 = 0, B10 = 0, B11 = 0, B12 = 0, B13 = 0 
# Our alternative null hypotheses states that: H0:B9 != 0, B10 != 0, B11 != 0, B12 != 0, B13 != 0
# After testing for joint significance, the F-stat comes out to be '1.777', which is less than the c value (1.85) 10% significance level. Since F-stat is not greater than 1.85, we cannot reject the null. Therefore, there is insignificant difference in average salary once all the other factors are controlled. 

# part3: The results are consistent because when we only take the variable 'catcher', the result is significant at 10% significance level. However, when we associate that significant individual variable with other insignificant individual variables, the result is insignificant at the 10% level. 


# part1
model1 <- lm(log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher , data = mb)
summary(model1)
salary_difference <- 100 * (exp(0.2535598) - 1) 
salary_difference  # Estimated salary difference

# part2:
model2 <- lm(log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher , data =mb)
summary(model2)
# Testing for Joint Significance
model3<- lm(log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr + runsyr + fldperc + allstar + frstbase + scndbase + thrdbase + shrtstop + catcher , data = MLB1) 
summary(model3)

            
#question 2:library(data.table) 
library(DBI)
library(RSQLite)
con <- dbConnect(RSQLite::SQLite(),'wooldridge2.db')
dbListTables(con)
gpa <- dbReadTable(con,'gpa2')
dbReadTable(con,'gpa2_labels')
summary(gpa)

#Part1:
#for a any hsize the gpa should be good. That means if hsize increases the colgpa shuld increase.
#same goes with Hsperc which is the acedmic percentile of class. It should increase with colgpa.
# the two variavles that i am unsure about is female and athlete. I am not sure how 
# they are related to college GPA.

#Part2:# The estimated equation is:
# colgpa= 1.241+ 0.0569hsize+0.00468 hisize^2-0.0132hsperc+0.00165sat+0.1549female+0.1693athlete
#             R^2 = 29.25%, Df= 4130

# the estimated difference between athlete and no athlete is 0.1693.
# Yes it is significant at even 0.1% level.

#part3:
# After dropping sat the estimated effect on gpa of being an athlete is 0.00545
# which is insignificant even at 1% level.
# there athlete score is only considered when their sat score is considered and so 
# the variable athlete is insiginficat here.

#part4:  H = B5 != 0
# the coefficients of female is 0.1549 and is higly significant.
# also the confidence interval is from 0.1195825910 to 0.190180231. Hence Beta 5 cannot be zero.
# hence we cannit reject the null hypothesis.

#part5:
#not much variation based on gender is seen in SAT.

#Part2: 
model2<- lm(colgpa ~hsize+I(hsize^2)+hsperc+sat+female+athlete, data=gpa)
summary(model2)  

#Part3:
model3<- lm(colgpa ~hsize+I(hsize^2)+hsperc+female+athlete, data=gpa)
summary(model3)
cor(gpa$sat,gpa$athlete)
lm(colgpa~athlete, data=gpa)


#part4:

confint(model2)

#part5:
model4<- lm(colgpa ~hsize+I(hsize^2)+hsperc+athlete+sat, data=gpa)
summary(model4)
cor(gpa$sat,gpa$female)


#Question3:
#part1: Since beta 1 is the dumy variable which will explain the effect of discrimination between the
# white and non white people we expect it to be positive.
#part2: the OLS equation is :
#       approve= 0.70779+ 0.20060(white)+u
#         R^2 = 0.04893, DF= 1987
#the coefficients on white is higly significant even at 0.1%. with coefficient as 0.20060
#part3:After adding the said variables, the coefficient on white decreases to 0.1288. However, since the difference is still statistically significant, there is still evidence of discrimination against nonwhites.
#part4: the interaction term is higly significant at 0.1%
#part5:When obrat = 32, the probabilitiy of approval for white becomes approximately 11.2%. The upper and lower limits, of the 95% confidence interval, are 0.1524 and 0.0732 respectively. 

lp <- dbReadTable(con,'loanapp')
summary(lp)

#part2:
model0<- lm(approve~white ,data=lp)
summary(model0)

#part3:
model0<- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr ,data=lp)
summary(model0)

#part4:
model1<- lm(approve~white+hrat+obrat+I(white*obrat)+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr ,data=lp)
summary(model1)
#part5:
mean(lp$obrat)
obrat_32<- lp$obrat-32
obrat_32
model2<- lm(approve~white+hrat+I(white*obrat_32)+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr+obrat_32 ,data=lp)
summary(model2)
confint(model2)
#question4:
#part1: the standard error for lotsize and sqrft were significant in the usval OLS model but lotsize is insignificant
# in the heteroskedsticity model. The standard error in the usval OLS model were:
# lotsize: 0.0006421, sqrft: 0.0132, bdrms: 9.010. the heteroskedesticity standard
# errors are: lotsize: 0.0071, sqrft: 0.0407, bdrms: 11.561

#part2: the coefficient for intercept was significant at 5 %, forlotsize and sqrft were highly significant in the usval OLS model but the intercept is insignificant
# in the heteroskedsticity model. Also lotsize is significant at 10% level.The standard error in the usval OLS model with log were:
# lotsize: 0.03828, sqrft: 0.09287, bdrms: 0.02753. the heteroskedesticity standard
# errors are: lotsize: 0.053275, sqrft: 0.121392, bdrms: 0.035576

#part3:
#from the model it can be observed that implementing the log model reduces the heteroskedesticity such that the difference in
#the standard errors of usval ols model and the correction for heteroskedestcity fiffers by a marginal value.
library(data.table) 
library(DBI)
library(RSQLite)
library(broom)
library(lmtest) 
library(sandwich)
con <- dbConnect(RSQLite::SQLite(),'wooldridge2.db')
dbListTables(con)
#part1:
price <- dbReadTable(con,'hprice1')
price
model5<- lm(price~lotsize+sqrft+bdrms, data=price)
summary(model5)
coeftest(model5,sandwich::vcovHC)
#Part2:
model6 <-lm(log(price)~log(lotsize)+log(sqrft)+bdrms, data=price)
summary(model6)
coeftest(model6,sandwich::vcovHC)


#Question5:
#part1: the sum of residuals is 14.37
#part2: in code
#pat3: the fittetd values are strictly positive. the coefficients on skipped is higly significant
# even at 0.1%. if the value of skipped increases by 1 then colgpa decreases by 0.076 points.
# The coefficient of PC is significant at 5%. If the value of PC is increased by 1 the colgpa will
# be increased by 0.126005 points.
#part4: the standard errors do not change much.
library(data.table) 
library(DBI)
library(RSQLite)
library(broom)
library(lmtest) 
library(sandwich)
con <- dbConnect(RSQLite::SQLite(),'wooldridge2.db')
dbListTables(con)
#part1:
gpa <- dbReadTable(con,'gpa1')
gpa
model7 <- lm(gpa$colGPA ~ gpa$hsGPA+ gpa$ACT+gpa$skipped+gpa$PC, data=gpa)
summary(model7)
coeftest(model7, sandwich::vcovHC)
dt <- augment(model7)
res<-residuals(model7)
gpa$res2 <- res^2
sum(res2)

#part2:
col_hat<-predict(model7)
colhat_sq<-(col_hat)^2
colhat_sq
model8<- lm(res2~col_hat+colhat_sq, data=gpa)
summary(model8)
dt<- augment(model8)
gpa$h<-fitted(model8)
summary(gpa$h)
#part3:
model9<-(lm(gpa$colGPA ~ gpa$hsGPA+ gpa$ACT+gpa$skipped+gpa$PC,weights=1/h, data=gpa))
summary(model9)

#Question6:
price <- read.csv("bitcoin.csv")
names(price) <- c("date","bitcoin","sp500","gold","exchange","oil")
price$date <- as.Date(price$date,format="%d-%b-%y")
head(price)
summary(price)
head(price)

# Q6 (iv)
# Plotting individual series
ggplot(price,aes(x = date, y = bitcoin)) + geom_line(color = "#00AFBB", size = 2) +
  scale_x_date('Date') +
  scale_y_continuous('Bitcoin Prices ($)')

ggplot(price,aes(x = date, y = sp500)) + geom_line(color = "#00AFBB", size = 2) +
  scale_x_date('Date') +
  scale_y_continuous('SP500 ($)')

ggplot(price,aes(x = date, y = gold)) + geom_line(color = "#00AFBB", size = 2) +
  scale_x_date('Date') +
  scale_y_continuous('GOLDAMGBD228NLBM')

ggplot(price,aes(x = date, y = exchange)) + geom_line(color = "#00AFBB", size = 2) +
  scale_x_date('Date') +
  scale_y_continuous('Exchange Rate (DEXUSEU)')

ggplot(price,aes(x = date, y = oil)) + geom_line(color = "#00AFBB", size = 2) +
  scale_x_date('Date') +
  scale_y_continuous('Oil Prices ($) DCOILWTICO.')+ theme(legend.position="right")

# Plotting all series together
price1 <- subset(price, select=c("bitcoin","sp500","gold","exchange","oil"))
ts.plot(subset(price, select=c("bitcoin","sp500","gold","exchange","oil")),gpars= list(col=rainbow(10)))
legend("topleft", legend=colnames(price1), lty=1, col=rainbow(10))


# Q6 (v)
price2 <- price
price2$date <- as.Date(price2$date)
model_naive <- lm(bitcoin~sp500+gold+exchange+oil+date,data=price2)
summary(model_naive)
class(price2$date)
# 

# Call:
#   lm(formula = bitcoin ~ sp500 + gold + exchange + oil + date, 
#      data = price2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3633.6 -1106.9   -78.8   737.5 11721.8 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.513e+04  5.519e+03  -2.742   0.0062 ** 
# sp500        1.260e+01  6.283e-01  20.050  < 2e-16 *
# gold         3.832e+00  7.866e-01   4.872 1.25e-06 *
# exchange     2.025e+04  1.361e+03  14.886  < 2e-16 *
# oil         -6.374e+01  7.576e+00  -8.414  < 2e-16 *
# date        -2.081e+00  3.789e-01  -5.491 4.85e-08 *
#   ---
#   Signif. codes:  0 '*' 0.001 '*' 0.01 '' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1835 on 1235 degrees of freedom
# Multiple R-squared:  0.7834,	Adjusted R-squared:  0.7825 
# F-statistic: 893.1 on 5 and 1235 DF,  p-value: < 2.2e-16

# The regression coefficients are all significant
# Every 1 point increase from sp500 will lead to 12.6 increase in bitcoin price controlling for other variables
# R-squared is 78.34%

# Q6 (vi)
rep.kpss(price2$bitcoin)
rep.kpss(price2$sp500)
rep.kpss(price2$gold)
rep.kpss(price2$exchange)
rep.kpss(price2$oil)

# Q6 (vii)
modela <- lm(diff(bitcoin)~diff(sp500)+diff(gold)+diff(exchange)+diff(oil)+date[2:nrow(price2)],data=price2)
summary(modela)

# Call:
#   lm(formula = diff(bitcoin) ~ diff(sp500) + diff(gold) + diff(exchange) + 
#        diff(oil) + date[2:nrow(price2)], data = price2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2498.4   -28.6    -1.8    31.7  3615.4 
# 
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)
# (Intercept)          -3.936e+01  2.980e+02  -0.132    0.895
# diff(sp500)           2.327e-01  4.663e-01   0.499    0.618
# diff(gold)            1.369e+00  9.353e-01   1.463    0.144
# diff(exchange)        1.382e+02  1.590e+03   0.087    0.931
# diff(oil)             9.029e-01  7.641e+00   0.118    0.906
# date[2:nrow(price2)]  2.615e-03  1.726e-02   0.151    0.880
# 
# Residual standard error: 325.1 on 1234 degrees of freedom
# Multiple R-squared:  0.002178,	Adjusted R-squared:  -0.001865 
# F-statistic: 0.5387 on 5 and 1234 DF,  p-value: 0.7471

# Every 1 point increase from sp500 will lead to 0.2327 increase in bitcoin price controlling for other variables
# R-squared has come down 0.2178% from 78.34%


# Q6 (viii)
price2$date
price3 <- subset(price2,price2$date>="2017-01-01")
nrow(price3)

# Q6 (ix)
par(mfrow=c(2,2))
acf(price3$bitcoin)
acf(diff(price3$bitcoin))
pacf(price3$bitcoin)
pacf(diff(price3$bitcoin))

# Q6 (x)
outp <- matrix(0,4^2,5)
count <- 1
for(i in 0:15){
  for(j in 0:15){
    modeld <- Arima(price3$bitcoin,c(i,1,j),include.drift = TRUE)
    outp[count,] <- c(i,1,j,AIC(modeld),BIC(modeld))
    count <- count + 1
  }
}
outp <- data.table(outp)
names(outp) <- c('p','d','q','aic','bic')
outp
outp[aic==0,]$aic <- 9999
outp[bic==0,]$bic <- 9999
outp[which.min(outp$aic),,]

head(outp[order(aic),])
head(outp[order(bic),])

# ARIMA model with parameters 0,1,2 fits the data best

library(ggplot2)
#Q6 (xi)
par(mfrow=c(1,1))

modeld <- Arima(price3$bitcoin,c(0,1,2),include.drift = TRUE)
modeld
plot(forecast(modeld, h=30))

#Q6 (xii)
hist((price3$bitcoin)
TSA::periodogram(diff(price3$bitcoin))
# It is difficult to identify seasonality 
     
#Q6 (xiii)
price3$days <- weekdays(price3$date)
price3$days <- as.factor(price3$days)
# price3$days <- as.numeric(price3$days)
price3$days
class(price3$days)
     
n <- nrow(price3)
linear <- lm(diff(bitcoin)~days [2:n],data=price3)
summary(linear)
augment(linear)$.resid
TSA::periodogram(augment(linear)$.resid)
# there is no visible change in the periodogram even after this transformation.
     
     
#Q6 (xiv)
     
xdata <- cbind(price3$bitcoin, price3$sp500, price3$gold, price3$exchange, price$oil)
AIC(vars::VAR(xdata,1,type="both"))
AIC(vars::VAR(xdata,2,type="both"))
AIC(vars::VAR(xdata,3,type="both"))
AIC(vars::VAR(xdata,4,type="both"))
AIC(vars::VAR(xdata,6,type="both"))
     
vars::VARselect(xdata, lag.max=10, type = "both")
     
     
#According to AIC, VAR(2) gives us the best fit model
     
#Q6 (xiv)
     
modele <- Arima(price3$bitcoin,c(2,1,2),include.drift = TRUE)
AIC(modele)
plot(forecast(modele, h=30))
     
#VAR(2) model is a better fit than the ARIMA model
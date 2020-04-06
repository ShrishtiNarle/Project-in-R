library(data.table) 
library(DBI)
library(RSQLite)
library(broom)
library(plm)
library(lmtest)
library(sandwich)
library(margins)
con <- dbConnect(RSQLite::SQLite(),'wooldridge2.db')
dbListTables(con)

#Question1:
hprice <- dbReadTable(con,'hprice1')
summary(hprice)
model<- lm(price~assess+bdrms+lotsize+sqrft+colonial, data=hprice)
AIC(model)
BIC(model)
model1<- step(model)
glance(model1)
model2<- step(model,k=log(nrow(hprice)))
BIC(model2)

#Question2:
gpa <- dbReadTable(con,'gpa2')
summary(gpa)


#qUESTION4:
#Part1: The pooled OLS regression is 0.5688+0.2622y90+0.04068ln(pop)+0.57144ln(avginc)+0.00504pctsu+u with
# Adjusted R sq. value as 0.8612 and DF as 123.The dummy variable 1990 is statistically significant. It implies that
#over the course of 10 years keeping all the variables constant, the  rent grows by 26.22%. The pctsu is the student population percentage of the total population which
# is highly significant. it implies that 1 % increase in pctsu increases rent by 0.5%.

#part2: The standard error reorted in part one are not valid as we have not taken into consideration the alpha1 value that is in the equation.
# there can be aposiibilty that the alpha may impact the variability of the data.

#Part3: LATER(

#PART4: The estimates and standard errors are identical to the one in part 3.
  
library(plm)
rtl <- dbReadTable(con,'rental')
summary(rtl)
dbReadTable(con,'rental_labels')
rtl$pctsu <- (rtl$enroll/rtl$pop)*100
rtl$pctsu1<-unlist(pctsu)
as.numeric(pctsu1)
typeof(pctsu1)
#part1:
class(rtl)
prtl<-pdata.frame(rtl,index=c('city','year'))
model1 <- plm(log(rent)~year+log(pop) +log(avginc)+pctsu ,model="pooling", data= prtl)
summary(model1)

#Pat3:

prtl$rent1<- as.numeric(prtl$rent)
prtl$avgnic<- as.numeric(rtl$avgnic)
type.convert(rtl$pctsu)
typeof(prtl$avginc)

typeof(rtl$pctsu)
as.factor(rtl$pctsu)
is.null(prtl$avginc) 
 ?diff

na.omit(prtl)
model2 <- plm(diff(log(rent))~diff(year)+diff(log(pop))+diff(log(avginc))+diff(pctsu),model='pooling' ,data= prtl)
summary(model2)
?plm()

#PART4:

model3 <- plm(log(rent)~year+log(pop) +log(avginc)+pctsu ,model="within", data= prtl)
summary(model3)

#Question5:
#Part1: if the execution in the murders have the deterent effect then the sign of the beta 1 coefficient will be negative, as
# more executions will increase the feeling of terror in murderers. The unemployed people will be more intended to murder so the sigh of beta2 will be positive.

#Part2: There is no evidence to support deterent effect. It apperas that increase in execution increases the murder rate.

#Part3: On estimating equation we get the evidence that the exceution has deterent effect on murder rates and it is statistically significant.
# 1 point increase in execution in murders, decreases murder rates by 0.103.

#part4: After calculating heteroschkedesticity the variable 'unem' is no more significant.

#part5: The state with highest execution is Texas with value of 34. the second highest execution value is 11 which is 23 points lower than Texas.

#part6: After correcting for heteroschdesticity, the variable was higly significant at 0.1%.

#part7: After including all the years the coefficent on deterant effects went from -0.103 to 0.114. Also, the coefficients were significant before for execution. By including all the years 
#  the coefficient became insignificant.

murder <- dbReadTable(con,'murder')
summary(murder)
dbReadTable(con,'murder_labels')

#Part2:
year1 <- murder[which( murder$year == 90 | murder$year == 93) , ]
nrow(year1)
head(year1)
class(year1)
year1<-pdata.frame(year1,index=c('id','year'))
model1<- plm(mrdrte~exec+unem, model='pooling', data=year1)
summary(model1)

#Part3:
model2<- plm(mrdrte~as.numeric(year)+exec+unem, model='within', data=year1)
summary(model2)

model3<- plm(mrdrte~exec+unem, model='fd', data=year1)
summary(model3)

#part4:
coeftest(model1,vcov=vcovHC)

#part5:

a<-murder[which(murder$year=='93') ,]
a
am<-max(a$exec)
am
newdata <- subset(a,a$exec==am,
                  select=state)
newdata

dp<- subset(year1, state != "TX")
a1<- subset(a, state != "TX")
asm<-max(a1$exec)
asm
max_sstae<-subset(a1,a$exec==asm,
                  select=state)
max_sstae

#part6:

model4<- plm(mrdrte~exec+unem, model='fd', data=year1)
summary(model4)
coeftest(model4,vcov=vcovHC)

#part7:
model5<- plm(mrdrte~exec+unem+year, model='within', data=murder)
summary(model5)
summary(model2)
                   
#Question7:
#part1: the logit model is: approve= 0.8847+1.490white+u.The probability for white people to get the loan approved is 14.43% higher than the non white people.
# The estimated probability of white people to get their loan approved is 90.84% and for non-white people is 70.78%.
# The linear probability estimates are: approve= 0.70779 + 0.20060White+u
# the probabilities are not that different.

#part2: The probability of a white person to get loan approved got low by 8.2%. But, there is a statsitical significance that there is discrimination among non whites and whites as the coefficient on white is 0.93 and is higly significant.
lp <- dbReadTable(con,'loanapp')
summary(lp)

#part1:
logit_model<- glm(approve~white ,data=lp, family='binomial')
summary(logit_model)
margins(logit_model)      #marginal probability on white people
summary(predict(logit_model, lp, type='response'))

linear_model <- lm(approve~white, data=lp)
summary(linear_model)

0.7077+0.2066*1 # for White
0.7077+0.2066*0 # for non-white

#part2:
omit.na(lp$married)
omit.na(lp$dep)
omit.na(lp$male)
logit_model2<- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr ,data=lp, family='binomial')
summary(logit_model2)
margins(logit_model2)
summary(predict(logit_model2, lp, type='response'))

#Question 8:
#Part1: 89.81% of the people are employed at the time  of the interview. 9.91% of the people has abused alochol.
#part2: the estimated equation after heteroschdesticity is: employ=0.900 - 0.0283*abuse + u.
# the relationship is as I expected as 1 point increase in abuse decreases employement by 0.0283 which is a good sign.
# It is statisticaly significant at 99%.
#part3: Yes, we got the same negative sign and statistical significance as we got in part2.
# The probability of the average marginal effect is -0.025, for linear model it is -0.028, which is almost similar.
#part4: the fitted value when abuse = 0 is 0.900 and when abuse= 1 is 0.872. there is just one dependent variable so the fitted values are almost same.
#part5: After adding all the variables the significancy of abuse decreases. it is now significant at 90%.
#part6: The marginal effect on abuse is -0.01906 and the t stat is -2.105. The estimated effect is somewhat identical. yes, it is close.
#part7: Health variables can be included in the model but it will result in an over contolling factor. the result may go vague and we may not get desired outcomes.
#part8: There can be some other factors apart from abuse that can be endogeneous factors in determining employment.
# Whether a men's mother or father were alcoholics make sense in the effect of employment as the varibales are significant.

alcohal <- dbReadTable(con,'ALCOHOL')
summary(alcohal)
dbReadTable(con,'alcohol_labels')

#part1
sum(as.numeric(alcohal$status=='3'))
(8822/9822)*100

sum(as.numeric(alcohal$abuse=='1'))
(974/9822)*100

#part2:
linear_model<-lm(employ~abuse, data=alcohal)
summary(linear_model)
coeftest(linear_model,sandwich::vcovHC)

#part3:
logit_model<-glm(employ~abuse, data=alcohal, family='binomial')
summary(logit_model)
margins(logit_model)

#part4:
summary(fitted(linear_model))
summary(fitted(logit_model))

#part5:
linear_added_model<-lm(employ~abuse+age+I(age)+educ+I(educ)+married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3, data=alcohal)
summary(linear_added_model)

#part6:
logit_added_model<-glm(employ~abuse+age+I(age)+educ+I(educ)+married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3, data=alcohal, family='binomial')
summary(logit_added_model)
margins(logit_added_model)
-0.01906/0.106746

#part8:
linear_fm_model<-lm(abuse~age+I(age)+educ+I(educ)+married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3+mothalc+fathalc, data=alcohal)
summary(linear_fm_model)

#Question9
#Part1:
fertil<- dbReadTable(con,'fertil1')
summary(alcohal)
dbReadTable(con,'fertil1_labels')
     
fertil$y76<- as.numeric(fertil$year=='76')
fertil$y78<- as.numeric(fertil$year=='78')
fertil$y80<- as.numeric(fertil$year=='80')
fertil$y82<- as.numeric(fertil$year=='82')
fertil$y84<- as.numeric(fertil$year=='84')

logit_model<-glm(kids~educ+age+I(age^2)+black+east+northcen+west+farm+othrural+town+smcity+y76+y78+y80+y82+y84, data=fertil, family='poisson')
summary(logit_model)

#part2:
margins(logit_model)
#part3:
fited_model<-(fitted(logit_model))
summary(fited_model)
cor(fertil$kids, fited_model)
linear_mod<-lm(kids~educ+age+I(age^2)+black+east+northcen+west+farm+othrural+town+smcity+y76+y78+y80+y82+y84, data=fertil)
summary(linear_mod)

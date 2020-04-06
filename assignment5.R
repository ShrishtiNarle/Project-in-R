library(data.table) 
library(DBI)
library(RSQLite)
library(broom)
library(plm)
library(lmtest)
library(sandwich)
library(margins)
library(leaps)
library(ggplot2)
library(party) 
library(dplyr)
library(partykit)
con <- dbConnect(RSQLite::SQLite(),'wooldridge2.db')

dbListTables(con)

n <- 500
set.seed(75080)

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 50
y   <- -100*z+ 1100 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt1 <- data.table('id'=1:500,'sat'=y,'income'=x,'group'=rep(1,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 80
y   <- -80*z+ 1200 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt2 <- data.table('id'=501:1000,'sat'=y,'income'=x,'group'=rep(2,n))

z   <- rnorm(n)
w   <- rnorm(n)
x   <- 5*z + 30
y   <- -120*z+ 1000 + 50*w
y   <- 10*round(y/10)
y   <- ifelse(y<200,200,y)
y   <- ifelse(y>1600,1600,y)
dt3 <- data.table('id'=1001:1500,'sat'=y,'income'=x,'group'=rep(3,n))

dtable <- merge(dt1    ,dt2, all=TRUE)
dtable <- merge(dtable ,dt3, all=TRUE)

ggplot(dtable,aes(x=income,y=sat,color=as.factor(group)))+geom_point()

#Question1:
#Part1: A variable 'n' is defined and given a value of 500. Then two other variables, x and y are defined which represents
#'income' and 'SAT Scores'. an if loop is set on income so that the income lies in between 200 to 1600. A group variable is set which represents
#'#what group the income and SAT score belongs to. Similarly, 3 such groups are created ranging n, from 500 to 1500 and then all of the three
#'#groups are merged in a single data table.

#part2: The signs between pooled OLS and fixed effect model plus individual models is different because in pooled OLS the groups are not
# taken into consideration and the variation shown is on 17.3% while in individual and in fixed- effects model the groups has a mjor effect on the SAT scores and so the sign on income is negative.

#Part3: interpretation with code
#Part4: by modeling sat on income and partioning by group we get that the plots are similar to the one we ran with group models separatley.
#Part5: Cluster 1 has  641  data points and cluster 2 has 859 data points.
# cluster 2 has relatively lower income and sat levels than cluster 1.
#Part6: interpretation is done with code.
#part7: interpretation is done with code.
#part8: interpreattion is done with code.
#part9: interpretation is done with code.
library(partykit)

#part2:
typeof(dtable$sat1)
dtable$sat1<-as.factor(dtable$sat)

pooled_model<- lm(sat~income,data=dtable)
summary(pooled_model)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 950.8914     9.1279  104.17   <2e-16 ***
#  income        2.7923     0.1593   17.53   <2e-16 ***

fixed_eff<- lm(sat~income+as.factor(group)-1, data=dtable)
summary(fixed_eff)
plot(fixed_eff)


#income             -20.173      0.268  -75.26   <2e-16 ***
#  as.factor(group)1 2111.255     13.559  155.71   <2e-16 ***
#  as.factor(group)2 2812.183     21.518  130.69   <2e-16 ***
#  as.factor(group)3 1605.304      8.469  189.56   <2e-16 ***

modelg1<- lm(sat~income, data=dtable[group==1])
summary(modelg1)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 2095.3391    21.8084   96.08   <2e-16 ***
#  income       -19.8534     0.4352  -45.62   <2e-16 ***

modelg2<- lm(sat~income, data=dtable[group==2])
summary(modelg2)

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 2505.1204    36.8883   67.91   <2e-16 ***
#  income       -16.3257     0.4613  -35.39   <2e-16 ***

modelg3<- lm(sat~income, data=dtable[group==3])
summary(modelg3)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1722.484     13.373  128.81   <2e-16 ***
#  income       -24.027      0.434  -55.36   <2e-16 ***

#part3:
# After running sat on income and observing the plotted tree, people with income less than or equal to $66.989 have a median sat score of approximately 1050. 
# People with 66.989 < income <= 72.834 have a median sat score of approximately 1354. 
# People with 72.834 < income <= 75.531 have a median sat score of approximately 1290.
# People with 75.531 < income <= 78.926 have a median sat score of approximately 1244. 
# People with 78.926 < income <= 81.031 have a median sat score of 1200.
# People with 81.031 < income <= 83.338 have a median sat score of approximately 1161.
# People with 83.338 < income <= 86.812 have a median sat score of approximately 1121.
# People with 86.812 < income <= 90.549 have a median sat score of approximately 1062.
# People with 90.549 < income have a median sat score of approximately 950. 

model_tree<- ctree(sat~income, data=dtable)
summary(model_tree)
print(model_tree)
plot(model_tree)

# After running recurvisive model on sat and groups:
#[1] root
#|   [2] as.factor(group) in 1, 2
#|   |   [3] as.factor(group) in 1: 1105.620 (n = 500, err = 6291907.8)
#|   |   [4] as.factor(group) in 2: 1202.020 (n = 500, err = 4292859.8)
#|   [5] as.factor(group) in 3: 992.000 (n = 500, err = 8435200.0)
tree_group<- ctree(sat~as.factor(group)-1, data=dtable)
summary(tree_group)
plot(tree_group, type='simple')
print(tree_group)


#using both the models:
tree_both<- ctree(sat~income+as.factor(group)-1, data=dtable)
summary(tree_both)
plot(tree_both, type='simple')
print(tree_both)

#part4:
tree<- glmtree(sat~income| group-1, data=dtable)
print(tree)
plot(tree)

#part5:
kmeans.wss <- function(data,maxclu=10,seed=1,nstart=10) {
  wss <- rep(NA,maxclu)
  for (i in 1:maxclu) { 
    set.seed(seed)
    model <- kmeans(data,centers=i,nstart=nstart)
    wss[i] <- model$tot.withinss
  }
  return(wss)
}

eratio <- function(wss) {
  # Creates the eigenvalue ratio estimator for the number of clusters
  n <- NROW(wss)
  dss <- -diff(wss) # Create differences in wss (eigenvalues)
  dss <- c(wss[1]/log(n),dss) # Assign a zero case
  erat <- dss[1:(n-1)]/dss[2:n] # Build the eigenvalue ratio statistic
  gss <- log(1+dss/wss) # Create growth rates
  grat <- gss[1:(n-1)]/gss[2:n] # Calucluate the growth rate statistic
  return(c(which.max(erat),which.max(grat))) # Find the maximum number for each estimator
}

plot.wss <- function(wss) {
  plot(1:NROW(wss), wss, type="b", xlab="Number of Clusters", ylab="Aggregate Within Group SS")
}
modelk<- select(dtable, sat,income)
#view(modelk)

plot.wss(kmeans.wss(modelk))
eratio(kmeans.wss(modelk))    # optimal number of clusters is 2

set.seed(10)
modelkc<- kmeans(modelk, centers=2, nstart=10 )
summary(modelkc)
table(modelkc$cluster)
#modelkl<-kmeans(modelk,centers=2, nstart=10)$centers

modelkc$centers
table(modelkc$cluster, dtable$group)      #accuracy of the model
mean(dtable$income)

#part6:
table(modelkc$cluster, dtable$group)

#the overall accuracy for group 1 in k means is 59.4%
(297/500)*100
#the overall accuracy for group 2 in k means is 89.2%
(446/500)*100
#the overall accuracy for group 3 in k means is 76.8%
(384/500)*100
#the overall accuracy for kmeans model is 75.13%
((297+446+384)/1500)*100


#Hierarchial clustering
hclust.wss <- function(data,model=hclust(dist(data)),maxclu=10) {
  # Within sum of squares function for hierarchical clustering
  wss <- rep(NA,maxclu)
  for(i in 1:maxclu){
    gps <- cutree(model,i) # get groups for i clusters
    means <- data[,lapply(.SD,mean),by=gps] # get means
    demeaned <- data-means[gps,2:(ncol(data)+1)] # difference data from means
    wss[i] <- sum(demeaned^2) # sum squared distanaces
  }
  return(wss)
}
modelh<- hclust(dist(modelk))
modelp<-hclust.wss(data.table(modelk))
plot.wss(modelp)
eratio(modelp)     # number of clustering is 4

set.seed=99990
modelh1<- (cutree(hclust(dist(data.table(modelk))),k=4))
plot(modelh1)
table(modelh1, dtable$group)

#the overall accuracy for group 1 in hierarchial clustering is 45.6%
(228/500)*100
#the overall accuracy for group 2 in hierarchial clustering is 53.6%
(268/500)*100
#the overall accuracy for group 3 in hierarchial clustering is 50%
(250/500)*100
#the overall accuracy of herarchial model is 49.73% which is very low as comapre to k-means model.
#This shows that k means do a better job here.
((228+268+250)/1500)*100
#Part7:

dtable$k_means_cluster <- as.factor(modelkc$cluster)

pooled<- lm(sat~income, data=dtable)
summary(pooled)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 950.8914     9.1279  104.17   <2e-16 ***
#  income        2.7923     0.1593   17.53   <2e-16 ***
within<- lm(sat~income + group-1, data=dtable)
summary(within)

#Estimate Std. Error t value Pr(>|t|)    
#income   11.904      0.200   59.51   <2e-16 ***
#  group   207.577      5.307   39.11   <2e-16 ***

pooled_k_means<- lm(sat~income, data=dtable)
summary(pooled_k_means)
#(Intercept) 950.8914     9.1279  104.17   <2e-16 ***
#  income        2.7923     0.1593   17.53   <2e-16 ***

within_k_menns<- lm(sat~income+k_means_cluster-1, data=dtable)
summary(within_k_means)
#Estimate Std. Error t value Pr(>|t|)    
#income              0.6112     0.1106   5.524  3.9e-08 ***
#  k_means_cluster1 1160.9197     7.2695 159.698  < 2e-16 ***
#  k_means_cluster2  938.2959     5.7745 162.490  < 2e-16 ***

# Pooled relationship from the generated data is: sat = 950.89 + 2.79*income. A 1 dollar increase in income increases the sat score by 2.79 points. 
# Pooled relationship from k means is: sat = 950.89 + 2.79*income. A 1 dollar increase in income increases the sat score by 2.79 points.

# the data generated in within and within k means model is different.

#Herarchial model:

dtb<- select(dtable, -k_means_cluster)
dtb$heir<- modelh1
dtb$heir<- as.factor(dtb$heir)

pooled<- lm(sat~income, data=dtb)
within<- lm(sat~ income + group-1, data=dtb)

pooled_h<- lm(sat~income, data=dtb)
within_h<- lm(sat~income + heir -1, data=dtb)

summary(pooled)
summary(within)
summary(pooled_h)
summary(within_h)
# Relationships from hierarchial clustering do not reflect the relationships obtained from the data generation process. 

#part8:

dt_inc<- select(dtable, income)
plot.wss(kmeans.wss(dt_inc))
eratio(kmeans.wss(dt_inc))    # optimal number of clusters is 3

set.seed(10)
inc_k1<- kmeans(dt_inc, centers=2, nstart=10 )
summary(inc_k1)
table(inc_k1$cluster)

inc_k1$centers
table(inc_k1$cluster, dtable$group)      #accuracy of the model


485/500

485+500+500


#from the output it can be infered that out of 500 points in group 1, k means clustering correctly identified 485 data points.
#the k-means accuracy for group 1 is  97%.

#from the output it can be infered that out of 500 points in group 2, k means clustering correctly identified all the 500 data points.
#the k means accuracy for group 2 is 100%.

#from the output it can be infered that out of 500 points in group 3, k means clustering correctly identifird all the 500 data points.
#the k means accuracy for group 3 is 100%.
1485/1500

#with overall accuracy of 99% taking only income variable into consideration significantly increases the value of the estimation.
#the relationship is similar to the one we obtained in the data generating process.

#Part9:
set.seed(100)
dtata<- select(dtable, sat, income)
sc_k<- kmeans.wss(scale(dtata))

plot(sc_k)
eratio((sc_k))    # optimal number of clusters is 3

inc_k1<- kmeans(scale(dtata), centers=3, nstart=10 )
summary(inc_k1)
table(inc_k1$cluster)

inc_k1$centers
table(inc_k1$cluster, dtable$group)      #accuracy of the model

371/500
330/500

#from the output it can be infered that out of 500 data points in group 1, k means clustering have correctly identified 371 points.
#the accuracy for group 1 is 74.2%.

#from the output it can be infered that out of 500 data points in group 2, k means clustering have correctly identified all 500 data points.
# the accuacy for group 2 is 100%.

#from the output it can be infered that out of 500 data points in group 3, k means clustering have correctly identified 330 data points.
# the accuracy for group 3 is 66%.


(371+500+330)/1500
#with the overall acuracy of 80.06%, with scaled data we can say that this is a good k means estimation model.



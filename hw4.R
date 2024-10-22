library(tidyverse) 

# 1(a)
m=4
n=6
N=m*n
alpha=0.05
f_crit <- qf(1-alpha, m-1, N-m)
lambda<- n* (4+25+9)/5^2
power <- 1-pf(f_crit,m-1,N-m,ncp=lambda)
#Power=0.6232

# 1(b)
while(power<0.8){
  n<-n+1
  lambda<-n*(38)/25
  N=n*m
  f_crit <- qf(1-alpha, m-1, N-m)
  power <- 1-pf(f_crit,m-1,N-m,ncp=lambda)
}
n
#n=9

#2(a)
group<-factor(c(1,1,1,2,2,2,3,3,3))
X<-model.matrix(~group+0)

#2(b)
meanA<-(78.0+86.8+103.8)/3
meanB<-(83.8+81.5+86.2)/3
meanC<-(83.7+89.0+99.2)/3
beta<-c(meanA,meanB,meanC)
Y<- X %*% beta
estimated_beta<-solve(t(X)%*%X)%*%t(X)%*%Y
estimated_mu_pork<-estimated_beta[3]

#2(c)
type<-(c(1,1,1,2,2,2,3,3,3))
result<-(c(78.0,86.8,103.8,83.8,81.5,86.2,83.7,89.0,99.2))
data <- cbind.data.frame(type, result)
f1 <- lm(result ~ -1+as.factor(type), data=data)
confint(f1)


#2(e)
covariance<-var(resid(f1))*solve(t(X)%*%X)

a<-resid(f1)
cov(resid(f1)[1:3],resid(f1)[7:9])

sample_data <- data.frame( var1 = c(-11.53333333,-2.73333333,14.26666667),
                           var2 = c(-0.03333333,-2.33333333,2.36666667),
                           var3 = c(-6.93333333, -1.63333333,8.56666667 ))

# create covariance matrix
cov( sample_data )

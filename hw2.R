library(tidyverse) 

set.seed(4)
nA <- 20
nB <- 10

yA <- rnorm(nA, mean = 0, sd = sqrt(1))
yB <- rnorm(nB, mean = 0, sd = sqrt(4))
sample <- c(yA, yB)
trt <-c(rep("A", nA), rep("B", nB))
data <- cbind.data.frame(trt, sample)

# 1(a)
result <- t.test(yB, yA, var.equal = TRUE)
#calculate by myself
#sp2 <- ( (nA-1)*var(yA) + (nB-1)*var(yB) )/(nA-1+nB-1)
#sp <- sqrt(sp2)
#(mean(yB) - mean(yA)) / (sp*sqrt(1/nA+1/nB))
##t-statistic is 2.3679

# 1(b)
set.seed(4)
runSim <- function(nSim, response, group.label) {
  g.null <- numeric(nSim)
  for(i in 1:nSim){
    xsim <- sample(group.label)
    g.null[i] <- abs(t.test(response[xsim=="B"],(response[xsim=="A"]) )$statistic)
  }
  return(g.null)
}
gn1 <- runSim(nSim=10000, response=sample, group.label=trt)
mean(gn1 >= abs(result$statistic))
hist(gn1,main="T Test", freq=F); abline(v=result$statistic, col="red", lwd=2)
##p-value is 0.0283, reject the null hypothesis

# 1(c)i
df <- result$parameter
##Degrees of Freedom is 28

# 1(c)ii
alpha <- 0.05
tcrit <- qt(1-alpha/2,df)
##critical value is 2.0484

#1(c)iii
pvalue <- result$p.value
#calculate by myself
#sp2 <- ( (nA-1)*var(yA) + (nB-1)*var(yB) )/(nA-1+nB-1)
#sp <- sqrt(sp2)
#2*pt((mean(yB) - mean(yA)) / (sp*sqrt(1/nA+1/nB)),df=df, lower.tail=FALSE)

##p-value is 0.0250, reject the null hypothesis


# 2(a)
result2 <- t.test(yB, yA)
#calculate by myself
#mod_t_statistic <- (mean(yB) - mean(yA)) / (sqrt(var(yA)/nA+var(yB)/nB))
##observed test statistic is 1.9085

# 2(b)
df2 <-result2$parameter
#calculate by myself
#df2 <- ((var(yA)/nA+var(yB)/nB)^2) / ((var(yA)/nA)^2/(nA-1)+(var(yB)/nB)^2/(nB-1))
##Degrees of Freedom is 11.2278

# 2(c)
alpha <- 0.05
tcrit2 <- qt(1-alpha/2, result2$parameter)
##critical value is 2.1955

# 2(d)
pvalue2<-result2$p.value
#calculate by myself
#2*pt(t_statistic, df2, lower.tail=FALSE)
##p-value is 0.0822, don't reject the null hypothesis

# 2(e)
ggplot(data, aes(x = trt, y = sample, fill = trt)) +
  geom_boxplot() +
  labs(title = "Boxplot of yA and yB",
       x = "Group",
       y = "Value") +
  theme_bw() +                                
  theme(panel.grid.minor = element_blank())+
  guides(color=guide_legend(title="group"))

# 4(a)i
par(mfrow=c(2,2))
set.seed(4)
d1<-rcauchy(1000,0,1)
qqnorm(d1,main = "i")
qqline(d1,col = 2)
# 4(a)ii
set.seed(4)
d2<-runif(1000,0,1)
qqnorm(d2,main = "ii")
qqline(d2,col = 2)
# 4(a)iii
set.seed(4)
d3<-rbeta(1000,5,1)
qqnorm(d3,main = "iii")
qqline(d3,col = 2)
# 4(a)iii
set.seed(4)
d4<-rchisq(1000,2)
qqnorm(d4,main = "iv")
qqline(d4,col = 2)


library(tidyverse) 
Plants <- readRDS("~/Downloads/Plants.RDS")
mean_1=mean(Plants$weight[Plants$group == 1])
mean_2=mean(Plants$weight[Plants$group == 2])
median_1=median(Plants$weight[Plants$group == 1])
median_2=median(Plants$weight[Plants$group == 2])
sd_1=sd(Plants$weight[Plants$group == 1])
sd_2=sd(Plants$weight[Plants$group == 2])



plot_of_CDF <- ggplot(Plants, aes(weight,color=Plants$group)) + stat_ecdf(geom = "step", size=1)
plot_of_CDF + labs(x="weight", y="cumulative percentage", title="CDFs for Plant growth") + 
  theme_bw() +                                
  theme(panel.grid.minor = element_blank()) + 
  guides(color=guide_legend(title="group"))

#ks_test

result_ks <- ks.test(Plants$weight[Plants$group == "1"], Plants$weight[Plants$group == "2"])


##variance ratio statistic(not sure)


#result_var <- var.test(Plants$weight[Plants$group == 1], Plants$weight[Plants$group == 2])


result_ks <- ks.test(Plants$weight[Plants$group == "1"], Plants$weight[Plants$group == "2"])
runSim <- function(nSim, response, group.label) {
  g.null <- numeric(nSim)
  for(i in 1:nSim){
    xsim <- sample(group.label) 
    g.null[i] <- ks.test(Plants$weight[xsim == "1"], Plants$weight[xsim == "2"])$statistic
    
  }
  return(g.null) 
}

set.seed(5153)
ks <- runSim(nSim=10000, response=Plants$weight, group.label=Plants$group)
mean(ks >= result_ks$statistic)
hist(ks,main="KS statistic", freq=F); abline(v=result_ks$statistic, col="red", lwd=2)



var1 <- var(Plants$weight[Plants$group == 1])
var2 <- var(Plants$weight[Plants$group == 2])
result_var <- max((var1)/(var2), (var2)/(var1))

runSim2 <- function(nSim, response, group.label) {
  g.null <- numeric(nSim)
  for(i in 1:nSim){
    xsim <- sample(group.label)
    var1 <- var(Plants$weight[xsim == "1"])
    var2 <- var(Plants$weight[xsim == "2"])
    g.null[i] <-max((var1)/(var2), (var2)/(var1))
  }
  return(g.null)
}

set.seed(3823)
var <- runSim2(nSim=1000, response=Plants$weight, group.label=Plants$group)
mean(var >= result_var)
hist(var,main="Var statistic", freq=F); abline(v=result_var, col="red", lwd=2)

#var <- runSim2(nSim=10000, response=Plants$weight, group.label=Plants$group)
#mean(var >= result_var$statistic)
#hist(var,main="Var statistic", freq=F); abline(v=result_var$statistic, col="red", lwd=2)

yA <- c(256 , 159, 149)
yB <- c(54 , 123, 248)
copepoda <- c(yA, yB)
trt <- rep(c("A", "B"), each=3)
data <- cbind.data.frame(trt, copepoda)

result_q4_exact <- abs(mean(yA)-mean(yB)) 

n.combs <- choose(6,3); n.combs
all.combinations <- combn(6,3)
g.null <- numeric(n.combs)
for(i in 1:n.combs){
  B.indices <- all.combinations[ ,i]
  g.null[i] <- abs( mean(data$copepoda[B.indices] ) - mean(data$copepoda[-B.indices]) )
}
mean(g.null >= result_q4_exact)
hist(g.null,main="Exact Randomization Test", freq=F); abline(v=result_q4_exact, col="red", lwd=2)



yA <- c(256 , 159, 149)
yB <- c(54 , 123, 248)
copepoda <- c(yA, yB)
trt <- rep(c("A", "B"), each=3)
data <- cbind.data.frame(trt, copepoda)
 
result_q4_t <- t.test(yA,yB)

n.combs <- choose(6,3); n.combs
all.combinations <- combn(6,3)
g.null <- numeric(n.combs)
for(i in 1:n.combs){
  B.indices <- all.combinations[ ,i]
  g.null[i] <- abs(t.test(data$copepoda[B.indices],data$copepoda[-B.indices])$statistic)
}
mean(g.null >= abs(result_q4_t$statistic))
hist(g.null,main="T Test", freq=F); abline(v=result_q4_t$statistic, col="red", lwd=2)



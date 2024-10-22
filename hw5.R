library(tidyverse) 
library(multcomp)

#2(a)
lentilexp<-read.table("lentilexp.dat",header=T)
mean_1=mean(lentilexp$Y[lentilexp$TRT == 1])
mean_2=mean(lentilexp$Y[lentilexp$TRT == 2])
mean_3=mean(lentilexp$Y[lentilexp$TRT == 3])
mean_4=mean(lentilexp$Y[lentilexp$TRT == 4])
mean_5=mean(lentilexp$Y[lentilexp$TRT == 5])
mean_6=mean(lentilexp$Y[lentilexp$TRT == 6])

y<-lentilexp$Y
trt<-lentilexp$TRT
dat <- cbind.data.frame(y, trt)
anova(lm(lentilexp$Y~lentilexp$TRT,data=dat))
#2(b)
C<- matrix(c(-5, 1, 1, 1, 1, 1,
              0, -1, -1, 0, 1, 1,
              0, 1, -1, 0, 1, -1,
              0, 0, 1, -2, 1, 0), ncol = 6, byrow = TRUE)
g_means <- dat %>% group_by(trt) %>% summarise(samp_means = mean(y))
est.C <- C %*% as.matrix(g_means[,2])
MSE <- anova(lm(lentilexp$Y~lentilexp$TR, data=dat))$M[2]
se.C <- sqrt(MSE)*sqrt(rowSums(C^2/6))
K.bon <- 4
alpha<-0.05
bon.ci <- cbind(est.C - qt(1-(alpha/(2*K.bon)), 16)*se.C, 
                est.C + qt(1-(alpha/(2*K.bon)), 16)*se.C)
colnames(bon.ci) <- c("Bon.LowerCI", "Bon.UpperCI")
bon.ci

unadj.p <- 2*pt(abs(est.C/se.C), 16, lower.tail=F)
round(p.adjust(unadj.p, method="holm"), 3)

#3(a)
Days = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
Temperature = c(22, 22, 22, 24, 24, 24, 26, 26, 26, 28, 28, 28)
FAN = c(39.4, 56.4, 70.2, 49.9, 68.0, 81.5, 55.1, 76.4, 95.6, 59.5, 88.8, 99.6)
data <- cbind.data.frame(Days,Temperature,FAN)

interaction.plot(
  x.factor = Days,
  trace.factor = Temperature,
  response = FAN,
  type = "b", 
  legend = TRUE, 
  col = c("red", "blue","green","purple"), 
  pch = c(16, 17, 18, 19), 
  main = "Interaction Plot",  
  xlab = "Days",  
  ylab = "FAN" 
)

#3(b)
anova(lm(FAN ~ Temperature * Days, data=data))

#4(a)
Cars<-readRDS("cars.RDS")
ggplot(Cars, aes(x = CAR, y = MPG, fill=CITY, color=CITY)) +
  geom_point() +
  labs(title = "Scatterplot",
       x = "CAR",
       y = "MPG") +
  theme_bw() +                                
  theme(panel.grid.minor = element_blank())+
  guides(color=guide_legend(title="CITY"))
#4(b)
Cars$CAR <- factor(Cars$CAR)
Cars$CITY <- factor(Cars$CITY)
summary(aov(MPG ~ CAR * CITY, data = Cars))
#4(c)
f<-lm(MPG~CAR*CITY, data=Cars)
ggplot(Cars, aes(x = CAR, y = residuals(f), fill=CITY)) +
  geom_boxplot() +
  labs(title = "Boxplot",
       x = "CAR",
       y = "Residuals") +
  theme_bw() +                                
  theme(panel.grid.minor = element_blank())+
  guides(color=guide_legend(title="CITY"))
qqnorm(residuals(f), pch=16); qqline(residuals(f))
#4(d)
interaction.plot(
  x.factor = Cars$CAR,
  trace.factor = Cars$CITY,
  response = Cars$MPG,
  type = "b", 
  legend = TRUE, 
  col = c("red", "blue","green"), 
  pch = c(16, 17, 18), 
  main = "Interaction Plot",  
  xlab = "CAR",  
  ylab = "MPG",
  trace.label="CITY")




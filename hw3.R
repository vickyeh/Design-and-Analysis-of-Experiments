library(tidyverse) 


# 1(a)
result <- t.test(birds$typical, birds$odd, paired=TRUE)
birds_diff<-birds$typical-birds$odd
result2<-t.test(birds_diff)

# 1(b)
delta <- 0.1
n<-nrow(birds)
sp2 <- ( (n-1)*var(birds$typical) + (n-1)*var(birds$odd) )/(n-1+n-1)
sp <- sqrt(sp2)
gamma <- delta/(sp*sqrt(1/n +1/n))

# 1(c)
alpha=0.05
t.crit <- qt(1-alpha/2, df=15)
power <- pt(-t.crit, df=15, ncp=gamma) + (1 - pt(t.crit, df=15, ncp=gamma))

# 1(d)
tryn=seq(16,30)
gamma <- delta/(sp*sqrt(1/tryn +1/tryn))
t.crit <- qt(1-alpha/2, df=tryn-1)
pt(-t.crit, df=tryn-1, ncp=gamma) + (1 - pt(t.crit, df=tryn-1, ncp=gamma))


library(tidyverse) 
# 3(b)
traffic$Type <- factor(traffic$Type, levels = c("Pretimed", "Semi-actuated", "Fully-actuated"))
ggplot(traffic, aes(x = Type, y = Time, fill = Type )) +
  geom_boxplot() +
  labs(title = "Boxplot of three Signal Type",
       x = "Group",
       y = "Value") +
  theme_bw() +                                
  theme(panel.grid.minor = element_blank())+
  guides(color=guide_legend(title="group"))

mean_all<-mean(traffic$Time)
mean_Pretimed<-mean(traffic$Time[traffic$Type=="Pretimed"])
mean_Semiactuated<-mean(traffic$Time[traffic$Type=="Semi-actuated"])
mean_Fullyactuated<-mean(traffic$Time[traffic$Type=="Fully-actuated"])

var1=var(traffic$Time[traffic$Type=="Pretimed"])
var2=var(traffic$Time[traffic$Type=="Semi-actuated"])
var3=var(traffic$Time[traffic$Type=="Fully-actuated"])

dat<-cbind.data.frame(c(traffic$Time), c(traffic$Type))
anova(lm(traffic$Time~traffic$Type, data=dat))
f<-lm(traffic$Time~traffic$Type, data=dat)
qqnorm(residuals(f), pch=16); qqline(residuals(f))

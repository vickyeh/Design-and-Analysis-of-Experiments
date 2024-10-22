install.packages("lsmeans")   # You will get a message about "esmeans" taking over. It should not affect anything we do.
install.packages("pbkrtest")  # Allows us to get "Kenward-Rogers" approximations to DF when needed, see bonus discussion.
library(lsmeans)
library(pbkrtest)
library(tidyverse) 
#1(a)
ex14<-read.table("ex14-1.txt",header=TRUE)
ex14$block <- factor(ex14$block)
ex14$trt <- factor(ex14$trt)
str(ex14)

#1(b)
mod <- lm(response ~ trt+block, data=ex14)
anova(mod)
lsmeans(mod, pairwise~trt, adjust="none")

#1(c)
par(mfrow=c(1,2))
plot(mod, which=1:2, pch=16)
dev.off()


options("install.lock"=FALSE)
install.packages("Matrix", dependencies = TRUE, INSTALL_opts = '--no-lock')

#4(a)
calc<-read.table("calc.txt",stringsAsFactors=TRUE)
calc$Exam <- as.factor(calc$Exam)
calc$Text <- as.factor(calc$Text)
calc$School <- as.factor(calc$School)
str(calc)
install.packages("lmerTest") 
library(lmerTest)

#4(b)
par(mfrow=c(1,1))
with(calc, interaction.plot(response=Score, x.factor=Exam, trace.factor=Text, 
                               type="b", col=1:2,
                               xlab = "Exam"))

ggplot(calc %>% group_by(School, Exam) %>% summarise(mean_score = mean(Score)), 
       aes(Exam, mean_score, group=School, color=School)) + geom_point(size=4) + geom_line(size=1.25) +
  theme_bw() + 
  theme(panel.grid.minor =  element_blank()) + 
  labs(x="Exam", y="Mean Score", color = "School")

ggplot(calc, 
       aes(Text, Score, group=WholePlot, color=WholePlot)) + geom_point() + geom_line() +
  theme_bw() +
  theme(panel.grid.minor =  element_blank()) +
  labs(x="Text", y="Score", color = "WholePlot")


#4(c)
calc <- calc %>% mutate(WholePlot = as.factor(School:Exam))
add.me <- lmer(Score ~ School+Exam*Text +(1|WholePlot), data=calc)  # Note the syntax for random effects (1|WholePlot)
anova(add.me)
summary(add.me)

#4(d)
lsmeans(add.me, pairwise~Text, adjust="none")
difflsmeans(add.me)

#4(e)
qqnorm(residuals(add.me), main="Epsilon QQ-plot", pch=16)
qqline(residuals(add.me), main="Epsilon QQ-plot")

#4(f)

library(ggplot2)
library(car)
library(lmtest)
library(normtest)
library(MASS)

setwd("~/Desktop/1A School/1A Winter 2021/STAT330/HW5")

## Read in life Data
life = read.table("life.txt",header=TRUE,stringsAsFactors = TRUE)

names(life)
head(life)

## Relabel the Group factor levels so HS is baseline
levels(life$Group) #just displays

## relevel(factor_variable, ref = "name")

life$Group = relevel(life$Group, ref = "other")
#life$Group = factor(life$Group,levels=c("HS","BS","BS+"))

levels(life$Group)

##life$Country = factor(life$Country,levels=c("No","Yes"))

life$PPGDP

## Boxplots
boxplot(LifeExp~Group,data=life)
boxplot(LifeExp~Country,data=life)

with(life,tapply(life,Country,mean))
with(life,tapply(life,Group,mean))

plot(life$PPGDP,life$life,pch=19)

ggplot(life,aes(y=life,x=Group,fill = Group)) +
  geom_boxplot()

## Exploratory Plots of Group with Colored Dots
ed.colors = c("red","blue","black")

plot(log(life$PPGDP),life$LifeExp,
     col=ed.colors[life$Group],pch=19,
     xlab="PPGDP",ylab="life")

legend("bottomright",
       legend=levels(life$Group),
       col=ed.colors,pch=19,cex = 0.8)


ggplot(life,aes(y=life,x=PPGDP,color=Group))+
  geom_point() 


ggplot(life,aes(y=life,x=PPGDP,color=Group:Country))+
  geom_point() 


ggplot(life,aes(y=life,x=PPGDP,color=Country))+
  geom_point() 


ggplot(life,aes(y=life,x=PPGDP,color=Group:Country))+
  geom_point() +
  geom_smooth(method="lm",se=FALSE)

## Exploratory Plots of Country with Colored Dots

man.colors = c("firebrick","blue","black",
               "green","cyan","purple")

plot(life$PPGDP,life$life,
     col=man.colors[life$Country:life$Group],pch=19,
     xlab="PPGDP",ylab="life")

legend("bottomright",
       legend=levels(life$Country:life$Group),
       col=man.colors,pch=19,ncol = 2)

## Fit a Model with Group/Country Interaction

sal.lm = lm(formula = LifeExp ~ log(PPGDP) + Group +
              log(PPGDP):Group, data=life)

summary(sal.lm)

ggplot(data=life, mapping = aes(x=log(PPGDP), y=LifeExp, col=Group)) + geom_point() +
              geom_smooth(method="lm")

#sal.lm = lm(formula = life~PPGDP+Group*Country,data=life)

#summary(sal.lm)


#################### Design Matrix


X.mat = model.matrix(sal.lm)

head(X.mat)

################ ################ ################ 
################ Look at assumptions
################ ################ ################ 

######### linearity

avPlots(sal.lm)

######### linearity, indep, and equal variance

par(mar = c(4,4,1,1)) #margins
plot(sal.lm$fitted.values,stdres(sal.lm))
abline(h = 0,col = "red",lty = 2)


bptest(sal.lm)

######### normality

hist(stdres(sal.lm),freq = FALSE,ylim = c(0,.4))
curve(dnorm,from = -3,to = 3,add = TRUE)

qqnorm(stdres(sal.lm))
abline(a = 0,b=1,col = "red",lty = 2)

jb.norm.test(stdres(sal.lm),nrepl = 1e5)
ks.test(stdres(sal.lm),"pnorm")



# head(X.mat[,colnames(X.mat) != "GroupBS:CountryYes"])
# 
# 
# d = X.mat[,colnames(X.mat) != "GroupBS:CountryYes"]
# lm(life$life ~ d[,-1])


## F-test for overall regression
summary(sal.lm)
summary(sal.lm)$fstat

## F-test for test of interaction terms

noint.lm = lm(LifeExp~log(PPGDP)+Group,data=life)
summary(noint.lm)

#### H_0: \beta_5 = 0 and \beta = 6 = 0
#### H_A: At least one is nonzero
#### F = num/den
#### num = (R^2_P - R^2_Q)/(P-Q)
#### den = (1-R^2_P)/ (N-P-1)

anova(noint.lm,sal.lm)  ### Test on whether interaction is significant


## F-test for Group x PPGDP interations

#### H_0: \beta_7 = 0 and \beta = 8 = 0
#### H_A: at least one is nonzero
#### Test on Group x PPGDP interation

exp.edu.int.lm = lm(formula = life ~ Group*PPGDP +
                      Group*Country,data=life)

summary(exp.edu.int.lm)

anova(sal.lm,exp.edu.int.lm)

noman.lm = lm(life ~ PPGDP + Group,data=life)

anova(noman.lm,noint.lm,sal.lm)


anova(noint.lm,sal.lm)

## T-tests for individual effects
summary(sal.lm)

## Confidence Intervals
confint(sal.lm)

## Plot Fitted Regression lines
plot(life~PPGDP,ylim = c(1e4,4e4),
     data=life,pch=19)
abline(a=sal.lm$coef[1],b=sal.lm$coef[2],col="black") #HS, No
abline(a=sal.lm$coef[1]+sal.lm$coef[3]+sal.lm$coef[5]+
         sal.lm$coef[6],b=sal.lm$coef[2],col="green") #BS, Yes

abline(a=sal.lm$coef[1]+sal.lm$coef[4],b=sal.lm$coef[2],col="purple") #BS+, No
abline(a=sal.lm$coef[1]+sal.lm$coef[3],b=sal.lm$coef[2],col="red") #BS, No
abline(a=sal.lm$coef[1]+sal.lm$coef[5],b=sal.lm$coef[2],col="orange") #HS, Yes
abline(a=sal.lm$coef[1]+sal.lm$coef[4]+sal.lm$coef[5]+sal.lm$coef[7],
       b=sal.lm$coef[2],col="cyan") #BS+, Yes
legend("topleft",c("HS,NO","HS,Yes","BS,NO","BS,Yes","BS+,NO","BS+,Yes"),
       col = c("black","orange","red","green","purple","cyan"),
       lwd = 4,lty = 1,ncol = 2)


#F test 



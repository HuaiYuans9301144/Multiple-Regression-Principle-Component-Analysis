rm(list=ls())
library(PoEdata) #for PoE datasets
library(knitr) #for referenced tables with kable()
library(xtable) #makes data frame for kable
library(printr) #automatically prints output nicely
library(effects)
library(car)
library(AER)
library(broom) #for tidy lm output and function glance()
library(stats)
library(tidyverse)
#1,2
alpha <- 0.05
data("andy", package="PoEdata")
N <- NROW(andy) #Number of observations in dataset
K <- 4 #Four Betas in the unrestricted model
J <- 2 #Because Ho has two restrictions
fcr <- qf(1-alpha, J, N-K) 
mod1 <- lm(sales~price+advert+I(advert^2), data=andy) 
anov <- anova(mod1)   
anov # prints 'anova' table for the unrestricted model
SSEu <- anov[4,2]

mod2 <- lm(sales~price, data=andy) # restricted
anov <- anova(mod2)    
anov # prints the 'anova' table for the restrictred model
SSEr <- anov[2,2]

fval <- ((SSEr-SSEu)/J) / (SSEu/(N-K))
pval <- 1-pf(fval, J, N-K)

Hnull <- c("advert=0", "I(advert^2)=0")
linearHypothesis(mod1,Hnull) 

summary(mod1)

smod1 <- summary(mod1)
kable(tidy(smod1), caption="Tidy 'summary(mod1)' output") 
fval <- smod1$fstatistic

library(broom)
kable(tidy(mod1), caption="'Tidy(mod1)' output")
glance(mod1)$statistic #Retrieves the F-statistic 
names(glance(mod1)) #Shows what is available in 'glance
hyp <- c("advert+3.8*I(advert^2)=1",
         "(Intercept)+6*price+1.9*advert+3.61*I(advert^2)=80")
kable(glance(mod1),
      caption="Function 'glance(mod1)' output", digits=2,
      col.names=(c("Rsq","AdjRsq","sig","F",
                   "pF","K","logL","AIC","BIC","dev","df.res","Nobs")))

lhout <- tidy(linearHypothesis(mod1,hyp))
kable(lhout,caption="Joint hypotheses with the 'linearHypothesis' function")
#3
?edu_inc
data("edu_inc", package="PoEdata")
mod1 <- lm(faminc~he+we, data=edu_inc)
mod2 <- lm(faminc~he, data=edu_inc)
kable(tidy(mod1), caption="The correct model")
kable(tidy(mod2),caption="The incorrect model ('we' omitted)")

#4
data(edu_inc)
mod3 <- lm(faminc~he+we+kl6, data=edu_inc)
mod4 <- lm(faminc~he+we+kl6+xtra_x5+xtra_x6, data=edu_inc)
kable(tidy(mod3), caption="Correct 'faminc' model")  
kable(tidy(mod4), caption="Incorrect 'faminc' with irrelevant variables")
        

#5 
mod1 <- lm(faminc~he, data=edu_inc)
mod2 <- lm(faminc~he+we, data=edu_inc)
mod3 <- lm(faminc~he+we+kl6, data=edu_inc)
mod4 <- lm(faminc~he+we+kl6+xtra_x5+xtra_x6, data=edu_inc)
r1 <- as.numeric(glance(mod1))
r2 <- as.numeric(glance(mod2))   #as.numeric 轉數值
r3 <- as.numeric(glance(mod3))
r4 <- as.numeric(glance(mod4))
tab <- data.frame(rbind(r1, r2, r3, r4))[,c(1,2,8,9)]
row.names(tab) <- c("he","he, we","he, we, kl6", 
                    "he, we, kl6, xtra_x5, xtra_x6")
kable(tab
      ,caption="Model comparison, 'faminc' ", digits=4
      ,col.names=c("Rsq","AdjRsq","AIC","BIC"))

library(stats)
smod1 <- summary(mod1)
Rsq <- smod1$r.squared    
AdjRsq <- smod1$adj.r.squared 
aic <- AIC(mod1)
bic <- BIC(mod1)
c(Rsq, AdjRsq, aic, bic)

mod3 <- lm(faminc~he+we+kl6, data=edu_inc)
resettest(mod3, power=2, type="fitted")   
resettest(mod3, power=2:3, type="fitted")

#6
data("cars", package="PoEdata")
mod1 <- lm(mpg~cyl, data=cars)
kable(tidy(mod1), caption="A simple linear 'mpg' model")

head(cars)

mod2 <- lm(mpg~cyl+eng+wgt, data=cars)
kable(tidy(mod2), caption="Multivariate 'mpg' model")

tab <- tidy(vif(mod2)) #VIF>10 具有共線性
kable(tab,caption="Variance inflation factors for the 'mpg' regression model",
      col.names=c("regressor","VIF"))

#7
predpoint <- data.frame(price=6, advert=1.9)
mod3 <- lm(sales~price+advert+I(advert^2), data=andy)
pre<-data.frame(predict(mod3, newdata=predpoint,interval="prediction"))
kable(pre, caption="Forecasting in the quadratic 'andy' model")

#(a)
data(pizza4)
mod <- lm(pizza~age+income+age:income,data= pizza4)
summary(mod)
Hnull <- c("age=0", "age:income=0")
linearHypothesis(mod,Hnull)

#(b)  dPizza/dIncome= b3+b4*age
age<-c(20,30,40,50,55)
MPS<- mod$coefficients[3]+mod$coefficients[4]*age

df <- df.residual(mod)
alpha<-0.05
tcr1 <- qt(1-(alpha/2),df)
seMPS <- sqrt(vcov(mod)[3,3]+(age^2)*vcov(mod)[4,4]+2*age*vcov(mod)[4,3])

upper<- MPS+tcr1*seMPS
lower<- MPS-tcr1*seMPS
Interval<-cbind(MPS,seMPS,lower,upper)
rownames(Interval)<-c("AGE=20","AGE=30","AGE=40","AGE=50","AGE=55")
colnames(Interval)<-c("Point estimates","standard errors","Lower","Upper")
kable(Interval)

#(c)
mod2<-lm(pizza~age+income+age:income+income:I(age^2),data= pizza4)
summary(mod2)
Hnull2 <- c("income:I(age^2)=0")
linearHypothesis(mod2,Hnull2)

#(d)dPizza/dIncome= b3+b4*age+b5*age^2
mps2<- mod2$coefficients[3]+mod2$coefficients[4]*age+mod2$coefficients[5]*age^2
df2 <- df.residual(mod2)
tcr2 <- qt(1-(alpha/2),df2)
seMPS2<- sqrt(vcov(mod2)[3,3]+(age^2)*vcov(mod2)[4,4]+(age^4)*vcov(mod2)[5,5]+
                2*age*vcov(mod2)[3,4]+2*age^2*vcov(mod2)[3,5]+ 2*age^3*vcov(mod2)[4,5])
upper<- mps2+tcr2*seMPS2
lower<- mps2-tcr2*seMPS2
Interval2<-cbind(mps2,seMPS2,lower,upper)
rownames(Interval2)<-c("AGE=20","AGE=30","AGE=40","AGE=50","AGE=55")
colnames(Interval2)<-c("Point estimates","standard errors","Lower","Upper")
kable(Interval2)

#(e)
Hnull3 <- c("age=0", "age:income=0","income:I(age^2)=0")
linearHypothesis(mod2,Hnull3)

#(f)
mod3<-lm(pizza~age+income+age:income+income:I(age^2)+I(age^3):income,data= pizza4)
library(Hmisc)
mydata<- data.frame(pizza4$age,pizza4$income,(pizza4$age*pizza4$income),
                    (pizza4$age^2*pizza4$income),(pizza4$age^3*pizza4$income))
colnames(mydata)<-c("AGE","INC","AGE*INC","AGE2*INC","AGE3*INC")
res2 <- rcorr(as.matrix(mydata))   #相關係數法
res2
tab <- tidy(vif(mod3)) #vif法  以VIF>10代表有共線性存在
kable(tab,caption="Variance inflation factors for the 'mpg' regression model",
      col.names=c("regressor","VIF"))
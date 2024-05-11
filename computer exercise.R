#(a)
library(PoEdata) #for PoE datasets
library(knitr) #for referenced tables with kable()
library(xtable) #makes data frame for kable
library(effects)
library(broom) #for tidy lm output and function glance()
library(car)

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
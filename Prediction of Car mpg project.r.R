#read file and install library
dataset<-read.csv(file.choose(),header = T)
install.packages("ggpubr")
library("ggpubr")
#pearson
cor(dataset$carlength, dataset$curbweight)#0.8777285
cor(dataset$enginesize, dataset$horsepower)#0.8097687
cor(dataset$highwaympg, dataset$citympg)#0.971337


library(stringr)

dataset$company <- word(dataset$CarName, 1)
plot(x=dataset$horsepower,y=dataset$highwaympg,xlab="horsepower",ylab="highwaympg")
abline(lm(dataset$highwaympg ~dataset$horsepower ),col="blue")
plot(x=dataset$enginesize,y=dataset$citympg,xlab="enginesize",ylab="citympg")
abline(lm(dataset$citympg ~dataset$enginesize ),col="blue")

plot(ecdf(dataset[,"price"]))
plot(ecdf(dataset[,"carlength"]))
plot(ecdf(dataset[,"carwidth"]))
plot(ecdf(dataset[,"carheight"]))


##one dimension
t1<-as.data.frame(table(dataset$company))
t1$"%" <- t1$Freq/sum(t1$Freq)*100

t2<-as.data.frame(table(cut(dataset$curbweight,breaks=c(0,1000,2000,3000,4000,5000),labels=c("0-1000","1000-2000","2000-3000","3000-4000","4000-5000"))) )
t2$"%" <- t2$Freq/sum(t2$Freq)*100

#two dimension

ta1<-as.data.frame(table(dataset$carbody, dataset$enginetype))
ta1$"%" <- ta1$Freq/sum(ta1$Freq)*100


ta2<-as.data.frame(table(dataset$aspiration, dataset$enginelocation))

ta2$"%" <- ta2$Freq/sum(ta2$Freq)*100



#pass to table
library(gridExtra)
library(grid)
grid.table(t1)
grid.table(t2)
grid.table(ta1)
grid.table(ta2)

install.packages("infotheo")
mutinformation(dataset$horsepower, dataset$highwaympg, method="emp")

#part two--------------------------------
#delet variabels-------------------------
plot(x=dataset$fuelsystem,y=dataset$highwaympg,xlab="fuelsystem",ylab="highwaympg")
#interaction variables-------------------
mod1<-lm(dataset$highwaympg~dataset$enginesize*factor(dataset$fueltype))
summary(mod1)
plot(dataset$enginesize[factor(dataset$fueltype)=='gas'],dataset$highwaympg[dataset$fueltype=='gas'],col="blue",xlim=c(61,326),ylim=c(16,54),xlab='enginesize',ylab='highwaympg',main="highwaympg vs.enginesize")
points(dataset$enginesize[dataset$fueltype=='diesel'],dataset$highwaympg[dataset$fueltype=='diesel'],col="red")
legend(326,54,legend=c("gas","diesel"),col=c("blue","red"),pch=c(1,1),bty="n")
abline(a= 65.94282, b= -0.22987 , col="blue")
abline(a= (65.94282-22.01947), b= (-0.22987+0.12186) , col="red")

#model variables-----------------------
I2<-factor(dataset$fueltype)
I3<-factor(dataset$aspiration)
I6<-factor(dataset$carbody)
I7<-factor(dataset$drivewheel)
I14<-factor(dataset$enginetype)
I15<-factor(dataset$cylindernumber)
x10<-dataset$carlength
x11<-dataset$carwidth
x12<-dataset$carheight
x13<-dataset$curbweight
x16<-dataset$enginesize
x18<-dataset$boreratio
x19<-dataset$stroke
x20<-dataset$compressionratio
x21<-dataset$horsepower
x22<-dataset$peakrpm
x23<-dataset$citympg
x24<-dataset$price
y<-dataset$highwaympg
basemodel<-lm(y~1,data=dataset)
fullmodel<-lm(y~I2:x16+I3:x18+I6+I7+x10+x11+x12+x13+I14+I15+x16+x21+x22+x23+x24, data=dataset)
step(basemodel,scope = formula(fullmodel),direction = "forward",trace = TRUE)
step(fullmodel,scope~1 ,direction = "backward",trace = TRUE)
step(fullmodel,direction = "both",trace = TRUE)

Ourmodel<-lm(y ~ I7 + x12 + x13 + I14 + I15 + x16 + x21 + x22 + x23 + x24 + x16:I2 + I3:x18)
#Model assumptions test------------------
#הנחת שוויון שונויות
res <- residuals(Ourmodel)
yPredicted <- fitted(Ourmodel) # predicted values, y^
StandarizedRes <- res/sd(res) # שגיאות מתוקננות
par(mar=rep(2,4))
plot(yPredicted,StandarizedRes,main="StandarizedResiduals vs Fitted",xlab="Fitted",ylab="StandarizedResiduals")
abline(0,0)
install.packages("lmtest", dependencies = T, repos='http://cran.us.r-project.org' )
library(lmtest)
gqtest(Ourmodel,fraction = 80,data=dataset)# מבחן לשיוויון שוניות
#הנחת נורמליות
qqnorm(StandarizedRes)
qqline(StandarizedRes)
ks.test(StandarizedRes,y="pnorm")
#הנחת לינאריות
hist(StandarizedRes,pro=TRUE,xlab="StandarizedResiduals",ylab="density",col="pink")# היסטוגרמה
par(mar=rep(2,4))#הנחת נורמאליות החל מכאן
lines(density(StandarizedRes),col="blue",lw=2)
res <- residuals(Ourmodel)
yPredicted <- fitted(Ourmodel) # predicted values, y^
StandarizedRes <- res/sd(res) # שגיאות מתוקננות
par(mar=rep(2,4))
plot(yPredicted,StandarizedRes,main="StandarizedResiduals vs Fitted",xlab="Fitted",ylab="StandarizedResiduals")
abline(0,0)
#Example for our Model---------------------
predict(Ourmodel, data.frame(I7="fwd",x12=54.3,x13=2337,I14="ohc",I15="four",x16=109,x21=102,x22=5500,x23=24,x24=13950,I2="gas",I3="std",x18=3.19),interval="predict")
#Hypoteis test-----------------------------
test<-lm(y ~ I7 + x12 + x13 + I14 + I15 + x16 + x21 + x22 + x23 + x24 + x16:I2 + I3:x18 + I7:x21 )
summary((test))
anova(Ourmodel,test)
#improve model-----------------------------
summary(Ourmodel)
improved<-lm(y ~ I7 + x12 + x13 + I14 + I15 + x16 + x21 + x22+poly(x23,2)+poly(x21,2) + x23 + x24 + x16:I2 + I3:x18)
summary(improved)
rest_new<- residuals(improved)
StandarizedRes_new<- rest_new/sd(rest_new) # שגיאות מתוקננות
ks.test(StandarizedRes_new,y="pnorm")
#box cox to improve model-----------------------------
library(MASS)
library(forecast)
library(car)
library(nortest)
bc <- boxcox(Ourmodel,lambda =seq(-2,2))
best.lam <- bc$x[which.max(bc$y)]
print(best.lam)
improvedmodel<-lm(y^0.55~I7 + x12 + x13 + I14 + I15 + x16 + x21 + x22 + x23+ x24 + x16:I2 + I3:x18)
summary(improvedmodel)
rest_im<- residuals(improvedmodel)
yPredicted_im <- fitted(improvedmodel) # predicted values, y^
StandarizedRes_im <- rest_im/sd(rest_im) # שגיאות מתוקננות
ks.test(StandarizedRes_im,y="pnorm")
#shapiro wilks normality test--------------
restim2<-residuals(improvedmodel)
shapiro.test(restim2)
#בדיקת שוויון שונויות
rest <- residuals(improvedmodel)
yPredictedt <- fitted(improvedmodel) # predicted values, y^
StandarizedRest <- rest/sd(rest) # שגיאות מתוקננות
par(mar=rep(2,4))
plot(yPredictedt,StandarizedRest,main="StandarizedResiduals vs Fitted",xlab="Fitted",ylab="StandarizedResiduals")
abline(0,0)
#Example for final model---------------------
predict<-predict(improvedmodel, data.frame(I7="fwd",x12=54.3,x13=2337,I14="ohc",I15="four",x16=109,x21=102,x22=5500,x23=24,x24=13950,I2="gas",I3="std",x18=3.19),interval="predict")
install.packages("pracma")
library(pracma)
nthroot(predict,0.55)

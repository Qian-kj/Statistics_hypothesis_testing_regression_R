# Question 1
#set up
rm(list = ls())
library(knitr)
library(ggplot2)
library(cowplot)
library(lmtest)

# Import data
setwd('C:/Users/qiankj/Desktop/KCL/Statistics/Coursework')
Protein_dat<-read.csv('protein.csv',header = TRUE)
Prdata<-head(Protein_dat)
kable(Prdata)
attach(Protein_dat)


### (a) summary statistics
#(1)Level of the data  
#It is continuous data, so its location includes arithmetic mean, median, quantiles(25th percentile and 75th percentile, 1st Qu and 3rd Qu for short), minimum and maximum, which are shown as follows:
#summary_all
summ<-summary(Protein_dat[2:9])
kable(summ)

#(2)Spread of the data  
#Spread of data contains its standard deviation, IQR(Interquartile range), variance and range:
#spread
var<-c('RedMeat','WhiteMeat','Eggs','Milk','Fish','Cereals','Starch','Nuts','Fr.Veg')
for (i in var) {
  a <- sd(Protein_dat[[i]])
  b <- IQR(Protein_dat[[i]])
  c <- max(Protein_dat[[i]])
  d <- min(Protein_dat[[i]])
  e <- var(Protein_dat[[i]])
  cat(i,': standard deviation=',a,'IQR=',b,'variance=',e,'range=',c-d,'\n')
}

### (b)Plot the data in different graphs
#(1)Boxplot  
#i.Outliers of the RedMeat  
outliers = boxplot(Protein_dat$RedMeat, plot=FALSE)$out
ctr_rm<-Protein_dat[Protein_dat$RedMeat %in% outliers,]
kable(ctr_rm)

#ii.Outliers of the Eggs  
outliers = boxplot(Protein_dat$Eggs, plot=FALSE)$out
ctr_eg<-Protein_dat[Protein_dat$Eggs %in% outliers,]
kable(ctr_eg)


#iii.Outliers of the Fish  
outliers = boxplot(Protein_dat$Fish, plot=FALSE)$out
ctr_fs<-Protein_dat[Protein_dat$Fish %in% outliers,]
kable(ctr_fs)


#(2)Histograms  
opar<-par(no.readonly=T)
par(mfrow=c(3,3)) # set more than one plot per figure
for (i in 2:10){
  hist(Protein_dat[,i],main ="",xlab=names(Protein_dat)[i],probability = TRUE)
}
par(opar)


#(3)Scatter diagrams  
plot(Protein_dat[2:10], main="Scatter diagrams for Protein")

### (c)Association of the Fr.Vb and the others
var<-c('RedMeat','WhiteMeat','Eggs','Milk','Fish','Cereals','Starch','Nuts')
for (i in var) {
  #covariance
  a <- cov(Fr.Veg,Protein_dat[[i]])
  #correlation coefficient
  b <- cor(Fr.Veg,Protein_dat[[i]])
  cat(i,': covariance=',a,'correlation=',b,'\n')
}

### (d)Plot association of the Fr.Vb and the others  
par(mfrow = c(2,2))
var<-c('RedMeat','WhiteMeat','Eggs','Milk','Fish','Cereals','Starch','Nuts')
for (i in var) {
  plot(Fr.Veg,Protein_dat[[i]],pch=19,cex.axis=1.5,cex.axis=1.5,ylab = i, 
       main = paste('Fruit&Vegetables versus',i))
}

### (e)Confidence intervals

#normal dististribution
var<-c('RedMeat','WhiteMeat','Eggs','Milk','Fish','Cereals','Starch','Nuts','Fr.Veg')
par(mfrow = c(3,3))
for (i in var) {
  shapiro.test(Protein_dat[[i]])
  {qqnorm(Protein_dat[[i]], main =paste("Q-Q plot for",i))
    qqline(Protein_dat[[i]])}
}

#confidence intervals
#sample number
n<-nrow(Protein_dat)
for (i in var) {
  #Lower confidence interval
  CI_low<-mean(Protein_dat[[i]])-qt(0.975,n-1)*sd(Protein_dat[[i]])/sqrt(n)
  #Upper confidence interval
  CI_up<-mean(Protein_dat[[i]])+qt(0.975,n-1)*sd(Protein_dat[[i]])/sqrt(n)
  cat('95% confidence interval for',i,'is','[',CI_low,',',CI_up,']\n')
}
  
 ### (f) Appropriate test of hypothesis
#(1) Independence test  
#Chi-square test
chisq.test(Starch,Nuts)

#(2)Variance test  
#F-test
ftest<-var.test(Starch, Nuts, ratio = 1,
         alternative = c("two.sided"),
         conf.level = 0.95)

#(3)T test  
t.test(Starch,Nuts,paired = FALSE,alternative = 'greater')

#######################################################
#################Question 2############################
#######################################################
#setup_2
rm(list = ls())
library(do)
library(MASS)
library(leaps)
library(knitr)
library(ggplot2)
library(lmtest)
library(car)


### Import data
dat<-read.csv('DartPoints.csv',header = TRUE)
head(dat)

#Omit NAN
dp_dat<-na.omit(dat)
attach(dp_dat)

### (a) Scaling variables

### (b)Relationship between Length and the other vaiables by graphies
#(1)Graphs for Numerical variables  
par(mfrow = c(2,3))
var<-c('Width','Thickness','B.Width','J.Width','H.Length','Weight')
for (i in var) {
  m<-lm(dat[[i]]~dat[['Length']])
  {plot(dat[['Length']],dat[[i]],pch=19,cex.axis=1.5,cex.axis=1.5,xlab = 'Length',
        ylab = i, main = paste(i,'versus Length')) 
  abline(m)}
}

#(2)Graphs for categorical variables  
par(mfrow = c(2,2))
#Name vs Length graph
p1<-qplot(Name,Length, data=dat,main = 'Name vs Length')
p1

#Blade.Sh vs Length
p2<-qplot(Blade.Sh,Length, data=dat, main = 'Blade.Sh vs Length')
#Base.Sh vs Length
p3<-qplot(Base.Sh,Length, data=dat,main = 'Base.Sh vs Length')
#Should.Sh vs Length
p4<-qplot(Should.Sh,Length, data=dat,main = 'Should.Sh vs Length')
#Should.Or vs Length
p5<-qplot(Should.Or,Length, data=dat,main = 'Should.Or vs Length')
#Haft.Sh vs Length
p6<-qplot(Haft.Sh,Length, data=dat,main = 'Haft.Sh vs Length')
#Haft.Or vs Length
p7<-qplot(Haft.Or,Length, data=dat,main = 'Haft.Or vs Length')
p8 <- cowplot::plot_grid(p2, p3, p4, p5, p6, p7, nrow = 2)
p8

### (c) Association with Length
#(1)Numerical variables  
#correlation matrix
cor(dp_dat[,3:9])

#(2)categorical variables  
#Chi-square to test if they are independent. 
chisq.test(Name,Length)

#ANOVA test
nl<-data.frame(Name,Length)
a<-aov(Length~Name, data=nl)
summary(a)

### (d) Relative frequency distribution of Weight
#labeling
#Five numbers for levels splitting: minimum, lower-hinge, median, upper-hinge, maximum
break1<-fivenum(dat[['Weight']])

 #labeling
labels = c('Light','Normal','Medium','Heavy')
#Split into four levels: 
#minimum-lower-hinge, (lower-hinge)-median, median-(upper-hinge),(upper-hinge)- maximum.
Weight2<-cut(dp_dat[['Weight']],break1,labels,ordered_result = TRUE) #

#Spare DataFrame and replace Weight
Wgt<-Weight2
Wgt_Blsh = data.frame(Wgt,Blade.Sh)

#Omit NAN
Wgt_Blsh<-na.omit(Wgt_Blsh)

#Plotting
par (mfrow = c(2,2))
#Blade.shape = S
barplot(table(Wgt_Blsh[['Blade.Sh']],Wgt_Blsh[['Wgt']])[4,]/length(which(
  Wgt_Blsh[['Blade.Sh']]=='S')),main = 'Blade.shape = S',xlab = 'Weight',
  ylab='Relative frequency')
#Blade.shape = E
barplot(table(Wgt_Blsh[['Blade.Sh']],Wgt_Blsh[['Wgt']])[1,]/length(which(
  Wgt_Blsh[['Blade.Sh']]=='E')),main = 'Blade.shape = E',xlab = 'Weight',
  ylab='Relative frequency')
#Blade.shape = I
barplot(table(Wgt_Blsh[['Blade.Sh']],Wgt_Blsh[['Wgt']])[2,]/length(which(
  Wgt_Blsh[['Blade.Sh']]=='I')), main = 'Blade.shape = I',xlab = 'Weight',
  ylab='Relative frequency')
#Blade.shape = R
barplot(table(Wgt_Blsh[['Blade.Sh']],Wgt_Blsh[['Wgt']])[3,]/length(which(
  Wgt_Blsh[['Blade.Sh']]=='R')), main = 'Blade.shape = R',xlab = 'Weight',
  ylab='Relative frequency')

### (e) Multiple regression model
#(1)Forward variable selection
#Forward variable selection
lm.wgt=lm(Weight~Length,data = dp_dat)
stepAIC(lm.wgt, direction = 'forward',
        scope = list(lower = lm.wgt, upper = ~Length+Width+factor(Name)+J.Width+H.Length +
              Thickness + B.Width +	factor(Blade.Sh)	+ factor(Base.Sh) +	factor(Should.Sh) 
              +factor(Should.Or) + factor(Haft.Sh) +	factor(Haft.Or)))

#After forward
lm.wgt2=lm(Weight ~ Length + Width + factor(Blade.Sh) + factor(Should.Sh) + 
    factor(Should.Or),data = dp_dat)

#category
#Blade.Sh ranks
t1<-table(dp_dat$Blade.Sh)
kable(t1)
#Should.Sh
t2<-table(dp_dat$Should.Sh)
kable(t2)
#Should.Or
t3<-table(dp_dat$Should.Or)
kable(t3)

#(2)The fit model  
#Fit model
ft.model = lm(Weight ~ Length + Width, data = dp_dat)

#(3)Test of this model  
#Ncv test
ncvTest(ft.model) 

#(4)Data transformation and final model  
#The multiplicative inverse of each variables
w<-1/(dp_dat$Weight)
l<-1/(dp_dat$Length)
wd<-1/(dp_dat$Width)

ft_mi = lm(w ~ l + wd, data = dp_dat)

#ncv test
ncvTest(ft_mi)
#final model
ft_fl = lm(I(1/Weight) ~ I(1/Length) + I(1/Width))

### (f) The fit of the model
#(1)The fit of the model by numerical methods  
#i.Summary of the linear model  
summ<-summary(ft_fl)
summ
str(summ)

#ii.Hypothesis tests  
# Coefficients
b.est <- ft_fl$coefficients

# Standard deviation of the errors
sigma.u <- summ$sigma

#Covariates
x<-cbind(1/dp_dat[['Length']],1/dp_dat[['Width']])
colnames(x)<-c('1/Length','1/Width')

desx <- cbind(1,x)
#Standard error
var.B <- sigma.u^2*solve(t(desx)%*%desx)

# t value: Length
tval1 <- b.est[2]/sqrt( var.B[2,2] )

tval1
summ[["coefficients"]][, 3][2]

# Critical values
datf<-data.frame(cbind(dp_dat[['Weight']],x))
datf
n<-nrow(datf)
p<-ncol(x)
c(qt(0.025, df = n-2), qt(0.975, df = n - p - 1))

# t value: Width
tval2 <- b.est[3]/sqrt( var.B[3,3] )

tval2
summ[["coefficients"]][, 3][3]

# Critical values
datf<-data.frame(cbind(dp_dat[['Weight']],x))
datf
n<-nrow(datf)
p<-ncol(x)
c(qt(0.025, df = n-2), qt(0.975, df = n - p - 1))

#iii.Confidence intervals  
result_CI<-confint(ft_fl, level = 0.95)
kable(result_CI)

#(2)Diagnostic plots  
#graphical_method}
par(mfrow = c(2,2))
#Variables cannot be as denominators
plot(ft_mi)

### (g) Description of its prediction 

### (h) Prediction of dart weight
#prediction}
#New data of a dart point
x0 <- c(70,60)
new.data <- data.frame(Length = x0[1], Width = x0[2])

#Prediction results
a<-predict.lm(ft_fl, newdata = new.data, interval = "confidence", level=0.95)
#Confidence interval
a
#Result
1/a

#compare}
#Put new data into the original dataset
new_dart<-cbind(new.data$Length,new.data$Width)
new_df<-cbind(dp_dat$Length,dp_dat$Width)
new_dat<-rbind(new_dart,new_df)
colnames(new_dat) <-c('Length',"Width")
#find the outlier
boxplot(new_dat,main='The boxplot for new dart point')

# Packages installed
install.packages('fastDummies')
install.packages('ggiraphExtra')
library(tidyverse)
library(fastDummies)
library(ggplot2)
library(cowplot)
library(ggiraphExtra)

#Loaded dataset
x<-read.csv('Lung_capacity.csv')
df<-data.frame(x)
df
d<-dummy_cols(df,select_columns = 'Smoke')
d

# Dataset of Non- smoker with their dependent variables
no<-subset(d,Smoke_no>0,select= c('Age','LungCap','Height'))
S_no <-data.frame(no)

# Summary of Non smokers
summary(S_no)

# Dataset of Smokers with their dependent variables
yes<-subset(d,Smoke_yes>0,select =c('Age','LungCap','Height'))
S_yes<-data.frame(yes)

# Summary
summary(S_yes)

# Count of Lungcapacity and height for smokers and non smokers
CountLungcapn<-S_no$LungCap
CountHeightn<-S_no$Height
CountLungcapy<-S_yes$LungCap
CountHeighty<-S_yes$Height

#Single regression.

plot1<-ggplot(S_no,aes(Age,LungCap,color= CountLungcapn))+geom_point()+geom_smooth(method = 'lm')+xlab('Age')+ylab('Lung Capacity of non Smokers')
plot2<-ggplot(S_no,aes(Age,Height, color= CountHeightn))+geom_point()+geom_smooth(method = 'lm')+xlab('Age')+ylab('Height')

plot_grid(plot1,plot2, align = 'AUTO')


plot3<-ggplot(S_yes,aes(Age,LungCap,color =CountLungcapy))+geom_point()+geom_smooth(method = 'lm')+xlab('Age')+ylab('Lung Capacity of Smokers')
plot4<-ggplot(S_yes,aes(Age,Height,color =CountHeighty))+geom_point()+geom_smooth(method = 'lm')+xlab('Age')+ylab('Height')

plot_grid(plot3,plot4,labels = 'AUTO')


#Multiple regression
t<-lm(Age ~ LungCap+Height,data = S_no)
summary(t)

plot1
ggPredict(t,interactive = TRUE)


t2<-lm(Age~LungCap+Height,data = S_yes)
summary(t2)

plot3
ggPredict(t2,interactive = TRUE)

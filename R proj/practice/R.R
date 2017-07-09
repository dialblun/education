eamRank = c(1,2,3,3,4,4,4,4,5,5)install.packages("ggplot2")
library(ggplot2)
data(diamonds)
summary(diamonds)
names(diamonds)
install.packages('gridExtra') 
library(gridExtra) 

str(diamonds$cut)
levels(diamonds$cut)

?diamonds

ggplot(data=diamonds, aes(x=price))+ geom_histogram()
ggplot(data=diamonds, aes(x=price))+ geom_histogram()+  scale_x_continuous(breaks = seq(0,18823,1000))

p1=ggplot(data=diamonds[diamonds$cut=="Fair",], aes(x=price))+ geom_histogram()
p2=ggplot(data=diamonds[diamonds$cut=="Good",], aes(x=price))+ geom_histogram()
p3=ggplot(data=diamonds[diamonds$cut=="Very Good",], aes(x=price))+ geom_histogram()
p4=ggplot(data=diamonds[diamonds$cut=="Premium",], aes(x=price))+ geom_histogram()
p5=ggplot(data=diamonds[diamonds$cut=="Ideal",], aes(x=price))+ geom_histogram()

grid.arrange(p1,p2,p3,p4,p5, ncol=3)

a=diamonds[diamonds$price==18823,]
a
b=diamonds[diamonds$price==326,]
b
summary(diamonds[diamonds$cut=="Fair",])
summary(diamonds[diamonds$cut=="Good",])
summary(diamonds[diamonds$cut=="Very Good",])
summary(diamonds[diamonds$cut=="Premium",])
summary(diamonds[diamonds$cut=="Ideal",])


qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free_y")
diamonds$cut

qplot(x = price/carat, data = diamonds) + facet_wrap(~cut, scales = "free_y") +
  scale_x_log10() 

ggplot(aes(x=cut, y = price/carat), data = diamonds)+geom_boxplot()

IQR(diamonds[diamonds$color == 'D',]$price)
summary(diamonds[diamonds$color == 'D',]$price)
summary(diamonds[diamonds$color == 'J',]$price)

IQR(diamonds[diamonds$color == 'D',]$price)

IQR(diamonds[diamonds$color == 'J',]$price)


(1860+7695)/2


ggplot(aes(x=carat), data= diamonds)+ geom_freqpoly(binwidth=0.1)+
  scale_x_continuous(breaks = seq(0,5,0.1))

ôûâasd

WHO=read.csv("WHO.csv")
mean(WHO$Over60)
which.min(WHO$Over60)
WHO[183,]
which.max(WHO$LiteracyRate)
WHO[44,]
?tapply
tapply(WHO$ChildMortality, WHO$Region, mean)

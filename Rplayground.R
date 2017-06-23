present<- present %>%
  mutate(total = total %>%
  arrange(desc(total)))

mutate(present, total = total) %>%
  arrange(desc(total))


reddit <- read.csv('reddit.csv')
str(reddit)
?factor
qplot(data=reddit, x=age.range)


getwd()
setwd("C:/Users/DBelyakov/education/udacity/Data Analysis in R/EDA_Course_Materials/lesson3")

pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
pf
names(pf)

qplot(x=dob_day, data = pf)+scale_x_continuous(breaks = 1:31)


ggplot(data = pf, aes(x = dob_day)) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = 1:31) + 
  facet_wrap(~dob_month)


ggplot(data = pf, aes(x = friend_count)) + geom_histogram()


ggplot(aes(x = friend_count), data = pf[!is.na(pf$gender),]) + 
  geom_histogram(binwidth = 25) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))+facet_grid(.~gender)


library("ggplot2")
ggplot(data =pf, aes(x = tenure/365)) + geom_histogram(binwidth = 0.1)


ggplot(data =pf, aes(x=age)) + geom_histogram(binwidth = 1)+scale_x_continuous(breaks=seq(0,120,5))

pf[!is.na(pf$gender),]


install.packages('gridExtra') 
library(gridExtra) 

p1=ggplot(data=pf,aes(x = friend_count) )+ geom_histogram()
p2=ggplot(data=pf,aes(x = friend_count) )+ geom_histogram() + scale_x_log10()
p3=ggplot(data=pf,aes(x = friend_count) )+ geom_histogram() + scale_x_sqrt()
grid.arrange(p1,p2,p3, ncol=1)

ggplot(data=pf[!is.na(pf$gender),], 
       aes(x=www_likes))+ geom_freqpoly(aes(color=gender)) +
  scale_x_continuous())+
      scale_x_log10()

males<-pf[pf$gender=='male',]
males <- males[!is.na(males$www_likes),]
sum(males$www_likes)

fmales<-pf[pf$gender=='female',]
fmales <- fmales[!is.na(fmales$www_likes),]
sum(fmales$www_likes)




p1<-ggplot(data= pf[!is.na(pf$gender),], aes(x=gender,y=friend_count))+ 
  geom_boxplot() +
 scale_y_continuous(limits=c(0,1000))

p2<-ggplot(data= pf[!is.na(pf$gender),], aes(x=gender,y=friend_count))+ 
  geom_boxplot() + ylim(c(0,1000))
       
grid.arrange(p1,p2)


by(pf$friend_count, pf$gender, summary)


pf$mobile_check_in <- NA
names(pf)
pf$mobile_check_in<-ifelse(pf$mobile_likes>0,1,0)
sum(pf$mobile_check_in)/length(pf$mobile_likes)




data(diamonds)
summary(diamonds)
diamonds$color
names(diamonds)
str(diamonds)


ggplot(data=diamonds, aes(x=price))+ geom_histogram(binwidth = 500)
summary(diamonds$price)
length(diamonds$price[diamonds$price>=15000])
diamonds[diamonds$price>250,]





setwd("C:/Users/DBelyakov/education/udacity/Data Analysis in R")
birthdays<-read.csv("birthdaysExample.csv")
names(birthdays)
str(birthdays)
install.packages("lubridate")
library(lubridate)
birthdays$dates_parsed <- parse_date_time(birthdays$dates, orders = "m/d/y")

birthdays[birthdays$dates_parsed == as.Date("2014/07/22"),]
library("scales")
ggplot(data = birthdays, aes(x=birthdays$dates_parsed)) + geom_histogram(bins=12)+scale_x_datetime(breaks = date_breaks("1 months"))
ggplot(data = birthdays, aes(x=yday(birthdays$dates_parsed))) + geom_histogram(bins=365)

ggplot()

summary(yday(birthdays$dates_parsed))

?count
d<-table(yday(birthdays$dates_parsed))
sort(d)

as.Date("2016-01-01")+37-1
yday(as.Date("2016-02-06"))
??wich

Sys.setlocale("LC_ALL", "C")


ggplot(aes(x=age, y=friend_count), data =pf[pf$friend_count > 0,]) + geom_jitter(alpha=1/20)+xlim(13,90)+coord_trans(y="sqrt")

ggplot(aes(x=age, y=friendships_initiated), data =pf) + geom_jitter(alpha=1/20)
ggplot(aes(x=age, y=friendships_initiated), data =pf) + geom_point(alpha=1/20,position = "jitter")


install.packages("dplyr")
library("Lahman")
library("dplyr")

games <- ddply(Batting, "playerID", summarise, total = sum(G))
head(arrange(games, desc(total)), 5)
Batting



str(age_groups)

age_groups =group_by(pf, age)
pf.fc_by_age = summarise(age_groups, frined_count_mean = mean (friend_count),
          friend_count_median= median(friend_count),
          n=n())

pf.fc_by_age
arrange(pf.fc_by_age)

ggplot(aes(x=age, y= frined_count_mean), data=pf.fc_by_age) +geom_line()


ggplot(aes(x=age, y=friend_count), data =pf) +
  xlim(13,90) +
  geom_point(alpha=1/20,
             position = position_jitter(h=0),
             color = "orange") +
  coord_trans(y="sqrt") +
  geom_line(stat = "summary", fun.y = mean)



cor.test(pf$age, pf$friend_count)


ggplot(aes(x=www_likes_received, y = likes_received ), data = pf) + geom_point(alpha=1/10)

1700/60

cor.test(pf$likes_received,pf$www_likes_received )

8
install.packages('alr3')
library(alr3)
data("Mitchell")
str(Mitchell)
ggplot(aes(y=Temp, x= Month),data=Mitchell)+ geom_point()+
  scale_x_continuous(breaks = seq(0,200,12))

cor.test(Mitchell$Temp,Mitchell$Month)

ggplot(aes(y=Temp, x= Month),data=Mitchell)+ geom_line()

ggplot(aes(x=(Month%%12),y=Temp),data=Mitchell)+ 
  geom_point() 

install.packages("energy")
library(energy)
dcor.ttest() 

x <- seq(0, 4*pi, pi/20)
y <- cos(x)
qplot(x = x, y = y)
dcor.ttest(x, y)

str(pf)



pf$age_with_month = pf$age + (12-pf$dob_month)/12
pf[23,]
12/pf$dob_month+ pf$age
9/12

a=group_by(pf,age_with_month)

pf.fc_by_age_months = summarise(a,
    friend_count_mean = mean(friend_count),
    friend_count_median = median(friend_count),
    n=n()
) 

pf.fc_by_age_months=arrange(pf.fc_by_age_months, age_with_month)
head(pf.fc_by_age_months)



age_groups <- group_by(pf, age) 
pf.fc_by_age <- summarise(age_groups, 
                          friend_count_mean = mean(friend_count), 
                          friend_count_median = median(friend_count), 
                          n = n()) 
pf.fc_by_age <- arrange(pf.fc_by_age, age) 

head(pf.fc_by_age)



USDA=read.csv("USDA.csv")
USDA
str(USDA)
USDA_Iron=arrange(USDA$, desc(Iron))
?arrange
head(USDA_Iron)
USDA_Iron=USDA_Iron[,c("Description","Iron")]


mvt=read.csv("mvtWeek1.csv")
str(mvt)
max(mvt$ID)
min(mvt$Beat)
str(mvt[mvt$Arrest==TRUE,])
summary(mvt)
mvt$Date[1]
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(mvt$Month)
table(mvt$Weekday)
table(mvt[mvt$Arrest==TRUE,]$Month)
table(mvt$Arrest, mvt$Month)
hist(mvt$Date, breaks=100)
ggplot(aes(x=Date), data=mvt)+geom_histogram(breaks=100)
ggplot(aes(y=Date, x=Arrest), data=mvt)+geom_boxplot()
boxplot(mvt$Date ~ mvt$Arrest)



table(mvt$Arrest, mvt$Year)
550/(550+13542)
sort(table(mvt$LocationDescription))
Top5=subset(mvt, LocationDescription == "STREET" | LocationDescription== "PARKING LOT/GARAGE(NON.RESID.)" |  
              LocationDescription== "ALLEY"  |  LocationDescription== "GAS STATION"  |   
              LocationDescription== "DRIVEWAY - RESIDENTIAL"  )


str(Top5)
table(Top5$Arrest,Top5$LocationDescription) 
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription, Top5$Weekday)


IBM = read.csv("IBMStock.csv")
GE= read.csv("GEStock.csv")
ProcterGamble= read.csv("ProcterGambleStock.csv")
CocaCola= read.csv("CocaColaStock.csv") 
Boeing= read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)
sd(ProcterGamble$StockPrice)
ggplot() + geom_line(data=CocaCola, aes(x=Date, y=StockPrice)) +
  geom_line(data=ProcterGamble, aes(x=Date, y=StockPrice), color='Red') + 
  geom_vline(xintercept = as.numeric(as.Date("2000-04-01") )) 

ggplot() + geom_line(data=CocaCola[301:432,], aes(x=Date, y=StockPrice), color = "red")+
  geom_line(data=IBM[301:432,], aes(x=Date, y=StockPrice), color = "orange")+
  geom_line(data=GE[301:432,], aes(x=Date, y=StockPrice), color = "black")+
  geom_line(data=ProcterGamble[301:432,], aes(x=Date, y=StockPrice), color = "green")+
  geom_line(data=Boeing[301:432,], aes(x=Date, y=StockPrice), color = "blue") +
  geom_vline(xintercept=as.numeric(as.Date("1997-09-01"))) + 
  geom_vline(xintercept=as.numeric(as.Date("1997-11-01"))) +              
  ylim(c(0,210))


months(IBM$Date)



colors()

?tapply
summary(IBM$StockPrice)
tapply(IBM$StockPrice,months(IBM$Date), mean)
tapply(CocaCola$StockPrice,months(CocaCola$Date), mean)
tapply(GE$StockPrice,months(GE$Date), mean)
tapply(ProcterGamble$StockPrice,months(ProcterGamble$Date), mean)
tapply(Boeing$StockPrice,months(Boeing$Date), mean)



CPS=read.csv("CPSData.csv")
str(CPS)
table(CPS$Industry)
sort(table(CPS$State))
names(CPS)
table(CPS$Citizenship)


levels(CPS$Citizenship)
(116639   +              7073)/(116639   +              7073 + 7590)
table(CPS$Race, CPS$Hispanic)


table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))


table(CPS$State, is.na(CPS$MetroAreaCode))
table(CPS$Region, is.na(CPS$MetroAreaCode))
tapply(is.na(CPS$MetroAreaCode), CPS$Region, mean)
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))


MetroAreaCode = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")

str(MetroAreaCode)
str(CountryMap)

CPS = merge(CPS, MetroAreaCode, by.x= "MetroAreaCode", by.y = "Code", all.x = TRUE)
names(CPS)
sort(summary(CPS$MetroArea))
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

sort(tapply(CPS$Race == "Asian" , CPS$MetroArea, mean))
table(CPS$Race == "Asian" , CPS$MetroArea)

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = TRUE))
CPS = merge(CPS, CountryMap, by.x= "CountryOfBirthCode", by.y = "Code", all.x = TRUE)
str(CPS)
summary(CPS$Country)


sort(table(CPS$Country))

table(CPS[CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA",]$MetroArea,CPS$Country )
     
levels(CPS$MetroArea)
head(CPS[CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA",]$MetroArea)



table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")
table(CPS$MetroArea , CPS$Country == "Somalia")

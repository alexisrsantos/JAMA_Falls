options(scipen = 10000) 

library(readxl)
data <- read_excel("/Data/Processed/Book7.xlsx")

data$ID<-data$Year-1998

model1<-lm(log(Overall)~ID,data=data)
summary(lm(log(Overall)~ID,data=data))
confint(model1,level=0.95)

#By Sex
model2<-(lm(log(Male)~ID,data=data))
summary(lm(log(Male)~ID,data=data))
confint(model2,level=0.95)


model3<-(lm(log(Female)~ID,data=data))
summary(lm(log(Female)~ID,data=data))
confint(model3,level=0.95)

#By Race/Ethnicity
model4<-(lm(log(AIAN)~ID,data=data))
summary(lm(log(AIAN)~ID,data=data))
confint(model4,level=0.95)

model5<-(lm(log(API)~ID,data=data))
summary(lm(log(API)~ID,data=data))
confint(model5,level=0.95)

model6<-(lm(log(Black)~ID,data=data))
summary(lm(log(Black)~ID,data=data))
confint(model6,level=0.95)

model7<-(lm(log(Hispanic)~ID,data=data))
summary(lm(log(Hispanic)~ID,data=data))
confint(model7,level=0.95)

model7<-(lm(log(White)~ID,data=data))
summary(lm(log(White)~ID,data=data))
confint(model7,level=0.95)


summary(lm(log(White)~ID,data=data))

library(ggplot2)

sex <- read_excel("/Data/Processed/Sex.xlsx")

a<-ggplot(data=sex, aes(x=Year, y=Rate, color=Sex)) +
  geom_line()+theme_bw()+
  geom_point(size=1.5)+
  labs(title ="Death rates for falls among adults 65 years and older and by sex, 1999-2020",
       x = "", y= "AAMR per 100,000 persons")+
  scale_x_continuous(breaks = seq(from = 1998, to = 2020, by = 2))+
  theme(legend.title = element_blank(),legend.position ="bottom",axis.title.x = element_blank())+
  scale_y_continuous(limits = c(0,90),breaks = seq(from = 0, to = 90, by = 15))

a

race <- read_excel("C:/Users/ars39/Documents/Falls/R an R/Data/Processed/Race_Ethnicity.xlsx")

b<-ggplot(data=race, aes(x=Year, y=AAMR, color=Race)) +
  geom_line()+theme_bw()+
  geom_point(size=1.5)+
  labs(title ="Death rates for falls among adults 65 and older by race/ethnicity, 1999-2020",
       x = "Year ", y= "AAMR per 100,000 persons")+
  scale_x_continuous(breaks = seq(from = 1998, to = 2020, by = 2))+
  theme(legend.title = element_blank(),legend.position ="bottom")+
  scale_y_continuous(limits = c(0,90),breaks = seq(from = 0, to = 90, by = 15))

b

library(ggpubr)
ggarrange(a,b,labels=c("A","B"),ncol=1,common.legend = FALSE)

#MULTI<-ggarrange(a,b,labels=c("A","B"),ncol=1,common.legend = FALSE)

#library(rasterpdf)

ggsave("Fig.pdf", width = 20, height = 20, units = "cm")
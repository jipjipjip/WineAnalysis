library("ggplot2")
library ("dplyr")
library("gridExtra")
library(GGally)
library("patchwork")
library('corrplot')
library('tidyr')


#correlation table 

correlation <- dataset %>% 
  select (-c(X, rating)) %>%
  mutate(quality = as.numeric(quality))

round(cor(correlation), digits =4)

ggcorr(correlation, label = TRUE, label_round = 4,label_size = 2.5, hjust = 0.8, size = 3, layout.exp=2) + ggtitle("Correlation between variables") + 
  theme(plot.title = element_text(hjust = 0.5))

#highest positive correlations: fixed acid w/citric acid; free w/total sulfur; alcohol w/quality
#highest negative correlations: PH w/fixed acid; PH w/citric; volatile w/citric; density w/alcohol 

#Wine Quality
#Distribution of red wine quality ratings

ggplot(dataset, aes(x = quality)) + geom_bar(width = 0.75, color = "black", fill = I('#99CCFF')) + ggtitle("Quality of Wine") + 
theme(plot.title = element_text(hjust = 0.5))


#Distribution of good/bad red wines
#1. create a factor to define good wines (quality >6)

dataset$good.wine<-ifelse(dataset$quality>6,1,0) 
summary(dataset)

ggplot(dataset, aes(x =good.wine)) + geom_bar(width = 0.75, color = "black", fill = I('#FF99CC')) + ggtitle("Distribution of Good/Bad Wines") + 
  scale_x_continuous(breaks = seq(0,1,1)) + theme(plot.title = element_text(hjust = 0.5))

#Physiochemical Properties and Wine Quality
#Look at the relationship between the physiochemical properties and whether a wine is good or not.
# Fixed Acidity and Wine Quality

ggplot(dataset, aes(fixed.acidity, fill=factor(good.wine)))+ 
  geom_density(alpha = 0.25) + 
  geom_vline(aes(xintercept=mean(fixed.acidity[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1) + 
  geom_vline(aes(xintercept=mean(fixed.acidity[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1) + 
  scale_x_continuous(breaks = seq(4,16,1))+
  xlab(label = "Fixed Acidity Level")+
  ggtitle("Distribution of Fixed Acidity Levels") + 
  theme_classic()

#Volatile Acidity and Wine Quality

ggplot(dataset, aes(volatile.acidity, fill = factor(good.wine)))+
  geom_density(alpha = 0.25)+
  geom_vline(aes(xintercept = mean(volatile.acidity[good.wine==0],na.rm=T)), color = "red", linetype = "dashed",lwd=1)+
  geom_vline(aes(xintercept = mean(volatile.acidity[good.wine==1],na.rm=T)), color = "blue", linetype = "dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,1.6,0.1))+
  xlab(label = "Volatile Acidity Levels")+
  ggtitle("Distribution of Volatile Acidity Levels") + 
  theme_classic()
 
#Citric Acid and Wine Quality
ggplot(dataset, aes(citric.acid, fill = factor(good.wine)))+
  geom_density(alpha = 0.25)+
  geom_vline(aes(xintercept = mean(citric.acid[good.wine==0],na.rm=T)), color = "red", linetype = "dashed",lwd=1)+
  geom_vline(aes(xintercept = mean(citric.acid[good.wine==1],na.rm=T)), color = "blue", linetype = "dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,1.1,0.1))+
  xlab(label = "Citric Acidity Levels")+
  ggtitle("Distribution of Citric Acidity Levels") + 
  theme_classic()

#Residual Sugar and Wine Quality
ggplot(dataset, aes(residual.sugar, fill = factor(good.wine)))+
  geom_density(alpha = 0.25)+
  geom_vline(aes(xintercept = mean(residual.sugar[good.wine==0],na.rm=T)), color = "red", linetype = "dashed",lwd=1)+
  geom_vline(aes(xintercept = mean(residual.sugar[good.wine==1],na.rm=T)), color = "blue", linetype = "dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,16,1))+
  xlab(label = "Residual Sugar Levels")+
  ggtitle("Distribution of Residual Sugar Levels") + 
  theme_classic()                

#Chlorides and Wine Quality
ggplot(dataset,aes(x=chlorides,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(chlorides[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(chlorides[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0.01,0.62,0.1))+
  xlab(label = "Chlorides Level")+
  ggtitle("Distribution of Chlorides Levels")+
  theme_classic()

#Free Sulfur Dioxide and Wine Quality
ggplot(dataset,aes(x=free.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,72,8))+
  xlab(label = "Free Sulfur Dioxide Level")+
  ggtitle("Distribution of Free Sulfur Dioxide Levels")+
  theme_classic()

#Total Sulfur Dioxide and Wine Quality
ggplot(dataset,aes(x=total.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,300,20))+
  xlab(label = "Total Sulfur Dioxide Level")+
  ggtitle("Distribution of Total Sulfur Dioxide Levels")+
  theme_classic()

#Density and Wine Quality
ggplot(dataset,aes(x=density,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(density[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(density[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0.9,1.1,0.05))+
  xlab(label = "Red Wine Density Level")+
  ggtitle("Distribution of Red Wine Density Levels")+
  theme_classic()

#PH and Wine Quality
ggplot(dataset,aes(x=pH,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(pH[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(pH[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(2.5,5,0.5))+
  xlab(label = "Red Wine PH Level")+
  ggtitle("Distribution of Red Wine PH Levels")+
  theme_classic()

#Box plots for variables and quality
#fixed acidity and quality

ggplot(dataset, aes(x = quality, y = fixed.acidity))+
  geom_jitter(alpha = 0.3)+
  geom_boxplot(alpha=0.5, colour = "#3300FF")+
  stat_summary(fun="mean", geom = "point", colour = "red" ,shape = 8, size = 4)+
  ggtitle("Boxplot of Fixed Acidity against Quality")+
  theme(plot.title = element_text(hjust = 0.5))

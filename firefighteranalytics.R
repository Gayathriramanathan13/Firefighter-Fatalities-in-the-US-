library(tidyr)
library(ggplot2)
library(googleVis)
rm(list=ls(all=TRUE))
firefighterdata = read.csv('database.csv', sep = ',',header = TRUE)
str(firefighterdata)
firefighterdata$Year = substr(as.character(firefighterdata$Date.of.Death), 8,12)
f1<-as.data.frame((table(firefighterdata$Year)))
colnames(f1)<-c("Year","Number")
ggplot(data=f1, aes(x=Year, y=Number, group=1)) + 
  geom_line(colour="red", size=1.5) + 
  geom_point(colour="red", size=2, shape=21, fill="red")
f2<-as.data.frame((table(firefighterdata$Cause.Of.Death)))
colnames(f2)<-c("CauseofDeath","Number")
f3<-as.data.frame((table(firefighterdata$Cause.Of.Death,firefighterdata$Emergency)))
colnames(f3)<-c("CauseofDeath","Emergency","Number")
fd.sub <- subset(firefighterdata, Year=='2001')
fd.f4<-as.data.frame((table(fd.sub$Cause.Of.Death)))
colnames(fd.f4)<-c("CauseofDeath","Number")
f5<-as.data.frame((table(firefighterdata$Age)))
colnames(f5)<-c("Age","Number")
ggplot(data=f2, 
            aes(x=factor(`CauseofDeath`), y=Number, 
                fill = factor(`CauseofDeath`))) + 
  geom_bar(stat = "identity") + xlab("Cause of Death of Firefighters") + 
  theme(legend.position="bottom" 
        ,plot.title = element_text(size=15, face="bold")) + 
  labs(fill = "CauseofDeath")
ggplot(data = f3, 
       aes(x=(`CauseofDeath`), y=Number, fill=factor(Emergency, levels = c("No","Yes")))) + 
  geom_bar(position = "dodge", stat = "identity") + ylab("Number of Deaths") + 
  xlab("Cause of Death") + theme(legend.position="bottom" 
                             ,plot.title = element_text(size=15, face="bold")) + 
  ggtitle("Cause of Deaths by Frequency and Emergency Classification") + labs(fill = "Status")
ggplot(data=fd.f4, 
       aes(x=factor(`CauseofDeath`), y=Number, 
           fill = factor(`CauseofDeath`))) + 
  geom_bar(stat = "identity") + xlab("Cause of Death of Firefighters in 2001") + 
  theme(legend.position="bottom" 
        ,plot.title = element_text(size=15, face="bold")) + 
  labs(fill = "CauseofDeath")
ggplot(data=f5, 
       aes(x=Age, y=Number, 
           fill = factor(`Age`))) + 
  geom_bar(stat = "identity") + xlab("Age  of Firefighters") + 
  theme(legend.position="bottom" 
        ,plot.title = element_text(size=15, face="bold")) + 
  labs(fill = "Age")
  
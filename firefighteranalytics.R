library(tidyr)
library(ggplot2)
library(googleVis)

#Clears the memory of the current r session
rm(list=ls(all=TRUE))

#reads the dataset
firefighterdata = read.csv('database.csv', sep = ',',header = TRUE)

#see the structure of the data
str(firefighterdata)

#Create an additional column in the dataset to add the year of death based on Date of Death
firefighterdata$Year = substr(as.character(firefighterdata$Date.of.Death), 8,12)

#Create a dataframe showing the number of deaths in each year
f1<-as.data.frame((table(firefighterdata$Year)))
colnames(f1)<-c("Year","Number")

#Creates a line graph of the number of deaths by year
ggplot(data=f1, aes(x=Year, y=Number, group=1)) + 
  geom_line(colour="red", size=1.5) + 
  geom_point(colour="red", size=2, shape=21, fill="red")
  
 #Creates a dataframe of the various cause of deaths and the number of firefighters who died this way 
f2<-as.data.frame((table(firefighterdata$Cause.Of.Death)))
colnames(f2)<-c("CauseofDeath","Number")

#Creates a dataframe indicating cause of death, whether it was classified an emergency, and the number of deaths
f3<-as.data.frame((table(firefighterdata$Cause.Of.Death,firefighterdata$Emergency)))
colnames(f3)<-c("CauseofDeath","Emergency","Number")

#Subsets the data to analyze only those deaths in the year 2001 as we can see from the line graph this was
the year with the highest number of firefighter fatalities possibly explained as an effect of 911
fd.sub <- subset(firefighterdata, Year=='2001')

#Creates a dataframe to analyze the causes of death and number of deaths per cause for the year 2001
fd.f4<-as.data.frame((table(fd.sub$Cause.Of.Death)))
colnames(fd.f4)<-c("CauseofDeath","Number")

#Creates a dataframe of the various ages of the firefighters and the number that died in each age
f5<-as.data.frame((table(firefighterdata$Age)))
colnames(f5)<-c("Age","Number")

#Creates a bar plot of the cause of death vs number of deaths of each type
ggplot(data=f2, 
            aes(x=factor(`CauseofDeath`), y=Number, 
                fill = factor(`CauseofDeath`))) + 
  geom_bar(stat = "identity") + xlab("Cause of Death of Firefighters") + 
  theme(legend.position="bottom" 
        ,plot.title = element_text(size=15, face="bold")) + 
  labs(fill = "CauseofDeath")
  
 #Creates a bar plot that shows emergency classification along with the cause of death and number information 
ggplot(data = f3, 
       aes(x=(`CauseofDeath`), y=Number, fill=factor(Emergency, levels = c("No","Yes")))) + 
  geom_bar(position = "dodge", stat = "identity") + ylab("Number of Deaths") + 
  xlab("Cause of Death") + theme(legend.position="bottom" 
                             ,plot.title = element_text(size=15, face="bold")) + 
  ggtitle("Cause of Deaths by Frequency and Emergency Classification") + labs(fill = "Status")
  
#Creates a bar plot of cause of death of firefighters vs number in the year 2001  
ggplot(data=fd.f4, 
       aes(x=factor(`CauseofDeath`), y=Number, 
           fill = factor(`CauseofDeath`))) + 
  geom_bar(stat = "identity") + xlab("Cause of Death of Firefighters in 2001") + 
  theme(legend.position="bottom" 
        ,plot.title = element_text(size=15, face="bold")) + 
  labs(fill = "CauseofDeath")
  
 #Creates a barplot of the number of deaths by age  
ggplot(data=f5, 
       aes(x=Age, y=Number, 
           fill = factor(`Age`))) + 
  geom_bar(stat = "identity") + xlab("Age  of Firefighters") + 
  theme(legend.position="bottom" 
        ,plot.title = element_text(size=15, face="bold")) + 
  labs(fill = "Age")

library(dplyr)
##Import Data

library(readr)
athlete_events <- read_csv("~/Downloads/archive/athlete_events.csv")


###Medal rename
athlete_events$Medcod<-ifelse(is.na(athlete_events$Medal),0,1)#If any medal
athlete_events$Gold<-ifelse(athlete_events$Medal=="Gold",1,0)#If any medal
athlete_events$Silver<-ifelse(athlete_events$Medal=="Silver",1,0)#If any medal
athlete_events$Bronze<-ifelse(athlete_events$Medal=="Bronze",1,0)#If any medal

# Visualising Age ---------------------------------------------------------
#age of Winners Vs Age of non-Winners
mean(athlete_events$Age[athlete_events$Medcod==1],na.rm = TRUE)
mean(athlete_events$Age[athlete_events$Medcod==0],na.rm = TRUE)
mean(athlete_events$Age,na.rm = TRUE)

athmed<-filter(athlete_events,athlete_events$Medcod==1)
athgold<-filter(athlete_events,athlete_events$Gold==1)
athbronze<-filter(athlete_events,athlete_events$Bronze==1)
athsilver<-filter(athlete_events,athlete_events$Silver==1)
athnonmed<-filter(athlete_events,athlete_events$Medcod==0)

meanage_pooled <-summarize(group_by(athlete_events, Year), meanage=mean(Age,na.rm=TRUE))
meanage_nonmedal <-summarize(group_by(athnonmed, Year), meanage=mean(Age,na.rm=TRUE))
meanage_silver <-summarize(group_by(athsilver, Year), meanage=mean(Age,na.rm=TRUE))
meanage_bronze<-summarize(group_by(athbronze, Year), meanage=mean(Age,na.rm=TRUE))
meanage_medal<-summarize(group_by(athmed, Year), meanage=mean(Age,na.rm=TRUE))
meanage_gold<-summarize(group_by(athgold, Year), meanage=mean(Age,na.rm=TRUE))

meanage_bronze$Type<-"Bronze"
meanage_pooled$Type<-"Pooled"
meanage_nonmedal$Type<-"NonMedal"
meanage_medal$Type<-"Medal"
meanage_gold$Type<-"Gold"
meanage_silver$Type<-"Silver"

ageoveryears<-rbind(meanage_bronze,meanage_gold)
ageoveryears<-rbind(ageoveryears,meanage_medal)
ageoveryears<-rbind(ageoveryears,meanage_nonmedal)
ageoveryears<-rbind(ageoveryears,meanage_silver)

library(ggplot2)
#Combined Plot
a<-ggplot(ageoveryears, aes(x=Year,y= meanage))
a+geom_line(aes(colour=Type))

###Plot of Gold Vs Non Medal
ageoveryears4<-filter(ageoveryears,Type=="Gold"|Type=="NonMedal")
d<-ggplot(ageoveryears4, aes(x=Year,y= meanage))
d+geom_line(aes(colour=Type)) #Definite Difference between Gold and non-Medals

###Plot of Medal Vs Non Medal
ageoveryears2<-filter(ageoveryears,Type=="Medal"|Type=="NonMedal")
b<-ggplot(ageoveryears2, aes(x=Year,y= meanage))
b+geom_line(aes(colour=ype)) #Now olders are winning medals


###Plot of Gold Vs Bronze
ageoveryears3<-filter(ageoveryears,Type=="Gold"|Type=="Bronze")
c<-ggplot(ageoveryears3, aes(x=Year,y= meanage))
c+geom_line(aes(colour=Type)) #No Difference

# Correlation ----------------------------------------------------

print(cor.test(athlete_events$Medcod,athlete_events$Age, use = "complete.obs")$estimate)
print(cor.test(athlete_events$Medcod,athlete_events$Age, use = "complete.obs")$p.value)

# Regression --------------------------------------------------------------
###Pooled

model<-(Medcod~Age+Height+Weight+as.factor(Sex))
reg.data<-try(glm(model,data=athlete_events, na.action = na.exclude ),silent=TRUE)
summary(reg.data)


##Controlling for sport

impact3<-data.frame()
try(
  for (i in 1:length(athlete_events$Sport)){
    ath<-filter(athlete_events,athlete_events$Sport==unique(athlete_events$Sport)[i])
    model<-(Medcod~Age+Height+Weight)
    reg.data<-try(glm(model,data=ath, na.action = na.exclude ),silent=TRUE)
    summary(reg.data)
    impact3[i,1]<-unique(athlete_events$Sport)[i]
    impact3[i,2]<-summary(reg.data)$coefficients[,1][2]
    impact3[i,3]<-summary(reg.data)$coefficients[,4][2]
    impact3[i,4]<-summary(reg.data)$coefficients[,1][3]
    impact3[i,5]<-summary(reg.data)$coefficients[,4][2]
    impact3[i,6]<-summary(reg.data)$coefficients[,1][4]
    impact3[i,7]<-summary(reg.data)$coefficients[,4][2]
  },silent = TRUE)
colnames(impact3)<-c("Sport","Age","AgeP","Height","heightP","Weight","WeightP")

impact3age<-filter(impact3,impact3$AgeP<0.05)
impact3age<-select(impact3age,"Sport","Age")

###Controlling for year
impact<-data.frame()
try(
for (i in 1:length(athlete_events$Sport)){
  ath<-filter(athlete_events,athlete_events$Sport==unique(athlete_events$Sport)[i])
model<-(Medcod~Age+Height+Weight+as.factor(Year))
reg.data<-try(lm(model,data=ath, na.action = na.exclude ),silent=TRUE)
summary(reg.data)
impact[i,1]<-unique(athlete_events$Sport)[i]
impact[i,2]<-summary(reg.data)$coefficients[,1][2]
impact[i,3]<-summary(reg.data)$coefficients[,4][2]
impact[i,4]<-summary(reg.data)$coefficients[,1][3]
impact[i,5]<-summary(reg.data)$coefficients[,4][2]
impact[i,6]<-summary(reg.data)$coefficients[,1][4]
impact[i,7]<-summary(reg.data)$coefficients[,4][2]
},silent = TRUE)
colnames(impact)<-c("Sport","Age","AgeP","Height","heightP","Weight","WeightP")

impactage<-filter(impact,impact$AgeP<0.05)
impactage<-select(impactage,"Sport","Age")

###Controlling for Event
impact2<-data.frame()
try(
  for (i in 1:length(athlete_events$Event)){
    ath<-filter(athlete_events,athlete_events$Event==unique(athlete_events$Event)[i])
    model<-(Medcod~Age+Height+Weight+as.factor(Year))
    reg.data<-try(lm(model,data=ath, na.action = na.exclude ),silent=TRUE)
    summary(reg.data)
    impact2[i,1]<-unique(athlete_events$Event)[i]
    impact2[i,2]<-summary(reg.data)$coefficients[,1][2]
    impact2[i,3]<-summary(reg.data)$coefficients[,4][2]
    impact2[i,4]<-summary(reg.data)$coefficients[,1][3]
    impact2[i,5]<-summary(reg.data)$coefficients[,4][2]
    impact2[i,6]<-summary(reg.data)$coefficients[,1][4]
    impact2[i,7]<-summary(reg.data)$coefficients[,4][2]
  },silent = TRUE)
colnames(impact2)<-c("Event","Age","AgeP","Height","heightP","Weight","WeightP")

impact2age<-filter(impact2,impact2$AgeP<0.05)
impact2age<-select(impact2age,"Sport","Age")


##Working with Gold
model<-(Gold~Age+Height+Weight+as.factor(Sex))
reg.data<-try(lm(model,data=athlete_events, na.action = na.exclude ),silent=TRUE)
summary(reg.data)

# TODO---------------------------------------------------------
#Same person who won later.

#Get Medal count #Get Name instances #Get unique years length #Get a number on the appearances
#summarise, group by name, year, sum medal 
athsametry<-athlete_events%>%group_by(ID)%>%summarise(Appearances=length(Medcod), WhetherMedal=sum(Medcod),DiffOlympics=length(unique(Year))) #This works
athsame<-filter(athsametry,DiffOlympics>1,WhetherMedal>0) #only useful ones
athsame<-merge(athsame, athlete_events,by="ID",all.x = TRUE,all.y = FALSE)#We have a full sheet with only double olympians with medals

datalist = list()
for (i in 1:length(unique(athsame$ID))){
 # for (i in 1:5){
       numbridge<-data.frame()
  athnum<-filter(athsame,ID==unique(athsame$ID)[i])
  athnum$IDYear<-paste(athnum$ID,athnum$Year)
  for (j in 1:length(unique(athnum$Year))){
    numbridge[j,1]<-unique(sort(athnum$Year))[j]
    numbridge[j,2]<-j
colnames(numbridge)<-c("Year","Count")

  }
  athnum<-merge(athnum,numbridge,by="Year",all.x=TRUE,all.y=TRUE)
 datalist[[i]]<-athnum
}
athmulti = do.call(rbind, datalist)

#Get mean of medal in first outing versus second versus third
athmultimedalpooled<-athmulti%>%group_by(Count)%>%summarise(AvgMedals=mean(Medcod)) #This is overall, showing no fun
m1<-ggplot(athmultimedalpooled, aes(x=Count,y= AvgMedals))
m1<-m1+geom_bar(stat = "identity")+xlab("Appearance")+ylab("Average Number of Medals") #Now olders are winning medals
print(m1)

#Get it for after 1950 also
athmultimedallater<-athmulti%>%filter(Year>1950)%>%group_by(Count)%>%summarise(AvgMedals=mean(Medcod)) #This is overall, showing no fun
m2<-ggplot(athmultimedallater, aes(x=Count,y= AvgMedals))
m2<-m2+geom_bar(stat = "identity")+xlab("Appearance")+ylab("Average Number of Medals") #Now olders are winning medals
print(m2)

#Get it for 1980
athmultimedalrecent<-athmulti%>%filter(Year>1980)%>%group_by(Count)%>%summarise(AvgMedals=mean(Medcod)) #This is overall, showing no fun
m3<-ggplot(athmultimedalrecent, aes(x=Count,y= AvgMedals))
m3<-m3+geom_bar(stat = "identity")+xlab("Appearance")+ylab("Average Number of Medals") #Now olders are winning medals
print(m3)

#First Gold
athmultigoldpooled<-athmulti%>%group_by(Count)%>%summarise(AvgMedals=mean(Gold,na.rm=TRUE)) #This is overall, showing no fun
g1<-ggplot(athmultigoldpooled, aes(x=Count,y= AvgMedals))
g1<-g1+geom_bar(stat = "identity")+xlab("Appearance")+ylab("Average Number of Medals") #Now olders are winning medals
print(g1)

#Get it for after 1950 also
athmultigoldlater<-athmulti%>%filter(Year>1950)%>%group_by(Count)%>%summarise(AvgMedals=mean(Gold,na.rm=TRUE)) #This is overall, showing no fun
g2<-ggplot(athmultigoldlater, aes(x=Count,y= AvgMedals))
g2<-g2+geom_bar(stat = "identity")+xlab("Appearance")+ylab("Average Number of Medals") #Now olders are winning medals
print(g2)

#After 1980
athmultigoldrecent<-athmulti%>%filter(Year>1980)%>%group_by(Count)%>%summarise(AvgMedals=mean(Gold,na.rm=TRUE)) #This is overall, showing no fun
g3<-ggplot(athmultigoldrecent, aes(x=Count,y= AvgMedals))
g3<-g3+geom_bar(stat = "identity")+xlab("Appearance")+ylab("Average Number of Medals") #Now olders are winning medals
print(g3)





# comment import data from xlsx
library(here)
library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)
dx<-NULL
df<-NULL

demog<-openxlsx::read.xlsx("data/DemographicsData.xlsx", sheet=1, skipEmptyRows=FALSE)

for(i in 1:24){
  dx<-openxlsx::read.xlsx("data/rawDataDrone2.xlsx", sheet=i, skipEmptyRows=FALSE)
  df<-rbind(df,dx)
  }
colnames(df)[colnames(df)=="#"] <- "droneNum"
df<-df[!df$Speed==2,]
df$TimeInSession<-df$Time
df$TimeInSession<-gsub("[+]","",df$TimeInSession)
df$TimeInSession<-as.numeric(df$TimeInSession)
df$minutesIntoSession<-

#  analysis of angular movement
#mean participant height mPH
mPH<-1.7

#gradient angle and calculation for 
ga1<-atan2(13.75,7.48-mPH)*180/pi
ga2<-atan2(13.24,6.86-mPH)*180/pi
ga2-ga1
ga3<-atan2(13.75,7.48-mPH)*180/pi
ga4<-atan2(12.25,6.86-mPH)*180/pi
ga4-ga3

dfc<-df[!is.na(df$Correct.drone) & df$Correct.drone==TRUE ,]
mean(as.numeric(dfc$Highlight.distance),na.rm=TRUE)
mean(as.numeric(dfc$Altitude),na.rm=TRUE)



df$pprev<-c(0, df[1:nrow(df)-1,]$Participant)
df$pSpeed<-c(0.5, df[1:nrow(df)-1,]$Speed)
df$newPerson<-ifelse(df$Participant==df$pprev,0,1)
df$newSpeed<-ifelse(df$Speed==df$pSpeed,0,1)
df$newTask
df$nPID<-cumsum(df$newPerson)
participants<-openxlsx::read.xlsx("data/EverythingNumAndSurveysFinal.xlsx", sheet=27, skipEmptyRows=FALSE)
participants$age<-as.numeric(participants$age)


# df[df$Correct.drone==TRUE,]$outcome<-'TP'
df$outcome<-ifelse(is.na(df$Correct.drone),"FN", ifelse(df$Correct.drone==FALSE,"FP","TP"))
ppm<-sqldf::sqldf('select participant as PID, speed, outcome, count(participant) as NumsOf from df group by participant, speed,outcome')

ppmx<-sqldf::sqldf('select participant, speed, count(participant) as NumsOf from df group by participant, speed')
df<-merge(df,ppmx)

ppmy<-sqldf::sqldf('select participant, speed, outcome, count(participant) as NumsOf from df group by participant, speed, outcome')
# ppmy$outcome<-as.factor(ppmy$outcome)
dfpp<-dcast(ppmy, Participant+ Speed ~outcome,sum)
dfpp$prec<-dfpp$TP/(dfpp$TP+dfpp$FP)
dfpp$acc<-dfpp$TP/(dfpp$TP+dfpp$FN)
dfpp<-dfpp[!is.na(dfpp$Participant),]
dfsummary<-sqldf::sqldf('select participant, speed, count(participant) as NumsOf from df  where participant>-1 group by participant, speed')
ggmeans<-sqldf::sqldf('select speed, avg(prec), avg(acc) from dfpp group by speed')
ggmeans<-melt(ggmeans,id.vars="Speed")
ggmeans$Speed<-as.numeric(ggmeans$Speed)
ggplot(ggmeans,aes(x=Speed,y=value,group=variable,color=variable))+geom_point()+geom_line()+xlim(0,5)+ylim(0,1)+scale_x_continuous(limits = c(0, 5),breaks = c(0.5, 1.5, 3,4.5))+theme_bw()
df$precision<-df$
# df %>%
#   group_by(Participant,Speed) %>%
#   mutate(Count=n())


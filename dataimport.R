# comment import data from xlsx
library(here)
library(dplyr)
library(sqldf)
library(reshape2)
library(ggplot2)
library(stringr)
dx<-NULL
df<-NULL

demog<-openxlsx::read.xlsx("data/DemographicsData.xlsx", sheet=1, skipEmptyRows=FALSE)
dmogpP<-sqldf('select PID, videoGamesXP, VRxp, robotXP, droneXP,avg(PrecisionRate) as prec, avg(RecallRate ) as rec, avg(AccuracyRate) as acc from demog group by PID, videoGamesXP, VRxp, robotXP, droneXP')

for(i in 1:24){
  dx<-openxlsx::read.xlsx("data/rawDataDrone2.xlsx", sheet=i, skipEmptyRows=FALSE)
  df<-rbind(df,dx)
  }
colnames(df)[colnames(df)=="#"] <- "droneNum"
df<-df[!df$Speed==2,]
df$RunningNumAll<-seq(1,nrow(df))
dfPrunMax<-sqldf('select Participant, min(RunningNumall) as PminRun from df group by Participant')
df<-merge(df,dfPrunMax)
df$personDroneNum<-df$RunningNumAll-df$PminRun+1
dfR<-sqldf('select Participant, count(Participant) as pRuns from df group by Participant having pRuns>0')
df<-merge(df,dfR)
df$pRunSection<-floor((df$personDroneNum)/(df$pRuns/3))

df$TimeInSession<-df$Time
df$TimeInSession<-gsub("[+]","",df$TimeInSession)
df$TimeInSession<-as.numeric(df$TimeInSession)
df$totalDrones

df$Participant<-as.numeric(df$Participant)
df$Drone.number<-as.numeric(df$Drone.number)
df$Speed<-as.numeric(df$Speed)
df$Altitude<-as.numeric(df$Altitude)
df$droneNum<-as.numeric(df$droneNum)
df$Selction.distance<-as.numeric(df$Selction.distance)
df$Highlight.distance<-as.numeric(df$Highlight.distance)
df$selDist<-as.numeric(df$Selction.distance)
allSel<-data.frame(as.numeric(df$Selction.distance))
allSelHRIr<-sqldf("select Participant, selDist, outcome from df")

#  analysis of angular movement
#mean participant height mPH
mPH<-1.7

#gradient angle and calculation for 
ga1<-atan2(7.48-mPH,13.85+.5-.75)*180/pi
ga2<-atan2(6.979-mPH,13.85-.75)*180/pi
ga2-ga1
ga3<-atan2(8.75-mPH,13.85+1.5-.75)*180/pi
ga4<-atan2(6.979-mPH,13.85-.75)*180/pi
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
df$learningStage<-
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
# df$precision<-df$
# df %>%
#   group_by(Participant,Speed) %>%
#   mutate(Count=n())
# 
# ## demographic analysis
# 
summary(lm(prec~robotXP,dmogpP))
summary(lm(prec~droneXP,dmogpP))
summary(lm(prec~videoGamesXP,dmogpP))
summary(lm(prec~VRxp,dmogpP))
summary(lm(rec~robotXP,dmogpP))
summary(lm(rec~droneXP,dmogpP))
summary(lm(rec~videoGamesXP,dmogpP))
summary(lm(rec~VRxp,dmogpP))
summary(lm(acc~robotXP,dmogpP))
summary(lm(acc~droneXP,dmogpP))
summary(lm(acc~videoGamesXP,dmogpP))
summary(lm(acc~VRxp,dmogpP))

summary(lm(PrecisionRate~robotXP*speed,demog))
summary(lm(PrecisionRate~droneXP*speed,demog))
summary(lm(PrecisionRate~videoGamesXP*speed,demog))
summary(lm(PrecisionRate~VRxp*speed,demog))
summary(lm(RecallRate~robotXP*speed,demog))
summary(lm(RecallRate~droneXP*speed,demog))
summary(lm(RecallRate~videoGamesXP*speed,demog))
summary(lm(RecallRate~VRxp*speed,demog))
summary(lm(AccuracyRate~robotXP*speed,demog))
summary(lm(AccuracyRate~droneXP*speed,demog))
summary(lm(AccuracyRate~videoGamesXP*speed,demog))
summary(lm(AccuracyRate~VRxp*speed,demog))

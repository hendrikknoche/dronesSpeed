#import data from xlsx
library(here)
library(dplyr)
library(reshape2)
library(ggplot2)

dx<-NULL
df<-NULL

for(i in 1:24){
  dx<-openxlsx::read.xlsx("data/rawDataDrone2.xlsx", sheet=i, skipEmptyRows=FALSE)
  df<-rbind(df,dx)
  }

df<-df[!df$Speed==2,]
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


#import data from xlsx
library(here)
library(dplyr)

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


dfsummary<-sqldf::sqldf('select participant, speed, count(participant) as NumsOf from df  where participant>-1 group by participant, speed')

df$precision<-df$
# df %>%
#   group_by(Participant,Speed) %>%
#   mutate(Count=n())


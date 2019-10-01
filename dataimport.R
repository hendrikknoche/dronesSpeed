#import data from xlsx
library(here)

dx<-NULL
df<-NULL

for(i in 1:24){
  dx<-openxlsx::read.xlsx("data/rawDataDrone2.xlsx", sheet=i, skipEmptyRows=FALSE)
  df<-rbind(df,dx)
  }

df<-df[!df$Speed==2,]
df[df$Correct.drone==TRUE,]$outcome<-'TP'

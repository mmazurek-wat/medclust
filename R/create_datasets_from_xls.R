
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_77/jre")
library(xlsx)

library(Hmisc)

library(chron)
library(data.table)

df=read.xlsx("C:/Users/mmazurek/Documents/RWorkDir/HipokalcemiaRShiny/Przytarczyce.xls", 1 )
load(file='C:/Users/mmazurek/Documents/RWorkDir/HipokalcemiaRShiny/column_headers.rda')
names(df)<-colheaders

#Liczba obserwacji
N<-nrow(df)


#przygotowanie zmiennych

df$Data_operacji<-as.Date(df$Data_operacji, "%d-%m-%Y")
df$Data_urodzenia<-as.Date(df$Data_urodzenia, "%d-%m-%Y")
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
df$Wiek<-as.numeric.factor(years(df$Data_operacji)) - as.numeric.factor(years(df$Data_urodzenia))


df$czy_dolegliwosci<-ifelse(df$dolegliwosci ==1, 'TAK', 'NIE')

df$anty.TG<-as.numeric(ifelse(as.character(df$anty.TG) %in% c( "<0,9", "<0.9"), '0.9',as.character(df$anty.TG)))
df$log.anty.TG<-log(df$anty.TG)


df$PTH1<-as.numeric(ifelse(as.character(df$PTH1) %in% c( "<6"), '6',as.character(df$PTH1)))
df$PTH2<-as.numeric(ifelse(as.character(df$PTH2) %in% c( "<6"), '6',as.character(df$PTH2)))
df$PTH3<-as.numeric(ifelse(as.character(df$PTH3) %in% c( "<6"), '6',as.character(df$PTH3)))
df$PTH6<-as.numeric(ifelse(as.character(df$PTH6) %in% c( "<6"), '6',as.character(df$PTH6)))


df$Ca3<-as.numeric(ifelse(as.character(df$Ca3) %in% c( "<6"), '6',as.character(df$Ca3)))

df$log.anty.TPO<-log(df$anty.TPO)


df[c(1:5,12:18,16:28)]
colnames(df)[c(1:5,12:17,20:25, 26:27, 29:33)]
pthseries<-df[c(1:5,35, 60, 12:17,20:25, 26:27, 29:33  )]
colnames(pthseries)<-c("PID",  "DOB", "Operation_date",  "Sex", "Group" , "Discomfort", "Age", "PTH.X7", "PTH.X1",  "PTH.1","PTH.2",  "PTH.3",
"PTH.6", "Ca.X7", "Ca.X1", "Ca.1",  "Ca.2"  ,"Ca.3"  ,"Ca.5" , "P.X7",  "P.X1" ,"Mg.X7",  "Mg.X1",  "Mg.1" , "Mg.2",  "Mg.3")

save(pthseries, file= "C:/Users/mmazurek/Documents/RWorkDir/medclust/data/pthseries.rda")


library(plyr)
###PTHS time series
PTH.var.seq<-c("PTH.X7", "PTH.X1",  "PTH.1","PTH.2",  "PTH.3","PTH.6")
df.traj<-pthseries[c("PID", PTH.var.seq)]
df.ts <-data.table(melt(df.traj, "PID"))

time.seq<-  c(-7,-1, 1,2,3,6)
time.seq.df<-data.frame(time.seq, variable=PTH.var.seq)

pth.ts<-merge(time.seq.df, df.ts, by='variable')
pth.ts<-pth.ts[,c("PID", "time.seq", "value")]
head(pth.ts)
nrow(df.ts)

pth.ts<-plyr::rename(pth.ts, replace=c("value"="PTH"))

#Ca
Ca.var.seq<-c("Ca.X7", "Ca.X1", "Ca.1",  "Ca.2"  ,"Ca.3"  ,"Ca.5" )
df.traj<-pthseries[c("PID", Ca.var.seq)]
df.ts <-data.table(melt(df.traj, "PID"))

time.seq<-  c(-7,-1, 1,2,3,5)
time.seq.df<-data.frame(time.seq, variable=Ca.var.seq)

ca.ts<-merge(time.seq.df, df.ts, by='variable')
ca.ts<-ca.ts[,c("PID", "time.seq", "value")]
head(ca.ts)
nrow(ca.ts)

ca.ts<-plyr::rename(ca.ts, replace=c("value"="Ca"))

#Mg

Mg.var.seq<-c("Mg.X7",  "Mg.X1",  "Mg.1" , "Mg.2",  "Mg.3")
df.traj<-pthseries[c("PID", Mg.var.seq)]
df.ts <-data.table(melt(df.traj, "PID"))

time.seq<-  c(-7,-1, 1,2,3)
time.seq.df<-data.frame(time.seq, variable=Mg.var.seq)

Mg.ts<-merge(time.seq.df, df.ts, by='variable')
Mg.ts<-Mg.ts[,c("PID", "time.seq", "value")]

Mg.ts<-plyr::rename(Mg.ts, replace=c("value"="Mg"))
head(Mg.ts)
nrow(Mg.ts)

#P

P.var.seq<-c("P.X7",  "P.X1")
df.traj<-pthseries[c("PID", P.var.seq)]
df.ts <-data.table(melt(df.traj, "PID"))

time.seq<-  c(-7,-1)
time.seq.df<-data.frame(time.seq, variable=P.var.seq)

P.ts<-merge(time.seq.df, df.ts, by='variable')
P.ts<-P.ts[,c("PID", "time.seq", "value")]

P.ts<-plyr::rename(P.ts, replace=c("value"="P"))
head(P.ts)
nrow(P.ts)

o<-merge(P.ts, pth.ts, by=c("PID",  "time.seq"), all=TRUE)
o<-merge(o, Mg.ts, by=c("PID",  "time.seq"), all=TRUE)
o<-merge(o, ca.ts, by=c("PID",  "time.seq"), all=TRUE)
os<-o[order(o$PID,o$time.seq),]
head(os)
nrow(o)
head(o)
o[o$PID==555866,]

pthseries_l<-os
save(pthseries_l, file= "C:/Users/mmazurek/Documents/RWorkDir/medclust/data/pthseries_l.rda")

library(dplyr)
library(zoo)

pthseries_l %>% dplyr::group_by(PID) %>% do(zoo(.))
pthzoo<-by(pthseries_l, pthseries_l$PID, function(x)  zoo(x[,3:ncol(x)], x$time.seq))
z1<-lzoo[[1]]
z1
time(z1)
z1


save(pthzoo, file= "C:/Users/mmazurek/Documents/RWorkDir/medclust/data/pthzoo.rda")



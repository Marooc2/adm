install.packages("dplyr")
install.packages("ggplot2")
install.packages("quantreg")

library(dplyr)
library(ggplot2)
library(quantreg)

numArchivo<-1
coins<-c(0.10,0.20,0.50,1.00,2.00,5.00)

generar <- function(n){
  for(j in 1:n){
    archivoTabla<-paste("coins",numArchivo,".tsv",sep="")
    for(i in 1:sample(1000:2500,1)){
      coin1<-sample(coins,1)
      coin2<-sample(coins,1)
      coin3<-sample(coins,1)
      coin4<-sample(coins,1)
      lineTSV<-c(coin1,coin2,coin3,coin4)
      
      write.table(lineTSV,file=archivoTabla,sep="\t",append=TRUE,col.names=FALSE,row.names = FALSE)
    }
    numArchivo<-numArchivo+1
  }
}

generar(5)
dataset1<-read.table(file="coins1.tsv",header=FALSE,sep="\t")
dataset2<-read.table(file="coins2.tsv",header=FALSE,sep="\t")
dataset3<-read.table(file="coins3.tsv",header=FALSE,sep="\t")
dataset4<-read.table(file="coins4.tsv",header=FALSE,sep="\t")
dataset5<-read.table(file="coins5.tsv",header=FALSE,sep="\t")

#Reduccion


Rds1<-data.frame(dataset1%>%summarise(sum(V1)),dataset1%>%summarise(mean(V1)),dataset1%>%summarise(sd(V1)),dataset1%>%filter(V1==0.1)%>%summarise(n()),dataset1%>%filter(V1==0.2)%>%summarise(n()),dataset1%>%filter(V1==0.5)%>%summarise(n()),dataset1%>%filter(V1==1)%>%summarise(n()),dataset1%>%filter(V1==2)%>%summarise(n()),dataset1%>%filter(V1==5)%>%summarise(n()))
Rds2<-data.frame(dataset2%>%summarise(sum(V1)),dataset2%>%summarise(mean(V1)),dataset2%>%summarise(sd(V1)),dataset2%>%filter(V1==0.1)%>%summarise(n()),dataset2%>%filter(V1==0.2)%>%summarise(n()),dataset2%>%filter(V1==0.5)%>%summarise(n()),dataset2%>%filter(V1==1)%>%summarise(n()),dataset2%>%filter(V1==2)%>%summarise(n()),dataset2%>%filter(V1==5)%>%summarise(n()))
Rds3<-data.frame(dataset3%>%summarise(sum(V1)),dataset3%>%summarise(mean(V1)),dataset3%>%summarise(sd(V1)),dataset3%>%filter(V1==0.1)%>%summarise(n()),dataset3%>%filter(V1==0.2)%>%summarise(n()),dataset3%>%filter(V1==0.5)%>%summarise(n()),dataset3%>%filter(V1==1)%>%summarise(n()),dataset3%>%filter(V1==2)%>%summarise(n()),dataset3%>%filter(V1==5)%>%summarise(n()))
Rds4<-data.frame(dataset4%>%summarise(sum(V1)),dataset4%>%summarise(mean(V1)),dataset4%>%summarise(sd(V1)),dataset4%>%filter(V1==0.1)%>%summarise(n()),dataset4%>%filter(V1==0.2)%>%summarise(n()),dataset4%>%filter(V1==0.5)%>%summarise(n()),dataset4%>%filter(V1==1)%>%summarise(n()),dataset4%>%filter(V1==2)%>%summarise(n()),dataset4%>%filter(V1==5)%>%summarise(n()))
Rds5<-data.frame(dataset5%>%summarise(sum(V1)),dataset5%>%summarise(mean(V1)),dataset5%>%summarise(sd(V1)),dataset5%>%filter(V1==0.1)%>%summarise(n()),dataset5%>%filter(V1==0.2)%>%summarise(n()),dataset5%>%filter(V1==0.5)%>%summarise(n()),dataset5%>%filter(V1==1)%>%summarise(n()),dataset5%>%filter(V1==2)%>%summarise(n()),dataset5%>%filter(V1==5)%>%summarise(n()))
reduccion<-rbind(Rds1,Rds2,Rds3,Rds4,Rds5)
rownames(reduccion)<-c("Dia 1","Dia 2","Dia 3","Dia 4","Dia 5")
colnames(reduccion)<-c("Sumatoria","Media","Desviacion estÃ¡ndar","S/.0.10","S/.0.20","S/.0.50","S/.1.00","S/.2.00","S/.5.00")

#Correlacion

dias<-c(1:5)

#Sumatorias
sumaD<-sum(dias)
suma10<-sum(reduccion$`S/.0.10`)
suma20<-sum(reduccion$`S/.0.20`)
suma50<-sum(reduccion$`S/.0.50`)
suma100<-sum(reduccion$`S/.1.00`)
suma200<-sum(reduccion$`S/.2.00`)
suma500<-sum(reduccion$`S/.5.00`)

#Medias
mD<-mean(dias)
m10<-mean(reduccion$`S/.0.10`)
m20<-mean(reduccion$`S/.0.20`)
m50<-mean(reduccion$`S/.0.50`)
m100<-mean(reduccion$`S/.1.00`)
m200<-mean(reduccion$`S/.2.00`)
m500<-mean(reduccion$`S/.5.00`)

#Covarianzas
covD10<-(sum(dias*reduccion$`S/.0.10`)/5)-(mD*m10)
covD20<-(sum(dias*reduccion$`S/.0.20`)/5)-(mD*m20)
covD50<-(sum(dias*reduccion$`S/.0.50`)/5)-(mD*m50)
covD100<-(sum(dias*reduccion$`S/.1.00`)/5)-(mD*m100)
covD200<-(sum(dias*reduccion$`S/.2.00`)/5)-(mD*m200)
covD500<-(sum(dias*reduccion$`S/.5.00`)/5)-(mD*m500)

#Desviaciones
dD<-sqrt((sum(dias*dias)/5)-(mD*mD))
d10<-sqrt((sum(reduccion$`S/.0.10`*reduccion$`S/.0.10`)/5)-(m10*m10))
d20<-sqrt((sum(reduccion$`S/.0.20`*reduccion$`S/.0.20`)/5)-(m20*m20))
d50<-sqrt((sum(reduccion$`S/.0.50`*reduccion$`S/.0.50`)/5)-(m50*m50))
d100<-sqrt((sum(reduccion$`S/.1.00`*reduccion$`S/.1.00`)/5)-(m100*m100))
d200<-sqrt((sum(reduccion$`S/.2.00`*reduccion$`S/.2.00`)/5)-(m200*m200))
d500<-sqrt((sum(reduccion$`S/.5.00`*reduccion$`S/.5.00`)/5)-(m500*m500))

#Correlacion
cr10<-covD10/(dD*d10)
cr20<-covD20/(dD*d20)
cr50<-covD50/(dD*d50)
cr100<-covD100/(dD*d100)
cr200<-covD200/(dD*d200)
cr500<-covD500/(dD*d500)

reduccion<-cbind(reduccion,dias)
#Diagrama 1
ggplot(reduccion,aes(x=reduccion$dias,y=reduccion$`S/.0.10`))+geom_point()+xlab("Dias")+ylab("Monedas de 10c")+geom_smooth(method="lm")

#Diagrama 2
ggplot(reduccion,aes(x=reduccion$dias,y=reduccion$`S/.0.20`))+geom_point()+xlab("Dias")+ylab("Monedas de 20c")+geom_smooth(method="lm")

#Diagrama 3
ggplot(reduccion,aes(x=reduccion$dias,y=reduccion$`S/.0.50`))+geom_point()+xlab("Dias")+ylab("Monedas de 50c")+geom_smooth(method="lm")

#Diagrama 4
ggplot(reduccion,aes(x=reduccion$dias,y=reduccion$`S/.1.00`))+geom_point()+xlab("Dias")+ylab("Monedas de 1s")+geom_smooth(method="lm")

#Diagrama 5
ggplot(reduccion,aes(x=reduccion$dias,y=reduccion$`S/.2.00`))+geom_point()+xlab("Dias")+ylab("Monedas de 2s")+geom_smooth(method="lm")

#Diagrama 6
ggplot(reduccion,aes(x=reduccion$dias,y=reduccion$`S/.5.00`))+geom_point()+xlab("Dias")+ylab("Monedas de 5s")+geom_smooth(method="lm")


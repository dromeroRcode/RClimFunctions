ClicomPTtsMens<-function(dt0){
  colnames(dt0)<-c("DATASETID","Station","ELEMENT","YEAR","VALUE1","FLAG11","VALUE2","FLAG12","VALUE3","FLAG13","VALUE4","FLAG14","VALUE5","FLAG15","VALUE6","FLAG16","VALUE7","FLAG17","VALUE8","FLAG18","VALUE9","FLAG19","VALUE10","FLAG110","VALUE11","FLAG111","VALUE12","FLAG112")
  base<-data.frame(Station=dt0$Station,Element=as.numeric(paste(dt0$ELEMENT)),YEAR=as.numeric(paste(dt0$YEAR)),M1=as.numeric(paste(dt0$VALUE1)),M2=as.numeric(paste(dt0$VALUE2)),M3=as.numeric(paste(dt0$VALUE3)),M4=as.numeric(paste(dt0$VALUE4)),M5=as.numeric(paste(dt0$VALUE5)),M6=as.numeric(paste(dt0$VALUE6)),M7=as.numeric(paste(dt0$VALUE7)),M8=as.numeric(paste(dt0$VALUE8)),M9=as.numeric(paste(dt0$VALUE9)),M10=as.numeric(paste(dt0$VALUE10)),M11=as.numeric(paste(dt0$VALUE11)),M12=as.numeric(paste(dt0$VALUE12)))
  base203<- base[ which(base$Element==203),]
  base208<- base[ which(base$Element==208),]
  base2<-rbind(base203,base208)
  
  x1 <- base2; names(x1)[4:ncol(x1)] <- 1:12
  x2 <- melt(setDT(x1), id=c("Station","Element","YEAR"))
  names(x2)[c(4,5)] <- c("Month", "Value")
  x2 <- x2[order(x2$Station, x2$Element,x2$YEAR, x2$Month),]; row.names(x2) <- 1:nrow(x2)
  x2[,4]<-as.numeric(paste(x2$Month))
  x21<-x2[!duplicated(x2), ]
  x22<-x21 %>% drop_na()
  x23<- dcast(x22,Station+YEAR+Month~Element,value.var=c("Value"))
  colnames(x23)<- c("Station","YEAR","Month","Tc","Pmm")
  return (x23)
}

ClicomPTtsDia<-function(dt0){
  colnames(dt0)<-c("DATASETID","Station","ELEMENT","YEAR_MONTH","VALUE1","FLAG1","VALUE2","FLAG2","VALUE3","FLAG3","VALUE4","FLAG4","VALUE5","FLAG5","VALUE6","FLAG6","VALUE7","FLAG7","VALUE8","FLAG8","VALUE9","FLAG9","VALUE10","FLAG10","VALUE11","FLAG11","VALUE12","FLAG12","VALUE13","FLAG13","VALUE14","FLAG14","VALUE15","FLAG15","VALUE16","FLAG16","VALUE17","FLAG17","VALUE18","FLAG18","VALUE19","FLAG19","VALUE20","FLAG20","VALUE21","FLAG21","VALUE22","FLAG22","VALUE23","FLAG23","VALUE24","FLAG24","VALUE25","FLAG25","VALUE26","FLAG26","VALUE27","FLAG27","VALUE28","FLAG28","VALUE29","FLAG29","VALUE30","FLAG30","VALUE31","FLAG31")
  fec<-as.data.table(strsplit(as.character(dt0$YEAR_MONTH),"-"))
  fec<-data.table(t(fec))
  base<-data.frame(Station=dt0$Station,Element=as.numeric(paste(dt0$ELEMENT)),YEAR=as.numeric(paste(fec$V1)),MONTH=as.numeric(paste(fec$V2)),D1=as.numeric(paste(dt0$VALUE1)),D2=as.numeric(paste(dt0$VALUE2)),D3=as.numeric(paste(dt0$VALUE3)),D4=as.numeric(paste(dt0$VALUE4)),D5=as.numeric(paste(dt0$VALUE5)),D6=as.numeric(paste(dt0$VALUE6)),D7=as.numeric(paste(dt0$VALUE7)),D8=as.numeric(paste(dt0$VALUE8)),D9=as.numeric(paste(dt0$VALUE9)),D10=as.numeric(paste(dt0$VALUE10)),D11=as.numeric(paste(dt0$VALUE11)),D12=as.numeric(paste(dt0$VALUE12)),D13=as.numeric(paste(dt0$VALUE13)),D14=as.numeric(paste(dt0$VALUE14)),D15=as.numeric(paste(dt0$VALUE15)),D16=as.numeric(paste(dt0$VALUE16)),D17=as.numeric(paste(dt0$VALUE17)),D18=as.numeric(paste(dt0$VALUE18)),D19=as.numeric(paste(dt0$VALUE19)),D20=as.numeric(paste(dt0$VALUE20)),D21=as.numeric(paste(dt0$VALUE21)),D22=as.numeric(paste(dt0$VALUE22)),D23=as.numeric(paste(dt0$VALUE23)),D24=as.numeric(paste(dt0$VALUE24)),D25=as.numeric(paste(dt0$VALUE25)),D26=as.numeric(paste(dt0$VALUE26)),D27=as.numeric(paste(dt0$VALUE27)),D28=as.numeric(paste(dt0$VALUE28)),D29=as.numeric(paste(dt0$VALUE29)),D30=as.numeric(paste(dt0$VALUE30)),D31=as.numeric(paste(dt0$VALUE31)))
  base2<- base[ which(base$Element==2),]
  base3<- base[ which(base$Element==3),]
  base5<- base[ which(base$Element==5),]
  basex<-rbind(base2,base3,base5)
  
  x1 <- basex; names(x1)[5:ncol(x1)] <- 1:31
  x2 <- melt(setDT(x1), id=c("Station","Element","YEAR","MONTH"))
  names(x2)[c(5,6)] <- c("Day", "Value")
  x2[,5]<-as.numeric(paste(x2$Day))
  x2 <- x2[order(x2$Station, x2$Element,x2$YEAR, x2$MONTH, x2$Day),]; row.names(x2) <- 1:nrow(x2)
  x21<-x2[!duplicated(x2), ]
  x22<-x21 %>% drop_na()
  x23<- dcast(x22,Station+YEAR+MONTH+Day~Element,value.var=c("Value"))
  colnames(x23)<- c("Station","YEAR","Month","Day","Tmax","Tmin","Pmm")
  x23$Tc<-round((x23$Tmax+x23$Tmin)/2,2)
  return (x23)
}

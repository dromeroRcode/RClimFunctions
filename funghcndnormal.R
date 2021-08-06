ghcndnormal<-function(fileghcnd,iniyear,minyear){
  
  require(stringi)
  require(dplyr)
  require(tidyr)
  
  if(is.na(iniyear)){iniyear=1981}
  if(is.na(minyear)){minyear=25}
  
  widt = c(11, 4, 2, 4, rep(c(5, 1, 1, 1),31))
  
  beg<-widt
  end<-widt
  
  col<-data.frame(cbind(widt,beg,end))
  
  col[1,2]=1
  
  for (ii in 2:nrow(col)){
    col[ii,2]<-col[ii-1,2]+col[ii-1,1]
    col[ii,3]<-col[ii-1,3]+col[ii,1]
  }
  
  col_ends <- list(beg = col[,2], end = col[,3])
  
  rm(col,beg,end,widt,ii)
  
  a0<- fread(fileghcnd,header = FALSE, sep = NULL)[,lapply(1:(length(col_ends$beg)),function(ii) stri_sub(V1, col_ends$beg[ii], col_ends$end[ii]))]
  
  a1<- a0 %>% select(-c(6,7,8,10,11,12,14,15,16,18,19,20,22,23,24,26,27,28,30,31,32,34,35,36,38,39,40,42,43,44,46,47,48,50,51,52,54,55,56,58,59,60,62,63,64,66,67,68,70,71,72,74,75,76,78,79,80,82,83,84,86,87,88,90,91,92,94,95,96,98,99,100,102,103,104,106,107,108,110,111,112,114,115,116,118,119,120,122,123,124,126,127,128)) %>%
    filter(V4=="PRCP" | V4=="TAVG" | V4=="TMAX" | V4=="TMIN")
  
  names(a1)[1:4]<-c("Station","Year","Month","Element")
  names(a1)[5:ncol(a1)] <- 1:31
  
  a2<-a1 %>% mutate_if(is.character, as.numeric) %>% replace(.==-9999, NA)
  
  a2$Station<-a1$Station
  a2$Element<-a1$Element
 
  a21<- a2 %>% gather ("Day","Value",5:35) %>% drop_na()
  a21$Day<-as.numeric(a21$Day)
  a21$Value<-a21$Value/10
  
  if( length(unique(a21$Element)) > 1) {
    a3<-a21 %>% filter(Year > iniyear-1 & Year < iniyear+30) %>%
    group_by(Station,Year,Month,Element) %>% summarise( Val = mean(Value, na.rm = TRUE),Num = n()) %>%
    filter(Num>20) %>% select(1:5) %>% group_by(Station,Month,Element) %>%
    summarise(Val = mean(Val, na.rm = TRUE),Num = n()) %>% filter(Num>minyear) %>% select(1:4) %>% spread(Station+Month,Val)
    
    if("PRCP" %in% colnames(a3)){a3$PRCP<-a3$PRCP*30} else {a3$PRCP<-NA}
    if("TMIN" %in% colnames(a3)){}else{a3$TMIN<-NA}
    if("TMAX" %in% colnames(a3)){}else{a3$TMAX<-NA}    
    if("TAVG" %in% colnames(a3)){a3$TAVG[is.na(a3$TAVG)]<-(a3$TMAX+a3$TMIN)/2}else{a3$TAVG<-(a3$TMAX+a3$TMIN)/2}

    sal<-data.frame(Station=a,Month=a3$Month,PRCP=round(a3$PRCP,1),TMIN=round(a3$TMIN,2),TMAX=round(a3$TMAX,2),TAVG=round(a3$TAVG,2))
    } else {sal<-data.frame(Station=a,Month=NA,PRCP=NA,TMIN=NA,TMAX=NA,TAVG=NA)}
  return (sal)
}

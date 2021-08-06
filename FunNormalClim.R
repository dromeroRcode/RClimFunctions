normalClim<-function(table,iniyear,minyear){
  
  require(tidyr)
  require(dplyr)
  
  if(is.na(iniyear)){iniyear=1981}
  if(is.na(minyear)){minyear=28}
  
  table$nT<-0
  table$nP<-0
  table$nT[which(!is.na(table$Tc))]=1
  table$nP[which(!is.na(table$Pmm))]=1
  b1<-table %>% filter(YEAR >= iniyear & YEAR < (iniyear+30)) %>% group_by(Station,Month) %>% summarise(Tc=mean(Tc),nT=sum(nT),Pmm=mean(Pmm),nP=sum(nP)) %>% filter(nP >= minyear & nT >= minyear) %>% select(Station,Month,Pmm,Tc) %>% drop_na()
  
  b2 <- b1 %>% mutate(one=1) %>% group_by(Station) %>% summarise(nM=sum(one)) %>% filter(nM== 12)
  
  b3<-b1[which(b1$Station %in% b2$Station),]
  return (b3)
}

# calculate SPI from dataframe with column "Station" and "Pmm" with the accumulate rainfall for the SPI computation

SPI<-function(a){
a$SPI=NA
for (i in unique(a$Station)){
  a2<-a[a$Station==i,]
  fit.cdf <- ecdf(a2$Pmm)
  cdfs <- sapply(a2$Pmm,fit.cdf)
  a[a$Station==i,4]<- qnorm(cdfs)
  return (a)
}


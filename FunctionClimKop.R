ClimKop<-function (x,y){
# x=sal
# y=hem
b11<-unique(x$Station)
sal0<-data.frame(Station=b11,Pmm=NA,Tmean=NA,Pmin=NA,Tmin=NA,Tmax=NA,Hemi=NA,Clim=NA)
# i=1
for (i in 1:length(b11)) {
  b2<-x %>% filter(Station==b11[i])
  
  tmax=max(b2$Tc)
  sal0[i,6]<-tmax
  tmin=min(b2$Tc)
  sal0[i,5]<-tmin
  tprom=round(mean(b2$Tc),2)
  sal0[i,3]<-tprom
  amp=tmax-tmin
  Ptot=sum(b2$Pmm)
  sal0[i,2]<-Ptot
  Pmax=max(b2$Pmm)
  Pmin=min(b2$Pmm)
  sal0[i,4]<-Pmin
  Tclim2=NA
  Tclim3=NA
  
  ############## HEMISFERIO #################
  if(is.na(y)){if (amp<1) {hemi="E"} else if (max(b2$Tc[4:7])<max(b2$Tc[1],b2$Tc[11:12])) {hemi="N"} else {hemi="S"}} else {hemi=y}
  sal0[i,7]<-hemi
  if (hemi=="N") {Pver=sum(b2$Pmm[4:9])
  PPInv=round(sum(b2$Pmm[1:3])/Ptot*100,2)
  minhinv=min(min(b2$Pmm[10:12]),min(b2$Pmm[1:3]))
  maxhinv=max(max(b2$Pmm[10:12]),max(b2$Pmm[1:3]))
  minest=min(b2$Pmm[4:9])} else if (hemi=="S") {Pver=sum(b2$Pmm[10:12])+sum(b2$Pmm[1:3])
  PPInv=round(sum(b2$Pmm[7:9])/Ptot*100,2)
  minhinv=min(b2$Pmm[4:9])
  maxhinv=max(b2$Pmm[4:9])
  minest=min(min(b2$Pmm[10:12]),min(b2$Pmm[1:3]))}
  
  ############## TIPOS #################
  if ((Pver/Ptot)<0.3 && Ptot<(10*tprom)) {Tclim="BW"} else
    if ((Pver/Ptot)<0.3 && Ptot<(20*tprom)) {Tclim="BS"} else
      if ((Pver/Ptot)>0.7 && Ptot<(280+20*tprom)/2) {Tclim="BW"} else
        if ((Pver/Ptot)>0.7 && Ptot<(280+20*tprom)) {Tclim="BS"} else
          if (Ptot<(140+20*tprom)/2) {Tclim="BW"} else if (Ptot<(140+20*tprom)) {Tclim="BS"} else
            if (tmin>18) {Tclim="A"} else if (tprom>5 && tmin>(-3)) {Tclim="C"} else
                if (tmax>10 && tmin<(-3)) {Tclim="D"} else
                  if (tmax<10) {Tclim="E"}
  
  ############## 2da LETRA wsfm #################
  if (Tclim=="A") {if (Pmin>=60) {Tclim2="f"} else if (Pmin>=(100-Ptot/25)) {Tclim2="m"} else if (Pver>(Ptot-Pver)) {Tclim2="w"} else {Tclim2="s"}}
  if (Tclim=="C" | Tclim=="D") {if (minhinv<Pmax/10) {Tclim2="w"} else if (minest<40 & minest<maxhinv/3) {Tclim2="s"} else {Tclim2="f"}}

  ############# 3da LETRAS abcdhk ################
  if (Tclim=="BW" | Tclim=="BS") {if (tprom>18) {Tclim3="h"} else {Tclim3="k"}}
  if (Tclim=="C" | Tclim=="D") {if (tmax>22) {Tclim3="a"} else if (length(b2$Tc[b2$Tc>10])>3) {Tclim3="b"} else if (tmin>(-38)) {Tclim3="c"} else {Tclim3="d"}}
  if (Tclim=="E") {if (tmin>(-10)) {Tclim="EM"} else if (tmax>0) {Tclim="ET"} else {Tclim="EF"}}
  sal0[i,8]<-paste(Tclim,na.omit(Tclim2),na.omit(Tclim3),sep="")
}
return(sal0)
}

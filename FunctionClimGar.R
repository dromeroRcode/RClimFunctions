ClimGar<-function (x,y){
  x$ind=1
  b0 <- x %>% drop_na() %>% group_by (Station) %>% summarise (nn=sum(ind)) %>% filter (nn==12)

  if (nrow(b0)>0){
    sal0<-data.frame(Station=b0$Station,Pmm=NA,Pmin=NA,Pmax=NA,Tmean=NA,Tmin=NA,Tmax=NA,Pver=NA,Regi=NA,P_T=NA,Osc=NA,PPInv=NA,Hum=NA,Hemi=NA,Tclim1=NA,Tclim2=NA,Tclim3=NA,Tclim4=NA,Tclim5=NA,Clim=NA)
    
    for (ii in 1:nrow(b0)) {
      b2<-x %>% filter(Station==b0$Station[ii])
      
      tmax=max(b2$Tc)
      sal0[ii,7]<-tmax
      tmin=min(b2$Tc)
      sal0[ii,6]<-tmin
      tprom=round(mean(b2$Tc),2)
      sal0[ii,5]<-tprom
      amp=tmax-tmin
      sal0[ii,11]<-amp
      Ptot=sum(b2$Pmm)
      sal0[ii,2]<-Ptot
      Pmax=max(b2$Pmm)
      sal0[ii,4]<-Pmax
      Pmin=min(b2$Pmm)
      sal0[ii,3]<-Pmin
      PaTa<-round(Ptot/tprom,2)
      sal0[ii,10]<-PaTa
      md<-length(b2$Tc[b2$Tc>10])
      Tclim1=NA
      Tclim2=NA
      Tclim3=NA
      Tclim4=NA
      Tclim5=NA
      graph=NA
      
      ############## AMPLITUD ################
      if (amp>14) {ampi="(e')"} else if (amp>7) {ampi="(e)"} else if (amp>5) {ampi="(i')"} else {ampi="i"}
      
      ############## HEMISFERIO #################
      if(is.na(y)){if (amp<1) {hemi="E"} else if (max(b2$Tc[4:7])<max(b2$Tc[1],b2$Tc[11:12])) {hemi="N"} else {hemi="S"}} else {hemi=y}
      sal0[ii,14]<-hemi
      if (hemi=="N") {Pver=sum(b2$Pmm[4:9])
      PPInv=round(sum(b2$Pmm[1:3])/Ptot*100,2)
      Pmav=max(b2$Pmm[4:9])
      Pmah=max((b2$Pmm[10:12]),(b2$Pmm[1:3]))} else if (hemi=="S") {Pver=sum(b2$Pmm[10:12])+sum(b2$Pmm[1:3])
      PPInv=round(sum(b2$Pmm[7:9])/Ptot*100,2)
      Pmav=max((b2$Pmm[10:12]),(b2$Pmm[1:3]))
      Pmah=max(b2$Pmm[4:9])}
      
      sal0[ii,12]<-PPInv
      sal0[ii,8]<-Pver
      
      if (Pmav>(10*Pmin)){reg="ver"} else if (Pmah>(3*Pmin) && Pmax==Pmah){reg="inv"} else {reg="int"}
      
      ############## TIPOS ################# 
      if (reg=="ver" && PPInv<10.2) {if (Ptot/10<2*tprom+28 && PaTa>22.9){Tclim1="BS1"} else if (Ptot/10<2*tprom+28 && PaTa<22.9){Tclim1="BS0"} else if (Ptot/10<tprom+14){Tclim1="BW"}}
      if (reg=="ver" && PPInv>10.2) {if (Ptot/10<2*tprom+21 && PaTa>22.9){Tclim1="BS1"} else if (Ptot/10<2*tprom+21 && PaTa<22.9){Tclim1="BS0"} else if (Ptot/10<tprom+10.5){Tclim1="BW"}} 
      if (reg=="int") {if (Ptot/10<2*tprom+14 && PaTa>22.9){Tclim1="BS1"} else if (Ptot/10<2*tprom+14 && PaTa<22.9){Tclim1="BS0"} else if (Ptot/10<tprom+7){Tclim1="BW"}}
      if (reg=="inv") {if (Ptot/10<2*tprom && PaTa>22.9){Tclim1="BS1"} else if (Ptot/10<2*tprom && PaTa<22.9){Tclim1="BS0"} else if (Ptot/10<tprom){Tclim1="BW"}}
      if (is.na(Tclim1)) {if(tprom>22) {if (tmin>18) {Tclim1="A"} else {Tclim1="A(C)"}} else if (tprom>18) {Tclim1="(A)C"} else if (tprom>5 && tmin>(-3)) {Tclim1="C"} else
        if (tmax>10 && tmin<(-3)) {Tclim1="D"} else if (tmax<10) {Tclim1="E"}}
      
      ############## 2da LETRA wsxfm #################
      if (Tclim1=="A" | Tclim1=="A(C)") {if (Pmin>60 | Pmin*25<Ptot-1000) {graph="hum"} else {graph="subhum"}}
      if (Tclim1=="C" | Tclim1=="D" | Tclim1=="(A)C" | Tclim1=="E") {if (Pmin>40 | Pmin*32.5<Ptot-500) {graph="hum"} else {graph="subhum"}}
      
      if (reg=="ver"){if ((Tclim1=="A" | Tclim1=="A(C)") && graph=="hum") {if (PPInv<5) {Tclim2="m(w)"} else if (PPInv<10.2) {Tclim2="m"} else {Tclim2="m(f)"}}
        if ((Tclim1=="C" | Tclim1=="D" | Tclim1=="(A)C") &&  graph=="hum") {if (PPInv<5) {Tclim2="(m)(w)"} else if (PPInv<10.2) {Tclim2="(m)"} else {Tclim2="(f)(m)"}}
        if ((Tclim1=="C" | Tclim1=="D" | Tclim1=="(A)C" | Tclim1=="E") && PaTa>55 &&  graph=="subhum") {if (PPInv<5) {Tclim2="(w2)(w)"} else if (PPInv<10.2) {Tclim2="(w2)"} else {Tclim2="(w2)(x')"}}
        if ((Tclim1=="A" | Tclim1=="A(C)") && PaTa>55.3 && graph=="subhum") {if (PPInv<5) {Tclim2="w2(w)"} else if (PPInv<10.2) {Tclim2="w2"} else {Tclim2="w2(x')"}}    
        if ((Tclim1=="C" | Tclim1=="D" | Tclim1=="(A)C" | Tclim1=="E") && PaTa<55 && PaTa>43.2 && (Pmin<(100-(Ptot/25)))) {if (PPInv<5) {Tclim2="(w1)(w)"} else if (PPInv<10.2) {Tclim2="(w1)"} else {Tclim2="(w1)(x')"}}
        if ((Tclim1=="A" | Tclim1=="A(C)") && PaTa<55.3 && PaTa>43.2 &&  graph=="subhum") {if (PPInv<5) {Tclim2="w1(w)"} else if (PPInv<10.2) {Tclim2="w1"} else {Tclim2="w1(x')"}}
        if ((Tclim1=="C" | Tclim1=="D" | Tclim1=="(A)C" | Tclim1=="E") && PaTa<43.2 &&  graph=="subhum") {if (PPInv<5) {Tclim2="(w0)(w)"} else if (PPInv<10.2) {Tclim2="(w0)"} else {Tclim2="(w0)(x')"}}
        if ((Tclim1=="A" | Tclim1=="A(C)") && PaTa<43.2 &&  graph=="subhum") {if (PPInv<5) {Tclim2="w0(w)"} else if (PPInv<10.2) {Tclim2="w0"} else {Tclim2="w0(x')"}}
        if (Tclim1=="BS0" | Tclim1=="BS1"| Tclim1=="BW") {if (PPInv<5) {Tclim2="w(w)"} else if (PPInv<10.2) {Tclim2="w"} else {Tclim2="w(x')"}}} else if (reg=="int"){
          if ((Tclim1=="A" | Tclim1=="A(C)") &&  graph=="hum") {if (PPInv<18) {Tclim2="f(m)"} else {Tclim2="f"}}
          if ((Tclim1=="C" | Tclim1=="D" | Tclim1=="(A)C" | Tclim1=="E") &&  graph=="hum") {if (PPInv<18) {Tclim2="(fm)"} else {Tclim2="(f)"}}
          if ((Tclim1=="C" | Tclim1=="D" | Tclim1=="(A)C" | Tclim1=="E") && PaTa>55 &&  graph=="subhum") {if (PPInv<18) {Tclim2="(x')(w2)"} else {Tclim2="x'"}}
          if ((Tclim1=="A" | Tclim1=="A(C)") && PaTa>55.3 &&  graph=="subhum") {if (PPInv<18) {Tclim2="x'(w2)"} else {Tclim2="x'"}}
          if ((Tclim1=="C" | Tclim1=="D" | Tclim1=="(A)C" | Tclim1=="E") && PaTa<55 && PaTa>43.2 && (Pmin<(100-(Ptot/25)))) {if (PPInv<18) {Tclim2="(x')(w1)"} else {Tclim2="x'"}}
          if ((Tclim1=="A" | Tclim1=="A(C)") && PaTa<55.3 && PaTa>43.2 &&  graph=="subhum") {if (PPInv<18) {Tclim2="x'(w2)"} else {Tclim2="x'"}}
          if ((Tclim1=="C" | Tclim1=="D" | Tclim1=="(A)C" | Tclim1=="E") && PaTa<43.2 &&  graph=="subhum") {if (PPInv<18) {Tclim2="(x')(w0)"} else  {Tclim2="x'"}}
          if ((Tclim1=="A" | Tclim1=="A(C)") && PaTa<43.2 &&  graph=="subhum") {if (PPInv<18) {Tclim2="x'(w2)"} else {Tclim2="x'"}}
          if (Tclim1=="BS0" | Tclim1=="BS1"| Tclim1=="BW") {if (PPInv<18) {Tclim2="x'(w)"} else {Tclim2="x'"}}} else if (reg=="inv"){
            if (Pmin>60 | Pmin>(100-(Ptot/25))) {if (PPInv<36) {Tclim2="f(s)"} else {Tclim2="s(f)"}} else if (PPInv<36) {Tclim2="s(x')"} else {Tclim2="s"}}
      
      ############## 3da LETRAS abcdhk ################# 
      if (Tclim1=="BW" | Tclim1=="BS0" | Tclim1=="BS1") {if (tprom>22) {if (tmin>18) {Tclim3="h'"} else {Tclim3="(h')h"}} else if (tprom>18) {Tclim3="h'(h)"} else if (tmin>18 && tmax>22){Tclim3="h"} else if
        (tprom>12 && tmin>(-3)){if (tmax>18){Tclim3="k"}else{Tclim3="k'"}} else {Tclim3="k''"}}
      
      if (Tclim1=="(A)C") {Tclim3="a"}
      if ((Tclim1=="C" | Tclim1=="D") && tprom>12) {if (tmax>22) {Tclim3="a"} else {Tclim3="b"}}
      if ((Tclim1=="C" | Tclim1=="D") && tprom<12) {if (md>3) {Tclim3="b'"} else if (tmin>(-38)) {Tclim3="c"} else {Tclim3="d"}} 
      
      ############## 4 y 5ta LETRAS w'' g H ################# 
      if (hemi=="N" && max(b2$Tc[1:5]>max(b2$Tc[6:10]))){Tclim4="g"}
      if (hemi=="S" && max(b2$Tc[7:11]>(max(b2$Tc[12],(b2$Tc[1:4]))))){Tclim4="g"}
      if (Tclim1=="E" && amp<10){Tclim3="H"}
      
      if (Tclim1=="E") {if (tmin>0 && tprom>-2) {Tclim1="E(T)C"} else if (tmin<0 && tprom>-2) {Tclim1="E(T)"} else if (tmax<0) {Tclim1="EF"} else if (tmin>(-10)) {Tclim1="EM"}}
      
      if ((Tclim1=="A" | Tclim1=="A(C)") && Pver>Ptot-Pver && hemi=="N" && ((b2$Pmm[7]<b2$Pmm[6] && b2$Pmm[7]<b2$Pmm[8]) | (b2$Pmm[8]<b2$Pmm[7] && b2$Pmm[8]<b2$Pmm[9]))){Tclim5="w''"}
      if ((Tclim1=="A" | Tclim1=="A(C)") && Pver>Ptot-Pver && hemi=="S" && ((b2$Pmm[1]<b2$Pmm[12] && b2$Pmm[1]<b2$Pmm[2]) | (b2$Pmm[2]<b2$Pmm[1] && b2$Pmm[2]<b2$Pmm[3]))){Tclim5="w''"} 
      
      sal0[ii,9]<-reg
      sal0[ii,13]<-graph
      sal0[ii,15]<-Tclim1
      sal0[ii,16]<-Tclim2
      sal0[ii,17]<-Tclim3
      sal0[ii,18]<-Tclim4
      sal0[ii,19]<-Tclim5
      sal0[ii,20]<-paste(Tclim1,na.omit(Tclim2),na.omit(Tclim3),ampi,sep="",na.omit(Tclim4),na.omit(Tclim5))
    }} else {
      sal0<-data.frame(Station=NA,Pmm=NA,Pmin=NA,Pmax=NA,Tmean=NA,Tmin=NA,Tmax=NA,Pver=NA,Regi=NA,P_T=NA,Osc=NA,PPInv=NA,Hum=NA,Hemi=NA,Tclim1=NA,Tclim2=NA,Tclim3=NA,Tclim4=NA,Tclim5=NA,Clim=NA)
      Sal0<-sal0[-1,]}
  return(sal0)}

# [R] netCDF4 to FSL 
# netCDF4 data is used to generate FSL upper  meteorological  input  for AERMET. FSL https://ruc.noaa.gov/raobs/fsl_format-new.html. 

rm(list=ls()) 
library(fields) 
library(ncdf4) 
library(dplyr) 
library(gdata) 
library(readr) 
setwd("D:/R/") 
lista <- list.files(pattern = "\\.nc$") 
N <- length(lista) 
for(i in 1:N) { 
 f <- nc_open(lista[i], verbose = FALSE, write = FALSE) 
   
#Reading 
  ymd = ncvar_get(f,"initial_time0") 
  lev = ncvar_get(f,"lv_ISBL4") 
  hgt = ncvar_get(f,"HGT_3_ISBL") 
  tmp = ncvar_get(f,"TMP_3_ISBL") 
  r_h = ncvar_get(f,"R_H_3_ISBL") 
 
#Missing values
  r_h <- unknownToNA(x=r_h, unknown=0) 
  u_w = ncvar_get(f,"U_GRD_3_ISBL") 
  v_w = ncvar_get(f,"V_GRD_3_ISBL") 
   
#Data gathering
  ano <- substr(ymd,9,10) 
  mes <- substr(ymd,1,2) 
  dia <- substr(ymd,4,5) 
  hora <- substr(ymd,13,14) 
   
  lev.vec <- as.vector(lev)[-(1:13)] 
  hgt.vec <- as.vector(hgt)[-(1:13)] 
  tmp.vec <- as.vector(tmp)[-(1:13)] 
  r_h.vec <- as.vector(r_h)[-(1:8)] 
  u_w.vec <- as.vector(u_w)[-(1:13)] 
  v_w.vec <- as.vector(v_w)[-(1:13)] 
   
  niveles<-length(lev.vec) 
   
  tab <- data.frame(cbind(lev.vec,hgt.vec,tmp.vec,r_h.vec,u_w.vec,v_w.vec)) 
  names(tab) <- c("lev","hgt","tmp","r_h","u_w","v_w") 
   
#Conversion for temperature and wind formats
  tmp.vec <- (tmp.vec-273.15) 
  w_s.vec <- sqrt(u_w.vec^2+v_w.vec^2) 
  t=atan(v_w.vec/u_w.vec)*180/3.1416 
  tab1 <- data.frame(cbind(u_w.vec,v_w.vec,t)) 
  names(tab1) <- c("u_w","v_w","t") 
  tab1<-data.frame(mutate(tab1,w_d = case_when ( u_w.vec<=0 & v_w.vec>0 ~ 270-t, 
                                                 u_w.vec<0 & v_w.vec<=0 ~ 270-t, 
                                                 u_w.vec>=0 & v_w.vec<=0 ~ 90-t, 
                                                 TRUE ~ 90-t ))) 
   
  w_d.vec <- tab1$w_d 
  tdw.vec <- tmp.vec-(100-r_h.vec)/5 
   
#Formating units
   
  lev.vec <- lev.vec*10 
  hgt.vec <- round(hgt.vec) 
  tmp.vec <- round(tmp.vec*10) 
  tdw.vec <- round(tdw.vec*10) 
  w_d.vec <- round(w_d.vec) 
  w_s.vec <- round(w_s.vec*10) 
   
#Dataframe 
   
  tab2 <- data.frame(cbind(lev.vec, hgt.vec,tmp.vec,tdw.vec,w_d.vec,w_s.vec)) 
  tab2 <- tab2[order(-lev.vec),] 
  names(tab2) <- c(paste(ano,mes,dia,sep=""), paste(hora,"   ",niveles,sep=""),"","","","") 
  colnames(tab2) <- formatC(colnames(tab2), width = 7, flag = " ") 
  csvfile <-paste(lista[i],".txt",collapse="") 
  write.fwf(tab2, csvfile, width = rep(7,ncol(tab2)), sep="") 
  rm(f)   rm(lev)   rm(hgt)   rm(tmp)   rm(r_h)   rm(u_w)   rm(v_w)   rm(lev.vec)   rm(hgt.vec) 
  rm(tmp.vec)   rm(r_h.vec)   rm(u_w.vec)   rm(v_w.vec)   rm(tab)   rm(w_s.vec)   rm(t) 
  rm(tab1)   rm(w_d.vec)   rm(tdw.vec)   rm(tab2) 
 
} 
 
txts <- list.files(pattern = "\\.txt$") 
 
combined=NULL 
 
for (file in txts){ 
   
  temp_dataset <-read.table(file, header=FALSE, sep="\t") 
  combined<-rbind(combined, temp_dataset) 
  rm(temp_dataset) 
} 
 
#Export 
 
write.fwf(combined, file = "output.txt", sep="") 
 
rm(combined) 
library(ncdf4)
library(fields)
library(kali)

#variables sst
nc_sst = nc_open("datos/sst.nc4")
sst_var = ncvar_get(nc_sst, names(nc_sst$var))
sst_lat = ncvar_get(nc_sst, "latitude")
sst_lon = ncvar_get(nc_sst, "longitude") - 360
sst_time = ncvar_get(nc_sst, "time")

#variables sss
nc_sss = nc_open("datos/sss.nc4")
sss_var = ncvar_get(nc_sss, names(nc_sss$var))
sss_lat = ncvar_get(nc_sss, "latitude")
sss_lon = ncvar_get(nc_sss, "longitude") - 360
sss_time = ncvar_get(nc_sss, "time")

#variables ph
nc_ph = nc_open("datos/ph.nc4")
ph_var = ncvar_get(nc_ph, names(nc_ph$var))
ph_lat = ncvar_get(nc_ph, "latitude")
ph_lon = ncvar_get(nc_ph, "longitude") - 360
ph_time = ncvar_get(nc_ph, "time")
ph_var_molar=ph_var/1000
log_ph=abs(log10(ph_var_molar))
log_ph

is.na(log_ph)=sapply(log_ph, is.infinite)
summary(log_ph)
Var_1991(log_ph, "pH")
image.plot(sst_lon, sst_lat, log_ph)

#variables sphy
nc_sphy = nc_open("datos/sphy.nc4") 
sphy_var = ncvar_get(nc_sphy, names(nc_sphy$var))
sphy_lat = ncvar_get(nc_sphy, "latitude")
spy_lon = ncvar_get(nc_sphy, "longitude") - 360
sphy_time = ncvar_get(nc_sphy, "time")
sphy_var2=sphy_var*10^6 #micromolar

#variables lphy
nc_lphy = nc_open("datos/lphy.nc4") 
lphy_var = ncvar_get(nc_lphy, names(nc_lphy$var))
lphy_lat = ncvar_get(nc_lphy, "latitude")
lphy_lon = ncvar_get(nc_lphy, "longitude") - 360
lphy_time = ncvar_get(nc_lphy, "time")
lphy_var2=lphy_var*10^6 #micromolar

#variable tphy
tphy_var=lphy_var+sphy_var
tphy2=tphy_var*10^6 #micromolar

?data.frame
ONI=c(0.4, 0.3, 0.2, 0.2, 0.4, 0.6, 0.7, 0.7, 0.7, 0.8, 1.2, 1.4)
ICEN=c(-0.65, -0.47, -0.39, -0.21, -0.08, 0.21, 0.30, 0.35, 0.39, 0.48, 0.61, 0.55)
months=(month.abb)
Tabla=data.frame(months, ONI, ICEN)
Tabla

#atsm 1991
par(oma=c(0,0,0,1))

año_1991=c(385:396)
Anom_1991 = function (var_nc, var_nom, var_lab){
var_mean=apply(var_nc[,,], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
var_1991=apply(var_nc[,,año_1991], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
anom_var=var_mean-var_1991
image.map(sst_lon, sst_lat, anom_var, main = paste("Anomalía de", var_nom,
"durante 1991", sep =" "), legend.lab = var_lab)
}


Var_1991 = function (var_nc, var_nom, var_lab){
  image.map(sst_lon, sst_lat, apply(var_nc[,,año_1991], MARGIN = c(1,2),
  FUN = mean, na.rm = TRUE), main = paste(var_nom, "durante 1991", sep =" "),
  legend.lab = var_lab)
}

var_1991=apply(tphy2[,,año_1991], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
Var_1991(tphy2,"Productividad", "umol C")



Var_1991(sst_var, "Productividad", "umol C")
image.map(sst_lon, sst_lat, sphy_var2[,,año_1991])

Anom_1991(sss_var, "salinidad", "SSSA")
Anom_1991(tphy2, "plancton", "Anomalía de pH")
image.map(sst_lon, sst_lat, sphy_var2[,,1])
lphy_var
nc_lphy
nc_sphy

########### HOV

image.plot(sst_time[año_1991], sst_lat, t(apply(sst_var[,,año_1991], MARGIN = c(2,3),
          FUN = mean, na.rm = TRUE)),
          xlab="", ylab="Latitud", axes = FALSE,
          main = "Variaci?n mensual de la SSS en el Oc?ano Pac?fico")


plot(1:12, apply(sst_var[,,año_1991],
    MARGIN = 3, FUN = mean, na.rm = TRUE), type = "l", 
    lwd=2, col="green", ylim = c(15,30), ylab = "TSM (°C)",
    xlab="", main ="TSM (max, mean & min) durante el año 1991", axes = FALSE, las = 1)
axis(2, at = axTicks(2), las = 1)
axis(1, at = axTicks(1), labels = month.abb[seq(from = 2, to = 12, by = 2)])
lines(1:12, apply(sst_var[,,año_1991], MARGIN = 3, FUN = min, na.rm = TRUE),
      col="blue")
lines(1:12, apply(sst_var[,,año_1991], MARGIN = 3, FUN = max, na.rm = TRUE),
      col="red")
lines(1:12, mean_month, col="black", lty = 2)
lines(1:12, max_month, col="black", lty = 2)
lines(1:12, min_month, col="black", lty = 2)
box()

func=c("min", "max", "mean")

for (op in func){
    assign(paste("month",op,sep="."), vector())
}

for (op in func){
  for (mes in 1:12){
    assign(paste("month",op,sep="."), as.vector((op(apply
                  (sst_var[,,seq(from = mes, to = 552, by = 12)],
                  MARGIN = 3, FUN = op, na.rm = TRUE)))))
   
  }
}

assign(paste("month",op,sep="_"), vector())[[mes]]=(op(apply
              (sst_var[,,seq(from = mes, to = 552, by = 12)],
              MARGIN = 3, FUN = op, na.rm = TRUE)))

#promedio meses
mean_month = vector()
for (mes in 1:12){
  mean_month[[mes]]=(mean(apply(sst_var[,,seq(from = mes, to = 552, by = 12)],
  MARGIN = 3, FUN = mean, na.rm = TRUE)))
}

#max meses
max_month = vector()
for (mes in 1:12){
  max_month[[mes]]=(mean(apply(sst_var[,,seq(from = mes, to = 552, by = 12)],
                                MARGIN = 3, FUN = max, na.rm = TRUE)))
}

#min meses
min_month = vector()
for (mes in 1:12){
  min_month[[mes]]=(mean(apply(sst_var[,,seq(from = mes, to = 552, by = 12)],
                               MARGIN = 3, FUN = min, na.rm = TRUE)))
}


ONI=c(0.4, 0.3, 0.2, 0.2, 0.4, 0.6, 0.7, 0.7, 0.7, 0.8, 1.2, 1.4)
MM_3_meses=c("DJF", "JFM", "FMA", "MAM", "AMJ", "MJJ", "JJA",
        "JAS", "ASO", "SON", "OND", "NDJ")

##Calculo ICEN

mean_1991_1_2=apply(sst_var[4:14,11:21,384:397], 
                    MARGIN = 3, FUN = min, na.rm = TRUE)
MM_1991=as.numeric(na.omit(filter(mean_1991_1_2, rep(1/3,3))))
mean_1_2=apply(sst_var[4:14,11:21,], MARGIN = 3, FUN = min, na.rm = TRUE)
MM_1_2=as.numeric(filter(mean_1_2, rep(1/3,3)))
MM_1_2
meanMM_month_1_2 = vector()
for (mes in 1:12){
  meanMM_month_1_2[[mes]]=mean(MM_1_2[seq(from = mes, to = 552, by = 12)],
                             na.rm=TRUE)
}
meanMM_month_1_2
ICEN_2=round(meanMM_month_1_2-MM_1991, digits=2)
ICEN_2

par(oma=c(0,0,1,2))
Anom_1991(sst_var, "temperatura", "ATSM")



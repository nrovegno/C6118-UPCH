library(ncdf4)
library(fields)
library(kali)

#variables sst
nc_sst = nc_open("datos/sst.nc4")
sst_var = ncvar_get(nc_sst, names(nc_sst$var))
sst_lat = ncvar_get(nc_sst, "latitude")
sst_lon = ncvar_get(nc_sst, "longitude") - 360

#variables sss
nc_sss = nc_open("datos/sss.nc4") 
sss_var = ncvar_get(nc_sss, names(nc_sss$var))
sss_lat = ncvar_get(nc_sss, "latitude")
sss_lon = ncvar_get(nc_sss, "longitude") - 360

#variables ph
nc_ph = nc_open("datos/ph.nc4") 
ph_var = ncvar_get(nc_ph, names(nc_ph$var))
ph_lat = ncvar_get(nc_ph, "latitude")
ph_lon = ncvar_get(nc_ph, "longitude") - 360
log_ph=abs(log10(ph_var)) #-log

#variables sphy
nc_sphy = nc_open("datos/sphy.nc4") 
sphy_var = ncvar_get(nc_sphy, names(nc_sphy$var))
sphy_lat = ncvar_get(nc_sphy, "latitude")
spy_lon = ncvar_get(nc_sphy, "longitude") - 360
sphy_var2=sphy_var*10^6 #micromolar

#variables lphy
nc_lphy = nc_open("datos/lphy.nc4") 
lphy_var = ncvar_get(nc_lphy, names(nc_lphy$var))
lphy_lat = ncvar_get(nc_lphy, "latitude")
lphy_lon = ncvar_get(nc_lphy, "longitude") - 360
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

Anom_1991(sss_var, "salinidad", "SSSA")
Anom_1991(tphy2, "plancton", "Anomalía de pH")
image.map(sst_lon, sst_lat, sphy_var2[,,1])
lphy_var
nc_lphy
nc_sphy

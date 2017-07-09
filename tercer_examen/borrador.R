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

#variables sphy
nc_sphy = nc_open("datos/sphy.nc4") 
sphy_var = ncvar_get(nc_sphy, names(nc_sphy$var))
sphy_lat = ncvar_get(nc_sphy, "latitude")
spy_lon = ncvar_get(nc_sphy, "longitude") - 360

#variables lphy
nc_lphy = nc_open("datos/lphy.nc4") 
lphy_var = ncvar_get(nc_lphy, names(nc_lphy$var))
lphy_lat = ncvar_get(nc_lphy, "latitude")
lphy_lon = ncvar_get(nc_lphy, "longitude") - 360

#atsm 1991
par(oma=c(0,0,0,1))
sst_mean=apply(sst_var[,,1:552], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
sst_1991=apply(sst_var[,,385:396], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
anom_sst=sst_mean-sst_1991
image.map(sst_lon, sst_lat, sst_mean, legend.lab = "TSM (°C)")
image.map(sst_lon, sst_lat, anom_sst, legend.lab = "ATSM (°C)", 
        main ="Anomalía de la TSM durante el año 1991")

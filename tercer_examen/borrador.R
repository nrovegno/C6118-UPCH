library(ncdf4)
library(fields)
library(kali)

nc = nc_open("datos/sst.nc4")
sst = ncvar_get(nc, "to")
lat = ncvar_get(nc, "latitude")
lon = ncvar_get(nc, "longitude") - 360
image.map(lon, lat, sst[,,1])
sst_mean=apply(sst[,,1:552], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
image.map(lon, lat, sst_mean)
sst_1991=apply(sst[,,384:396], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
anom_sst=sst_mean-sst_1991
image.map(lon, lat, anom_sst)
anom_sst

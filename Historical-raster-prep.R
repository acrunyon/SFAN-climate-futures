library(ggplot2)
library(dplyr)
library(sf)
library(terra)


OutDir <- "~/Projects/SFAN-P4CC-workshop/Historical/rasters/"

nps_centroids <- st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary/nps_boundary.shp')
centroid <- filter(nps_centroids, UNIT_CODE %in% c("GOGA","PINN","PORE"))

box <- st_bbox(centroid)
expand_by <- .2
expanded_bbox <- st_bbox(c(
  xmin = as.numeric(box[1] - expand_by),
  ymin = as.numeric(box[2] - expand_by),
  xmax = as.numeric(box[3] + expand_by),
  ymax = as.numeric(box[4] + expand_by)
), crs = st_crs(box))  # Include CRS if necessary

bbox_polygon <- st_as_sfc(expanded_bbox)

ggplot() +
  geom_sf(data = bbox_polygon, fill = "lightblue", color = "darkblue") +  # Bounding box
  geom_sf(data = centroid[1]) +     # Polygon
  theme_minimal()


## Precip
file <- "D:/nclim_2302/nclimgrid_prcp.nc"
raster.obj = terra::rast(file)

crop_raster <- terra::crop(raster.obj, bbox_polygon) 

plot(crop_raster[[1]])

y <- tapp(crop_raster, "years", sum) 

y<- subset(y, time(y) > "1970") 
y<-y/25.4
r <- regress(y, 1:nlyr(y))

plot(r$x*100)
p <- r$x*100
writeRaster(p, paste0(OutDir,"Hist-precip-regress.tif"), filetype = "GTiff", overwrite = TRUE)



## Temp
file <- "D:/nclim_2302/nclimgrid_tavg.nc"
raster.obj = terra::rast(file)

crop_raster <- terra::crop(raster.obj, bbox_polygon) 

plot(crop_raster[[1]])

y <- tapp(crop_raster, "years", mean) 

y<- subset(y, time(y) > "1970") 
y<-y* 9/5 + 32
r <- regress(y, 1:nlyr(y))
plot(r$x*100)
t <- r$x*100

writeRaster(t, paste0(OutDir,"Hist-temp-regress.tif"), filetype = "GTiff", overwrite = TRUE)

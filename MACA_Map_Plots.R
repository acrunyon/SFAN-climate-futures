library(stringr)
library(tidyverse)
library(basemaps)
library(rasterVis)
# Link to MACA tifs https://climate.northwestknowledge.net/PATH_TO_TIFS/MACAV2METDATA/TIF/
# WB from Tercek http://screenedcleanedsummaries.s3-website-us-west-2.amazonaws.com/
library(ggplot2)
library(tidyterra)
library(ggthemes)
library(viridis)
library(readxl)
library(gcookbook)
library(lemon)
library(grid)
library(gridExtra)
library(ggpubr)
library(terra)
library(dplyr)
library(sf)

rm(list=ls())

OutDir <- "~/Projects/SFAN-P4CC-workshop/maps/"
tifDir <- "~/Projects/SFAN-P4CC-workshop/maps/geotiffs/"


CFs <- c("Warm Wet", "Hot Dry")
cols <- c("#2B83BA", "#D7191C")

## Get CF info
GCMs <- c("CNRM-CM5.rcp45","MIROC-ESM-CHEM.rcp85")

CF.GCM <- data.frame(CF=CFs,GCM=GCMs)
CF1 <- CF.GCM |> filter(CF == CFs[1])
CF2 <- CF.GCM |> filter(CF == CFs[2])

## Extract spatial info

nps_boundary <- sf::st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary/nps_boundary.shp')
park <- filter(nps_boundary, UNIT_CODE %in% c("GOGA","PINN","PORE"))

box <- st_bbox(park)
expand_by <- .2
expanded_bbox <- st_bbox(c(
  xmin = as.numeric(box[1] - expand_by),
  ymin = as.numeric(box[2] - expand_by),
  xmax = as.numeric(box[3] + expand_by),
  ymax = as.numeric(box[4] + expand_by)
), crs = st_crs(box))  # Include CRS if necessary

bbox_polygon <- st_as_sfc(expanded_bbox)
polygons.crs <- st_crs(bbox_polygon)

#Get basemaps
x <- basemap_raster(bbox_polygon, map_service = "esri", map_type = "world_hillshade") #world_street_map great but need simpler; world_imagery too dark;
x_terr <- rast(x)


# read in files from variable and CF names
#Find file name that contains var and CF | historical

# sub("\\..*", "", CF1$GCM) # Extract part before period
# sub('.*\\.', '', CF1$GCM) # Extract part after period

# function to find 
map.file <- function(GCM.name){
  CF1.file <- map(c(sub('.*\\.', '', GCM.name), sub("\\..*", "", GCM.name)), 
                  str_detect, string = list.files(path=tifDir, pattern = Var)) %>%
    reduce(`&`) %>% 
    magrittr::extract(list.files(path=tifDir, pattern = Var), .)
}

map.file.season <- function(GCM.name,seas){
  CF1.file <- map(c(sub('.*\\.', '', GCM.name), sub("\\..*", "", GCM.name)), 
                  str_detect, string = list.files(path=tifDir, pattern = paste0(Var,"_",seas))) %>%
    reduce(`&`) %>% 
    magrittr::extract(list.files(path=tifDir, pattern = paste0(Var,"_",seas)), .)
}

# Hist plot
historical.plot <- function(rast){
  ggplot() +
    geom_spatraster_rgb(data = x_terr) +
    geom_spatraster(data = rast, alpha=.8)+
    geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=.75) +
    # scale_fill_viridis(direction=-1, option = "viridis", oob = scales::squish) + 
    scale_fill_gradientn(colours = div.pal$Hex) +
    labs(title = "Historical",fill =units) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.text=element_text(size=10), legend.title=element_text(size=12),
          # plot.title=element_blank(),
          plot.title=element_text(size=14,face="bold",hjust=0.5),
          plot.background = element_rect(colour = "gray", fill=NA, linewidth=5),
          plot.margin = unit(c(.5,0,0,0), "cm")) + 
    labs(fill =  units)
}  

map.plot.div <- function(data, title,xaxis,metric,col){ #use with divergent palettes, best for absolute metrics
  ggplot() +
    geom_spatraster_rgb(data = x_terr) +
    geom_spatraster(data = data, alpha=.8)+
    geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=.75) +
    # scale_fill_viridis(direction=1, option = "mako", 
    #                    limits = c(scale.min, scale.max), oob = scales::squish) + 
    scale_fill_gradientn(colours = div.pal$Hex, rescaler = ~ scales::rescale_mid(.x, mid = 0),
                         limits = c(scale.min, scale.max)) +
    # scale_fill_gradient2(colours = div.pal$Hex,
    #                      low=scale.min, mid=0, high=scale.max, oob = scales::squish) +
    labs(title = title, fill=metric,legend=units) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.text=element_text(size=10), legend.title=element_text(size=12),
          legend.title=element_blank(),
          # plot.title=element_blank(),
          plot.title=element_text(size=14,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, linewidth=5),
          plot.margin = unit(c(.5,0,0,0), "cm"))  
  # labs(fill =metric) +
  # guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
}

map.plot.seq <- function(data, title,xaxis,metric,col){ #use with sequence palettes (best for temp)
  ggplot() +
    geom_spatraster_rgb(data = x_terr) +
    geom_spatraster(data = data, alpha=.8)+
    geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=.75) +
    # scale_fill_viridis(direction=1, option = "mako", 
    #                    limits = c(scale.min, scale.max), oob = scales::squish) + 
    scale_fill_gradientn(colours = seq.pal$Hex,
                         limits = c(scale.min, scale.max)) +
    # scale_fill_gradient2(colours = div.pal$Hex,
    #                      low=scale.min, mid=0, high=scale.max, oob = scales::squish) +
    labs(title = title, fill=metric,legend=units) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.text=element_text(size=10), legend.title=element_text(size=12),
          # plot.title=element_blank(),
          plot.title=element_text(size=14,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, linewidth=5),
          plot.margin = unit(c(.5,0,0,0), "cm"))  
  # labs(fill =metric) +
  # guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
}

map.plot.seq.zero <- function(data, title,xaxis,metric,col){ #use with divergent patterns centered on zero (best for wetter/drier palettes)
  ggplot() +
    geom_spatraster_rgb(data = x_terr) +
    geom_spatraster(data = data, alpha=.8)+
    geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=.75) +
    scale_fill_gradientn(colours = div.pal$Hex,
                         values = scales::rescale(c(scale.min,0,scale.max)),
                         guide = "colorbar", limits=c(scale.min,scale.max)) +
    labs(title = title, fill=metric,legend=units) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.text=element_text(size=10), legend.title=element_text(size=12),
          # plot.title=element_blank(),
          plot.title=element_text(size=14,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, linewidth=5),
          plot.margin = unit(c(.5,0,0,0), "cm"))
  # labs(fill =metric) +
  # guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
}

map.plot.seq.pos <- function(data, title,xaxis,metric,col){ #use with divergent patterns where all values are positive but just want to show drier side
  ggplot() +
    geom_spatraster_rgb(data = x_terr) +
    geom_spatraster(data = data, alpha=.8)+
    geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=.75) +
    scale_fill_gradientn(colours = div.pal$Hex,
                         values = scales::rescale(c(0,scale.min,scale.max)),
                         guide = "colorbar", limits=c(0,scale.max)) +
    labs(title = title, fill=metric,legend=units) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.text=element_text(size=10), legend.title=element_text(size=12),
          # plot.title=element_blank(),
          plot.title=element_text(size=14,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, linewidth=5),
          plot.margin = unit(c(.5,0,0,0), "cm"))
  # labs(fill =metric) +
  # guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
}

######################################################
### For each variable
## Temp
# Var
Var = "tasmean_ANN" #tasmean_ANN, pr_ANN, Deficit
long.title = "Annual Temperature (°F)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
units = "(°F)" #(°F), (in/year)

#Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
# temp = temp_div , temp_seq
# prcp = prec_div , prec_seq
# snow = cryo_div , cryo_seq
VarType = "temp" #temp, prec, cryo

inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
# SheetNames

# first sheet is only figures of color maps
for(i in 2:length(SheetNames)){
  # cat(str_c("i ->  ", i , "   "))
  d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
  d1$PalName <- SheetNames[i]
  d1 <- na.omit(d1)
  ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
}

# head(PalData)
PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)

div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))

if(Var=="Deficit") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
# 
# # test palettes
# hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
#   geom_point(size = 3)
# hw_plot + scale_color_gradientn(colours = div.pal$Hex)

Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")
CF1.file <- map.file(CF1$GCM) 
CF2.file <- map.file(CF2$GCM) 

Hist.rast <- rast(paste0(tifDir, Hist.file)) #Read in raster 
CF1.rast <- rast(paste0(tifDir, CF1.file)) #Read in raster 
CF2.rast <- rast(paste0(tifDir, CF2.file)) 

# Need to flip MACA WB data
# Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
# CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
# CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it

# # Clip raster to park

Hist.rast <- crop(project(Hist.rast, polygons.crs$wkt), bbox_polygon)
CF1.rast.temp <- crop(project(CF1.rast, polygons.crs$wkt), bbox_polygon)
CF2.rast.temp <- crop(project(CF2.rast, polygons.crs$wkt), bbox_polygon)

# #Tercek plots code -- make delta files, convert to inches

# 
# CF1.rast <- CF1.rast.temp - Hist.rast
# CF2.rast <- CF2.rast.temp - Hist.rast
# 
# Hist.rast <- Hist.rast/25.4
# CF1.rast <- CF1.rast/25.4
# CF2.rast <- CF2.rast/25.4

# Historical plot
Hist.plot <- historical.plot(Hist.rast)
Hist.plot
  
  # Future delta plots
scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])

cf1.plot <- map.plot.seq(data=CF1.rast, title=CFs[1],metric=paste0("Change in ",units),col=cols[1])
cf2.plot <- map.plot.seq(data=CF2.rast, title=CFs[2],metric=paste0("Change in ",units),col=cols[2])


# Merge into one plot

maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                   top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                  gp=gpar(fontface="bold", col="black", fontsize=18)))

hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                   face = "bold", size = 18))

a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
a

if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
ggsave(paste0("SFAN-", long.title,".png"), width = 13, height = 11, path = OutDir,bg="white", a)


### Precip
### For each variable
# Var
Var = "pr_ANN" #tasmean_ANN, pr_ANN, Deficit
long.title = "Annual Precipitation (in/year)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
units = "(in/year)" #(°F), (in/year)

#Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
# temp = temp_div , temp_seq
# prcp = prec_div , prec_seq
# snow = cryo_div , cryo_seq
VarType = "prec" #temp, prec, cryo

inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
# SheetNames

# first sheet is only figures of color maps
for(i in 2:length(SheetNames)){
  # cat(str_c("i ->  ", i , "   "))
  d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
  d1$PalName <- SheetNames[i]
  d1 <- na.omit(d1)
  ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
}

# head(PalData)
PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)

div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))

if(Var=="Deficit") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
# 
# # test palettes
# hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
#   geom_point(size = 3)
# hw_plot + scale_color_gradientn(colours = div.pal$Hex)

Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")
CF1.file <- map.file(CF1$GCM) 
CF2.file <- map.file(CF2$GCM) 

Hist.rast <- rast(paste0(tifDir, Hist.file)) #Read in raster 
CF1.rast <- rast(paste0(tifDir, CF1.file)) #Read in raster 
CF2.rast <- rast(paste0(tifDir, CF2.file)) 

# Need to flip MACA WB data
# Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
# CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
# CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it

# # Clip raster to park

Hist.rast <- crop(Hist.rast, bbox_polygon)
CF1.rast <- crop(CF1.rast, bbox_polygon)
CF2.rast <- crop(CF2.rast, bbox_polygon)

# #Tercek plots code -- clip raster to park and make delta files, convert to in
# 
# Hist.rast <- crop(project(Hist.rast, crs(bbox_polygon)), bbox_polygon)
# CF1.rast.temp <- crop(project(CF1.rast, crs(bbox_polygon)), bbox_polygon)
# CF2.rast.temp <- crop(project(CF2.rast, crs(bbox_polygon)), bbox_polygon)
# 
# CF1.rast <- CF1.rast.temp - Hist.rast
# CF2.rast <- CF2.rast.temp - Hist.rast
# 
# Hist.rast <- Hist.rast/25.4
# CF1.rast <- CF1.rast/25.4
# CF2.rast <- CF2.rast/25.4

# Historical plot
Hist.plot <- historical.plot(Hist.rast)
Hist.plot

# Future delta plots
scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])

cf1.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])} else{
  map.plot.seq.pos(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])
}
cf2.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])} else{
  map.plot.seq.pos(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])
}


# Merge into one plot

maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                   top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                  gp=gpar(fontface="bold", col="black", fontsize=18)))

hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                                       face = "bold", size = 18))

a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
a

if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
ggsave(paste0("SFAN-", long.title,".png"), width = 13, height = 11, path = OutDir,bg="white", a)


##########################
# Tercek plots # 
##########################
  ### Deficit
  ### For each variable
  # Var
  Var = "Deficit" #tasmean_ANN, pr_ANN, Deficit
  long.title = "Annual Climatic Water Deficit (in/year)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
  units = "(in/year)" #(°F), (in/year)
  
  #Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
  # temp = temp_div , temp_seq
  # prcp = prec_div , prec_seq
  # snow = cryo_div , cryo_seq
  VarType = "prec" #temp, prec, cryo
  
  inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
  SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
  # SheetNames
  
  # first sheet is only figures of color maps
  for(i in 2:length(SheetNames)){
    # cat(str_c("i ->  ", i , "   "))
    d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
    d1$PalName <- SheetNames[i]
    d1 <- na.omit(d1)
    ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
  }
  
  # head(PalData)
  PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)
  
  div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
  seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))
  
  if(Var=="Deficit") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
  # 
  # # test palettes
  # hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
  #   geom_point(size = 3)
  # hw_plot + scale_color_gradientn(colours = div.pal$Hex)
  
  Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")
  CF1.file <- map.file(CF1$GCM) 
  CF2.file <- map.file(CF2$GCM) 
  
  # For Tercek files, need to read in as raster::raster files then convert 
  Hist.rast <- raster::raster(paste0(tifDir, Hist.file))
  CF1.rast <- raster::raster(paste0(tifDir, CF1.file)) #Read in raster 
  CF2.rast <- raster::raster(paste0(tifDir, CF2.file))
  
  b <- bbox_polygon
  raster_crs <- crs(R.Hist.rast, describe = TRUE)
  b<- st_transform(b, crs = raster_crs)
  b.box <- st_bbox(b)
  
  H <- crop(Hist.rast, b.box)
  h <- rast(H)
  C1 <- crop(CF1.rast, b.box)
  c1 <- rast(C1)
  C2 <- crop(CF2.rast, b.box)
  c2 <- rast(C2)
  
  Hist.rast <- project(h, polygons.crs$wkt)
  CF1.rast.temp <- project(c1, polygons.crs$wkt)
  CF2.rast.temp <- project(c2, polygons.crs$wkt)
  rm(H,h,C1,c1,C2,c2)
  ##############################
  
  CF1.rast <- CF1.rast.temp - Hist.rast
  CF2.rast <- CF2.rast.temp - Hist.rast
  
  Hist.rast <- Hist.rast/25.4
  CF1.rast <- CF1.rast/25.4
  CF2.rast <- CF2.rast/25.4
  
  # Historical plot
  Hist.plot <- historical.plot(Hist.rast)
  Hist.plot
  
  # Future delta plots
  scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
  scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])
  
  cf1.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])} else{
    map.plot.seq.pos(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])
  }
  cf2.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])} else{
    map.plot.seq.pos(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])
  }
  
  
  # Merge into one plot
  
  maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                     top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                    gp=gpar(fontface="bold", col="black", fontsize=18)))
  
  hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                                         face = "bold", size = 18))
  
  a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
  a
  
  if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
  ggsave(paste0("SFAN-", long.title,".png"), width = 13, height = 11, path = OutDir,bg="white", a)



  ### Tercek runoff
  ### For each variable
  # Var
  Var = "runoff" #tasmean_ANN, pr_ANN, Deficit
  long.title = "Annual excess water (runoff; in/year)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
  units = "(in/year)" #(°F), (in/year)
  
  #Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
  # temp = temp_div , temp_seq
  # prcp = prec_div , prec_seq
  # snow = cryo_div , cryo_seq
  VarType = "cryo" #temp, prec, cryo
  
  inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
  SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
  # SheetNames
  
  # first sheet is only figures of color maps
  for(i in 2:length(SheetNames)){
    # cat(str_c("i ->  ", i , "   "))
    d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
    d1$PalName <- SheetNames[i]
    d1 <- na.omit(d1)
    ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
  }
  
  # head(PalData)
  PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)
  
  div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
  seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))
  
  # if(Var=="runoff") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
  # 
  # # test palettes
  # hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
  #   geom_point(size = 3)
  # hw_plot + scale_color_gradientn(colours = div.pal$Hex)
  
  Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")
  CF1.file <- map.file(CF1$GCM) 
  CF2.file <- map.file(CF2$GCM) 
  
  # For Tercek files, need to read in as raster::raster files then convert 
  Hist.rast <- raster::raster(paste0(tifDir, Hist.file))
  CF1.rast <- raster::raster(paste0(tifDir, CF1.file)) #Read in raster 
  CF2.rast <- raster::raster(paste0(tifDir, CF2.file))
  
  H <- crop(Hist.rast, b.box)
  h <- rast(H)
  C1 <- crop(CF1.rast, b.box)
  c1 <- rast(C1)
  C2 <- crop(CF2.rast, b.box)
  c2 <- rast(C2)
  
  Hist.rast <- project(h, polygons.crs$wkt)
  CF1.rast.temp <- project(c1, polygons.crs$wkt)
  CF2.rast.temp <- project(c2, polygons.crs$wkt)
  rm(H,h,C1,c1,C2,c2)
  
  CF1.rast <- CF1.rast.temp - Hist.rast
  CF2.rast <- CF2.rast.temp - Hist.rast
  
  Hist.rast <- Hist.rast/25.4
  CF1.rast <- CF1.rast/25.4
  CF2.rast <- CF2.rast/25.4
  
  # Historical plot
  Hist.plot <- historical.plot(Hist.rast)
  Hist.plot
  
  # Future delta plots
  scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
  scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])
  
  cf1.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])} else{
    map.plot.seq.pos(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])
  }
  cf2.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])} else{
    map.plot.seq.pos(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])
  }
  
  
  # Merge into one plot
  
  maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                     top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                    gp=gpar(fontface="bold", col="black", fontsize=18)))
  
  hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                                         face = "bold", size = 18))
  
  a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
  a
  
  if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
  ggsave(paste0("SFAN-", long.title,".png"), width = 13, height = 11, path = OutDir,bg="white", a)
  
  
  
  ### Tercek runoff
  ### For each variable
  # Var
  Var = "soil_water" #tasmean_ANN, pr_ANN, Deficit
  long.title = "Annual soil moisture (in/year)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
  units = "(in/year)" #(°F), (in/year)
  
  #Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
  # temp = temp_div , temp_seq
  # prcp = prec_div , prec_seq
  # snow = cryo_div , cryo_seq
  VarType = "prec" #temp, prec, cryo
  
  inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
  SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
  # SheetNames
  
  # first sheet is only figures of color maps
  for(i in 2:length(SheetNames)){
    # cat(str_c("i ->  ", i , "   "))
    d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
    d1$PalName <- SheetNames[i]
    d1 <- na.omit(d1)
    ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
  }
  
  # head(PalData)
  PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)
  
  div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
  seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))
  
  if(Var=="runoff") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
  # 
  # # test palettes
  # hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
  #   geom_point(size = 3)
  # hw_plot + scale_color_gradientn(colours = div.pal$Hex)
  
  Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")
  CF1.file <- map.file(CF1$GCM) 
  CF2.file <- map.file(CF2$GCM) 
  
  # For Tercek files, need to read in as raster::raster files then convert 
  Hist.rast <- raster::raster(paste0(tifDir, Hist.file))
  CF1.rast <- raster::raster(paste0(tifDir, CF1.file)) #Read in raster 
  CF2.rast <- raster::raster(paste0(tifDir, CF2.file))
  
  H <- crop(Hist.rast, b.box)
  h <- rast(H)
  C1 <- crop(CF1.rast, b.box)
  c1 <- rast(C1)
  C2 <- crop(CF2.rast, b.box)
  c2 <- rast(C2)
  
  Hist.rast <- project(h, polygons.crs$wkt)
  CF1.rast.temp <- project(c1, polygons.crs$wkt)
  CF2.rast.temp <- project(c2, polygons.crs$wkt)
  rm(H,h,C1,c1,C2,c2)
  
  CF1.rast <- CF1.rast.temp - Hist.rast
  CF2.rast <- CF2.rast.temp - Hist.rast
  
  Hist.rast <- Hist.rast/25.4
  CF1.rast <- CF1.rast/25.4
  CF2.rast <- CF2.rast/25.4
  
  # Historical plot
  Hist.plot <- historical.plot(Hist.rast)
  Hist.plot
  
  # Future delta plots
  scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
  scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])
  
  cf1.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])} else{
    map.plot.seq.pos(data=CF1.rast, title=CFs[1],metric=paste0("Change in\n",units),col=cols[1])
  }
  cf2.plot <- if(scale.min < 0) {map.plot.seq.zero(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])} else{
    map.plot.seq.pos(data=CF2.rast, title=CFs[2],metric=paste0("Change in\n",units),col=cols[2])
  }
  
  
  # Merge into one plot
  
  maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                     top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                    gp=gpar(fontface="bold", col="black", fontsize=18)))
  
  hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                                         face = "bold", size = 18))
  
  a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
  a
  
  if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
  ggsave(paste0("SFAN-", long.title,".png"), width = 13, height = 11, path = OutDir,bg="white", a)

rm(list=ls())
GCMs <- c("CNRM-CM5.rcp45","MIROC-ESM-CHEM.rcp85")

tifDir <- "~/Projects/SFAN-P4CC-workshop/maps/geotiffs/"


## Download from MACA server
period <- "20402069"

downloads <- data.frame(projection=GCMs)
downloads$GCM <- sub("\\..*", "", downloads$projection) #everything before period
downloads$RCP <- sub('.*\\.', '', downloads$projection) #everything after period

downloads$tasmean_ANN <- paste0("https://climate.northwestknowledge.net/PATH_TO_TIFS/MACAV2METDATA/TIF/macav2metdata_tasmean_ANN_",
                                period,"_",downloads$RCP,"_vs_19712000_",downloads$GCM,".tif")

downloads$pr_ANN <- paste0("https://climate.northwestknowledge.net/PATH_TO_TIFS/MACAV2METDATA/TIF/macav2metdata_pr_ANN_",
                                period,"_",downloads$RCP,"_vs_19712000_",downloads$GCM,".tif")

for (i in 1:nrow(downloads)){ #Run as loop to avoid timeout - which happens when calling too many units
  # for (i in 351:length(links)-1){
  file.name <- paste0("macav2metdata_tasmean_ANN_",period,"_",downloads$RCP[i],"_vs_19712000_",downloads$GCM[i],".tif")
  download.file(downloads$tasmean_ANN[i], paste(tifDir, file.name ,sep = ""), mode = "wb")
}

for (i in 1:nrow(downloads)){ #Run as loop to avoid timeout - which happens when calling too many units
  # for (i in 351:length(links)-1){
  file.name <- paste0("macav2metdata_pr_ANN_",period,"_",downloads$RCP[i],"_vs_19712000_",downloads$GCM[i],".tif")
  download.file(downloads$pr_ANN[i], paste(tifDir, file.name ,sep = ""), mode = "wb")
}


# Tercek wb downloads
wb.tercek.dls <- downloads

wb.tercek.dls$deficit_ANN <- paste0("http://screenedcleanedsummaries.s3-website-us-west-2.amazonaws.com/Deficit/",wb.tercek.dls$RCP,
                                    "/V_1_5_annual_",wb.tercek.dls$GCM,"_",wb.tercek.dls$RCP,"_Deficit_2040_2069_annual_means_cropped_units_mm.tif")

wb.tercek.dls$runoff_ANN <- paste0("http://screenedcleanedsummaries.s3-website-us-west-2.amazonaws.com/runoff/",wb.tercek.dls$RCP,
                                    "/V_1_5_annual_",wb.tercek.dls$GCM,"_",wb.tercek.dls$RCP,"_runoff_2040_2069_annual_means_cropped_units_mm.tif")

wb.tercek.dls$soil_water_ANN <- paste0("http://screenedcleanedsummaries.s3-website-us-west-2.amazonaws.com/soil_water/",wb.tercek.dls$RCP,
                                   "/V_1_5_annual_",wb.tercek.dls$GCM,"_",wb.tercek.dls$RCP,"_soil_water_2040_2069_annual_means_cropped_units_mm.tif")



for (i in 1:nrow(wb.tercek.dls)){ #Run as loop to avoid timeout - which happens when calling too many units
  # for (i in 351:length(links)-1){
  file.name <- paste0("V_1_5_annual_",wb.tercek.dls$GCM[i],"_",wb.tercek.dls$RCP[i],"_Deficit_2040_2069_annual_means_cropped_units_mm.tif")
  download.file(wb.tercek.dls$deficit_ANN[i], paste(tifDir, file.name ,sep = ""), mode = "wb")
}

for (i in 1:nrow(wb.tercek.dls)){ #Run as loop to avoid timeout - which happens when calling too many units
  # for (i in 351:length(links)-1){
  file.name <- paste0("V_1_5_annual_",wb.tercek.dls$GCM[i],"_",wb.tercek.dls$RCP[i],"_runoff_2040_2069_annual_means_cropped_units_mm.tif")
  download.file(wb.tercek.dls$runoff_ANN[i], paste(tifDir, file.name ,sep = ""), mode = "wb")
}

for (i in 1:nrow(wb.tercek.dls)){ #Run as loop to avoid timeout - which happens when calling too many units
  # for (i in 351:length(links)-1){
  file.name <- paste0("V_1_5_annual_",wb.tercek.dls$GCM[i],"_",wb.tercek.dls$RCP[i],"_soil_water_2040_2069_annual_means_cropped_units_mm.tif")
  download.file(wb.tercek.dls$soil_water_ANN[i], paste(tifDir, file.name ,sep = ""), mode = "wb")
}

write.csv(downloads,paste0(tifDir,"downloads.for.maps.csv"),row.names = FALSE)

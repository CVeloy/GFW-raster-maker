year <- seq(2013, 2019,1) # list of year for fishing data. Fishing info must be downloaded and stored locally
gfw_x_month <- list()
gfw_x_year <- list()

dir.create (file.path("F:/TEMP/"), showWarnings = FALSE) #throwaway folder for temp files storage, make sure it's in a disk with at least 180/190 GB of free space
	#D:/DPbox/Dropbox/Tesis Carlos Veloy/Scripts Carlos
	#C:/Users/carli/Dropbox/Tesis Carlos Veloy/Scripts Carlos
	InitialT<-Sys.time()
for (i in 1:length(year)){
  #gfw_file <- dir(paste("D:/DPbox/Dropbox/Tesis Carlos Veloy/Scripts Carlos/Data/FISHING_EFFORT/GFW_v2/Raster Fisheries/", year[i], sep=""), pattern=".csv") #list of files
  
 #dir <- "D:/DPbox/Dropbox/Tesis Carlos Veloy/Scripts Carlos/Data/FISHING_EFFORT/GFW_v2/Raster Fisheries"
   	### In case you were interested in a particular period, just selecte (subset) the dates from gfw_file object
	   dir.create (file.path("F:/TEMP/"), showWarnings = FALSE)
   for(m in 1:12){
   gfw<-raster(paste("C:/Users/carli/Dropbox/Tesis Carlos Veloy/Scripts Carlos/Data/FISHING_EFFORT/GFW_v2/Rasters_Fisheries/gfw",year[i],"_",m,".tif",sep=""))
		gfw_x_month[m] <- gfw
		print(Sys.time())
}
	FinalT<-Sys.time()
	gfw_x_month$fun<-sum
	gfw_x_month$na.rm<-TRUE
	gfw_x_month_sum<-do.call(mosaic,gfw_x_month)
	gfw_x_year[m] <- gfw_x_month_sum 
	writeRaster(gfw_x_month_sum, filename = paste("D:/DPbox/Dropbox/Tesis Carlos Veloy/Scripts Carlos/Data/FISHING_EFFORT/GFW_v2/Rasters_Fisheries/Full Years/gfw", year[i],"_Full",".tif", sep=''),overwrite=TRUE)
	Sys.time
	remove(gfw)
	unlink(file.path("F:/TEMP/"), recursive = TRUE)
	gc()
}		

FinalTF<-Sys.time()

##Make database with restricting by SPHs
SectorGSA6 <- read_sf(dsn = "C:/Users/Carlos Veloy/Dropbox/Tesis Carlos Veloy/Scripts Carlos/Shapefiles sectors/ALL SECTORS", layer = "WMedsector")

coordinates(SectorGSA6) <- x + y

for(i in 1:length(year)){
   rasterFE<-raster(paste("C:/Users/carli/Dropbox/Tesis Carlos Veloy/Scripts Carlos/Data/FISHING_EFFORT/GFW_v2/Rasters_Fisheries/Full years/gfw",year[i],"_Full.tif",sep=""))
rastername<-paste("gfw",year[i],"_Full",sep="")
FE_re <- raster::extract(rasterFE, SectorGSA6, fun = sum, na.rm = TRUE, sp = TRUE)
fedf<-as.data.frame(FE_re)
fedf<-dplyr::select(fedf,-(path),-(id))%>%
    rename("Fishing_effort(sumhours)" := (!!quo_name(rastername)))%>%
	mutate(year=year[i])
assign((paste("FE_regdf_year",year[i],sep="")),fedf)
df_format<-data.frame(matrix(ncol = (ncol(fedf)), nrow = 0))

}
colnames(df_format)<-colnames(fedf)
DF_FE<-df_format
DF_FE$layer<-as.character()
for(i in 1:length(year))
{
rasteryear<-paste("FE_regdf_year",year[i],sep="")
DF_FE<-dplyr::full_join(DF_FE,(get(rasteryear)))
}
DF_FE<-rename(DF_FE,region = layer)
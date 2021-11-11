year <- seq(2013, 2019,1) # list of year for fishing data. Fishing info must be downloaded and stored locally
gfw_x_month <- list()
gfw_x_year <- list()
directory<-paste(paste(""),#paste directory where FISHING_EFFORT folder is located
		 "/FISHING_EFFORT/GFW_v2/Raster Fisheries/",sep="")

dir.create (file.path("F:/TEMP/"), showWarnings = FALSE) #throwaway folder for temp files storage, make sure it's in a disk with at least 180/190 GB of free space
	
	InitialT<-Sys.time()
for (i in 1:length(year)){
  #gfw_file <- dir(paste(directory, year[i], sep=""), pattern=".csv") #list of files
  
 #dir <- "D:/DPbox/Dropbox/Tesis Carlos Veloy/Scripts Carlos/Data/FISHING_EFFORT/GFW_v2/Raster Fisheries"
   	### In case you were interested in a particular period, just selecte (subset) the dates from gfw_file object
	   dir.create (file.path("F:/TEMP/"), showWarnings = FALSE)
   for(m in 1:12){
   gfw<-raster(paste(directory,"gfw",year[i],"_",m,".tif",sep=""))
		gfw_x_month[m] <- gfw
		print(Sys.time())
}
	FinalT<-Sys.time()
	gfw_x_month$fun<-sum
	gfw_x_month$na.rm<-TRUE
	gfw_x_month_sum<-do.call(mosaic,gfw_x_month)
	gfw_x_year[m] <- gfw_x_month_sum 
	writeRaster(gfw_x_month_sum, filename = paste(directory,"Full Years/gfw", year[i],"_Full",".tif", sep=''),overwrite=TRUE)
	Sys.time
	remove(gfw)
	unlink(file.path("F:/TEMP/"), recursive = TRUE)
	gc()
}		

FinalTF<-Sys.time()

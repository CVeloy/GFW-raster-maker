#############################################################
#############################################################
#####        		   GFW data processing 			     ####
#############################################################
#############################################################

install.packages("tidyverse") # for general data wrangling and plotting
install.packages("furrr") # for parallel operations on lists
install.packages("lubridate") # for working with dates
install.packages("sf") # for vector data 
install.packages("raster") # for working with rasters
install.packages("maps") # additional helpful mapping packages
install.packages("maptools")
install.packages("rgeos")
	
# Load packages
library(tidyverse) # for general data wrangling and plotting
library(furrr) # for parallel operations on lists
library(lubridate) # for working with dates
library(sf) # for vector data 
library(raster) # for working with rasters
library(maps) # additional helpful mapping packages
library(maptools)
library(rgeos)
library(rgdal)
##############################################################################################
# Resulting daily raster may have different extents, which prevents from operating among them.
# In order to consider a common extent, this function gives the max extent for a list of rasters.

#Get maximum extent (not used in new method)
getMaxExtent <- function(rasters) {
  extents <- sapply(rasters, FUN = function(x) {
    raster::extent(x)
  })
  r <- raster(ext = extents[[1]], nrows = rasters[[1]]@nrows, ncols = rasters[[1]]@ncols)
  max_extent <- sapply(extents, FUN = function(x) {
    r <<- raster::extend(r, x,value=0)
  })
  raster::extent(r)
}

# and this other function will allow to sum all rasters using the max extent in the list of rasters
#Sum_all (not used in new method)
sum_all =
function(rasters, extent){
 re = lapply(rasters, function(r){extend(r,extent,value=0)})
 Reduce("+", re)
}
##############################################################################################

year <- seq(2013, 2019,1) # list of year for fishing data. Fishing info must be downloaded and stored locally
gfw_x_month <- list()
dir.create (file.path("F:/TEMP/"), showWarnings = FALSE) #throwaway folder for temp files storage, make sure it's in a disk with at least 180/190 GB of free space
	#D:/DPbox/Dropbox/Tesis Carlos Veloy/Scripts Carlos
	#C:/Users/carli/Dropbox/Tesis Carlos Veloy/Scripts Carlos
	InitialT<-Sys.time()
for (i in 1:length(year)){
  gfw_file <- dir(paste("D:/DPbox/Dropbox/Tesis Carlos Veloy/Scripts Carlos/Data/FISHING_EFFORT/GFW_v2/fleet-daily-csvs-100-v2-", year[i], sep=""), pattern=".csv") #list of files
  
for (m in 1:12){
gfw_x_day <- list() 
if((2012-year)/4==0){FebEnd=29}
else {FebEnd=28}
month_days <- c(31, FebEnd, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
if(m==1){IDay<-1} else
{IDay<-sum(month_days[1:(m-1)])+1}
FDay<-sum(month_days[1:m])

	
	### In case you were interested in a particular period, just selecte (subset) the dates from gfw_file object
	
	for(j in IDay:FDay){#1:length(gfw_file)
	  
	  
	  dir.create (file.path("F:/TEMP/"), showWarnings = FALSE)
		gfw <- read.csv(paste(paste("D:/DPbox/Dropbox/Tesis Carlos Veloy/Scripts Carlos/Data/FISHING_EFFORT/GFW_v2/fleet-daily-csvs-100-v2-", year[i], sep=''), gfw_file[j], sep="/"),sep=",")
		# unique(gfw_$geartype)
		# [1] "fishing"            "seiners"            "trawlers"           "trollers"           "fixed_gear"         "purse_seines"      
		# [7] "set_gillnets"       "squid_jigger"       "pole_and_line"      "set_longlines"      "dredge_fishing"     "pots_and_traps"    
		# [13] "tuna_purse_seines"  "drifting_longlines" "other_purse_seines" "other_seines" 
		gear <- c("trawlers") # in case you want additional gear, just incorporate them into the vector
			  
				rasterOptions(tmpdir="F:/TEMP/")

		gfw_ <- subset(gfw, gfw$geartype == gear)  #select the geartype of interest
		#if(is.null(gfw$hours)==FALSE) {gfw_<-dplyr::mutate(gfw_,fishing_hours=hours)}
		effort_all <- gfw_ %>% 
			group_by(cell_ll_lon,cell_ll_lat) %>%
			summarize(fishing_hours = sum(fishing_hours, na.rm = T))
		effort_all_raster <- rasterFromXYZ(effort_all, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
		if(m>1){listn<-j-(sum(month_days[1:(m-1)]))} else {listn<-j}
		gfw_x_day[listn] <- effort_all_raster
		print(listn)
		print(Sys.time())
	}
	FinalT<-Sys.time()
	gfw_x_day$fun<-sum
	gfw_x_day$na.rm<-TRUE
	gfw_x_day_sum<-do.call(mosaic,gfw_x_day)
	gfw_x_month[m] <- gfw_x_day_sum 
	writeRaster(gfw_x_day_sum, filename = paste("D:/DPbox/Dropbox/Tesis Carlos Veloy/Scripts Carlos/Data/FISHING_EFFORT/GFW_v2/Rasters_Fisheries/gfw", year[i],"_",m,".tif", sep=''),overwrite=TRUE)
	Sys.time
	remove(effort_all_raster)
	remove(gfw_x_day)
	unlink(file.path("F:/TEMP/"), recursive = TRUE)
	gc()
}}		

FinalTF<-Sys.time()

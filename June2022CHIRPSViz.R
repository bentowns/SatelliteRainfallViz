#-------------------------------------------------------------------------------
#Grabbing data from apis
#-------------------------------------------------------------------------------
#Required Packages
library(terra) #processing/visualization of raster
library(rnaturalearth) #Ghana shapefile for mapping and masking
library(chirps) #chirps data
library(sf) #Processing data
library(tidyverse) #Data manipulation and plotting
library(sfheaders) #For transforming geospatial objects to dataframe
#-------------------------------------------------------------------------------
#Collect Shapefile of Ghana
#-------------------------------------------------------------------------------
#connect to rnaturalearth
Ghana <- ne_countries(country = "Ghana", scale = "medium")
plot(Ghana)
#transform to sf
Ghana <- st_as_sf(Ghana)

#transform to SpatVector
Ghana <- terra::vect(Ghana)
#-------------------------------------------------------------------------------
#Get CHIRPS data
#-------------------------------------------------------------------------------
#Set dates
dates <- c("2022-06-01", "2022-06-30")

#Get CHIRPS data
June2022CHIRPS <- get_chirps(Ghana, dates, server = "CHC")

#Remove NAs?
June2022CHIRPS <- subst(June2022CHIRPS, -9999, NA)
#-------------------------------------------------------------------------------
#Create Visualizations
#-------------------------------------------------------------------------------
#Make Copies - for raster manipulation
DryDays <- June2022CHIRPS
ExtremeWetDays <- June2022CHIRPS
plot(DryDays)
#-------------------------------------------------------------------------------
#Dry days raster manipulation
#end goal is where 1 = dry day and 0 = wet day
#convert dry days to -1
for (i in 1:30){
  DryDays[[i]][DryDays[[i]] == 0 ] = -1
}
#convert wet days to 0
for (i in 1:30){
  DryDays[[i]][DryDays[[i]] > 0 ] = 0
}
#convert dry days from -1 to 1
for (i in 1:30){
  DryDays[[i]][DryDays[[i]] == -1 ] = 1
}

#Sum days
DryDays <- sum(DryDays[[1:30]])
plot(DryDays)
#-------------------------------------------------------------------------------
#Extreme Wet Days raster manipulation
#convert dry days to 0
for (i in 1:30){
  ExtremeWetDays[[i]][ExtremeWetDays[[i]] < 20 ] = 0
}

#convert extreme wet days to 1
for (i in 1:30){
  ExtremeWetDays[[i]][ExtremeWetDays[[i]] > 20 ] = 1
}

#Sum days
ExtremeWetDays <- sum(ExtremeWetDays[[1:30]])
plot(ExtremeWetDays)
#-------------------------------------------------------------------------------
#Define Functions
#-------------------------------------------------------------------------------
#Create a function to fast track cropping - create raster of shapefile boundary
CropRaster <- function(raster, boundary) {
  raster.crop <- crop(raster, boundary)
  raster.mask <- mask(raster.crop, boundary)
  return(raster.mask)
}

#Transforms a SpatRaster into dataframe - for visualization in ggplot
RasterToDataframe <- function(raster) {
  raster.pts <- as.points(raster)
  raster.sf <- st_as_sf(raster.pts)
  raster.df <- sf_to_df(raster.sf)
  raster.df$value <- raster.sf$sum
  return(raster.df)
}
#-------------------------------------------------------------------------------
#Prepare for visualization with ggplot
#-------------------------------------------------------------------------------
#Crop and mask
DryDays <- CropRaster(DryDays, Ghana)
ExtremeWetDays <- CropRaster(ExtremeWetDays, Ghana)

#Transform into dataframe
DryDays.df <- RasterToDataframe(DryDays)
ExtremeWetDays.df <- RasterToDataframe(ExtremeWetDays)

#Transform ghana shapefile into sf - for visualization in ggplot
Ghana <- st_as_sf(Ghana)
#-------------------------------------------------------------------------------
#ggplot
#-------------------------------------------------------------------------------
#Dry Days
ggplot() +
  geom_sf(data = Ghana) + 
  geom_raster(data = DryDays.df, aes(x = x, y = y, fill = value)) +
  scale_fill_stepsn(colours=c("#ffffb2","#fecc5c", "#fd8d3c",  "#e31a1c"),
                    name="Dry Days",
                    limits=c(0,19),
                    breaks=c(9, 12, 15, 19), 
                    labels=c(9, 12, 15, 19)) +
  ggtitle("Number of Dry Days, June 2022") +
  theme_void()

#Extreme wet days
ggplot() +
  geom_sf(data = Ghana) + 
  geom_raster(data = ExtremeWetDays.df, aes(x = x, y = y, fill = value)) +
  scale_fill_stepsn(colours=c("#ffffcc","#a1dab4", "#41b6c4",  "#225ea8"),
                    name="Extreme Wet Days",
                    limits=c(0,12),
                    breaks=c(0, 0.1, 4, 8, 12), 
                    labels=c(0, 0.1, 4, 8, 12)) +
  ggtitle("Extreme Wet Days (> 20mm per day), June 2022") +
  theme_void()

checkgauges <- read_csv()
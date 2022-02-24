# set working directory
setwd("Z:/Courtney/Stuart_MSc_Ch1/")

# data directories
# reading in habitat data from Courtney's MSc
data_wd = "Z:/Courtney/Stuart_MSc_Ch1/Spatial_Predictors/"

# saving data to our patch reef GIS folder
out_wd = "E:/BIOL398_Patch_Reef_Residency/GIS/Data/"

# load raster library
libraries("raster")

# change where large temp rasters are saved
rasterOptions(tmpdir = "E:/BIOL398_Patch_Reef_Residency/Temporary/")

# save PROJ.4 string for standard projection (ESPG:26958 NAD 83/Florida East) 
my_crs = CRS("+init=epsg:26958")

# read in the habitat raster from our study area
habitat = raster(paste0(data_wd, "Habitat.asc"))
raster::crs(habitat) = my_crs # set coordinate system info

# create a new raster, where the value in each cell reflects the
# Euclidean distance (meters) from that cell to the nearest aggregate 
# reef cell. Save this raster to our GIS folder for the patch reef project. 
reef_dist = writeRaster(raster::mask(raster::crop(gridDistance(habitat, origin = 6),
                                                  habitat), habitat),
                        file = file.path(out_wd, "Reef_Dist.asc"),
                        format = "ascii", overwrite = T)
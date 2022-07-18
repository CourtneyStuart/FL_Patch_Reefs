# install packages
#install.packages("easypackages")
#install.packages("sp")
#install.packages("sf")
#install.packages("raster")
#install.packages("tibble")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("conflicted")
#devtools::install_github('jeremiaheb/rvc')
#install.packages("rfishbase")
#install.packages("readxl")
#install.packages("purrr")

# load packages
library(easypackages)
libraries("sp", "sf", "raster", "tidyr", "dplyr", "tibble", 
          "conflicted", "rvc", "rfishbase", "readxl", "purrr")

# prevent conflicts between packages
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("distinct", "dplyr")

# set working directory (home project folder)
setwd("E:/BIOL398_Patch_Reef_Residency/")

# save path to our data folder
data_wd = "E:/BIOL398_Patch_Reef_Residency/Tabular_Data/"

# save coordinate system information for our target projected CS and source
# geographic CS
my_crs = CRS("+init=epsg:26958")
gcs = CRS("+init=epsg:4326")

# load reef visual census data for Florida Keys in years 2014, 2016, 2018
rvc = getRvcData(years = c(2014, 2016, 2018), regions = "FLA KEYS")

# separate taxonomic, stratum, and sample data
rvc_tax = rvc$taxonomic_data
rvc_strat = rvc$stratum_data
rvc_samp = rvc$sample_data

# save list of predatory species that we're interested in
pred_spp = c("Caranx bartholomaei", "Caranx crysos", "Caranx ruber",
             "Seriola dumerili", "Mycteroperca phenax", "Haemulon parra",
             "Halichoeres bivittatus", "Lutjanus apodus", "Lutjanus buccanella",
             "Lutjanus cyanopterus", "Lutjanus griseus", "Lutjanus jocu", 
             "Lutjanus mahogoni", "Lutjanus synagris", "Ocyurus chrysurus",
             "Sphyraena barracuda", "Cephalopholis cruentata", "Cephalopholis fulva",
             "Epinephelus morio", "Epinephelus striatus", "Mycteroperca bonaci",
             "Mycteroperca microlepis", "Scorpaena plumieri", 
             "Aulostomus maculatus")

# extract taxonomic information for predatory species
pred_tax = rvc_tax %>%
  filter(SCINAME %in% pred_spp) %>%
  tibble::add_column(SOURCE = "South Florida Reef Visual Census")

# extract observations of these predators from the rvc sample data
pred_obs = rvc_samp %>%
  filter(SPECIES_CD %in% pred_tax$SPECIES_CD)

# use filter statement to extract predatory fishes that we assume to be
# of large enough body size to consume sub-adult gray snapper. Where the lower
# bound for sub-adult gray snapper size is 9.51 cm FL and predators can only 
# consume prey 40% of their size or smaller. 
lg_pred = pred_obs %>%
  filter(9.51 <= (LEN * 0.40))

# now calculate the abundance of each predatory species at each site
lg_pred_abun = lg_pred %>%
  group_by(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR) %>%
  mutate(LON_DEGREES = mean(LON_DEGREES),
         LAT_DEGREES = mean(LAT_DEGREES)) %>%
  ungroup() %>%
  group_by(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR,
           LON_DEGREES, LAT_DEGREES, SPECIES_CD) %>%
  mutate(N = sum(NUM)) %>% 
  select(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR,
         LON_DEGREES, LAT_DEGREES, SPECIES_CD, N) %>%
  distinct() # how many of each predatory species were seen at each SSU?

# now, calculate the overall abundance for all predators (N_PRED). 
# finally, calculate the overall predator density at each second-stage sampling 
# unit as N_PRED / 177m2 (station survey area)
lg_pred_dens = lg_pred_abun %>%
  ungroup() %>%
  select(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR, LON_DEGREES,
         LAT_DEGREES, N) %>%
  distinct() %>%
  group_by(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR, LON_DEGREES,
           LAT_DEGREES) %>%
  mutate(N_PRED = sum(N)) %>%
  mutate(DEN_PRED = (N_PRED/177)) %>%
  select(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR, LON_DEGREES,
         LAT_DEGREES, N_PRED, DEN_PRED) %>%
  distinct()

# repeat for bluestriped grunts
# use filter statement to extract predatory fishes that we assume to be
# of large enough body size to consume sub-adult bluestriped grunt. Where the lower
# bound for sub-adult bluestriped grunt size is 11.90 cm FL and predators can only 
# consume prey 40% of their size or smaller. 
hs_pred = pred_obs %>%
  filter(11.90 <= (LEN * 0.40))

# now calculate the abundance of each predatory species at each site
hs_pred_abun = hs_pred %>%
  group_by(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR) %>%
  mutate(LON_DEGREES = mean(LON_DEGREES),
         LAT_DEGREES = mean(LAT_DEGREES)) %>%
  ungroup() %>%
  group_by(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR,
           LON_DEGREES, LAT_DEGREES, SPECIES_CD) %>%
  mutate(N = sum(NUM)) %>% 
  select(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR,
         LON_DEGREES, LAT_DEGREES, SPECIES_CD, N) %>%
  distinct() # how many of each predatory species were seen at each SSU?

# now, calculate the overall abundance for all predators (N_PRED). 
# finally, calculate the overall predator density at each second-stage sampling 
# unit as N_PRED / 177m2 (station survey area)
hs_pred_dens = hs_pred_abun %>%
  ungroup() %>%
  select(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR, LON_DEGREES,
         LAT_DEGREES, N) %>%
  distinct() %>%
  group_by(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR, LON_DEGREES,
           LAT_DEGREES) %>%
  mutate(N_PRED = sum(N)) %>%
  mutate(DEN_PRED = (N_PRED/177)) %>%
  select(REGION, STRAT, PRIMARY_SAMPLE_UNIT, STATION_NR, LON_DEGREES,
         LAT_DEGREES, N_PRED, DEN_PRED) %>%
  distinct()

# now we need to account for sites where predators meeting these size requirements
# were NOT seen.

# convert density data from tabular data frame to spatial data object
sf_lg_pred_dens = 
  st_as_sf(lg_pred_dens, coords = c(5,6), crs = gcs) %>%
  st_transform(., my_crs) %>%
  add_column(LON_M = unlist(map(.$geometry,1))) %>%
  add_column(LAT_M = unlist(map(.$geometry,2)))

# read in our sub-adult gray snapper patch reef data
lg = as.data.frame(read_xls(
  path = paste0(data_wd, "Subadult_Gray_Snapper_Patch_Reef_Data.xls")))

# convert to spatial data
sf_lg = lg %>%
  st_as_sf(., coords = c(11, 12), crs = my_crs) %>%
  add_column(LON_M = unlist(map(.$geometry,1))) %>%
  add_column(LAT_M = unlist(map(.$geometry,2)))

# round decimals for coords before joining based on matching coords
sf_lg_pred_dens = sf_lg_pred_dens %>%
  mutate(LON_M = round(LON_M, digits = 4),
         LAT_M = round(LAT_M, digits = 4))

sf_lg = sf_lg %>%
  mutate(LON_M = round(LON_M, digits = 4),
         LAT_M = round(LAT_M, digits = 4))

# join the two data sets together using matching LON_M and LAT_M coords
# if there is an NA value for N_PRED or DEN_PRED that means there were none
# of our selected predator species (or at least none meeting the size requirement)
# at that particular survey site, so replace NA --> 0
final_sf_lg = left_join(st_drop_geometry(sf_lg), 
                 st_drop_geometry(sf_lg_pred_dens), 
                 by = c("LON_M", "LAT_M")) %>%
  group_by(OBJECTID) %>%
  mutate(N_PRED = mean(N_PRED),
         DEN_PRED = mean(DEN_PRED)) %>%
  distinct(OBJECTID, .keep_all = TRUE) %>% 
  mutate(N_PRED = ifelse(is.na(N_PRED), 0, N_PRED),
         DEN_PRED = ifelse(is.na(DEN_PRED), 0, DEN_PRED)) %>%
  select(-REGION, -STRAT, -PRIMARY_SAMPLE_UNIT, -STATION_NR)

# save the new, updated dataset including information on predator
# abundance and density as both spatial data shapefiles and csv tables
st_write(st_as_sf(final_sf_lg, coords = c(56,57), crs = my_crs), 
         dsn = "E:/BIOL398_Patch_Reef_Residency/GIS/Data/Subadult_Gray_Snapper_Patch_Reefs_w_Predator.shp",
         driver = "ESRI Shapefile", append = F)

write.csv(final_sf_lg, 
          paste0(data_wd, "Subadult_Gray_Snapper_Patch_Reefs_w_Predator.csv"),
          row.names = FALSE)

# repeat for sub-adult bluestriped grunt
# convert density data from tabular data frame to spatial data object
sf_hs_pred_dens = 
  st_as_sf(hs_pred_dens, coords = c(5,6), crs = gcs) %>%
  st_transform(., my_crs) %>%
  add_column(LON_M = unlist(map(.$geometry,1))) %>%
  add_column(LAT_M = unlist(map(.$geometry,2)))

# read in our sub-adult gray snapper patch reef data
hs = as.data.frame(read_xls(
  path = paste0(data_wd, "Subadult_Bluestriped_Grunt_Patch_Reef_Data.xls")))

# convert to spatial
sf_hs = hs %>%
  st_as_sf(., coords = c(11, 12), crs = my_crs) %>%
  add_column(LON_M = unlist(map(.$geometry,1))) %>%
  add_column(LAT_M = unlist(map(.$geometry,2)))

# round decimals for coords before intersecting based on matching coords
sf_hs_pred_dens = sf_hs_pred_dens %>%
  mutate(LON_M = round(LON_M, digits = 4),
         LAT_M = round(LAT_M, digits = 4))

sf_hs = sf_hs %>%
  mutate(LON_M = round(LON_M, digits = 4),
         LAT_M = round(LAT_M, digits = 4))

# join the two data sets together using matching LON_M and LAT_M coords
# if there is an NA value for N_PRED or DEN_PRED that means there were none
# of our selected predator species (or at least none meeting the size requirement)
# at that particular survey site, so replace NA --> 0
final_sf_hs = left_join(st_drop_geometry(sf_hs), 
                        st_drop_geometry(sf_hs_pred_dens), 
                        by = c("LON_M", "LAT_M")) %>%
  group_by(OBJECTID) %>%
  mutate(N_PRED = mean(N_PRED),
         DEN_PRED = mean(DEN_PRED)) %>%
  distinct(OBJECTID, .keep_all = TRUE) %>% 
  mutate(N_PRED = ifelse(is.na(N_PRED), 0, N_PRED),
         DEN_PRED = ifelse(is.na(DEN_PRED), 0, DEN_PRED)) %>%
  select(-REGION, -STRAT, -PRIMARY_SAMPLE_UNIT, -STATION_NR)

# save the new, updated dataset including information on predator
# abundance and density as both spatial shapefile and csv table
st_write(st_as_sf(final_sf_hs, coords = c(56,57), crs = my_crs), 
         dsn = "E:/BIOL398_Patch_Reef_Residency/GIS/Data/Subadult_Bluestriped_Grunt_Patch_Reefs_w_Predator.shp",
         driver = "ESRI Shapefile", append = F)

write.csv(final_sf_hs, 
          paste0(data_wd, "Subadult_Bluestriped_Grunt_Patch_Reefs_w_Predator.csv"),
          row.names = FALSE)

#### SET-UP ####

# set working directory (home project folder)
setwd("G:/BIOL398_Patch_Reef_Residency/")

# data directories
# joined reef fish (abundance, presence/absence, etc.) and patch reef (area,
# perimeter, P:A ratio, distance to reef/mangrove, etc.) data exported from
# ArcGIS
data_wd = "G:/BIOL398_Patch_Reef_Residency/Tabular_Data/"

# temporary directory (for intermediate/temporary products or checks)
temp_wd = "G:/BIOL398_Patch_Reef_Residency/Temporary/"

# saving figures
fig_wd = "G:/BIOL398_Patch_Reef_Residency/Figures/"

# github repo project
git_wd = "G:/BIOL398_Patch_Reef_Residency/GitHub/FL_Patch_Reefs/"

# install packages
install.packages(c("conflicted", "tidyverse", "ggplot2", "PNWColors", "readxl",
"dplyr", "corrplot", "Cairo", "usdm", "ISLR"))

# load packages
library(easypackages) # lets you load more than one package at once
libraries("conflicted", "tidyverse", "ggplot2", "PNWColors", "readxl", 
          "dplyr", "corrplot", "Cairo", "usdm", "ISLR")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

# set seed to ensure reproducibility
set.seed(7)   

# read in our data file, which stores reef fish data (species, life stage,
# presence/absence, abundance, site info), corresponding environmental data
# (depth & seafloor morphometrics, seasonal water conditions, surrounding
# seascape context (i.e., area of seagrass, reef, and unconsolidated sediments
# in 500 m buffer around survey site)), and corresponding patch reef data 
# (patch area, perimeter, perimeter:area, nearest neighbor distance, predators)


#### SUBADULT GRAY SNAPPER (Lutjanus griseus) ####
# reading in the csv file containing gray snapper observations with patch reef 
# biogeophysical predictors
lg = read.csv(paste0(data_wd, "Subadult_Gray_Snapper_Patch_Reefs_w_Predator.csv"))

# str lets you preview data and column types/formats
str(lg)

# use View if you want to open the entire data frame, or click the name of
# the data frame in the global environment
#View(lg)

# save the raw data to github so we have a safe copy there
write.csv(lg, paste0(git_wd, "Data/Subadult_Gray_Snapper_Patch_Reef_Data.csv"),
          row.names = FALSE)

# first, let's do some data prep and cleaning to make the data frame easier
# to work with
lg = lg %>%
  # starting with some renaming of columns (where new name = old name)
  rename(Species = SPECIES_CD, # species
         Life_Stage = LIFE_STAGE, # life stage
         Presence = PRES, # binary presence (1 = yes, 0 = no)
         Presence2 = PRES2, # presence or absence as text
         Abundance = N, # fish counts (averaged over two buddy divers at site)
         Long_M = LON_M, # longitude of survey (meters)
         Lat_M = LAT_M, # latitude of survey (meters)
         Patch_Area = POLY_AREA, # patch reef area (square meters)
         Patch_Perimeter = PERIMETER, # patch reef perimeter (meters)
         Patch_Neigh_Dist = NEAR_DIST, # for each patch reef, distance to nearest neighbor patch reef (meters)
         Area_CRHB = Coral_Reef_and_Hardbottom, # coral reef and hard bottom area in 500 m buffer (square meters)
         Area_SG = Seagrass, # seagrass area in 500 m buffer (square meters)
         Area_US = Unconsolidated_Sediment, # unconsolidated sediment area in 500 m buffer (square meters)
         Pred_Abundance = N_PRED, # predator abundance
         Pred_Density = DEN_PRED) %>%  # predator density
  # let's make sure that our two response presence/absence columns are factors
  mutate(Presence = as.factor(Presence),
         Presence2 = as.factor(Presence2)) %>%
  # add new column storing the area sampled by each reef fish survey,
  # where area = pie*radius^2 --> pie*7.5m^2 --> ~177 m^2
  mutate(Survey_Area = rep(as.numeric(177), nrow(.))) %>%
  # add new column storing the density of sub-adult gray snapper,
  # where density = abundance/area of sampling = fish per m^2
  mutate(Density = Abundance/Survey_Area) %>%
  # now keep only the columns we need, the order here will determine column order
  # in the output dataframe 
  select(Patch_ID, Species, Life_Stage, Presence, Presence2, Abundance, Density, 
         Survey_Area, Long_M, Lat_M, Habitat, Mangrove_Dist, Reef_Dist,
         Depth, StDev_Depth, Slope, Curvature, Plan_Curve, BPI_Fine, 
         BPI_Broad, Rugosity, Mean_Sum_Temp, Mean_Sum_DO, Mean_Sum_Sal, 
         Mean_Win_Temp, Mean_Win_DO, Mean_Win_Sal, Patch_Area,
         Patch_Perimeter, PA_Ratio, Patch_Neigh_Dist, Area_CRHB, Area_SG, Area_US, 
         Pred_Abundance, Pred_Density)

# sort data by increasing latitude and then add a NEW object_id column based on 
# this sorting (later, we will repeat this for bluestriped grunt and use the 
# object_ids to extract data from the same training and testing sites as gray snapper)
lg = lg[order(lg$Lat_M),]
lg$Object_ID = seq.int(nrow(lg))

# keep only one survey per unique patch reef (so as to not over-represent any
# environmental conditions at patch reefs that had multiple surveys around them)
lg = lg %>% 
  group_by(Patch_ID) %>% 
  sample_n(1) %>%
  ungroup()
lg = as.data.frame(lg)

# set plotting margins
par(mar = c(2.5, 2.5, 2.5, 2.5))

# now we'll do a bit more pre-processing to keep only the variables that we
# think will be helpful in predictive modeling of patch reef residency

# some variables have only a small range with little variation. rugosity is
# a great example. this likely won't be a very helpful predictor, at least
# not at the spatial resolution of our data (5 x 5 m)...
summary(lg$Rugosity)
hist(lg$Rugosity) 

# curvature and plan curvature are also relatively homogeneous across patch reefs.
# although curvature influences the acceleration/deceleration and direction of
# benthic flow, this may not be as important for mobile sub-adult gray snapper 
# and bluestriped grunt that are feeding on mobile invertebrates and small fishes
# in seagrass, and that are protected from exposure by patch reefs. 
summary(lg$Curvature)
hist(lg$Curvature)
summary(lg$Plan_Curve)
hist(lg$Plan_Curve)

# bathymetric position index (BPI) represents the depth of a cell relative to its
# surroundings, where (-) values indicate valleys and depressions, values 
# near 0 represent flat areas or constant slopes, and (+) values represent
# peaks and summits. for my MSc, BPI was evaluated at two spatial scales across 
# the whole Florida Key's seascape:

# broad-scale BPI was evaluated using concentric rings of 125 m and 1250 m
hist(lg$BPI_Broad)

# fine-scale BPI was evaluated using concentric rings of 5 m and 125 m
hist(lg$BPI_Fine)

# based on my first chapter, which modeled habitat suitability for sub-adult
# gray snapper and bluestriped grunt across the seascape, broad-scale BPI was a 
# much stronger predictor of suitability than fine-scale BPI. also, as we can see
# in these histograms, fine-scale BPI takes on only a few values when examined
# for patch reefs only. this is because patch reefs within concentric rings of 
# 5 m and 125 m likely have very similar depth values. For these reasons, we will
# retain broad-scale BPI. 


# finally, some variables can be explained using a combination of others, for 
# example increasing temperature and/or salinity results in lower dissolved oxygen. 
# salinity and temperature data are also easier to collect/access. Also, previous
# research has revealed significant inter-species variation in response to 
# salinity and temperature, so these are of potential ecological relevance to 
# our patch reef study. 

# let's exclude some variables and create a new data frame of only those
# variables that we'd like to consider for modeling
lg_vars = lg %>%
  # where (-) lets you deselect a column
  select(11:35, -Curvature, -Plan_Curve, -Rugosity, -BPI_Fine,
         -Mean_Sum_DO, -Mean_Win_DO, -Pred_Abundance) # we'll focus on density

# let's check to make sure all the variable columns are of the numeric type
str(lg_vars)
lg_vars = as.data.frame(lapply(lg_vars, as.numeric))


# now we can check for correlation among these variables, where strong
# positive or negative correlation (> absolute 0.7) suggests that the two
# variables are collinear --> they will likely explain some of the same
# variance in the dependent variable if included together in the model. 
# collinearity (or multicollinearity, when more than two variables are 
# highly correlated) can distort model interpretations, coefficient 
# estimates, and levels of significance. as a rule of thumb, we'll use
# pearson pairwise correlation (r) threshold of |0.7| (abs). 

# first we'll create a full correlation matrix from all variables
lg_cormat = cor(lg_vars, use = "complete.obs")

# plot the full correlation matrix, this is the color palette we'll use
palette = pnw_palette("Shuksan2", 200, type = "continuous")

# set plotting margins
par(mar=c(0,0,0,0))

# full correlation plot
corrplot(lg_cormat, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)

# save full correlation plot as a png
# local folder
Cairo(file = paste0(fig_wd, "Correlation_Full_Predictor_Set.png"), 
      bg = "white", type = "png", units = "in", width = 6, 
      height = 6, pointsize = 12, dpi = 300)
par(mar=c(0,0,0,0))
corrplot(lg_cormat, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.75)
dev.off()

# github repo
Cairo(file = paste0(git_wd, "Figures/Correlation_Full_Predictor_Set.png"), 
      bg = "white", type = "png", units = "in", width = 6, 
      height = 6, pointsize = 12, dpi = 300)
par(mar=c(0,0,0,0))
corrplot(lg_cormat, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.75)
dev.off()


# we could run through the correlation matrix by hand to find problematic
# variables...but instead we'll automate the process using a "usdm" function. 
# the vifcor function runs through all pairs of variables in the lg_vars
# data frame and checks whether any exceed the r threshold. if so, it will
# tell us where the problems lie.
lg_corr = vifcor(lg_vars, th = 0.7)
lg_corr # check correlation results

# we can also use variance inflation factors (VIF) to assess multicollinearity.
# VIF measures how much the behavior (variance) of a variable is influenced
# by it's interaction with other variables. VIF allows a quick measure of how
# much a variable is contributing to the standard error in the regression. we
# want to keep standard errors as small as possible, so we will use a standard
# VIF threshold of 5.

# the vifstep function runs through all pairs of variables in the lg_vars
# data frame and checks whether any exceed the VIF threshold. if so, it will
# tell us where the problems lie.
lg_vif = vifstep(lg_vars, th = 5)
lg_vif # check VIF results

# these tests suggest that the standard deviation of depth variable needs
# to be removed due to collinearity. This is likely due to correlation with
# slope. let's check:
cor(lg_vars$StDev_Depth, lg_vars$Slope,  use = "complete")
# yes, they're extremely positively correlated. In this case, we will retain
# slope as suggested.

# what about the suggestion to remove patch perimeter? this is likely due to 
# correlation between patch area and patch area
cor(lg_vars$Patch_Area, lg_vars$Patch_Perimeter)
# once again, these variables display strong positive correlation.
# in this case, patch area may be a more ecologically relevant predictor.
# this is because we assume that reef fish are more likely to respond to
# patch area than perimeter. so we will retain patch area and remove patch 
# perimeter in the next step, hopefully this will resolve the issue.

# finally, what about seagrass area within a 500 m buffer? this is possibly 
# correlated with either area of coral & hardbottom or unconsolidated sediment
cor(lg_vars$Area_SG, lg_vars$Area_CRHB) # below our threshold
cor(lg_vars$Area_SG, lg_vars$Area_US) # strong negative correlation
# here, area of surrounding seagrass is what we're more so interested in, 
# because this can serve as a proxy for available prey (where fish
# feed overnight). let's keep seagrass instead of unconsolidated sediments,
# hopefully this resolves the issue. 

# remove collinear variables as described above.
lg_vars2 = lg_vars %>% select(-StDev_Depth, -Patch_Perimeter, -Area_US)

# reduced correlation matrix from retained variables
lg_cormat2 = cor(lg_vars2, use = "complete.obs")

# reduced correlation plot
corrplot(lg_cormat2, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.8)

# save reduced correlation plot as a png
# local folder
Cairo(file = paste0(fig_wd, "Correlation_Reduced_Predictor_Set.png"), 
      bg = "white", type = "png", units = "in", width = 6, 
      height = 6, pointsize = 12, dpi = 300)
par(mar=c(0,0,0,0))
corrplot(lg_cormat2, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.75)
dev.off()

# github repo
Cairo(file = paste0(git_wd, "Figures/Correlation_Reduced_Predictor_Set.png"), 
      bg = "white", type = "png", units = "in", width = 6, 
      height = 6, pointsize = 12, dpi = 300)
par(mar=c(0,0,0,0))
corrplot(lg_cormat2, method = "color", col = palette, type = "upper",
         order = "original", addCoef.col = "black", number.cex = 0.50, 
         number.digits = 2, tl.col = "black", tl.srt = 40, tl.cex = 0.75)
dev.off()

# check correlation again
lg_corr2 = vifcor(lg_vars2, th = 0.7)
lg_corr2

# check VIF again
lg_vif2 = vifstep(lg_vars2, th = 5)
lg_vif2

# reset ploting margins 
par(mar=c(2.5, 2.5, 2.5, 2.5))

# no outstanding multicollinearity issues! let's create and save some
# data frames with these variables. first, a data frame with all rows.
lg_full = lg %>%
  select(-Curvature, -Plan_Curve, -Rugosity, -BPI_Fine,
         -Mean_Sum_DO, -Mean_Win_DO, -StDev_Depth,
         -Patch_Perimeter, -Area_US, -Pred_Abundance)

# randomize the order of rows
lg_random = sample(nrow(lg_full))
lg_full = lg_full[lg_random, ]

# now randomly split data into two subsets: one for model calibration (70%)
# and one for model validation (30%)
lg_train_id = sample(seq_len(nrow(lg_full)), size = floor(0.70*nrow(lg_full)))  
lg_train = lg_full[lg_train_id,] # creates the training dataset (70%)
lg_test = lg_full[-lg_train_id,]  # creates the test dataset (30%)

# let's save these data frames as csv files 
# local folders
write.csv(lg_train, paste0(data_wd, "Subadult_Gray_Snapper_Model_Calibration_Data.csv"),
          row.names = FALSE)

write.csv(lg_test, paste0(data_wd, "Subadult_Gray_Snapper_Model_Validation_Data.csv"),
          row.names = FALSE)

# github repo
write.csv(lg_train, paste0(git_wd, "Data/Subadult_Gray_Snapper_Model_Calibration_Data.csv"),
          row.names = FALSE)

write.csv(lg_test, paste0(git_wd, "Data/Subadult_Gray_Snapper_Model_Validation_Data.csv"),
          row.names = FALSE)

#### SUBADULT BLUESTRIPED GRUNT (Haemulon sciurus) ####
# reading in the csv file containing bluestriped grunt observations with patch reef 
# biogeophysical predictors
hs = read.csv(paste0(data_wd, "Subadult_Bluestriped_Grunt_Patch_Reefs_w_Predator.csv"))

# str lets you preview data and column types/formats
str(hs)

# use View if you want to open the entire data frame, or click the name of
# the data frame in the global environment
#View(hs)

# save the raw data to github so we have a safe copy there
write.csv(hs, paste0(git_wd, "Data/Subadult_Bluestriped_Grunt_Patch_Reef_Data.csv"),
          row.names = FALSE)

# first, let's do some data prep and cleaning to make the data frame easier
# to work with
hs = hs %>%
  # starting with some renaming of columns (where new name = old name)
  rename(Species = SPECIES_CD, # species
         Life_Stage = LIFE_STAGE, # life stage
         Presence = PRES, # binary presence (1 = yes, 0 = no)
         Presence2 = PRES2, # presence or absence as text
         Abundance = N, # fish counts (averaged over two buddy divers at site)
         Long_M = LON_M, # longitude of survey (meters)
         Lat_M = LAT_M, # latitude of survey (meters)
         Patch_Area = POLY_AREA, # patch reef area (square meters)
         Patch_Perimeter = PERIMETER, # patch reef perimeter (meters)
         Patch_Neigh_Dist = NEAR_DIST, # for each patch reef, distance to nearest neighbor patch reef (meters)
         Area_CRHB = Coral_Reef_and_Hardbottom, # coral reef and hard bottom area in 500 m buffer (square meters)
         Area_SG = Seagrass, # seagrass area in 500 m buffer (square meters)
         Area_US = Unconsolidated_Sediment, # unconsolidated sediment area in 500 m buffer (square meters)
         Pred_Abundance = N_PRED, # predator abundance
         Pred_Density = DEN_PRED) %>%  # predator density
  # let's make sure that our two response presence/absence columns are factors
  mutate(Presence = as.factor(Presence),
         Presence2 = as.factor(Presence2)) %>%
  # add new column storing the area sampled by each reef fish survey,
  # where area = pie*radius^2 --> pie*7.5m^2 --> ~177 m^2
  mutate(Survey_Area = rep(as.numeric(177), nrow(.))) %>%
  # add new column storing the density of sub-adult bluestriped grunt,
  # where density = abundance/area of sampling = fish per m^2
  mutate(Density = Abundance/Survey_Area) %>%
  # now keep only the columns we need, the order here will determine column order
  # in the output dataframe 
  select(Patch_ID, Species, Life_Stage, Presence, Presence2, Abundance, Density, 
         Survey_Area, Long_M, Lat_M, Habitat, Mangrove_Dist, Reef_Dist,
         Depth, StDev_Depth, Slope, Curvature, Plan_Curve, BPI_Fine, 
         BPI_Broad, Rugosity, Mean_Sum_Temp, Mean_Sum_DO, Mean_Sum_Sal, 
         Mean_Win_Temp, Mean_Win_DO, Mean_Win_Sal, Patch_Area,
         Patch_Perimeter, PA_Ratio, Patch_Neigh_Dist, Area_CRHB, Area_SG, Area_US, 
         Pred_Abundance, Pred_Density)

# sort data by increasing latitude and then add a NEW object_id column based on 
# this sorting (these object_ids will match those above for gray snapper)
hs = hs[order(hs$Lat_M),]
hs$Object_ID = seq.int(nrow(hs))


# we DO NOT have to repeat the correlation assessment because all gray 
# snapper and bluestriped grunt data come from the SAME EXACT surveys.
# so if we were to repeat the correlation and VIF tests, we would get 
# identical results! instead, jump ahead to saving the modeling datasets:
# full, calibration, and validation. 

# now create a new data frame that stores only the variables retained for modeling
hs_full = hs %>%
  select(-Curvature, -Plan_Curve, -Rugosity, -BPI_Fine,
         -Mean_Sum_DO, -Mean_Win_DO, -StDev_Depth,
         -Patch_Perimeter, -Area_US, -Pred_Abundance)

# now, use the object_id values stored in the gray snapper training and testing
# dataframes to extract bluestriped grunt data from the same exact locations
hs_train = hs_full %>%
  filter(Object_ID %in% lg_train$Object_ID)

hs_test = hs_full %>%
  filter(Object_ID %in% lg_test$Object_ID)

# save these data as csv files for modeling
# local folders
write.csv(hs_train, paste0(data_wd, "Subadult_Bluestriped_Grunt_Model_Calibration_Data.csv"),
          row.names = FALSE)

write.csv(hs_test, paste0(data_wd, "Subadult_Bluestriped_Grunt_Model_Validation_Data.csv"),
          row.names = FALSE)

# github repo
write.csv(hs_train, paste0(git_wd, "Data/Subadult_Bluestriped_Grunt_Model_Calibration_Data.csv"),
          row.names = FALSE)

write.csv(hs_test, paste0(git_wd, "Data/Subadult_Bluestriped_Grunt_Model_Validation_Data.csv"),
          row.names = FALSE)

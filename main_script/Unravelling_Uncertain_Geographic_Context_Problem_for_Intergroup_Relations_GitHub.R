#############################################################################################################
#############################################################################################################

## ---------------------------
##
##
## Unravelling Uncertain Geographic Context Problem for Intergroup Relations: 
## Does Perceived Threat Depend on How the Neighborhood Context is Measured?
## 
##
## Guiding Script Instruction on Data Pre-Processing, Areal Weighted Interpolation and Analysis
##
##
## Authors: Dra. Daria Dementeva
##
## Email: daria.dementeva@kuleuven.be
##
##
##  2024-2025
##
## ---------------------------

## Set Working Directory 

# setwd("...")  
# Since the purpose of the script is to guide and instruct, I use my working directory throughout the script.

# For Reproducibility

set.seed(05081997)

################################################################################################################ 
# Install packages
################################################################################################################ 

# Uncomment if necessary

# install.packages("BelgiumMaps.StatBel")
# install.packages("car")
# install.packages("dplyr")
# install.packages("lmtest")
# install.packages("missMethods")
# install.packages("openxlsx")
# install.packages("readr")
# install.packages("sandwich")
# install.packages("sf")
# install.packages("sp")
# install.packages("stargazer")
# install.packages("tidyr")

################################################################################################################ 
# Load packages and Data
################################################################################################################

# Load packages for data manipulation 
library(readr) # read in csv files
library(tidyr) # data manipulation (1)
library(dplyr) # data manipulation (2)

# Load packages for spatial data manipulation 
library(sf) # managing spatial data using "simple features" (1)
library(sp) # managing spatial data using sp notation (2) 

# Load packages for wrapping results
library(stargazer) # format regression tables 
library(openxlsx) # create excel files 

# Load packages for statistical modeling and diagnostics
library(lmtest)  #  functions for assumption tests for linear regression 
library(sandwich) # robust covariance matrix estimators, robust standard errors
library(car) # functions for assumption tests 

# Load packages for data cleaning
library(missMethods) #  handle missing data 

# Load Belgian administrative boundaries and maps
library(BelgiumMaps.StatBel)

# Read in datasets from BelgiumMaps.StatBel package
data("BE_ADMIN_SECTORS") # statistical sectors
data("BE_ADMIN_MUNTY") # municipalities 

################################################################################################################ 
# Read in Data
################################################################################################################

# Read in relevant census characteristics recorded at the statistical sector level for areal weighted interpolation procedure (AWI) 

 contextual_measures_ss_for_buffers_full <- read_csv("/Users/dariadementeva/contextual_measures_ss_for_buffers_full_with_metadata.csv") 
# 
# # Read in relevant census characteristics recorded at the municipality level for to augment BNES responses 
# # For the municipality-level OLS with and without HAC robust standard errors
# 
 contextual_measures_mun_for_buffers <- read_csv("/Users/dariadementeva/contextual_measures_mun_for_buffers.csv")
# 
# # Read in the reduced BNES dataset with only the perceived threat variable
# 
 pt_to_merge <- read_csv("/Users/dariadementeva/pt_to_merge.csv")

################################################################################################################ 
# Read in Walking and Driving Time Polygons
################################################################################################################

# Note: 

# The polygons are processed in batches because the input points for the travel-time polygons layer in ArcGIS Pro cannot exceed 1000.

# The sample size is 1363 points, so I processed them in two halves due to ArcGIS Pro limits.

# The shapefiles of walking and driving time polygons are available upon request for privacy reasons.

# The points (although pseudoaddresses) are available upon request for privacy reasons.

################################################################################################################

# Read in Pseudoaddresses to Match with Walking and Driving Time Polygons
 
pt_buffer_points_final <- read_csv("/Users/dariadementeva/Desktop/pt_buffer_points_final.csv")
# View(pt_buffer_points_final)
 
pt_buffer_points_final <- pt_buffer_points_final[, -1]
 
buffer_points_final <-
   pt_buffer_points_final %>%
   filter(coordinates_for_buffer_pt != '0,0') %>%
   separate(coordinates_for_buffer_pt, into = c('lat', 'lon'), sep=",")
 
buffer_points_final$lat <- gsub("[^0-9.-]", "", buffer_points_final$lat)
buffer_points_final$lon <- gsub("[^0-9.-]", "", buffer_points_final$lon)
 
buffer_points_final_batch1 <- buffer_points_final[1:682,]
 
buffer_points_final_batch2 <- buffer_points_final[683:1363,]
 
# Read Batch 1
 
walking_time_polygons_batch_1 <- st_read("/Users/dariadementeva/Desktop/Polygons_Walking_Times_Batch1.shp")
walking_time_polygons_batch_1_5min <- walking_time_polygons_batch_1[walking_time_polygons_batch_1$FromBreak==0, ]
walking_time_polygons_batch_1_10min <-  st_read("/Users/dariadementeva/Desktop/Polygons_WalkingTime_10min_Batch1.shp")
walking_time_polygons_batch_1_15min <- st_read("/Users/dariadementeva/Desktop/Polygons_WalkingTime_15min_Batch1.shp")
 
# Match with respondent id
 
walking_time_polygons_5min_b1_rid <- cbind(walking_time_polygons_batch_1_5min[order(walking_time_polygons_batch_1_5min$FacilityID), ],
                                            buffer_points_final_batch1[1:682, c(1:3)]) # 5-minute polygons
 
walking_time_polygons_10min_b1_rid <- cbind(walking_time_polygons_batch_1_10min[order(walking_time_polygons_batch_1_10min$FacilityID), ],
                                             buffer_points_final_batch1[1:682, c(1:3)]) # 10-minute polygons
 
walking_time_polygons_15min_b1_rid <- cbind(walking_time_polygons_batch_1_15min[order(walking_time_polygons_batch_1_15min$FacilityID), ],
                                             buffer_points_final_batch1[1:682, c(1:3)]) # 15-minute polygons
 
driving_time_polygons_batch_1 <- st_read("/Users/dariadementeva/Desktop/Polygons_Driving_Times_Batch1.shp")
driving_time_polygons_batch_1_5min <- driving_time_polygons_batch_1[driving_time_polygons_batch_1$FromBreak==0, ]
driving_time_polygons_batch_1_10min <- st_read("/Users/dariadementeva/Desktop/Polygons_DrivingTime_10min_Batch1.shp")
driving_time_polygons_batch_1_15min <-  st_read("/Users/dariadementeva/Desktop/Polygons_DrivingTime_15min_Batch1.shp")
 
# Match with respondent id
 
driving_time_polygons_5min_b1_rid <- cbind(driving_time_polygons_batch_1_5min[order(driving_time_polygons_batch_1_5min$FacilityID), ],
                                            pt_buffer_points_final[1:682, c(1:3)]) # 5-minute polygons
 
driving_time_polygons_10min_b1_rid <- cbind(driving_time_polygons_batch_1_10min[order(driving_time_polygons_batch_1_10min$FacilityID), ],
                                             pt_buffer_points_final[1:682, c(1:3)]) # 10-minute polygons
 
driving_time_polygons_15min_b1_rid <- cbind( driving_time_polygons_batch_1_15min[order(driving_time_polygons_batch_1_15min$FacilityID), ],
                                              pt_buffer_points_final[1:682, c(1:3)]) # 15-minute polygons
 
 # Read Batch 2
 
walking_time_polygons_batch_2 <- st_read("/Users/dariadementeva/Desktop/Polygons_Walking_Times_Batch2.shp")
walking_time_polygons_batch_2_5min <- walking_time_polygons_batch_2[walking_time_polygons_batch_2$FromBreak==0, ]
walking_time_polygons_batch_2_10min <- st_read("/Users/dariadementeva/Desktop/Polygons_WalkingTime_10min_Batch2.shp")
walking_time_polygons_batch_2_15min <- st_read("/Users/dariadementeva/Desktop/Polygons_WalkingTime_15min_Batch2.shp")
 
 # Match with respondent id
 
walking_time_polygons_5min_b2_rid <- cbind(walking_time_polygons_batch_2_5min[order(walking_time_polygons_batch_2_5min$FacilityID), ],
                                            buffer_points_final_batch2[1:681, c(1:3)]) # 5-minute polygons
 
walking_time_polygons_10min_b2_rid <- cbind(walking_time_polygons_batch_2_10min[order(walking_time_polygons_batch_2_10min$FacilityID), ],
                                             buffer_points_final_batch2[1:681, c(1:3)]) # 10-minute polygons
 
walking_time_polygons_15min_b2_rid <- cbind(walking_time_polygons_batch_2_15min[order(walking_time_polygons_batch_2_15min$FacilityID), ],
                                             buffer_points_final_batch2[1:681, c(1:3)]) # 15-minute polygons
 
 
driving_time_polygons_batch_2 <- st_read("/Users/dariadementeva/Desktop/Polygons_Driving_Times_Batch2.shp")
driving_time_polygons_batch_2_5min <- driving_time_polygons_batch_2[driving_time_polygons_batch_2$FromBreak==0, ]
driving_time_polygons_batch_2_10min <- st_read("/Users/dariadementeva/Desktop/Polygons_DrivingTime_10min_Batch2.shp")
driving_time_polygons_batch_2_15min <- st_read("/Users/dariadementeva/Desktop/Polygons_DrivingTime_15min_Batch2.shp")
 
 # Match with respondent id
 
driving_time_polygons_5min_b2_rid <- cbind(driving_time_polygons_batch_2_5min[order(driving_time_polygons_batch_2_5min$FacilityID), ],
                                            buffer_points_final_batch2[1:681, c(1:3)]) # 5-minute polygons
 
driving_time_polygons_10min_b2_rid <- cbind( driving_time_polygons_batch_2_10min[order(driving_time_polygons_batch_2_10min$FacilityID), ],
                                              buffer_points_final_batch2[1:681, c(1:3)]) # 10-minute polygons
 
driving_time_polygons_15min_b2_rid <- cbind(driving_time_polygons_batch_2_15min[order(driving_time_polygons_batch_2_15min$FacilityID), ],
                                             buffer_points_final_batch2[1:681, c(1:3)]) # 15-minute polygons

################################################################################################################
# Calculate Area Size 
################################################################################################################

# Note: Results displayed in Table 1. Area Sizes for Different Neighborhood Contexts

walking_time_polygons_5min <- rbind(walking_time_polygons_5min_b1_rid, walking_time_polygons_5min_b2_rid) # nrow = 1363
walking_time_polygons_10min <- rbind(walking_time_polygons_10min_b1_rid, walking_time_polygons_10min_b2_rid) # nrow = 1363
walking_time_polygons_15min <- rbind(walking_time_polygons_15min_b1_rid, walking_time_polygons_15min_b2_rid) # nrow = 1363
driving_time_polygons_5min <- rbind(driving_time_polygons_5min_b1_rid, driving_time_polygons_5min_b2_rid) # nrow = 1363
driving_time_polygons_10min <- rbind(driving_time_polygons_10min_b1_rid, driving_time_polygons_10min_b2_rid) # nrow = 1363
driving_time_polygons_15min <- rbind(driving_time_polygons_15min_b1_rid, driving_time_polygons_15min_b2_rid) # nrow = 1363

# Create wrapper functions

calculate_median_area <- function(polygons, crs = 31370) { # transform to the specified CRS, default 31370 for meters, Belgian Lambert 72
  polygons_meters <- st_transform(polygons, crs)   # transform from degrees to meters
  areas_m2 <- st_area(polygons_meters)   # calculate area in square meters
  areas_km2 <- as.numeric(areas_m2) / 1e6   # convert to square kilometers
  median_area_km2 <- median(areas_km2)  # return median area in km^2
  return(median_area_km2)
}

calculate_median_area_sf <- function(polygons, crs = 31370) {  # transform to the specified CRS, default 31370 for meters, Belgian Lambert 72
  polygons_meters <- st_transform(polygons, crs) # transform from degrees to meters
  areas_m2 <- st_area(polygons_meters)   # calculate area in square meters
  areas_km2 <- as.numeric(areas_m2) / 1e6   # convert to square kilometers
  median_area_km2 <- median(areas_km2, na.rm = TRUE)  # return median area in km^2
  return(median_area_km2)
}

# NB: Takes a bit of time to run

calculate_median_area(walking_time_polygons_5min) # an sf object
calculate_median_area(walking_time_polygons_10min) # an sf object
calculate_median_area(walking_time_polygons_15min) # an sf object
calculate_median_area(driving_time_polygons_5min) # an sf object
calculate_median_area(driving_time_polygons_10min) # an sf object
calculate_median_area(driving_time_polygons_15min) # an sf object
calculate_median_area_sf(st_as_sf(BE_ADMIN_SECTORS)) # an st object, convert to sf (sf and sp are incompatible)
calculate_median_area_sf(st_as_sf(BE_ADMIN_MUNTY))  # an st object, convert to sf (sf and sp are incompatible)

################################################################################################################
# Areal Weighted Interpolation
################################################################################################################

################################################################################################################
# 5 Min Walking Time Area: AWI
################################################################################################################

# contextual_measures_ss_for_buffers_full <- read_csv("/Users/dariadementeva/contextual_measures_ss_for_buffers_full_with_metadata.csv")

# While developing the AWI solution, I encountered several errors. 
# I resolved the errors by consulting ChatGPT for explanations and suggestions, but all final codebase and code decisions are my own

contextual_measures_ss_for_buffers_full <- contextual_measures_ss_for_buffers_full[,-1]

BE_ADMIN_SECTORS_sf <- st_as_sf(BE_ADMIN_SECTORS)

ss_for_aw_interpolate <- BE_ADMIN_SECTORS_sf %>%
  left_join(
    contextual_measures_ss_for_buffers_full,
    by = c("CD_REFNIS_SECTOR", "TX_SECTOR_DESCR_NL"))

ss_for_aw_interpolate <- ss_for_aw_interpolate %>% 
  st_as_sf(coords = c("geometry"), 
           crs = CRS(proj4string(BE_ADMIN_SECTORS)))

source_polygons <- st_transform(ss_for_aw_interpolate, 31370)  # source  polygons, statistical sectors, CRS Belgian Lambert 72
target_polygons <- st_transform(walking_time_polygons_5min, 31370)  # target polygons, 5-Min walking-time, CRS Belgian Lambert 72

# check CRS of both polygons layers to confirm
st_crs(source_polygons)
st_crs(target_polygons)

# calculate area of source polygons for weighting
source_polygons$area <- st_area(source_polygons)

# intersect source and target polygons
intersections <- st_intersection(source_polygons, target_polygons)

# area of intersections
intersections$int_area <- st_area(intersections)

# compute area weight
intersections <- intersections %>%
  mutate(area_weight = as.numeric(int_area / area))

# list of variables to interpolate

vars_to_interpolate <- c(
  "ms_median_net_taxable_inc_2019", "total_all_11", "over_25_years_ss",
  "total_total_higher_education_graduate_edu_11", "total_total_doctorate_edu_11",
  "total_total_eduoverall_11", "overall_total_labmarket_11", "overall_total_under_15yo_11",
  "working_overall_less_than_20_years_11", "working_overall_total_population_11",
  "jobseekers_overall_less_than_20_years_11", "jobseekers_overall_total_population_11",
  "ISCO_08_high_status", "ISCO_08_skill_level_2", "ISCO_skill_level_1",
  "ISCO_not_elsewhere_classified", "nonbelgian_all_11", "belgian_all_11",
  "total_population_11", "area_km2_11", "n", "n_visitors_migrants",
  "owner_occupied_housing_11", "rental_properties_11", "inhabited_houses_not_indicated_11",
  "other_housing_or_collective_housing_1", "total_housing_11"
)

# apply interpolation to each variable
for (var in vars_to_interpolate) {
  weighted_var <- paste0("weighted_", var)
  intersections[[weighted_var]] <- intersections[[var]] * intersections$area_weight
}

# aggregating by target polygon id (target polygon id is respondent id, since each is unique to respondent)
aggregated <- intersections %>%
  st_drop_geometry() %>%
  group_by(rid) %>%
  summarise(across(starts_with("weighted_"), ~sum(.x, na.rm = TRUE)))

# clean names by removing "weighted_" prefix
names(aggregated) <- gsub("^weighted_", "", names(aggregated))

# join with target polygons
walking_time_5min_interpolated <- target_polygons %>%
  left_join(aggregated, by = "rid")

################################################################################################################
# 10 Min Walking Time Area: AWI
################################################################################################################

# Note: follow the above procedure

source_polygons <- st_transform(ss_for_aw_interpolate, 31370)  
target_polygons <- st_transform(walking_time_polygons_10min, 31370) 

st_crs(source_polygons)
st_crs(target_polygons)

source_polygons$area <- st_area(source_polygons)

intersections <- st_intersection(source_polygons, target_polygons)

intersections$int_area <- st_area(intersections)


intersections <- intersections %>%
  mutate(area_weight = as.numeric(int_area / area))

vars_to_interpolate <- c(
  "ms_median_net_taxable_inc_2019", "total_all_11", "over_25_years_ss",
  "total_total_higher_education_graduate_edu_11", "total_total_doctorate_edu_11",
  "total_total_eduoverall_11", "overall_total_labmarket_11", "overall_total_under_15yo_11",
  "working_overall_less_than_20_years_11", "working_overall_total_population_11",
  "jobseekers_overall_less_than_20_years_11", "jobseekers_overall_total_population_11",
  "ISCO_08_high_status", "ISCO_08_skill_level_2", "ISCO_skill_level_1",
  "ISCO_not_elsewhere_classified", "nonbelgian_all_11", "belgian_all_11",
  "total_population_11", "area_km2_11", "n", "n_visitors_migrants",
  "owner_occupied_housing_11", "rental_properties_11", "inhabited_houses_not_indicated_11",
  "other_housing_or_collective_housing_1", "total_housing_11"
)

for (var in vars_to_interpolate) {
  weighted_var <- paste0("weighted_", var)
  intersections[[weighted_var]] <- intersections[[var]] * intersections$area_weight
}

aggregated <- intersections %>%
  st_drop_geometry() %>%
  group_by(rid) %>%
  summarise(across(starts_with("weighted_"), ~sum(.x, na.rm = TRUE)))

names(aggregated) <- gsub("^weighted_", "", names(aggregated))

# join with target polygons
walking_time_10min_interpolated <- target_polygons %>%
  left_join(aggregated, by = "rid")

################################################################################################################
# 15 Min Walking Time Area: AWI
################################################################################################################

source_polygons <- st_transform(ss_for_aw_interpolate, 31370)  
target_polygons <- st_transform(walking_time_polygons_15min, 31370)  

st_crs(source_polygons)
st_crs(target_polygons)

source_polygons$area <- st_area(source_polygons)

intersections <- st_intersection(source_polygons, target_polygons)

intersections$int_area <- st_area(intersections)

intersections <- intersections %>%
  mutate(area_weight = as.numeric(int_area / area))

vars_to_interpolate <- c(
  "ms_median_net_taxable_inc_2019", "total_all_11", "over_25_years_ss",
  "total_total_higher_education_graduate_edu_11", "total_total_doctorate_edu_11",
  "total_total_eduoverall_11", "overall_total_labmarket_11", "overall_total_under_15yo_11",
  "working_overall_less_than_20_years_11", "working_overall_total_population_11",
  "jobseekers_overall_less_than_20_years_11", "jobseekers_overall_total_population_11",
  "ISCO_08_high_status", "ISCO_08_skill_level_2", "ISCO_skill_level_1",
  "ISCO_not_elsewhere_classified", "nonbelgian_all_11", "belgian_all_11",
  "total_population_11", "area_km2_11", "n", "n_visitors_migrants",
  "owner_occupied_housing_11", "rental_properties_11", "inhabited_houses_not_indicated_11",
  "other_housing_or_collective_housing_1", "total_housing_11"
)

for (var in vars_to_interpolate) {
  weighted_var <- paste0("weighted_", var)
  intersections[[weighted_var]] <- intersections[[var]] * intersections$area_weight
}

aggregated <- intersections %>%
  st_drop_geometry() %>%
  group_by(rid) %>%
  summarise(across(starts_with("weighted_"), ~sum(.x, na.rm = TRUE)))

names(aggregated) <- gsub("^weighted_", "", names(aggregated))

walking_time_15min_interpolated <- target_polygons %>%
  left_join(aggregated, by = "rid")

################################################################################################################
# 5 Min Driving Time Area: AWI
################################################################################################################

source_polygons <- st_transform(ss_for_aw_interpolate, 31370)  
target_polygons <- st_transform(driving_time_polygons_5min, 31370)  

st_crs(source_polygons)
st_crs(target_polygons)

source_polygons$area <- st_area(source_polygons)

intersections <- st_intersection(source_polygons, target_polygons)

intersections$int_area <- st_area(intersections)

intersections <- intersections %>%
  mutate(area_weight = as.numeric(int_area / area))

vars_to_interpolate <- c(
  "ms_median_net_taxable_inc_2019", "total_all_11", "over_25_years_ss",
  "total_total_higher_education_graduate_edu_11", "total_total_doctorate_edu_11",
  "total_total_eduoverall_11", "overall_total_labmarket_11", "overall_total_under_15yo_11",
  "working_overall_less_than_20_years_11", "working_overall_total_population_11",
  "jobseekers_overall_less_than_20_years_11", "jobseekers_overall_total_population_11",
  "ISCO_08_high_status", "ISCO_08_skill_level_2", "ISCO_skill_level_1",
  "ISCO_not_elsewhere_classified", "nonbelgian_all_11", "belgian_all_11",
  "total_population_11", "area_km2_11", "n", "n_visitors_migrants",
  "owner_occupied_housing_11", "rental_properties_11", "inhabited_houses_not_indicated_11",
  "other_housing_or_collective_housing_1", "total_housing_11"
)

for (var in vars_to_interpolate) {
  weighted_var <- paste0("weighted_", var)
  intersections[[weighted_var]] <- intersections[[var]] * intersections$area_weight
}

aggregated <- intersections %>%
  st_drop_geometry() %>%
  group_by(rid) %>%
  summarise(across(starts_with("weighted_"), ~sum(.x, na.rm = TRUE)))

names(aggregated) <- gsub("^weighted_", "", names(aggregated))

driving_time_5min_interpolated <- target_polygons %>%
  left_join(aggregated, by = "rid")

################################################################################################################
# 10 Min Driving Time Area: AWI
################################################################################################################

source_polygons <- st_transform(ss_for_aw_interpolate, 31370)  
target_polygons <- st_transform(driving_time_polygons_10min, 31370)  

st_crs(source_polygons)
st_crs(target_polygons)

source_polygons$area <- st_area(source_polygons)

intersections <- st_intersection(source_polygons, target_polygons)

intersections$int_area <- st_area(intersections)

intersections <- intersections %>%
  mutate(area_weight = as.numeric(int_area / area))

vars_to_interpolate <- c(
  "ms_median_net_taxable_inc_2019", "total_all_11", "over_25_years_ss",
  "total_total_higher_education_graduate_edu_11", "total_total_doctorate_edu_11",
  "total_total_eduoverall_11", "overall_total_labmarket_11", "overall_total_under_15yo_11",
  "working_overall_less_than_20_years_11", "working_overall_total_population_11",
  "jobseekers_overall_less_than_20_years_11", "jobseekers_overall_total_population_11",
  "ISCO_08_high_status", "ISCO_08_skill_level_2", "ISCO_skill_level_1",
  "ISCO_not_elsewhere_classified", "nonbelgian_all_11", "belgian_all_11",
  "total_population_11", "area_km2_11", "n", "n_visitors_migrants",
  "owner_occupied_housing_11", "rental_properties_11", "inhabited_houses_not_indicated_11",
  "other_housing_or_collective_housing_1", "total_housing_11"
)

for (var in vars_to_interpolate) {
  weighted_var <- paste0("weighted_", var)
  intersections[[weighted_var]] <- intersections[[var]] * intersections$area_weight
}

aggregated <- intersections %>%
  st_drop_geometry() %>%
  group_by(rid) %>%
  summarise(across(starts_with("weighted_"), ~sum(.x, na.rm = TRUE)))

names(aggregated) <- gsub("^weighted_", "", names(aggregated))

driving_time_10min_interpolated <- target_polygons %>%
  left_join(aggregated, by = "rid")

################################################################################################################
# 15 Min Driving Time Area: AWI
################################################################################################################

source_polygons <- st_transform(ss_for_aw_interpolate, 31370) 
target_polygons <- st_transform(driving_time_polygons_15min, 31370)  

st_crs(source_polygons)
st_crs(target_polygons)

source_polygons$area <- st_area(source_polygons)

intersections <- st_intersection(source_polygons, target_polygons)

intersections$int_area <- st_area(intersections)

intersections <- intersections %>%
  mutate(area_weight = as.numeric(int_area / area))

vars_to_interpolate <- c(
  "ms_median_net_taxable_inc_2019", "total_all_11", "over_25_years_ss",
  "total_total_higher_education_graduate_edu_11", "total_total_doctorate_edu_11",
  "total_total_eduoverall_11", "overall_total_labmarket_11", "overall_total_under_15yo_11",
  "working_overall_less_than_20_years_11", "working_overall_total_population_11",
  "jobseekers_overall_less_than_20_years_11", "jobseekers_overall_total_population_11",
  "ISCO_08_high_status", "ISCO_08_skill_level_2", "ISCO_skill_level_1",
  "ISCO_not_elsewhere_classified", "nonbelgian_all_11", "belgian_all_11",
  "total_population_11", "area_km2_11", "n", "n_visitors_migrants",
  "owner_occupied_housing_11", "rental_properties_11", "inhabited_houses_not_indicated_11",
  "other_housing_or_collective_housing_1", "total_housing_11"
)

for (var in vars_to_interpolate) {
  weighted_var <- paste0("weighted_", var)
  intersections[[weighted_var]] <- intersections[[var]] * intersections$area_weight
}

aggregated <- intersections %>%
  st_drop_geometry() %>%
  group_by(rid) %>%
  summarise(across(starts_with("weighted_"), ~sum(.x, na.rm = TRUE)))

names(aggregated) <- gsub("^weighted_", "", names(aggregated))


driving_time_15min_interpolated <- target_polygons %>%
  left_join(aggregated, by = "rid")

# Check population size: should be enlarging with each aggregation

mean(walking_time_5min_interpolated$total_population_11, na.rm=T)
mean(walking_time_10min_interpolated$total_population_11, na.rm=T)
mean(walking_time_15min_interpolated$total_population_11, na.rm=T)
mean(driving_time_5min_interpolated$total_population_11, na.rm=T)
mean(driving_time_10min_interpolated$total_population_11, na.rm=T)
mean(driving_time_15min_interpolated$total_population_11, na.rm=T)

################################################################################################################ 
# Merge Neighborhood Indicators with Perceived Ethnic Threat
################################################################################################################

# Statistical Sector 

pt_ss <- merge(pt_to_merge, contextual_measures_ss_for_buffers_full,
               by = c("CD_REFNIS_SECTOR", "TX_SECTOR_DESCR_NL"),
               all.x = T)
# Municipality

# contextual_measures_mun_for_buffers <- read_csv("/Users/dariadementeva/contextual_measures_mun_for_buffers.csv")

contextual_measures_mun_for_buffers  <- contextual_measures_mun_for_buffers[,-1]

pt_mun <- merge(pt_to_merge, contextual_measures_mun_for_buffers,
                by = c("CD_REFNIS_SECTOR", "TX_SECTOR_DESCR_NL"),
                all.x = T)


# Walking Time Areas

pt_walking_5min <- merge(pt_to_merge, walking_time_5min_interpolated,
                         by = "rid",
                         all.x = T)

pt_walking_10min <- merge(pt_to_merge, walking_time_10min_interpolated,
                          by = "rid",
                          all.x = T)


pt_walking_15min <- merge(pt_to_merge, walking_time_15min_interpolated,
                          by = "rid",
                          all.x = T)

# Driving Time Areas

pt_driving_5min <- merge(pt_to_merge, driving_time_5min_interpolated,
                         by = "rid",
                         all.x = T)


pt_driving_10min <- merge(pt_to_merge, driving_time_10min_interpolated,
                          by = "rid",
                          all.x = T)


pt_driving_15min <- merge(pt_to_merge, driving_time_15min_interpolated,
                          by = "rid",
                          all.x = T)


################################################################################################################ 
# Feature Engineering: Neighborhood Indicators for Perceived Ethnic Threat
################################################################################################################

################################################################################################################
# Walking Areas: 5 min
################################################################################################################

# Proportion of workers over 16 years old working in a managerial, technical, or professional (high-status) job.

pt_walking_5min  <- pt_walking_5min  %>% 
  group_by(rid) %>% 
  mutate(workers_over_20_years_old_ss = overall_total_labmarket_11 - overall_total_under_15yo_11 - working_overall_less_than_20_years_11 - jobseekers_overall_less_than_20_years_11, 
         prop_workers_over_20_years_old_high_status_jobs = ISCO_08_high_status/workers_over_20_years_old_ss)

# Proportion of residents over 25 years old with a BA 

pt_walking_5min  <- pt_walking_5min  %>% 
  group_by(rid) %>% 
  mutate(higher_edu_diploma_holders_ss = sum(total_total_higher_education_graduate_edu_11 + total_total_doctorate_edu_11, na.rm = T),
         prop_higher_edu_diploma_holders_over25 = higher_edu_diploma_holders_ss / over_25_years_ss)

pt_walking_5min_processed <-  pt_walking_5min  %>% 
  group_by(rid) %>% 
  mutate(
    prop_outgroup_residential_diversity = nonbelgian_all_11/total_population_11,
    median_net_taxable_income = ms_median_net_taxable_inc_2019,
    prop_over_25yo_with_BA = prop_higher_edu_diploma_holders_over25,
    prop_over16yo_in_high_status_job = prop_workers_over_20_years_old_high_status_jobs,
    prop_owner_occupied = owner_occupied_housing_11/total_housing_11,
    prop_rental_properties =  rental_properties_11/total_housing_11,
    prop_visitor_outgroup_nonresidential_diversity = n_visitors_migrants/n,
    population_density = total_population_11/area_km2_11,
    p_belgian = belgian_all_11/total_population_11, 
    ethnic_entropy_index = -sum(prop_outgroup_residential_diversity*log(prop_outgroup_residential_diversity) + p_belgian*log(p_belgian), na.rm=T))

################################################################################################################
# Walking Areas: 10 min
################################################################################################################

pt_walking_10min  <- pt_walking_10min  %>% 
  group_by(rid) %>% 
  mutate(workers_over_20_years_old_ss = overall_total_labmarket_11 - overall_total_under_15yo_11 - working_overall_less_than_20_years_11 - jobseekers_overall_less_than_20_years_11, 
         prop_workers_over_20_years_old_high_status_jobs = ISCO_08_high_status/workers_over_20_years_old_ss)

# Proportion of residents over 25 years old with a BA 

pt_walking_10min  <- pt_walking_10min  %>% 
  group_by(rid) %>% 
  mutate(higher_edu_diploma_holders_ss = sum(total_total_higher_education_graduate_edu_11 + total_total_doctorate_edu_11, na.rm = T),
         prop_higher_edu_diploma_holders_over25 = higher_edu_diploma_holders_ss / over_25_years_ss)

pt_walking_10min_processed <-  pt_walking_10min  %>% 
  group_by(rid) %>% 
  mutate(
    prop_outgroup_residential_diversity = nonbelgian_all_11/total_population_11,
    median_net_taxable_income = ms_median_net_taxable_inc_2019,
    prop_over_25yo_with_BA = prop_higher_edu_diploma_holders_over25,
    prop_over16yo_in_high_status_job = prop_workers_over_20_years_old_high_status_jobs,
    prop_owner_occupied = owner_occupied_housing_11/total_housing_11,
    prop_rental_properties =  rental_properties_11/total_housing_11,
    prop_visitor_outgroup_nonresidential_diversity = n_visitors_migrants/n,
    population_density = total_population_11/area_km2_11,
    p_belgian = belgian_all_11/total_population_11, 
    ethnic_entropy_index = -sum(prop_outgroup_residential_diversity*log(prop_outgroup_residential_diversity) + p_belgian*log(p_belgian), na.rm=T))

################################################################################################################
# Walking Areas: 15 min
################################################################################################################

pt_walking_15min  <- pt_walking_15min  %>% 
  group_by(rid) %>% 
  mutate(workers_over_20_years_old_ss = overall_total_labmarket_11 - overall_total_under_15yo_11 - working_overall_less_than_20_years_11 - jobseekers_overall_less_than_20_years_11, 
         prop_workers_over_20_years_old_high_status_jobs = ISCO_08_high_status/workers_over_20_years_old_ss)

# Proportion of residents over 25 years old with a BA 

pt_walking_15min  <- pt_walking_15min  %>% 
  group_by(rid) %>% 
  mutate(higher_edu_diploma_holders_ss = sum(total_total_higher_education_graduate_edu_11 + total_total_doctorate_edu_11, na.rm = T),
         prop_higher_edu_diploma_holders_over25 = higher_edu_diploma_holders_ss / over_25_years_ss)

pt_walking_15min_processed <-  pt_walking_15min  %>% 
  group_by(rid) %>% 
  mutate(
    prop_outgroup_residential_diversity = nonbelgian_all_11/total_population_11,
    median_net_taxable_income = ms_median_net_taxable_inc_2019,
    prop_over_25yo_with_BA = prop_higher_edu_diploma_holders_over25,
    prop_over16yo_in_high_status_job = prop_workers_over_20_years_old_high_status_jobs,
    prop_owner_occupied = owner_occupied_housing_11/total_housing_11,
    prop_rental_properties =  rental_properties_11/total_housing_11,
    prop_visitor_outgroup_nonresidential_diversity = n_visitors_migrants/n,
    population_density = total_population_11/area_km2_11,
    p_belgian = belgian_all_11/total_population_11, 
    ethnic_entropy_index = -sum(prop_outgroup_residential_diversity*log(prop_outgroup_residential_diversity) + p_belgian*log(p_belgian), na.rm=T))


################################################################################################################
# Driving Areas: 5 min
################################################################################################################

# Proportion of workers over 16 years old working in a managerial, technical, or professional (high-status) job.

pt_driving_5min  <- pt_driving_5min  %>% 
  group_by(rid) %>% 
  mutate(workers_over_20_years_old_ss = overall_total_labmarket_11 - overall_total_under_15yo_11 - working_overall_less_than_20_years_11 - jobseekers_overall_less_than_20_years_11, 
         prop_workers_over_20_years_old_high_status_jobs = ISCO_08_high_status/workers_over_20_years_old_ss)

# Proportion of residents over 25 years old with a BA 

pt_driving_5min  <- pt_driving_5min  %>% 
  group_by(rid) %>% 
  mutate(higher_edu_diploma_holders_ss = sum(total_total_higher_education_graduate_edu_11 + total_total_doctorate_edu_11, na.rm = T),
         prop_higher_edu_diploma_holders_over25 = higher_edu_diploma_holders_ss / over_25_years_ss)

pt_driving_5min_processed <-  pt_driving_5min  %>% 
  group_by(rid) %>% 
  mutate(
    prop_outgroup_residential_diversity = nonbelgian_all_11/total_population_11,
    median_net_taxable_income = ms_median_net_taxable_inc_2019,
    prop_over_25yo_with_BA = prop_higher_edu_diploma_holders_over25,
    prop_over16yo_in_high_status_job = prop_workers_over_20_years_old_high_status_jobs,
    prop_owner_occupied = owner_occupied_housing_11/total_housing_11,
    prop_rental_properties =  rental_properties_11/total_housing_11,
    prop_visitor_outgroup_nonresidential_diversity = n_visitors_migrants/n,
    population_density = total_population_11/area_km2_11,
    p_belgian = belgian_all_11/total_population_11, 
    ethnic_entropy_index = -sum(prop_outgroup_residential_diversity*log(prop_outgroup_residential_diversity) + p_belgian*log(p_belgian), na.rm=T))

################################################################################################################
# Driving Areas: 10 min
################################################################################################################

pt_driving_10min  <- pt_driving_10min  %>% 
  group_by(rid) %>% 
  mutate(workers_over_20_years_old_ss = overall_total_labmarket_11 - overall_total_under_15yo_11 - working_overall_less_than_20_years_11 - jobseekers_overall_less_than_20_years_11, 
         prop_workers_over_20_years_old_high_status_jobs = ISCO_08_high_status/workers_over_20_years_old_ss)

# Proportion of residents over 25 years old with a BA 

pt_driving_10min  <- pt_driving_10min  %>% 
  group_by(rid) %>% 
  mutate(higher_edu_diploma_holders_ss = sum(total_total_higher_education_graduate_edu_11 + total_total_doctorate_edu_11, na.rm = T),
         prop_higher_edu_diploma_holders_over25 = higher_edu_diploma_holders_ss / over_25_years_ss)

pt_driving_10min_processed <-  pt_driving_10min  %>% 
  group_by(rid) %>% 
  mutate(
    prop_outgroup_residential_diversity = nonbelgian_all_11/total_population_11,
    median_net_taxable_income = ms_median_net_taxable_inc_2019,
    prop_over_25yo_with_BA = prop_higher_edu_diploma_holders_over25,
    prop_over16yo_in_high_status_job = prop_workers_over_20_years_old_high_status_jobs,
    prop_owner_occupied = owner_occupied_housing_11/total_housing_11,
    prop_rental_properties =  rental_properties_11/total_housing_11,
    prop_visitor_outgroup_nonresidential_diversity = n_visitors_migrants/n,
    population_density = total_population_11/area_km2_11,
    p_belgian = belgian_all_11/total_population_11, 
    ethnic_entropy_index = -sum(prop_outgroup_residential_diversity*log(prop_outgroup_residential_diversity) + p_belgian*log(p_belgian), na.rm=T))

################################################################################################################
# Driving Areas: 15 min
################################################################################################################

pt_driving_15min  <- pt_driving_15min  %>% 
  group_by(rid) %>% 
  mutate(workers_over_20_years_old_ss = overall_total_labmarket_11 - overall_total_under_15yo_11 - working_overall_less_than_20_years_11 - jobseekers_overall_less_than_20_years_11, 
         prop_workers_over_20_years_old_high_status_jobs = ISCO_08_high_status/workers_over_20_years_old_ss)

# Proportion of residents over 25 years old with a BA 

pt_driving_15min  <- pt_driving_15min  %>% 
  group_by(rid) %>% 
  mutate(higher_edu_diploma_holders_ss = sum(total_total_higher_education_graduate_edu_11 + total_total_doctorate_edu_11, na.rm = T),
         prop_higher_edu_diploma_holders_over25 = higher_edu_diploma_holders_ss / over_25_years_ss)

pt_driving_15min_processed <-  pt_driving_15min  %>% 
  group_by(rid) %>% 
  mutate(
    prop_outgroup_residential_diversity = nonbelgian_all_11/total_population_11,
    median_net_taxable_income = ms_median_net_taxable_inc_2019,
    prop_over_25yo_with_BA = prop_higher_edu_diploma_holders_over25,
    prop_over16yo_in_high_status_job = prop_workers_over_20_years_old_high_status_jobs,
    prop_owner_occupied = owner_occupied_housing_11/total_housing_11,
    prop_rental_properties =  rental_properties_11/total_housing_11,
    prop_visitor_outgroup_nonresidential_diversity = n_visitors_migrants/n,
    population_density = total_population_11/area_km2_11,
    p_belgian = belgian_all_11/total_population_11, 
    ethnic_entropy_index = -sum(prop_outgroup_residential_diversity*log(prop_outgroup_residential_diversity) + p_belgian*log(p_belgian), na.rm=T))

################################################################################################################
# Statistical Sectors
################################################################################################################

pt_ss  <- pt_ss  %>% 
  group_by(rid) %>% 
  mutate(workers_over_20_years_old_ss = overall_total_labmarket_11 - overall_total_under_15yo_11 - working_overall_less_than_20_years_11 - jobseekers_overall_less_than_20_years_11, 
         prop_workers_over_20_years_old_high_status_jobs = ISCO_08_high_status/workers_over_20_years_old_ss)

# Proportion of residents over 25 years old with a BA 

pt_ss  <- pt_ss  %>% 
  group_by(rid) %>% 
  mutate(higher_edu_diploma_holders_ss = sum(total_total_higher_education_graduate_edu_11 + total_total_doctorate_edu_11, na.rm = T),
         prop_higher_edu_diploma_holders_over25 = higher_edu_diploma_holders_ss / over_25_years_ss)

pt_ss_processed <-  pt_ss  %>% 
  group_by(rid) %>% 
  mutate(
    prop_outgroup_residential_diversity = nonbelgian_all_11/total_population_11,
    median_net_taxable_income = ms_median_net_taxable_inc_2019,
    prop_over_25yo_with_BA = prop_higher_edu_diploma_holders_over25,
    prop_over16yo_in_high_status_job = prop_workers_over_20_years_old_high_status_jobs,
    prop_owner_occupied = owner_occupied_housing_11/total_housing_11,
    prop_rental_properties =  rental_properties_11/total_housing_11,
    prop_visitor_outgroup_nonresidential_diversity = n_visitors_migrants/n,
    population_density = total_population_11/area_km2_11,
    p_belgian = belgian_all_11/total_population_11, 
    ethnic_entropy_index = -sum(prop_outgroup_residential_diversity*log(prop_outgroup_residential_diversity) + p_belgian*log(p_belgian), na.rm=T))

################################################################################################################
# Municipality
################################################################################################################

pt_mun  <- pt_mun  %>% 
  group_by(rid) %>% 
  mutate(workers_over_20_years_old_ss = overall_total_labmarket_11 - overall_total_under_15yo_11 - working_overall_less_than_20_years_11 - jobseekers_overall_less_than_20_years_11, 
         prop_workers_over_20_years_old_high_status_jobs = ISCO_08_high_status/workers_over_20_years_old_ss)

# Proportion of residents over 25 years old with a BA 

pt_mun  <- pt_mun  %>% 
  group_by(rid) %>% 
  mutate(higher_edu_diploma_holders_ss = sum(total_total_higher_education_graduate_edu_11 + total_total_doctorate_edu_11, na.rm = T),
         prop_higher_edu_diploma_holders_over25 = higher_edu_diploma_holders_ss / over_25_years_ss)

pt_mun_processed <-  pt_mun  %>% 
  group_by(rid) %>% 
  mutate(
    prop_outgroup_residential_diversity = nonbelgian_all_11/total_population_11,
    median_net_taxable_income = ms_median_net_taxable_inc_2019,
    prop_over_25yo_with_BA = prop_higher_edu_diploma_holders_over25,
    prop_over16yo_in_high_status_job = prop_workers_over_20_years_old_high_status_jobs,
    prop_owner_occupied = owner_occupied_housing_11/total_housing_11,
    prop_rental_properties =  rental_properties_11/total_housing_11,
    prop_visitor_outgroup_nonresidential_diversity = n_visitors_migrants/n,
    population_density = total_population_11/area_km2_11,
    p_belgian = belgian_all_11/total_population_11, 
    ethnic_entropy_index = -sum(prop_outgroup_residential_diversity*log(prop_outgroup_residential_diversity) + p_belgian*log(p_belgian), na.rm=T))


################################################################################################################
# Create Summary Statistics per Aggregation
################################################################################################################

vars <- c("rid",
          "TX_SECTOR_DESCR_NL.x",
          "CD_REFNIS_SECTOR.x",
          "q49_cumulative",
          "gender",
          "subjective_social_class",
          "age6",
          "edu3",
          "q14",
          "prop_outgroup_residential_diversity",
          "ethnic_entropy_index",
          "median_net_taxable_income",
          "prop_over_25yo_with_BA",
          "prop_over16yo_in_high_status_job",
          "prop_owner_occupied",
          "prop_rental_properties",
          "prop_visitor_outgroup_nonresidential_diversity",
          "population_density")

pt_walking_5min_processed <- pt_walking_5min_processed[,vars]
pt_walking_10min_processed <- pt_walking_10min_processed[,vars]
pt_walking_15min_processed <- pt_walking_15min_processed[,vars]
pt_driving_5min_processed <- pt_driving_5min_processed[,vars]
pt_driving_10min_processed <- pt_driving_10min_processed[,vars]
pt_driving_15min_processed <- pt_driving_15min_processed[,vars]

pt_ss_processed <- pt_ss_processed[,c("rid",
                                      "TX_SECTOR_DESCR_NL",
                                      "CD_REFNIS_SECTOR",
                                      "q49_cumulative",
                                      "gender",
                                      "subjective_social_class",
                                      "age6",
                                      "edu3",
                                      "q14",
                                      "prop_outgroup_residential_diversity",
                                      "ethnic_entropy_index",
                                      "median_net_taxable_income",
                                      "prop_over_25yo_with_BA",
                                      "prop_over16yo_in_high_status_job",
                                      "prop_owner_occupied",
                                      "prop_rental_properties",
                                      "prop_visitor_outgroup_nonresidential_diversity",
                                      "population_density")]

pt_mun_processed <- pt_mun_processed[,c("rid",
                                        "TX_SECTOR_DESCR_NL",
                                        "CD_REFNIS_SECTOR",
                                        "q49_cumulative",
                                        "gender",
                                        "subjective_social_class",
                                        "age6",
                                        "edu3",
                                        "q14",
                                        "prop_outgroup_residential_diversity",
                                        "ethnic_entropy_index",
                                        "median_net_taxable_income",
                                        "prop_over_25yo_with_BA",
                                        "prop_over16yo_in_high_status_job",
                                        "prop_owner_occupied",
                                        "prop_rental_properties",
                                        "prop_visitor_outgroup_nonresidential_diversity",
                                        "population_density")]

datasets <- list(
  w_5 = pt_walking_5min_processed[, 10:18],
  w_10 = pt_walking_10min_processed[, 10:18],
  w_15 = pt_walking_15min_processed[, 10:18],
  d_5 = pt_driving_5min_processed[, 10:18],
  d_10 = pt_driving_10min_processed[, 10:18],
  d_15 = pt_driving_15min_processed[, 10:18],
  ss = pt_ss_processed[, 10:18],
  mun = pt_mun_processed[, 10:18]
)

# create a wrapper function to calculate summary statistics (mean, median, min, max, sd)
get_summary_statistics <- function(df) {
  summary_stats <- data.frame(
    Variable = names(df),
    Mean = sapply(df, mean, na.rm = TRUE),
    Median = sapply(df, median, na.rm = TRUE),
    Min = sapply(df, min, na.rm = TRUE),
    Max = sapply(df, max, na.rm = TRUE),
    SD = sapply(df, sd, na.rm = TRUE)
  )
  return(summary_stats)
}

# store the result in a list
summary_list <- lapply(datasets, get_summary_statistics)


# save to an Excel file
wb <- createWorkbook()

# add a separate sheet for each dataset summary
for (dataset_name in names(summary_list)) {
  # Add a worksheet for the dataset
  addWorksheet(wb, dataset_name)

  # the summary statistics to the sheet
  writeData(wb, sheet = dataset_name, summary_list[[dataset_name]])
}

saveWorkbook(wb, "summary_statistics_oct.xlsx", overwrite = TRUE)


################################################################################################################
# Analysis, Pre-Process IVs, Scale Predictors
################################################################################################################

set.seed(05081997)

pt_walking_5min_processed_imp <- impute_mode(pt_walking_5min_processed[, 5:9], type = "columnwise", convert_tibble = F) 
pt_walking_10min_processed_imp <- impute_mode(pt_walking_10min_processed[, 5:9], type = "columnwise", convert_tibble = F) 
pt_walking_15min_processed_imp <- impute_mode(pt_walking_15min_processed[, 5:9], type = "columnwise", convert_tibble = F) 
pt_driving_5min_processed_imp <- impute_mode(pt_driving_5min_processed[, 5:9], type = "columnwise", convert_tibble = F) 
pt_driving_10min_processed_imp <- impute_mode(pt_driving_10min_processed[, 5:9], type = "columnwise", convert_tibble = F)
pt_driving_15min_processed_imp <- impute_mode(pt_driving_15min_processed[, 5:9], type = "columnwise", convert_tibble = F) 
pt_ss_imp <- impute_mode(pt_ss_processed[, 5:9], type = "columnwise", convert_tibble = F) 
pt_mun_imp <- impute_mode(pt_mun_processed[, 5:9], type = "columnwise", convert_tibble = F) 

pt_walking_5min_processed_imp_1 <- impute_mode(pt_walking_5min_processed[, 10:18], type = "columnwise", convert_tibble = F) %>% scale()
pt_walking_10min_processed_imp_1 <- impute_mode(pt_walking_10min_processed[, 10:18], type = "columnwise", convert_tibble = F) %>% scale()
pt_walking_15min_processed_imp_1 <- impute_mode(pt_walking_15min_processed[, 10:18], type = "columnwise", convert_tibble = F) %>% scale()
pt_driving_5min_processed_imp_1 <- impute_mode(pt_driving_5min_processed[, 10:18], type = "columnwise", convert_tibble = F) %>% scale()
pt_driving_10min_processed_imp_1 <- impute_mode(pt_driving_10min_processed[, 10:18], type = "columnwise", convert_tibble = F) %>% scale()
pt_driving_15min_processed_imp_1 <- impute_mode(pt_driving_15min_processed[, 10:18], type = "columnwise", convert_tibble = F) %>% scale()
pt_ss_imp_1 <- impute_mode(pt_ss_processed[, 10:18], type = "columnwise", convert_tibble = F) %>% scale()
pt_mun_imp_1 <- impute_mode(pt_mun_processed[, 10:18], type = "columnwise", convert_tibble = F) %>% scale()


pt_walking_5min_processed_imp_scaled <-  cbind(pt_walking_5min_processed[1:4],
                                               pt_walking_5min_processed_imp,
                                               pt_walking_5min_processed_imp_1)

pt_walking_10min_processed_imp_scaled <-  cbind(pt_walking_10min_processed[1:4],
                                                pt_walking_10min_processed_imp,
                                                pt_walking_10min_processed_imp_1)

pt_walking_15min_processed_imp_scaled <-  cbind(pt_walking_15min_processed[1:4],
                                                pt_walking_15min_processed_imp,
                                                pt_walking_15min_processed_imp_1)

pt_driving_5min_processed_imp_scaled <-  cbind(pt_driving_5min_processed[1:4],
                                               pt_driving_5min_processed_imp,
                                               pt_driving_5min_processed_imp_1)

pt_driving_10min_processed_imp_scaled <-  cbind(pt_driving_10min_processed[1:4],
                                                pt_driving_10min_processed_imp,
                                                pt_driving_10min_processed_imp_1)

pt_driving_15min_processed_imp_scaled <-  cbind(pt_driving_15min_processed[1:4],
                                                pt_driving_15min_processed_imp,
                                                pt_driving_15min_processed_imp_1)

pt_ss_imp_scaled <-  cbind(pt_ss_processed[1:4],
                           pt_ss_imp,
                           pt_ss_imp_1)

pt_mun_imp_scaled <-  cbind(pt_mun_processed[1:4],
                            pt_mun_imp,
                            pt_mun_imp_1)



create_dummies <- function(data) {
  data %>%
    mutate( 
      # gender
      gender_man = ifelse(gender == 1, 1, 0),
      gender_woman = ifelse(gender == 2, 1, 0),
      
      # subjective social class    
      subjective_sc_working_class = ifelse(subjective_social_class == 1, 1, 0),
      subjective_sc_low_middle_class = ifelse(subjective_social_class == 2, 1, 0),
      subjective_sc_higher_middle_class = ifelse(subjective_social_class == 3, 1, 0),
      subjective_sc_upper_class = ifelse(subjective_social_class == 4, 1, 0),
      subjective_sc_other_class = ifelse(subjective_social_class %in% c(7, 9), 1, 0),
      
      # age
      age_18_24 = ifelse(age6 == 1, 1, 0),
      age_25_34 = ifelse(age6 == 2, 1, 0),
      age_35_44 = ifelse(age6 == 3, 1, 0),
      age_45_54 = ifelse(age6 == 4, 1, 0),
      age_55_64 = ifelse(age6 == 5, 1, 0),
      age_65_93 = ifelse(age6 == 6, 1, 0),
      
      # education
      edu_none_lower = ifelse(edu3 == 1, 1, 0),
      edu_higher_secondary = ifelse(edu3 == 2, 1, 0),
      edu_higher_university = ifelse(edu3 == 3, 1, 0),
      
      # tenure status
      owner = ifelse(q14 == 1, 1, 0)
    )
}

pt_walking_5min_final <- create_dummies(pt_walking_5min_processed_imp_scaled)
pt_walking_10min_final <- create_dummies(pt_walking_10min_processed_imp_scaled)
pt_walking_15min_final <- create_dummies(pt_walking_15min_processed_imp_scaled)

pt_driving_5min_final <- create_dummies(pt_driving_5min_processed_imp_scaled)
pt_driving_10min_final <- create_dummies(pt_driving_10min_processed_imp_scaled)
pt_driving_15min_final <- create_dummies(pt_driving_15min_processed_imp_scaled)

pt_ss_final <- create_dummies(pt_ss_imp_scaled)
pt_mun_final <- create_dummies(pt_mun_imp_scaled)


################################################################################################################
# OLS, regular standard errors
################################################################################################################

pt_walking_5min_model <- lm(
  q49_cumulative ~ 
    prop_outgroup_residential_diversity +
    prop_visitor_outgroup_nonresidential_diversity +
    median_net_taxable_income +
    prop_over_25yo_with_BA +
    prop_over16yo_in_high_status_job +
    prop_owner_occupied +
    population_density +
    gender_woman +
    subjective_sc_working_class +
    subjective_sc_low_middle_class +
    subjective_sc_upper_class + 
    subjective_sc_other_class +
    age_18_24 +
    age_25_34 +
    age_35_44 +
    age_45_54 +
    age_55_64 +
    edu_none_lower +
    edu_higher_secondary +
    owner,
  data = pt_walking_5min_final
)

summary(pt_walking_5min_model)

pt_walking_10min_model <- lm(
  q49_cumulative ~ 
    prop_outgroup_residential_diversity +
    prop_visitor_outgroup_nonresidential_diversity +
    median_net_taxable_income +
    prop_over_25yo_with_BA +
    prop_over16yo_in_high_status_job +
    prop_owner_occupied +
    population_density +
    gender_woman +
    subjective_sc_working_class +
    subjective_sc_low_middle_class +
    subjective_sc_upper_class + 
    subjective_sc_other_class +
    age_18_24 +
    age_25_34 +
    age_35_44 +
    age_45_54 +
    age_55_64 +
    edu_none_lower +
    edu_higher_secondary +
    owner,
  data = pt_walking_10min_final
)

summary(pt_walking_10min_model)


pt_walking_15min_model <- lm(
  q49_cumulative ~ 
    prop_outgroup_residential_diversity +
    prop_visitor_outgroup_nonresidential_diversity +
    median_net_taxable_income +
    prop_over_25yo_with_BA +
    prop_over16yo_in_high_status_job +
    prop_owner_occupied +
    population_density +
    gender_woman +
    subjective_sc_working_class +
    subjective_sc_low_middle_class +
    subjective_sc_upper_class + 
    subjective_sc_other_class +
    age_18_24 +
    age_25_34 +
    age_35_44 +
    age_45_54 +
    age_55_64 +
    edu_none_lower +
    edu_higher_secondary +
    owner,
  data = pt_walking_15min_final
)

summary(pt_walking_15min_model)


pt_driving_5min_model <- lm(
  q49_cumulative ~ 
    prop_outgroup_residential_diversity +
    prop_visitor_outgroup_nonresidential_diversity +
    median_net_taxable_income +
    prop_over_25yo_with_BA +
    prop_over16yo_in_high_status_job +
    prop_owner_occupied +
    population_density +
    gender_woman +
    subjective_sc_working_class +
    subjective_sc_low_middle_class +
    subjective_sc_upper_class + 
    subjective_sc_other_class +
    age_18_24 +
    age_25_34 +
    age_35_44 +
    age_45_54 +
    age_55_64 +
    edu_none_lower +
    edu_higher_secondary +
    owner,
  data = pt_driving_5min_final
)

summary(pt_driving_5min_model)


pt_driving_10min_model <- lm(
  q49_cumulative ~ 
    prop_outgroup_residential_diversity +
    prop_visitor_outgroup_nonresidential_diversity +
    median_net_taxable_income +
    prop_over_25yo_with_BA +
    prop_over16yo_in_high_status_job +
    prop_owner_occupied +
    population_density +
    gender_woman +
    subjective_sc_working_class +
    subjective_sc_low_middle_class +
    subjective_sc_upper_class + 
    subjective_sc_other_class +
    age_18_24 +
    age_25_34 +
    age_35_44 +
    age_45_54 +
    age_55_64 +
    edu_none_lower +
    edu_higher_secondary +
    owner,
  data = pt_driving_10min_final
)

summary(pt_driving_10min_model)


pt_driving_15min_model <- lm(
  q49_cumulative ~ 
    prop_outgroup_residential_diversity +
    prop_visitor_outgroup_nonresidential_diversity +
    median_net_taxable_income +
    prop_over_25yo_with_BA +
    prop_over16yo_in_high_status_job +
    prop_owner_occupied +
    population_density +
    gender_woman +
    subjective_sc_working_class +
    subjective_sc_low_middle_class +
    subjective_sc_upper_class + 
    subjective_sc_other_class +
    age_18_24 +
    age_25_34 +
    age_35_44 +
    age_45_54 +
    age_55_64 +
    edu_none_lower +
    edu_higher_secondary +
    owner,
  data = pt_driving_15min_final
)

summary(pt_driving_15min_model)

pt_ss_model <- lm(
  q49_cumulative ~ 
    prop_outgroup_residential_diversity +
    prop_visitor_outgroup_nonresidential_diversity +
    median_net_taxable_income +
    prop_over_25yo_with_BA +
    prop_over16yo_in_high_status_job +
    prop_owner_occupied +
    population_density +
    gender_woman +
    subjective_sc_working_class +
    subjective_sc_low_middle_class +
    subjective_sc_upper_class + 
    subjective_sc_other_class +
    age_18_24 +
    age_25_34 +
    age_35_44 +
    age_45_54 +
    age_55_64 +
    edu_none_lower +
    edu_higher_secondary +
    owner,
  data = pt_ss_final
)

summary(pt_ss_model)


pt_mun_model <- lm(
  q49_cumulative ~ 
    prop_outgroup_residential_diversity +
    prop_visitor_outgroup_nonresidential_diversity +
    median_net_taxable_income +
    prop_over_25yo_with_BA +
    prop_over16yo_in_high_status_job +
    prop_owner_occupied +
    population_density +
    gender_woman +
    subjective_sc_working_class +
    subjective_sc_low_middle_class +
    subjective_sc_upper_class + 
    subjective_sc_other_class +
    age_18_24 +
    age_25_34 +
    age_35_44 +
    age_45_54 +
    age_55_64 +
    edu_none_lower +
    edu_higher_secondary +
    owner,
  data = pt_mun_final
)
summary(pt_mun_model)


 model_list <- list(
   "W_5" = pt_walking_5min_model,
   "W_10" = pt_walking_10min_model,
   "W_15" = pt_walking_15min_model,
   "D_5" = pt_driving_5min_model,
   "D_10" = pt_driving_10min_model,
   "D_15" = pt_driving_15min_model,
   "SS" = pt_ss_model,
   "Mun" = pt_mun_model)

  
# # Capture stargazer output as plain text
 reg_output <- capture.output(
   stargazer(model_list,
             title = "Regression Models",
             column.labels = names(model_list),
             align = TRUE,
             no.space = TRUE,
             type="html",
             out="pt_results_ols_regular_se_oct.html"))


 regression_outputs_to_save <- c(
   "pt_walking_5min_model",
   "pt_walking_10min_model",
   "pt_walking_15min_model",
   "pt_driving_5min_model",
   "pt_driving_10min_model",
   "pt_driving_15min_model",
   "pt_ss_model",
   "pt_mun_model")
# 
save(list = regression_outputs_to_save, file = "OLS_SE_regular_printout_upd.RData")

################################################################################################################
# Check Assumptions
################################################################################################################

# Normality - ok

qqPlot(pt_walking_5min_model) 
qqPlot(pt_walking_10min_model) 
qqPlot(pt_walking_15min_model) 
qqPlot(pt_driving_5min_model) 
qqPlot(pt_driving_10min_model) 
qqPlot(pt_driving_15min_model) 
qqPlot(pt_ss_model) 
qqPlot(pt_mun_model) 

shapiro.test(pt_walking_5min_model$residuals) # ok
shapiro.test(pt_walking_10min_model$residuals) # ok
shapiro.test(pt_walking_15min_model$residuals) # ok
shapiro.test(pt_driving_5min_model$residuals) # ok
shapiro.test(pt_driving_10min_model$residuals) # ok
shapiro.test(pt_driving_15min_model$residuals) # ok
shapiro.test(pt_ss_model$residuals) # ok
shapiro.test(pt_mun_model$residuals) # ok

# Homoscedasticity - ok, no funnel shapes

plot(resid(pt_walking_5min_model))
plot(resid(pt_walking_10min_model))
plot(resid(pt_walking_15min_model))
plot(resid(pt_driving_5min_model))
plot(resid(pt_driving_10min_model))
plot(resid(pt_walking_15min_model))
plot(resid(pt_ss_model))
plot(resid(pt_mun_model))

bptest(pt_walking_5min_model) # no heteroscedasticity.
bptest(pt_walking_10min_model) # no heteroscedasticity.
bptest(pt_walking_15min_model) # no heteroscedasticity.
bptest(pt_driving_5min_model) # no heteroscedasticity.
bptest(pt_driving_10min_model) # no heteroscedasticity.
bptest(pt_walking_15min_model) # no heteroscedasticity.
bptest(pt_ss_model) # no heteroscedasticity.
bptest(pt_mun_model) # no heteroscedasticity.

# Autocorrelation - ok 
# DW statistic ≈ 2 means no autocorrelation.

dwtest(pt_walking_5min_model) # ~1.9 
dwtest(pt_walking_10min_model) # ~1.9
dwtest(pt_walking_15min_model) # ~1.9 
dwtest(pt_driving_5min_model) # ~1.9 
dwtest(pt_driving_10min_model) #  ~1.9  
dwtest(pt_driving_15min_model) #  ~1.9 
dwtest(pt_ss_model) # ~1.9
dwtest(pt_mun_model) # ~1.9

# Multicollinearity - ok; all less than 5-10

vif(pt_walking_5min_model)
vif(pt_walking_10min_model)
vif(pt_walking_15min_model)
vif(pt_driving_5min_model)
vif(pt_driving_10min_model)
vif(pt_driving_15min_model)
vif(pt_ss_model)
vif(pt_mun_model)

################################################################################################################
# OLS: Apply Robust HAC Standard Errors 
################################################################################################################

pt_walking_5min_model_re <- coeftest(pt_walking_5min_model, vcov = NeweyWest)
pt_walking_10min_model_re <- coeftest(pt_walking_10min_model, vcov = NeweyWest)
pt_walking_15min_model_re <- coeftest(pt_walking_15min_model, vcov = NeweyWest)

pt_driving_5min_model_re <- coeftest(pt_driving_5min_model, vcov = NeweyWest)
pt_driving_10min_model_re <- coeftest(pt_driving_10min_model, vcov = NeweyWest)
pt_driving_15min_model_re <-coeftest(pt_driving_15min_model, vcov = NeweyWest)

pt_ss_model_re <- coeftest(pt_ss_model, vcov = NeweyWest)
pt_mun_model_re <- coeftest(pt_mun_model, vcov = NeweyWest)

model_list_re <- list(
  "W_5" = pt_walking_5min_model_re,
  "W_10" = pt_walking_10min_model_re,
  "W_15" = pt_walking_15min_model_re,
  "D_5" = pt_driving_5min_model_re,
  "D_10" = pt_driving_10min_model_re,
  "D_15" = pt_driving_15min_model_re,
  "SS" = pt_ss_model_re,
  "Mun" = pt_mun_model_re)

################################################################################################################
# Check Final Outputs
################################################################################################################

pt_walking_5min_model_re
pt_walking_10min_model_re
pt_walking_15min_model_re
pt_driving_5min_model_re
pt_driving_10min_model_re
pt_driving_15min_model_re
pt_ss_model_re 
pt_mun_model_re

outputs_to_save <- c(
  "pt_walking_5min_model_re",
  "pt_walking_10min_model_re",
  "pt_walking_15min_model_re",
  "pt_driving_5min_model_re",
  "pt_driving_10min_model_re",
  "pt_driving_15min_model_re",
  "pt_ss_model_re",
  "pt_mun_model_re"
)

 save(list = outputs_to_save, file = "OLS_HAC_SE_printout_upd.RData")

reg_output_re <- capture.output(
  stargazer(model_list_re,
            title = "Regression Models",
            column.labels = names(model_list_re),
            align = TRUE,
            no.space = TRUE,
            type="html",
            out="pt_ugcp_upd_oct.html"))



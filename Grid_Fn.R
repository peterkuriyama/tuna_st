Grid_Fn = function(Data_Geostat,zone,grid_dim_km){
  
# setwd("C:/Users/hkxu/OneDrive - IATTC/IATTC/spatio-temporal workshop")

library(VAST)

lat_North <- Data_Geostat$Lat > 0
Data_Geostat_North <- Data_Geostat[lat_North,]

Region = "Other"
Extrapolation_List_North = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn( Region=Region, grid_dim_km=c(grid_dim_km,grid_dim_km), strata.limits=strata.limits, observations_LL=Data_Geostat_North[,c('Lat','Lon')], maximum_distance_from_sample=10,zone=zone)

lat_South <- Data_Geostat$Lat < 0
Data_Geostat_South <- Data_Geostat[lat_South,]

# source("Prepare_Extrapolation_Data_Fn.R")
# source("Prepare_Other_Extrapolation_Data_Fn.R")
Extrapolation_List_South = Prepare_Extrapolation_Data_Fn( Region=Region, strata.limits=strata.limits, grid_dim_km=c(grid_dim_km,grid_dim_km), observations_LL=Data_Geostat_South[,c('Lat','Lon')], maximum_distance_from_sample=10,zone=zone)

Extrapolation_List_South$Data_Extrap$N_km <- Extrapolation_List_South$Data_Extrap$N_km - 10000

a_el <- rbind(Extrapolation_List_North$a_el,Extrapolation_List_South$a_el)
Data_Extrap <- rbind(Extrapolation_List_North$Data_Extrap,Extrapolation_List_South$Data_Extrap)
zone <- zone
flip_around_dateline <- Extrapolation_List_North$flip_around_dateline
Area_km2_x <- c(Extrapolation_List_North$Area_km2_x,Extrapolation_List_South$Area_km2_x)

Extrapolation_List <- list("a_el" = a_el, "Data_Extrap" = Data_Extrap, "zone" = zone,
                           "flip_around_dateline" = flip_around_dateline,
                           "Area_km2_x" = Area_km2_x)
}
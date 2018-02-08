#-----------------------------------------------------------------------------------------------------
#Set working directory based on computer I'm using
setwd("C:\\Users\\Peter\\Dropbox\\postdoc\\tuna_st")
if(Sys.info()[1] == 'Windows') setwd('\\Users\\peter.kuriyama\\Desktop\\tuna_st')
if(Sys.info()[1] != 'Windows') setwd('/Users/peterkuriyama/Dropbox/postdoc/tuna_st')

#-----------------------------------------------------------------------------------------------------
#Install packages
# install.packages("TMB")

# devtools::install_github("james-thorson/VAST",force = TRUE)
# devtools::install_github("james-thorson/utilities",force = TRUE)
# devtools::install_github("nwfsc-assess/geostatistical_delta-GLMM",force = TRUE)

#Load Packages
library(TMB)
library(plyr)
library(VAST)
library(tidyverse)
library(SpatialDeltaGLMM)
library(reshape2)
source("Grid_Fn.R")
source("process_bigeye_data.R")
#-----------------------------------------------------------------------------------------------------
#Load the Data
the_data <- process_bigeye_data(nspp = 3)

#Check the proportion zeroes in the data
the_data[[2]] %>% group_by(spp, year, lat, lon) %>% summarize(nzeroes = length(which(cpue == 0))) %>% 
  ungroup %>% distinct(nzeroes)
hist(the_data[[2]]$cpue, breaks = 50)

head(the_data[[2]])

#-----------------------------------------------------------------------------------------------------
#Plot the data on a map
mapdata()


the_data[[1]] %>% ggplot(aes(x = lon, y = lat)) + geom_tile(aes(fill = cpue))  +
  facet_wrap(~year)

the_data[[1]] %>% ggplot(aes(x = lon, y = lat)) + geom_tile(fill = 'white') + facet_wrap(~ year)

#-----------------------------------------------------------------------------------------------------
#VAST Model

#Link for handling zero or 100 percent encounter probability
# https://github.com/nwfsc-assess/geostatistical_delta-GLMM/wiki/What-to-do-with-a-species-with-0%25-or-100%25-encounters-in-any-year

#---------------------------------
#Specify VAST Model Configurations
Version <- "VAST_v4_0_0"
Method <- c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km <- 10
n_x <- c(10, 45, 100, 200, 1000)[1] #number of stations
Kmeans_Config <-  list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )

#Specify number of species
nspp <- 1

#------------FieldConfig
#Omega refers to spatial variation
  #Omega1 - variation in encounter probability
  #Omega2 - variation in positive catch rates; 0 is off
#Epsilon is spatiotemporal variation
#AR1 is an AR1 process
#>0 is number of elements in factor-analysis covariance
FieldConfig <- c("Omega1"= nspp, "Epsilon1"=nspp, "Omega2"=nspp, "Epsilon2"=nspp) #

#------------Rhoconfig
#Specify structure of time intervals (fixed effect or random effect)
# 1: 
RhoConfig <- c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) #Parameters among years

#------------Overdispersion Config
#Optional, vector governing correlated overdispersion among categories v_i (vessels)
OverdispersionConfig <- c("Vessel"=0, "VesselYear"=0)  #Different vessel catchabilities, 0 means they the same

#------------ObsModel
#First element specifies distribution of positive catch rates
  #0 - normal
  #1 - lognormal
  #2 - Gamma
  #5 - negative binomial
  #7 - poisson
#Second element specifies functional form of encounter probabilities
  #0 - Conventional delta-model, logit-link encounter and log-link positive catch rates
  #1 - Alternative delta-model, log-link for numbers-density, log-link biomass per number
  #2 - Link for tweedie observation
  #3 - Conventional delta-model, fixes encounter probability to 1
ObsModel <- c(1, 0) 

#Specify options
Options <- c("SD_site_density"=0, "SD_site_logdensity"=0, "Calculate_Range"=1,
             "Calculate_evenness"=0, "Calculate_effective_area"=1, "Calculate_Cov_SE"=0,
             'Calculate_Synchrony'=0, 'Calculate_Coherence'=0)

strata.limits <- data.frame('STRATA'="EPO")
Region <- "Other"

#Look at proportion of zeroes
# the_data$bet_numbers_annual %>% group_by(year, lat, lon) 

#Look at number of zeroes 

run_vast(dat = the_data[[1]], 
  ObsModel = c(1, 0))

head(the_data[[1]])


#---------------------------------
run_vast <- function(dat, ObsModel){

  #Specify 
  DateFile = paste0(getwd(),"/VAST_output_", paste0("Obs_", 
    paste(ObsModel, collapse = "_")))
  dir.create(DateFile)

  #Save configurations
  Record = ThorsonUtilities::bundlelist( c("Version","Method","grid_size_km","n_x","FieldConfig","RhoConfig","OverdispersionConfig","ObsModel","Kmeans_Config") )
  save( Record, file=file.path(DateFile,"Record.RData"))
  capture.output( Record, file=paste0(DateFile,"Record.txt"))

  #Specify the geostatistical data  
  Data_Geostat <- data.frame(spp = as.character(dat$spp), 
                            Catch_num = dat$cpue,
                            Year = dat$year, 
                            Vessel = "missing", 
                            Lat = dat$lat,
                            AreaSwept_km2 = 1, 
                            Lon = dat$lon)

  
  #Specify extrapolation list
  Extrapolation_List <- Grid_Fn(Data_Geostat = Data_Geostat,
    zone = 12, grid_dim_km = 10)
  save(Extrapolation_List, file = paste0(DateFile, "Extrapolation_List.RData"))
  
  Spatial_List = Spatial_Information_Fn( grid_size_km = grid_size_km, n_x = n_x, 
    Method = Method, Lon = Data_Geostat[,'Lon'], Lat = Data_Geostat[,'Lat'], 
    Extrapolation_List = Extrapolation_List, 
    randomseed = Kmeans_Config[["randomseed"]], nstart = Kmeans_Config[["nstart"]],  
   iter.max = Kmeans_Config[["iter.max"]], DirPath = DateFile, Save_Results = FALSE)
  
  # Add knots to Data_Geostat
  Data_Geostat = cbind(Data_Geostat, "knot_i"=Spatial_List$knot_i)
  SpatialDeltaGLMM::Plot_data_and_knots(Extrapolation_List = Extrapolation_List,
   Spatial_List = Spatial_List, Data_Geostat = Data_Geostat, PlotDir = DateFile)
browser()
  #---------------------------------
  #Run the model
  TmbData = VAST::Data_Fn("Version" = Version, "FieldConfig" = FieldConfig, 
    "OverdispersionConfig" = OverdispersionConfig,  
    "RhoConfig" = RhoConfig, "ObsModel" = ObsModel, 
    "c_i" = as.numeric(Data_Geostat$spp) - 1, 
    "b_i" = Data_Geostat$Catch_num, 
    "a_i" = Data_Geostat$AreaSwept_km2, 
    "v_i" = as.numeric(Data_Geostat$Vessel) - 1, 
    "s_i" = Data_Geostat$knot_i - 1, 
    "t_i" = Data_Geostat$Year, 
    "a_xl" = Spatial_List$a_xl, 
    "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, 
    "Method"=Spatial_List$Method, "Options"=Options )
  
  # hist(dat$cpue, breaks = 50)
  
  TmbList = VAST::Build_TMB_Fn("TmbData" = TmbData, 
    "RunDir" = DateFile, 
    "Version" = Version, 
    "RhoConfig" = RhoConfig, 
    "loc_x" = Spatial_List$loc_x, "Method" = Method)
  Obj = TmbList[["Obj"]]
  
  Opt = TMBhelper::Optimize(obj = Obj, 
    newtonsteps = 1,
    lower = TmbList[["Lower"]], upper = TmbList[["Upper"]], getsd = TRUE, 
    savedir = DateFile, bias.correct = FALSE)
  browser()
}


#Specify data and 





#---------------------------------
#Process data
#Specify dat to use

dat <- bet_comps_annual


# For function: Specify data and ObsModel




#Specify extrapolation list
Extrapolation_List <- Grid_Fn(Data_Geostat = Data_Geostat,
  zone = 12, grid_dim_km = 10)
save(Extrapolation_List, file = paste0(DateFile, "Extrapolation_List.RData"))

Spatial_List = Spatial_Information_Fn( grid_size_km = grid_size_km, n_x = n_x, 
  Method = Method, Lon = Data_Geostat[,'Lon'], Lat = Data_Geostat[,'Lat'], 
  Extrapolation_List = Extrapolation_List, 
  randomseed = Kmeans_Config[["randomseed"]], nstart = Kmeans_Config[["nstart"]],  
 iter.max = Kmeans_Config[["iter.max"]], DirPath = DateFile, Save_Results = FALSE)

# Add knots to Data_Geostat
Data_Geostat = cbind(Data_Geostat, "knot_i"=Spatial_List$knot_i)
SpatialDeltaGLMM::Plot_data_and_knots(Extrapolation_List = Extrapolation_List,
 Spatial_List = Spatial_List, Data_Geostat = Data_Geostat, PlotDir = DateFile)

#---------------------------------
#Run the model
TmbData = VAST::Data_Fn("Version" = Version, "FieldConfig" = FieldConfig, 
  "OverdispersionConfig" = OverdispersionConfig,  
  "RhoConfig" = RhoConfig, "ObsModel" = ObsModel, 
  "c_i" = as.numeric(Data_Geostat$spp) - 1, 
  "b_i" = Data_Geostat$Catch_num, 
  "a_i" = Data_Geostat$AreaSwept_km2, 
  "v_i" = as.numeric(Data_Geostat$Vessel) - 1, 
  "s_i" = Data_Geostat$knot_i - 1, 
  "t_i" = Data_Geostat$Year, 
  "a_xl" = Spatial_List$a_xl, 
  "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, 
  "Method"=Spatial_List$Method, "Options"=Options )

# hist(dat$cpue, breaks = 50)

TmbList = VAST::Build_TMB_Fn("TmbData" = TmbData, 
  "RunDir" = DateFile, 
  "Version" = Version, 
  "RhoConfig" = RhoConfig, 
  "loc_x" = Spatial_List$loc_x, "Method" = Method)
Obj = TmbList[["Obj"]]

Opt = TMBhelper::Optimize(obj = Obj, 
  newtonsteps = 1,
  lower = TmbList[["Lower"]], upper = TmbList[["Upper"]], getsd = TRUE, 
  savedir = DateFile, bias.correct = FALSE)

#---------------------------------
#Save the results
Report = Obj$report()
Save = list("Opt"=Opt, "Report"=Report, "ParHat"=Obj$env$parList(Opt$par), "TmbData"=TmbData)
save(Save, file=paste0(DateFile,"Save.RData"))
save.image(paste0(DateFile,"Result.RData"))


# Enc_prob = SpatialDeltaGLMM::Check_encounter_prob( Report=Report, Data_Geostat=Data_Geostat, DirName=DateFile)
Q = SpatialDeltaGLMM::QQ_Fn( TmbData=TmbData, Report=Report, FileName_PP=paste0(DateFile,"Posterior_Predictive.jpg"), FileName_Phist=paste0(DateFile,"Posterior_Predictive-Histogram.jpg"), FileName_QQ=paste0(DateFile,"Q-Q_plot.jpg"), FileName_Qhist=paste0(DateFile,"Q-Q_hist.jpg"))

MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn( "Region"=Region, "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, "Extrapolation_List"=Extrapolation_List )
MapDetails_List[["Cex"]] = 2
# Decide which years to plot
Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))

residuals <- SpatialDeltaGLMM:::plot_residuals(Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], TmbData=TmbData, Report=Report, Q=Q, savedir=DateFile, MappingDetails=MapDetails_List[["MappingDetails"]], PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8)

# SpatialDeltaGLMM::PlotAniso_Fn( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )

# # pre <- SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set=1, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)
# # # pos <- SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set=2, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)
density_xt <- SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set=3, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)
presence_xt <- SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set=1, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)
positive_xt <- SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set=2, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)
# # e_presence_xt <- SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set=6, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)
# # e_positive_xt <- SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set=7, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)
# #
Index = SpatialDeltaGLMM::PlotIndex_Fn( DirName=DateFile, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Years2Include=Years2Include, strata_names="all_areas", use_biascorr=TRUE )
# #
SpatialDeltaGLMM::Plot_range_shifts(Report=Report, TmbData=TmbData, Sdreport=Opt[["SD"]], Znames=colnames(TmbData$Z_xm), PlotDir=DateFile, Year_Set=Year_Set)
# #
Plot_factors(Report = Report, ParHat = Obj$env$parList(), Data = TmbData, SD = Opt$SD, mapdetails_list = MapDetails_List,
             Year_Set = Year_Set, category_names = c("L1","L2","L3","L4","L5"), plotdir = DateFile)

Cov_List = Summarize_Covariance(Report = Report, ParHat = Obj$env$parList(), Data = TmbData, SD = Opt$SD, plot_cor = TRUE,
                                category_names = levels(Data_Geostat[, "spp"]), plotdir = DateFile, plotTF = FieldConfig,
                                mgp = c(2, 0.5, 0), tck = -0.02, oma = c(0, 5, 2, 2))







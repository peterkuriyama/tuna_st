#-----------------------------------------------------------------------------------------------------
#Set working directory based on computer I'm using
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
source("Grid_Fn.R")
#-----------------------------------------------------------------------------------------------------
#Read in Data
bill_catch <- read.csv("data/PublicLLTunaBillfishMt.csv", stringsAsFactors = FALSE)
bill_numbers <- read.csv("data/PublicLLTunaBillfishNum.csv", stringsAsFactors = FALSE)

#Filter the billfish data to keep bigeye from Japan dat
bet_catch <- bill_catch %>% filter(Flag == "JPN") %>% select(Year, Month, Flag,
  LatC5, LonC5, Hooks, BETn, BETmt)
bet_numbers <- bill_numbers %>% filter(Flag == "JPN") %>% select(Year, Month, Flag,
  LatC5, LonC5, Hooks, BETn, BETmt)

#---------------------------------
#Composition data
bet_comps <- read.csv("data/bet_length_comps.csv", stringsAsFactors = FALSE)

#Expand the comps for plotting purposes
expd_inds <- as.data.frame(bet_comps$Freq)
expd_inds$ind <- 1:nrow(expd_inds)
expd_inds <- rep(expd_inds$ind, expd_inds[, 1])

bet_comps_expd <- bet_comps[expd_inds, ]

#Check plot of length comps through time
# ggplot(bet_comps_expd) + geom_histogram(aes(x = Bin)) + facet_wrap(~ Year)

#Treat certain length bins as species
#bins are (<=60, 80, 100, 120, >=120)
#60 was too small, keep minimum at 80
names(bet_comps) <- tolower(names(bet_comps))
bet_comps$spp <- 1
bet_comps[which(bet_comps$bin > 60 & bet_comps$bin <= 80), 'spp'] <- 2
bet_comps[which(bet_comps$bin > 80 & bet_comps$bin <= 100), 'spp'] <- 3
bet_comps[which(bet_comps$bin > 100 & bet_comps$bin <= 120), 'spp'] <- 4
bet_comps[which(bet_comps$bin > 120), 'spp'] <- 5

#60 was too small, keep minimum at 80
# bet_comps$spp <- 1
# bet_comps[which(bet_comps$bin > 80 & bet_comps$bin <= 100), 'spp'] <- 2
# bet_comps[which(bet_comps$bin > 100 & bet_comps$bin <= 120), 'spp'] <- 3
# bet_comps[which(bet_comps$bin > 120), 'spp'] <- 4

#Consider 'CPUE' to be numbers of fish
#**might make this numbers/nhooks in the future**
bet_comps1 <- bet_comps %>% group_by(lat, lon, year, quarter, spp) %>% 
  summarize(cpue = sum(freq))

#**Use annual "cpue" for now can also switch to include the quarter columns
bet_comps_annual <- bet_comps %>% group_by(lat, lon, year, spp) %>% 
  summarize(cpue = sum(freq)) %>% as.data.frame

#**Also filter data so year >= 1990**
#Just to get model to fit
# bet_comps_annual <- bet_comps_annual %>% filter(year >= 1990)

#Fill the missing values
bet_comps_annual

bet_complete <- bet_comps_annual %>% complete(spp, nesting(lat, lon, year),
  fill = list(cpue = 0))
bet_complete %>% filter(lat == -32.5, lon == -95, year == 1993)

#Overwrite old version with filled version
bet_comps_annual <- bet_complete

#-----------------------------------------------------------------------------------------------------
#VAST Model

#---------------------------------
#Initial configurations
Version <- "VAST_v4_0_0"
Method <- c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km <- 10
n_x <- c(10, 50, 100, 200, 1000)[1] #number of stations
Kmeans_Config <-  list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )

#Specify spatial and temporal autocorrelation
#Fieldconfig is 5 because there are five size categories
# FieldConfig <- c("Omega1"=5, "Epsilon1"=5, "Omega2"=5, "Epsilon2"=5) 
FieldConfig <- c("Omega1"=5, "Epsilon1"=5, "Omega2"=5, "Epsilon2"=5) 
RhoConfig <- c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) 
OverdispersionConfig <- c("Vessel"=0, "VesselYear"=0)
ObsModel <- c(2, 0) #Gamma distributed catch rates, encounter probabilities conventional
#delta model

#Specify options
Options <- c("SD_site_density"=0, "SD_site_logdensity"=0, "Calculate_Range"=1,
             "Calculate_evenness"=0, "Calculate_effective_area"=1, "Calculate_Cov_SE"=0,
             'Calculate_Synchrony'=0, 'Calculate_Coherence'=0)

strata.limits <- data.frame('STRATA'="EPO")
Region <- "Other"

DateFile = paste0(getwd(),"/VAST_output_YFT_comp/")
dir.create(DateFile)

#Save configurations
Record = ThorsonUtilities::bundlelist( c("Version","Method","grid_size_km","n_x","FieldConfig","RhoConfig","OverdispersionConfig","ObsModel","Kmeans_Config") )
save( Record, file=file.path(DateFile,"Record.RData"))
capture.output( Record, file=paste0(DateFile,"Record.txt"))

#---------------------------------
#Process data
#Specify dat to use
dat <- bet_comps_annual

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

#---------------------------------
#Run the model
TmbData = VAST::Data_Fn("Version" = Version, "FieldConfig" = FieldConfig, 
  "OverdispersionConfig" = OverdispersionConfig,  
  "RhoConfig" = RhoConfig, "ObsModel" = ObsModel, 
  "c_i" = as.numeric(Data_Geostat$spp), 
  "b_i" = Data_Geostat$Catch_num, 
  "a_i" = Data_Geostat$AreaSwept_km2, 
  "v_i" = as.numeric(Data_Geostat$Vessel) - 1, 
  "s_i" = Data_Geostat$knot_i - 1, 
  "t_i" = Data_Geostat$Year, 
  "a_xl" = Spatial_List$a_xl, 
  "MeshList"=Spatial_List$MeshList, "GridList"=Spatial_List$GridList, 
  "Method"=Spatial_List$Method, "Options"=Options )

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







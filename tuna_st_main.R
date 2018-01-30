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
library(reshape2)
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

#Change column formatting
bet_numbers <- plyr::rename(bet_numbers, c("LatC5" = "Lat", "LonC5" = "Lon"))
names(bet_numbers) <- tolower(names(bet_numbers))

#Calculate annual values
bet_numbers_annual <- bet_numbers %>% group_by(year, lat, lon) %>% 
  summarize(hooks = sum(hooks), betn = sum(betn)) %>% as.data.frame

bet_numbers_complete <- bet_numbers_annual %>% complete(year, nesting(lat, lon), 
  fill = list(hooks = 0, betn = 0)) 

#Check proportion of zeroes
bet_numbers_complete %>% group_by(year) %>% 
  summarize(zeroes = length(which(betn == 0)),
    nrowz = length(betn), prop_zero = zeroes / nrowz) %>% tail
bet_numbers_complete$cpue <- bet_numbers_complete$betn
bet_numbers_complete$spp <- 1
bet_numbers_annual <- bet_numbers_complete

###plot the data to see what things look like
world_map <- map_data("world")
plot_numbers <- bet_numbers_annual %>% group_by(lat, lon) %>% summarize(cpue = sum(cpue)) 

ggplot() + geom_raster(data = plot_numbers, aes(x = lon, y = lat, fill = cpue)) + 
  scale_fill_gradient(low = 'white', high = 'red') +
  geom_map(data = world_map, map = world_map,
    aes(x = long, y = lat, map_id = region)) + 
  scale_x_continuous(limits = c(-148, -72)) + 
  scale_y_continuous(limits = c(-43, 48)) + theme_bw()
  
# bet_complete <- bet_co_annual %>% complete(spp, nesting(lat, lon, year),
#   fill = list(cpue = 0)) %>% as.data.frame

#---------------------------------
#Composition data
bet_comps <- read.csv("data/bet_length_comps.csv", stringsAsFactors = FALSE)

#Expand the comps for plotting purposes
expd_inds <- as.data.frame(bet_comps$Freq)
expd_inds$ind <- 1:nrow(expd_inds)
expd_inds <- rep(expd_inds$ind, expd_inds[, 1])

bet_comps_expd <- bet_comps[expd_inds, ]

#**assign 'spp' values for different length classes**#
#Specify number of length classes here, using 3 now
nspp <- 1
bet_comps_expd$spp <- ntile(bet_comps_expd$Bin, nspp)
names(bet_comps_expd) <- tolower(names(bet_comps_expd))

the_bins <- bet_comps_expd %>% group_by(spp) %>% summarize(mins = round(min(bin), -1), 
  maxes = round(max(bin), -1)) %>% as.data.frame

#reclassify the species
for(ii in the_bins$spp){
  #First bin
  if(ii == 1){
    bet_comps_expd[which(bet_comps_expd$bin <= the_bins$maxes[1]), 'spp'] <- 1    
  }

  if(ii %in% c(1, max(the_bins$spp)) == FALSE){
    the_inds <- which(bet_comps_expd$bin > the_bins$mins[ii] &
      bet_comps_expd$bin <= the_bins$maxes[ii])    
  }

  #Last bin
  if(ii == max(the_bins$spp)){
    bet_comps_expd[which(bet_comps_expd$bin > the_bins$maxes[ii]), 'spp'] <- max(the_bins$spp)
  }
}

#Treat certain length bins as species
names(bet_comps_expd) <- tolower(names(bet_comps_expd))

#Change frequency of all rows to 1 (because they are expanded)
bet_comps_expd$freq <- 1
sum(bet_comps_expd$freq) == sum(bet_comps$Freq)

#Group on annual or seasonal time scales
bet_comps_annual <- bet_comps_expd %>% group_by(lat, lon, year,
  spp) %>% summarize(cpue = sum(freq)) %>% as.data.frame
bet_comps_seasonal <- bet_comps_expd %>% group_by(lat, lon, year, quarter, spp) %>%
  summarize(cpue = sum(freq)) %>% as.data.frame

#Complete the data frames by adding zeroes
bet_complete_annual <- bet_comps_annual %>% complete(spp, nesting(lat, lon, year),
  fill = list(cpue = 0)) %>% as.data.frame
bet_complete_seasonal <- bet_comps_seasonal %>% complete(spp, nesting(lat, lon, year, quarter),
  fill = list(cpue = 0)) %>% as.data.frame

#Look at proportion of zeroes
bet_complete_annual %>% filter(year >= 1986) %>% group_by( spp, year) %>% summarize(nzeroes = length(which(cpue == 0)),
  nrows = length(cpue), prop_zeroes = nzeroes / nrows)

bet_complete_seasonal %>% filter(year >= 1986) %>% group_by(spp, year, quarter) %>%
  summarize(nzeroes = length(which(cpue == 0)), nrows = length(cpue), 
    prop_zeroes = nzeroes / nrows) %>% ungroup() %>% select(prop_zeroes) %>% unique

#Consider 'CPUE' to be numbers of fish
#**might make this numbers/nhooks in the future**
# bet_comps1 <- bet_comps %>% group_by(lat, lon, year, quarter, spp) %>% 
#   summarize(cpue = sum(freq))

#**Use annual "cpue" for now can also switch to include the quarter columns
# bet_comps_annual <- bet_comps %>% group_by(lat, lon, year, spp) %>% 
#   summarize(cpue = sum(freq)) %>% as.data.frame

#**Also filter data so year >= 1990**
#Just to get model to fit
# bet_comps_annual <- bet_comps_annual %>% filter(year >= 1990)

#Fill the missing values
# bet_comps_annual




#Overwrite old version with filled version
bet_comps_annual <- bet_complete %>% filter(year >= 1986)
# unique(bet_comps_annual$year)[order(unique(bet_comps_annual$year))]

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
FieldConfig <- c("Omega1"=1, "Epsilon1"=1, "Omega2"=1, "Epsilon2"=1) 
RhoConfig <- c("Beta1"=0, "Beta2"=0, "Epsilon1"=0, "Epsilon2"=0) #Parameters among years
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

# dat <- bet_comps_annual
dat <- bet_numbers_annual

# Error in VAST::Data_Fn(Version = Version, FieldConfig = FieldConfig, OverdispersionConfig = OverdispersionConfig,  : 
#   Some years and/or categories have either all or no encounters, and this is not permissible when ObsModel_ez[,'Link']=0
#Check missing values

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

SpatialDeltaGLMM::PlotAniso_Fn( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )

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







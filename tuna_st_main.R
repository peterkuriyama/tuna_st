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
library(VAST)
library(SpatialDeltaGLMM)
library(plyr)
library(dplyr)
library(ggplot2)

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
names(bet_comps) <- tolower(names(bet_comps))
bet_comps$spp <- 1
bet_comps[which(bet_comps$bin > 60 & bet_comps$bin <= 80), 'spp'] <- 2
bet_comps[which(bet_comps$bin > 80 & bet_comps$bin <= 100), 'spp'] <- 3
bet_comps[which(bet_comps$bin > 100 & bet_comps$bin <= 120), 'spp'] <- 4
bet_comps[which(bet_comps$bin > 120), 'spp'] <- 5

#Consider 'CPUE' to be numbers of fish
#**might make this numbers/nhooks in the future**
bet_comps1 <- bet_comps %>% group_by(lat, lon, year, quarter, spp) %>% 
  summarize(cpue = sum(freq))

#**Use annual "cpue" for now can also switch to include the quarter columns
bet_comps_annual <- bet_comps %>% group_by(lat, lon, year, spp) %>% 
  summarize(cpue = sum(freq)) %>% as.data.frame

#-----------------------------------------------------------------------------------------------------
#VAST Model

#---------------------------------
#Initial configurations













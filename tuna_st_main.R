#Set working directory based on computer I'm using
if(Sys.info()[1] == 'Windows') setwd('\\Users\\peter.kuriyama\\Desktop\\tuna_st')
if(Sys.info()[1] != 'Windows') setwd('/Users/peterkuriyama/Dropbox/postdoc/yellowtail_spatiotemporal')

#Install packages
# install.packages("TMB")

# devtools::install_github("james-thorson/VAST",force = TRUE)
devtools::install_github("james-thorson/utilities",force = TRUE)
# devtools::install_github("nwfsc-assess/geostatistical_delta-GLMM",force = TRUE)

library(TMB)
library(VAST)
library(SpatialDeltaGLMM)

#Read in Data




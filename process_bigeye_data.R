process_bigeye_data <- function(nspp){
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
  bet_numbers_annual <- bet_numbers_complete %>% as.data.frame
  
  ###plot the data to see what things look like
  # world_map <- map_data("world")
  # plot_numbers <- bet_numbers_annual %>% group_by(lat, lon) %>% summarize(cpue = sum(cpue)) 
  
  # ggplot() + geom_raster(data = plot_numbers, aes(x = lon, y = lat, fill = cpue)) + 
  #   scale_fill_gradient(low = 'white', high = 'red') +
  #   geom_map(data = world_map, map = world_map,
  #     aes(x = long, y = lat, map_id = region)) + 
  #   scale_x_continuous(limits = c(-148, -72)) + 
  #   scale_y_continuous(limits = c(-43, 48)) + theme_bw()
    
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
  # nspp <- 2
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
  
  
  outs <- list(bet_numbers_annual = bet_numbers_annual, 
               bet_complete_annual = bet_complete_annual,
               bet_complete_seasonal = bet_complete_seasonal)
  
  return(outs)
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
  # bet_comps_annual <- bet_complete_annual %>% filter(year >= 1986)
  # unique(bet_comps_annual$year)[order(unique(bet_comps_annual$year))]



}


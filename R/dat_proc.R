library(tidyverse)
library(sf)
library(lubridate)
library(rgdal)

load(file = '../SGRRMP/data/comid_statewide.Rdata')

prstr <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

##
# watershed bounding area 
load('../Santa_Ana_flow/data/shed.RData')
shed <- shed %>%
  spTransform(CRS(prstr)) %>% 
  st_as_sf %>% 
  dplyr::filter(SMC_Name %in% 'Upper Santa Ana')

##
# csci scores (whole state), clip all by shed
scrs <- read.csv('../SGRRMP/ignore/csci_061917.csv', header = T, stringsAsFactors = F) %>% 
  mutate(
    csci = CSCI, 
    lat = New_Lat, 
    long = New_Long
  ) %>% 
  mutate(
    SampleDate = dmy(SampleDate), 
    COMID = as.character(COMID)
  ) %>% 
  st_as_sf(coords = c('New_Long', 'New_Lat'), crs = st_crs(shed)) %>% 
  st_intersection(shed) %>% 
  dplyr::select(StationCode, lat, long, COMID, SampleDate, csci)
st_geometry(scrs) <- NULL

save(scrs, file = 'data/scrs.RData', compress = 'xz')

##
# get watershed flow lines
spat <- readOGR('S:/Spatial_Data/NHDPlus/NHDPlus18/Hydrography/nhdflowline_RB8.shp') %>% 
  spTransform(CRS(prstr)) %>% 
  st_as_sf %>% 
  st_intersection(shed) %>% 
  select(COMID)  

# simplify, join with all expectations
spat <- spat %>% 
  st_simplify(dTolerance = 0.003, preserveTopology = T) %>%
  left_join(comid, by = 'COMID') %>% 
  select(COMID, matches('^full0'))

save(spat, file = 'data/spat.RData', compress = 'xz')
  

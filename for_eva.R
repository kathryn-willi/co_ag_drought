# Colorado River Basin Stream Gage Data Collection Script
# This script downloads daily discharge data from USGS stream gages 
# in the Colorado River Basin 

library(tidyverse)      
library(dataRetrieval)  
library(nhdplusTools)  
library(tigris)       
library(mapview)       
library(sf)           
library(data.table)

# define time period of interest
start_time <- "1984-10-01" # start of 1985 water year
end_time <- "2015-09-30"  # 30-yr period

# Define Hydrologic Unit Codes (HUCs) for Colorado River Basin
# i.e., the HUC-04 level watersheds that make up the Colorado River Basin
hucs <- get_huc(id = c(
  "1401",  # Upper Colorado-Dirty Devil
  "1402",  # Gunnison
  "1403",  # Dolores
  "1404",  # Upper Green
  "1405",  # White-Yampa
  "1406",  # San Juan
  "1407",  # Little Colorado
  "1501",  # Lower Colorado
  "1502",  # Bill Williams
  "1503",  # Virgin
  "1504",  # Upper Gila
  "1505",  # Middle Gila
  "1506",  # Lower Gila
  "1507",  # Rio Sonoyta
  "1508"), # Willcox Playa
  type = "huc04")

# get state boundaries that overlap with the Colorado River Basin HUCs
# (identify which states to query for NWIS sites)
states_overlap <- tigris::states() %>%
  sf::st_transform(., sf::st_crs(hucs)) %>%
  .[hucs,]  # subset to only states that intersect with HUCs we care about

#get a list of NWIS sites for all overlapping states
nwis_sites_by_state <- map(states_overlap$STUSPS, 
                           ~{
                             # look for NWIS for discharge sites only (parameter code = 00060)
                             discharge_sites <- whatNWISsites(stateCd = .x, parameterCd = "00060") %>%
                               filter(site_tp_cd == 'ST') # filter to streams only (excludes ditches, canals, etc.)
                             
                             return(discharge_sites)
                           }
                           )

# combine all state results and filter to sites within the Colorado River Basin only
nwis_sites <- bind_rows(nwis_sites_by_state) %>%
  distinct(.) %>%  # remove any duplicate sites... precautionary
  st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = 4269) %>%
  # filter to only sites within the HUC boundaries
  .[st_transform(hucs, st_crs(.)),]

# A map of all USGS stream gages in the Colorado River Basin
mapview(nwis_sites)

# get metadata for each site to check data availability using `whatNWISdata()`
# filter sites to only those with sufficient data coverage... whatever that ends
# up looking like:
nwis_site_meta <- map_dfr(nwis_sites$site_no, function(site_chunk) {
  # Query metadata for each site
  dataRetrieval::whatNWISdata(siteNumber = site_chunk,
                              # streamflow only
                              parameterCd = "00060") %>%
    # drop irrelevant columns and rename for clarity
    dplyr::select(c(site_no,
                    site_name = station_nm,
                    n_obs = count_nu,
                    begin_date,
                    end_date,
                    code = parm_cd)) %>%
    # standardize data types
    mutate(across(everything(), as.character))
}) %>%
  # Filter to only gages that started collecting data by our start date 
  # and have data through at least the selected end date
  filter(ymd(begin_date) <= start_time & ymd(end_date) >= end_time)

# update nwis_sites to only include sites with this adequate data coverage
nwis_sites <- nwis_sites %>%
  filter(site_no %in% nwis_site_meta$site_no) %>%
  distinct(.)

# map of sites within the Colorado River Basin with data during the
# period of record we care about:
mapview(nwis_sites)

# Lastly, download daily discharge data for each site at a time
# and back up to a csv file
for (i in 1:length(nwis_sites$site_no)){
  # Download daily discharge data using `readNWISdv()`
  data <- dataRetrieval::readNWISdv(
    siteNumbers = nwis_sites[i,]$site_no,   # current site number
    parameterCd = "00060",                  # discharge parameter code
    startDate = start_time,                 # start date for data retrieval
    endDate = end_time) %>%                 # end date for data retrieval
    # convert all columns to character type for consistent csv output...
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  
  write_csv(data, paste0("data/", nwis_sites[i,]$site_no, ".csv"))
}

# ... so that we can easily stack them into a single data frame:

all_flow_data <- list.files("data/", full.names = TRUE) %>%
  map_dfr(~data.table::fread(.))

# "X_00060_00003" contains the actual discharge value (in CFS). the "X_00060_00003_cd" is that day's quality code.
# Anything that starts with "A" means its been approved by the USGS (so QA'ed and considered good data, basically). 
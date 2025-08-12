#' Create watershed boundary shapefile(s) for an area of interest.
#' 
#' This function uses the nhdplusTools package to import flowline, catchments and
#' water body spatial data for an aoi
#' 
#' @param aoi An sf polygon object

#' @return Watershed shapefile(s) for the aoi
#' 
getWatersheds <- function(aoi){
   # aoi <- sf::st_read("data/ClarkCounty_NV/ClarkCounty_NV.shp")
  sf::sf_use_s2(FALSE)
  # read in the complete NHD (in tabular form) to make for much more efficient nhd crawling. 
  # This data in tabular form doesn't exist anywhere online that I know of... -_-
  nhd <- data.table::fread('data/nhd_flow_network.csv') 
  
  # download flowlines for entire aoi
  nhd_flowlines <- nhdplusTools::get_nhdplus(AOI = aoi, 
                                             realization='flowline',
                                             t_srs = sf::st_crs(aoi))
  
  # add `tocomid` field to ID flowlines that cross over the aoi
  # this step massively speeds up run time by reducing the number
  # of watersheds that need to be created
  aoi_flowlines <- nhd_flowlines  %>%
    dplyr::distinct(comid, .keep_all=TRUE) %>%
    nhdplusTools::get_tocomid(., add=TRUE)
  
  # minimize number of origin points by selecting only those that cross AOI boundary
  outsiders <- aoi_flowlines %>%
    dplyr::filter(tocomid==0) %>%
    tibble::rowid_to_column(., "index")
  
  # if("Colorado River" %in% outsiders$gnis_name){
  #   
  #   mainstem <- outsiders %>%
  #     filter(gnis_name == "Colorado River")
  
  watersheds <- vector("list", length = nrow(outsiders))
  
  for(i in 1:nrow(outsiders)){
    
    watersheds[[i]] <- nhdplusTools::get_nldi_basin(list("comid", as.character(outsiders$comid[i])))
    
  }
  
  main_watershed <- dplyr::bind_rows(watersheds) %>%
    sf::st_make_valid() %>%
    dplyr::summarize() %>%
    nngeo::st_remove_holes()
  
  if(st_crs(main_watershed) != st_crs(aoi)){
    main_watershed <- main_watershed %>%
      st_transform(., st_crs(aoi))
  }
  
  
  
  sub_ws <- vector("list", length(nrow(outsiders)))
  
  for(i in 1:nrow(outsiders)){
    sub_ws[[i]] <- nhdplusTools::get_DM(network = nhd,
                                        comid = outsiders$comid[i],
                                        distance = 35) %>%
      as_tibble() %>%
      mutate(catchment = outsiders$comid[i])
  }
  

  # if no downstream segments in the AOI = escaping flowline OR something weird
  funky_ws <- bind_rows(sub_ws) %>%
    filter(value %in% nhd_flowlines$comid) %>%
    group_by(catchment) %>%
    summarize(count = n()) %>%
    filter(count == 1)
  
  escapers <-  bind_rows(sub_ws) %>%
    filter(!value %in% nhd_flowlines$comid) %>%
    group_by(catchment) %>%
    summarize(count = n()) %>%
    filter(count >= 1)
  
  funk_finder <- vector("list", length = nrow(funky_ws))
  
  # of those mystery locs...
  for(i in 1:nrow(funky_ws)){
    funk_finder[[i]] <- nhdplusTools::get_UT(network = nhd,
                                             comid = funky_ws$catchment[i],
                                             distance = 35) %>%
      as_tibble() %>%
      #filter(value %in% nhd_flowlines$comid) %>%
      mutate(catchment = funky_ws$catchment[i])
  }
  
  # Those that have no upstream or downstream segments inside the aoi
  # are strange. Remove their extents outside of the AOI.
  mystery_flowlines <- bind_rows(funk_finder) %>%
    filter(value %in% nhd_flowlines$comid) %>%
    group_by(catchment) %>%
    mutate(count = n()) %>%
    filter(count == 1) 
  
  mystery_cats <- get_nhdplus(comid = mystery_flowlines$catchment, realization = "catchment",
                              t_srs = sf::st_crs(aoi))
  
  escaper_cats <- get_nhdplus(comid = escapers$catchment, realization = "catchment",
                          t_srs = sf::st_crs(aoi))
  
  bads <- mystery_cats %>% bind_rows(escaper_cats)
  
  bad_cats_removed <- main_watershed %>%
    st_difference(., st_union(bads)) 
  
  final_ws <- bad_cats_removed %>%
    bind_rows(st_make_valid(aoi)) %>%
    summarize() %>%
    nngeo::st_remove_holes() %>%
    st_make_valid() %>%                           # guard against invalid rings
    st_cast("POLYGON") %>%                        # split MULTIPOLYGON into rows
    mutate(.area = as.numeric(st_area(geometry))) %>% 
    slice_max(.area, n = 1, with_ties = FALSE) %>% 
    select(-.area)
  
  gc()
  sf::sf_use_s2(TRUE)
  return(final_ws)

}

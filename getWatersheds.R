#' Create watershed boundary shapefile(s) for an area of interest.
#' 
#' This function uses the nhdplusTools package to import flowline, catchments and
#' water body spatial data for an aoi
#' 
#' @param aoi An sf polygon object

#' @return Watershed shapefile(s) for the aoi
#' 
getWatersheds <- function(aoi){
  
  # read in the complete NHD (in tabular form) to make for much more efficient nhd crawling. 
  # This data in tabular form doesn't exist anywhere online that I know of... -_-
  nhd <- data.table::fread('data/nhd_flow_network.csv') 
  
  # download flowlines for entire aoi
  nhd_flowlines <- nhdplusTools::get_nhdplus(AOI = aoi, 
                                             realization='flowline',
                                             t_srs = st_crs(aoi))
  
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
  
  # FUNCTION THAT, FOR EVERY "OUTSIDER" POUR POINT, IDENTIFIES ALL UPSTREAM FLOWLINES
  watersheds <- function(spid_union) {
    
    tracer <- function(samples) {
      outsiders <- as_tibble(outsiders)
      outlet <- outsiders %>%
        dplyr::filter(index == samples)
      
      upstream <- nhdplusTools::get_UT(nhd, outlet$comid) %>% #upstream trace function in nhdplusTools
        tibble::as_tibble() %>%
        dplyr::rename(comid_list = value) %>%
        dplyr::distinct(comid_list, .keep_all = TRUE) %>%
        dplyr::filter(comid_list != outlet$comid)
    }
    
    ws <- purrr::map(spid_union, tracer) %>% dplyr::bind_rows()
    
  }
  
  park_ws <- outsiders %>%
    dplyr::mutate(comid_list = map(index, watersheds)) %>%
    tidyr::unnest(cols = comid_list) %>%
    dplyr::distinct(comid_list) %>%
    sf::st_drop_geometry() %>%
    dplyr::rename(comid = comid_list)
  
  splitter_flowlines <- nhdplusTools::get_nhdplus(comid = outsiders$comid,
                                                  realization = 'flowline',
                                                  t_srs = st_crs(aoi)) %>%
    dplyr::select(comid)
  
  nhd_flowlines <-  park_ws$comid %>%
    purrr::map(~nhdplusTools::get_nhdplus(comid = .,
                                          realization='flowline',
                                          t_srs = st_crs(aoi)) %>% dplyr::select(comid)) %>%
    dplyr::bind_rows() %>%
    #dplyr::select(comid) %>%
    dplyr::bind_rows(splitter_flowlines) %>%
    dplyr::distinct(comid,.keep_all=TRUE)
  
  
  grouper <- igraph::components(igraph::graph.adjlist(sf::st_touches(nhd_flowlines)))[[1]] %>%
    tibble::as_tibble() %>%
    dplyr::rename(relationship = value)
  
  grouped_flowlines <- dplyr::bind_cols(nhd_flowlines, grouper) %>% 
    dplyr::group_by(relationship) %>%
    dplyr::summarize()
  
  # include catchments in the park that are not contained in trace (no NHD flowlines associated with them)
  nhd_empty_catchments <- nhdplusTools::get_nhdplus(AOI = aoi, 
                                                    realization='catchment',
                                                    t_srs = st_crs(aoi)) %>%
    dplyr::distinct(featureid, .keep_all=TRUE) %>%
    dplyr::filter(!featureid %in% nhd_flowlines$comid) %>%
    dplyr::filter(!featureid %in% outsiders$comid) %>%
    dplyr::mutate(relationship = 0) %>%
    dplyr::select(relationship) %>%
    sf::st_intersection(aoi)
  
  splitters <- nhdplusTools::get_nhdplus(comid = outsiders$comid,
                                         realization = 'catchment',
                                         t_srs = st_crs(aoi)) %>%
    sf::st_intersection(aoi)
  
  sf_use_s2(FALSE)
  
  grouped_catchments <- park_ws$comid %>%
    purrr::map(~nhdplusTools::get_nhdplus(comid = .,
                                          realization = 'catchment',
                                          t_srs = st_crs(aoi))) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(!featureid %in% outsiders$comid) %>%
    dplyr::bind_rows(splitters) %>%
    sf::st_join(.,grouped_flowlines) %>%
    dplyr::bind_rows(nhd_empty_catchments) %>%
    dplyr::group_by(relationship) %>%
    dplyr::summarize() %>%
    nngeo::st_remove_holes() %>%
    sf::st_join(aoi) %>%
    dplyr::select(-relationship) %>%
    dplyr::summarize()
  
  sf_use_s2(TRUE)
  
  return(grouped_catchments)
  
}
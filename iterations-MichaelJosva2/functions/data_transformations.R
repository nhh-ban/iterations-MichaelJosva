  transform_metadata_to_data_frame <- function(stations_metadata) {
    result <- stations_metadata[[1]] %>%
      map(as_tibble) %>%
      list_rbind() %>%
      mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>%
      mutate(latestData = as_datetime(latestData, tz = "UTC")) %>%
      unnest_wider(location) %>%
      unnest_wider(latLon)
    
    return(result)
  }
  
  
  to_iso8601 <- function(time, days) {
    result <- 
      
    return(result)
  }
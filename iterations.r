
# some changes

library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)
library(purrr)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml")


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
    ) 


#### 2: Transforming metadata

source("functions/data_transformations.R")


transform_metadata_to_data_frame <- function()
  
map(stations_metadata)

df <- stations_metadata[[1]] %>% 
  map_dfr(as.tibble) %>% 
  mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) |> 
  mutate(latestData = as_datetime(latestData, tz = "UTC"))  |> 
  unnest_wider(location) %>% 
  unnest_wider(latLon)


df


list.rbind()

stations_metadata[[1]][[1]]

  

stations_metadata_df <- 
stations_metadata %>% 
transform_metadata_to_df(.)



#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df)


### 5: Final volume query: 

source("gql-queries/vol_qry.r")

stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) %$% 
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>% 
  ggplot(aes(x=from, y=volume)) + 
  geom_line() + 
  theme_classic()





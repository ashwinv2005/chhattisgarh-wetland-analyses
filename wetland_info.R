library(tidyverse)
library(sf)

wetland_maps = st_read(dsn = "CG-WETLAND_CG-COST", layer = "CGWETLAND_2010") %>%
  st_transform("OGC:CRS84")

wetland_maps_buffer = st_read(dsn = "1500mWetlandBuffers", layer = "Wetlands_1500mBuffer") %>%
  st_transform("OGC:CRS84")

wetland_data = wetland_maps_buffer %>%
  st_drop_geometry()
wetland_maps = wetland_maps %>%
  dplyr::select(OBJECTID,geometry) %>%
  rename(wetland_ID = OBJECTID)
wetland_maps_buffer = wetland_maps_buffer %>%
  dplyr::select(OBJECTID,geometry) %>%
  rename(wetland_buffer_ID = OBJECTID)

sf_use_s2(FALSE)

load("rawdata_CT.RData")

temp1 = data %>%
  group_by(group.id) %>% 
  slice(1) %>%
  ungroup() %>% 
  distinct(group.id, LONGITUDE, LATITUDE) %>% 
  # joining map vars to EBD
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = F) %>% 
  st_set_crs(st_crs(wetland_maps)) %>%
  # main objects
  st_join(wetland_maps %>% dplyr::select(wetland_ID)) %>%
  st_drop_geometry() %>%
  dplyr::select(-LATITUDE,-LONGITUDE)

temp2 = data %>%
  group_by(group.id) %>% 
  slice(1) %>%
  ungroup() %>% 
  distinct(group.id, LONGITUDE, LATITUDE) %>% 
  # joining map vars to EBD
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = F) %>% 
  st_set_crs(st_crs(wetland_maps_buffer)) %>%
  # buffered objects
  st_join(wetland_maps_buffer %>% dplyr::select(wetland_buffer_ID)) %>%
  st_drop_geometry() %>%
  dplyr::select(-LATITUDE,-LONGITUDE) %>%
  group_by(group.id) %>%
  slice(1) %>% ungroup()

temp = temp1 %>% left_join(temp2)

data = data %>% 
  left_join(temp)

data_1 = data %>%
  filter(!is.na(wetland_buffer_ID), ALL.SPECIES.REPORTED == 1) %>%
  group_by(wetland_buffer_ID) %>%
  reframe(n = n_distinct(group.id)) %>%
  arrange(desc(n))

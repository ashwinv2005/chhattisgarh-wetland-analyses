library(tidyverse)
library(sf)

wetland_maps = st_read(dsn = "CG-WETLAND_CG-COST", layer = "CGWETLAND_2010") %>%
  st_transform("OGC:CRS84")

wetland_maps_buffer = st_read(dsn = "Wetlands_500mBuffer", layer = "Wetlands_500mBuffer") %>%
  st_transform("OGC:CRS84")

wetland_attributes = wetland_maps_buffer %>%
  rename(wetland_buffer_ID = OBJECTID) %>%
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

wetland_data = data %>%
  filter(!is.na(wetland_buffer_ID))



# data availability

sampling_year = wetland_data %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(year) %>%
  reframe(n = n_distinct(group.id)) %>%
  arrange(desc(n))

sampling_wetlands = wetland_data %>%
  filter(ALL.SPECIES.REPORTED == 1, year > 2014) %>%
  group_by(wetland_buffer_ID) %>%
  reframe(n = n_distinct(group.id)) %>%
  arrange(desc(n))

common_species = wetland_data %>%
  filter(ALL.SPECIES.REPORTED == 1, year > 2014) %>%
  mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME) %>%
  reframe(freq = n_distinct(group.id)/max(lists)) %>%
  arrange(desc(freq))

species_mapping = read.csv("species_mapping_2022.csv") %>%
  dplyr::select(eBird.English.Name.2022,Order,Migratory.Status.Within.India,Onepercent.Estimates) %>%
  rename(COMMON.NAME = eBird.English.Name.2022)

wetland_data = wetland_data %>%
  left_join(species_mapping)




# preparing summaries to add to wetland data for analyses

wetland_richness_total = wetland_data %>%
  filter(year > 2014) %>%
  filter(CATEGORY %in% c("species","issf") | COMMON.NAME == "Rock Pigeon") %>%
  group_by(wetland_buffer_ID) %>%
  reframe(richness = n_distinct(COMMON.NAME)) %>%
  mutate(metric = "total")

wetland_count_total = wetland_data %>%
  filter(ALL.SPECIES.REPORTED == 1, year > 2014) %>%
  filter(CATEGORY %in% c("species","issf") | COMMON.NAME == "Rock Pigeon") %>%
  filter(OBSERVATION.COUNT != "X") %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(group.id) %>%
  mutate(agg = sum(OBSERVATION.COUNT)) %>% slice(1) %>%
  group_by(wetland_buffer_ID) %>%
  reframe(count = round(mean(agg))) %>%
  mutate(metric = "total")

wetland_richness_shorebirds = wetland_data %>%
  filter(year > 2014) %>%
  filter(CATEGORY %in% c("species","issf") | COMMON.NAME == "Rock Pigeon") %>%
  filter(Order %in% c("Charadriiformes")) %>%
  group_by(wetland_buffer_ID) %>%
  reframe(richness = n_distinct(COMMON.NAME)) %>%
  mutate(metric = "shorebirds")

wetland_count_shorebirds = wetland_data %>%
  filter(ALL.SPECIES.REPORTED == 1, year > 2014) %>%
  filter(CATEGORY %in% c("species","issf") | COMMON.NAME == "Rock Pigeon") %>%
  filter(Order %in% c("Charadriiformes")) %>%
  filter(OBSERVATION.COUNT != "X") %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(group.id) %>%
  mutate(agg = sum(OBSERVATION.COUNT)) %>% slice(1) %>%
  group_by(wetland_buffer_ID) %>%
  reframe(count = round(mean(agg))) %>%
  mutate(metric = "shorebirds")

wetland_richness_ducks = wetland_data %>%
  filter(year > 2014) %>%
  filter(CATEGORY %in% c("species","issf") | COMMON.NAME == "Rock Pigeon") %>%
  filter(Order %in% c("Anseriformes")) %>%
  group_by(wetland_buffer_ID) %>%
  reframe(richness = n_distinct(COMMON.NAME)) %>%
  mutate(metric = "ducks")

wetland_count_ducks = wetland_data %>%
  filter(ALL.SPECIES.REPORTED == 1, year > 2014) %>%
  filter(CATEGORY %in% c("species","issf") | COMMON.NAME == "Rock Pigeon") %>%
  filter(Order %in% c("Anseriformes")) %>%
  filter(OBSERVATION.COUNT != "X") %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(group.id) %>%
  mutate(agg = sum(OBSERVATION.COUNT)) %>% slice(1) %>%
  group_by(wetland_buffer_ID) %>%
  reframe(count = round(mean(agg))) %>%
  mutate(metric = "ducks")

wetland_richness_rallids = wetland_data %>%
  filter(year > 2014) %>%
  filter(CATEGORY %in% c("species","issf") | COMMON.NAME == "Rock Pigeon") %>%
  filter(Order %in% c("Gruiformes","Podicipediformes")) %>%
  group_by(wetland_buffer_ID) %>%
  reframe(richness = n_distinct(COMMON.NAME)) %>%
  mutate(metric = "rallids")

wetland_count_rallids = wetland_data %>%
  filter(ALL.SPECIES.REPORTED == 1, year > 2014) %>%
  filter(CATEGORY %in% c("species","issf") | COMMON.NAME == "Rock Pigeon") %>%
  filter(Order %in% c("Gruiformes","Podicipediformes")) %>%
  filter(OBSERVATION.COUNT != "X") %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(group.id) %>%
  mutate(agg = sum(OBSERVATION.COUNT)) %>% slice(1) %>%
  group_by(wetland_buffer_ID) %>%
  reframe(count = round(mean(agg))) %>%
  mutate(metric = "rallids")

wetland_richness_largewaterbirds = wetland_data %>%
  filter(year > 2014) %>%
  filter(CATEGORY %in% c("species","issf") | COMMON.NAME == "Rock Pigeon") %>%
  filter(Order %in% c("Phoenicopteriformes","Ciconiiformes","Suliformes","Pelecaniformes")) %>%
  group_by(wetland_buffer_ID) %>%
  reframe(richness = n_distinct(COMMON.NAME)) %>%
  mutate(metric = "large waterbirds")

wetland_count_largewaterbirds = wetland_data %>%
  filter(ALL.SPECIES.REPORTED == 1, year > 2014) %>%
  filter(CATEGORY %in% c("species","issf") | COMMON.NAME == "Rock Pigeon") %>%
  filter(Order %in% c("Phoenicopteriformes","Ciconiiformes","Suliformes","Pelecaniformes")) %>%
  filter(OBSERVATION.COUNT != "X") %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(group.id) %>%
  mutate(agg = sum(OBSERVATION.COUNT)) %>% slice(1) %>%
  group_by(wetland_buffer_ID) %>%
  reframe(count = round(mean(agg))) %>%
  mutate(metric = "large waterbirds")

df1 = wetland_attributes %>% dplyr::select(wetland_buffer_ID) %>%
  left_join(wetland_richness_total) %>%
  left_join(wetland_count_total) 
df2 = wetland_attributes %>% dplyr::select(wetland_buffer_ID) %>%
  left_join(wetland_richness_shorebirds) %>%
  left_join(wetland_count_shorebirds) 
df3 = wetland_attributes %>% dplyr::select(wetland_buffer_ID) %>%
  left_join(wetland_richness_ducks) %>%
  left_join(wetland_count_ducks) 
df4 = wetland_attributes %>% dplyr::select(wetland_buffer_ID) %>%
  left_join(wetland_richness_rallids) %>%
  left_join(wetland_count_rallids) 
df5 = wetland_attributes %>% dplyr::select(wetland_buffer_ID) %>%
  left_join(wetland_richness_largewaterbirds) %>%
  left_join(wetland_count_largewaterbirds) 

df.a = df1 %>% bind_rows(df2,df3,df4,df5)



species_list = c("Little Cormorant","Asian Openbill","Lesser Whistling-Duck","Little Grebe",
                 "Pied Kingfisher","Common Kingfisher","Bronze-winged Jacana",
                 "Little Ringed Plover","Cotton Pygmy-Goose","Gray-headed Swamphen",
                 "Eurasian Moorhen","Indian Spot-billed Duck","Pheasant-tailed Jacana",
                 "Wood Sandpiper","Common Sandpiper","Gadwall","Red-crested Pochard",
                 "Northern Pintail","Garganey","Black-headed Ibis")

wetland_data_species_1 = wetland_data %>%
  filter(ALL.SPECIES.REPORTED == 1, year > 2014) %>%
  group_by(wetland_buffer_ID) %>%
  mutate(lists = n_distinct(group.id)) %>%
  group_by(wetland_buffer_ID,COMMON.NAME) %>%
  reframe(richness = n_distinct(group.id)/max(lists)) %>%
  filter(COMMON.NAME %in% species_list) %>%
  mutate(metric = COMMON.NAME) %>% dplyr::select(-COMMON.NAME)

wetland_data_species_2 = wetland_data %>%
  filter(ALL.SPECIES.REPORTED == 1, year > 2014) %>%
  filter(OBSERVATION.COUNT != "X") %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>%
  group_by(wetland_buffer_ID,COMMON.NAME) %>%
  reframe(count = round(mean(OBSERVATION.COUNT))) %>%
  filter(COMMON.NAME %in% species_list) %>%
  mutate(metric = COMMON.NAME) %>% dplyr::select(-COMMON.NAME)

wetland_data_species_temp = wetland_attributes %>% dplyr::select(wetland_buffer_ID)

wetland_data_species = bind_rows(replicate(length(species_list), wetland_data_species_temp, simplify = FALSE))
wetland_data_species$metric = rep(species_list, each = length(wetland_data_species_temp$wetland_buffer_ID))

df.b = wetland_data_species %>%
  left_join(wetland_data_species_1) %>%
  left_join(wetland_data_species_2) 

df = df.a %>% bind_rows(df.b)




# wetlands to select

selected_wetlands = sampling_wetlands %>%
  filter(n >= 25)

finaldata = df %>% filter(wetland_buffer_ID %in% selected_wetlands$wetland_buffer_ID) %>%
  left_join(selected_wetlands) %>%
  left_join(wetland_attributes) %>%
  filter(!is.na(metric))

write.csv(finaldata,"final_ct_data.csv",row.names=F)




# analysis

library(extrafont)

ggp = ggplot(data = finaldata %>% filter(metric %in% c("total","shorebirds","ducks",
                                                       "rallids","large waterbirds")), 
             aes(x = AREA, y = richness, col = metric)) +
  geom_point() +
  theme_bw() +
  xlab("Area of Wetland") +
  ylab("Species Richness")

ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_log10(expand = c(0,0)) +
  scale_y_log10(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        legend.title = element_blank(),
        #axis.line.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #plot.background = element_rect(fill = "transparent",colour = NA),
        #panel.background = element_rect(fill = "transparent",colour = NA),
        #strip.background = element_blank(),
        legend.position = "bottom")

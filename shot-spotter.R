# Dear Diary

library(tidyverse)
library(leaflet)
#library(sf)
#library(tigris)
#library(ggthemes)
#library(gganimate)

shot <- read_csv(file = "shot-spotter/camdencounty_nj.csv") %>% mutate(type = fct_recode(type, "Multiple Gunshots" = "Multiple_Gunshots",
                           "Single Gunshot" = "Single_Gunshot",
                           "Gunshot or Firecracker" = "Gunshot_or_Firecracker"))

shot_file <- tempfile()

write_rds(shot, "shot-spotter/shot_file.rds")

camden <- places("NJ", class = "sf") %>% filter(NAME == "Camden")

shot_sf <- st_as_sf(shot, 
                    coords = c("longitude", "latitude"), 
                    crs = st_crs(camden)) 

camden %>% 
  ggplot() + 
  geom_sf() +
  geom_sf(data = shot_sf, aes(color = type, fill = type), alpha = 0.5) +
  labs(title = "Gunshots in Camden, NJ | Year: {closest_state}") +
  coord_sf(crs = 5070, datum = NA) +
  theme_map() +
  theme(legend.position = "bottom") +
  transition_states(yearmonth, state_length = 1.5)

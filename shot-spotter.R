# Dear Diary

library(tidyverse)
library(janitor)
library(fs)
library(tigris)
library(ggthemes)

shot <- read_csv(file = "shot-spotter/camdencounty_nj.csv")

camden <- tracts("NJ", "Camden", class="sf")

camden %>% 
  ggplot() + 
  geom_sf() +
  labs(title = "Gunshots in Camden, NJ")+
  coord_sf(crs = 5070, datum = NA) +
  theme_map()

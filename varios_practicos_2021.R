
library(tidyverse)
library(readxl)
library(sf)
library(tmap)

zoo1 <- read_excel("datos/zoo.xls")

zoo1_sf <- st_as_sf(zoo1, coords = c("longitude","latitude"), crs=4326)

tmap_mode("view")

tm_shape(zoo1_sf)+
  tm_dots()

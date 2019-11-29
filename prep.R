# Loading necessary libraries

library(tidyverse)
library(sf)
library(fs)
library(ggplot2)
library(maps)
library(ggthemes)
library(janitor)
library(openintro)
library(scales)
library(infer)
library(readr)
library(broom)
library(ggthemes)
library(tidyverse)

# Reading data in from the raw-data folder csv files

map_data <- read_csv("raw-data/final_data_map.csv")

final_data <- read_csv("raw-data/final_data.csv")


# Writing them out into rds files in the rds_files app

write_rds(final_data, "gun_project/final_data.rds")

write_rds(map_data, "gun_project/map_data.rds")

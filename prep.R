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

map_data <- read_csv("raw-data/final_data_map.csv", col_types = cols(
                                                                    state_name = col_character(),
                                                                    cause = col_character(),
                                                                    intent = col_character(),
                                                                    pop = col_double(),
                                                                    state = col_double(),
                                                                    total_deaths = col_double(),
                                                                    crude_rate = col_double(),
                                                                    age_adjusted_rate = col_double(),
                                                                    year = col_double(),
                                                                    rate_per_1000 = col_double(),
                                                                    deaths_year = col_double(),
                                                                    long = col_double(),
                                                                    lat = col_double(),
                                                                    group = col_double(),
                                                                    order = col_double(),
                                                                    suicide_rate = col_double(),
                                                                    suicide_deaths = col_double()
                                                                  ))

final_data <- read_csv("raw-data/final_data.csv", col_types = cols(
                                                                  state_name = col_character(),
                                                                  cause = col_character(),
                                                                  intent = col_character(),
                                                                  pop = col_double(),
                                                                  state = col_double(),
                                                                  total_deaths = col_double(),
                                                                  crude_rate = col_double(),
                                                                  age_adjusted_rate = col_double(),
                                                                  year = col_double(),
                                                                  rate_per_1000 = col_double(),
                                                                  deaths_year = col_double(),
                                                                  suicide_rate = col_double(),
                                                                  suicide_deaths = col_double()
                                                                ))

# This is the data from 538's project on gun violence in America. It was one of the few clean and condensed data sources
# I could find on guns in America, which just shows how politicized the issue is.
# Numbers are averages from 2012-2014

data_538 <- read_csv('raw-data/interactive_data.csv', col_types = cols(
                                                                      X1 = col_double(),
                                                                      Intent = col_character(),
                                                                      Gender = col_character(),
                                                                      Age = col_character(),
                                                                      Race = col_character(),
                                                                      Deaths = col_double(),
                                                                      Population = col_double(),
                                                                      Rate = col_double()
                                                                    )) %>% 
  rename(index = "X1") %>% 
  clean_names()


# The data set was set up to filter for certain categories which where listed as none selected, I decided to 
# mutate this value into an X instead for ease of coding later 

data_538[data_538 == "None selected"] <- "X"


# Writing them out into rds files in the rds_files app

write_rds(final_data, "gun_project/final_data.rds")

write_rds(map_data, "gun_project/map_data.rds")

write_rds(data_538, "gun_project/data_538.rds")

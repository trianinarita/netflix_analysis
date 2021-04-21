
# library shiny
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(dashboardthemes)
library(bslib)

# library tidy
library(tidyverse)
library(lubridate)

# library visualization
library(ggrepel)
library(plotly)
library(tibble)
library(glue)
library(scales)
library(wesanderson)

# read data
netflix <- read.csv("data/netflix_titles.csv", na.strings = c("", "NA"), stringsAsFactors = FALSE)

# tidy data type
netflix$type <- as.factor(netflix$type)
netflix$country <- as.factor(netflix$country)
netflix$rating <- as.factor(netflix$rating)
netflix$listed_in <- as.factor(netflix$listed_in)
netflix$director <- as.factor(netflix$director)
netflix$date_added<- mdy(netflix$date_added)

# tidy data
netflix <- distinct(netflix, type, title, country, release_year, .keep_all = T)

netflix$year <- year(netflix$date_added)

plis <- netflix %>% 
  select("country", "year", "type") %>% 
  mutate(country = as.character(country)) %>% 
  separate_rows(country, sep = ",") %>% 
  mutate(country = as.factor(country))

amount_by_country <- na.omit(plis) %>%
  mutate(country = na_if(country," ")) %>% 
  group_by(country, type, year) %>%
  summarise(count = n()) %>% 
  ungroup()

netflix_agg <- netflix %>%
  count(type) %>% 
  mutate(text = paste("Total:", n)) %>% 
  group_by(type) %>%
  ungroup()

type_movies <- netflix_agg %>% 
  filter(netflix_agg$type == "Movie")

type_tvshows <- netflix_agg %>% 
  filter(netflix_agg$type == "TV Show")  

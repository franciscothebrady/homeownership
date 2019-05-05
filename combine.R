# francisco 
# data aggregation 
# this script creates a function that collects and combines 
# ACS data on Home Tenure (Table ID B25003)
# for the years 2005:2017

library(dplyr)
library(readr)
library(purrr)
# i downloaded the files by hand and saved them in my data folder
# we want the ones with annual data ("_with_ann")

filelist <- list.files("data/", pattern = "with_ann.csv", full.names = TRUE)

# now define a function that reads in the files,
# cleans them, and combines all the available years of data

 #varnames <- lapply(filelist, read_csv, skip = 1, n_max = 2)

clean_hor <- function(filelist){
  print(filelist)
  # for testing: filelist <- filelist[1]
  data <- read_csv(filelist, skip = 1)
  # grab year from filename
  # this assumes the files are named ACS_YY_EST_etc, which they are when downloaded from AmericanFactFinder
  year <- stringr::str_extract(filelist, "[0-9]{2}")
  # drop margins of error -- they're important but just not for this
  moe      <- grep(pattern = "Error", names(data))
  data <- data[, -c(moe)]
  data <- data %>%
    # create as a year variable
    mutate(year = year) %>%
    select(year,
           fips = Id2, 
           fips_name  = Geography, 
           owner_occ  = contains("Owner occupied"),
           renter_occ = contains("Renter Occupied"))
    # calculate homeownership rate
  data <- data %>% 
    mutate(hor = owner_occ / (owner_occ + renter_occ)*100)
  
}

# now apply the function to the whole list of files
county_hor <- purrr::map_df(filelist, clean_hor)

saveRDS(county_hor, "data/county_hor.Rds")

# for fun 
library(ggplot2)
county_hor %>% 
  mutate(date = as.Date(paste(year, "01","01", sep = "-"), format = "%y-%m-%d")) %>%
  group_by(date) %>%
  summarise(mean_hor = mean(hor)) %>%
  ggplot() + geom_line(aes(x = date, y = mean_hor), lwd = 1) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(title = "Average Home Ownership Rate across all Counties", 
        subtitle = "2005 - 2017") +
  xlab("Date") + ylab("Average Rate")

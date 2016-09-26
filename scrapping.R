#TODO: 1) write down all maker-markeId pairings
#      2) Find a way to create a dataframe with makerId-minKm-maxKm to use in map
#      3) Extract all the data from those pairings (how to map only to relevant number of pages?)
#      4)

library(tidyverse)
library(rvest)

url_base <- "&"
cars_id <- map_df(1:50, function(i){
  cat(".")
  pg <- read_html(paste0(url_base, i))
  ids <- as.integer(pg %>% html_nodes(".parking-block") %>% xml_attr("data-parking"))
  data.frame(id = ids[2:length(ids)])
})

cars_id_temp <- cars_id

url_base_car <- "http://suchen.mobile.de/fahrzeuge/details.html?id="


cars_id_temp <- map_df(cars_id_temp$id[1:2], function(i){
  cat(".")
  pg <- read_html(paste0(url_base_car, i))

  data.frame(id = i,
             price = pg %>% html_node(".rbt-prime-price") %>% html_text(),
             title = pg %>% html_node("#rbt-ad-title") %>% html_text(),
             stringsAsFactors = FALSE)
})

mobile_cars %>% html_node(".rbt-prime-price") %>% html_text() #price
mobile_cars %>% html_node("#rbt-ad-title") %>% html_text() #title of the ad


mobile_cars %>% html_node("#rbt-envkv") %>% html_text()
mobile_cars %>% html_node(".rbt-sl") %>% html_text() #category
mobile_cars %>% html_node("#rbt-mileage-v") %>% html_text() #mileage
mobile_cars %>% html_node("#rbt-cubicCapacity-v") %>% html_text() #engine volume
mobile_cars %>% html_node("#rbt-power-v") %>% html_text() #engine power
mobile_cars %>% html_node("#rbt-fuel-v") %>% html_text() #type of fuel
mobile_cars %>% html_node("#rbt-envkv\\.consumption-v") %>% html_nodes("div") %>% html_text() #fuel consumption
mobile_cars %>% html_node("#rbt-envkv\\.emission-v") %>% html_text() #CO2 emmissions
mobile_cars %>% html_node("#rbt-numSeats-v") %>% html_text() #Number of seats
mobile_cars %>% html_node("#rbt-doorCount-v") %>% html_text() #Number of doors
mobile_cars %>% html_node("#rbt-transmission-v") %>% html_text() #Transmission
mobile_cars %>% html_node("#rbt-emissionClass-v") %>% html_text() #Emission class
mobile_cars %>% html_node("#rbt-emissionsSticker-v") %>% html_text() #Emission sticker
mobile_cars %>% html_node("#rbt-firstRegistration-v") %>% html_text() #first registration
mobile_cars %>% html_node("#rbt-hu-v") %>% html_text() #HU
mobile_cars %>% html_node("#rbt-climatisation-v") %>% html_text() #Climatisation
mobile_cars %>% html_node("#rbt-features") %>% html_nodes("p") %>% html_text()  #Extra features


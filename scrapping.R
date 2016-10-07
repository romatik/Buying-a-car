library(tidyverse)
library(rvest)

# id_km -------------------------------------------------------------------
#creating a dataset with the form manufacturer-id-minMileage-maxMileage-url, where url is a search url for mobile.de site.
#url is also formatted to include relevant id, minMileage and maxMileage in it so that number of results per request is < 1000
prod_pairings <- read_csv("Producer-makeId-pairings.csv")
minMileage <- seq(0, 100000, by = 10000)
maxMileage <- seq(10000, 110000, by = 10000)
maxMileage[11] <- 200000
id_km <- map_df(1:nrow(prod_pairings), function(i){
  data.frame(carmake = prod_pairings$carmake[i], id = prod_pairings$id[i], minMileage = minMileage, maxMileage = maxMileage)
})

#id_km dataframe contains information needed to create search urls for all producers identified in prod_pairings.
#it queries mobile.de with base url to find how many pages are there for each search
url_base <- "http://suchen.mobile.de/fahrzeuge/search.html?zipcodeRadius=10&ambitCountry=DE&ambitAddress=10409&damageUnrepaired=NO_DAMAGE_UNREPAIRED%s&scopeId=C&minMileage=%d&maxMileage=%d&makeModelVariant1.makeId=%d&isSearchRequest=true"
id_km <- id_km %>% 
  group_by(id, minMileage, maxMileage) %>% 
  mutate(url = sprintf(url_base, "&userPosition=52.54503%2C13.44169", minMileage, maxMileage, id)) %>%
  mutate(counter = read_html(url) %>% html_node(css = "span.hit-counter:nth-child(2)") %>% html_text() %>% as.numeric() %/% 20 + 1) %>%
  mutate(url_page = paste0(url, "&pageNumber=", seq(1:counter, by = 1)))


# search_url_df -----------------------------------------------------------
#search_url_df uses id_km to create all needed urls that are queried to find out cars_id's in the next step.
search_url_df <- map_df(1:nrow(id_km), function(i){
  data.frame(carmake = id_km$carmake[i], 
             url = id_km$url[i], 
             search_url = paste0(id_km$url[i], "&pageNumber=", seq(from = 1, to = id_km$counter[i], by = 1)))
})


# Getting id's of cars ----------------------------------------------------
cars_id <- map_df(1:nrow(search_url_df), function(i){
  cat(".")
  Sys.sleep(abs(rnorm(n = 1, mean = 1, sd = 1)))
  ids <- read_html(search_url_df$search_url[i]) %>% html_nodes(".parking-block") %>% xml_attr("data-parking") %>% as.integer()
  data.frame(id = ids[2:length(ids)], car_maker = search_url_df$carmake[i]) #ids start from 2 because 1 corresponds to an ad
})

#cleaning up
cars_id$id <- as.numeric(cars_id$id)
cars_id <- cars_id %>% filter(!is.na(id)) %>% filter(!duplicated(id)) 
write.csv(cars_id, "cars_id.csv", fileEncoding = "UTF-8", row.names = FALSE) #caching resulting dataset just in case
#there are ~23,000 cars in total for that search query, so 19,326 is a good sample 


# scraping data -----------------------------------------------------------
url_base_car <- "http://suchen.mobile.de/fahrzeuge/details.html?id="

#dummy_df is needed in case there is an error while querying mobile.de
dummy_df <- data.frame(id = NA_real_,
                       carmake = NA_real_,
                       price = NA_real_,
                       title = NA_real_,
                       owners = NA_real_,
                       envir = NA_real_,
                       category = NA_real_, #category
                       mileage = NA_real_, #mileage
                       eng_vol = NA_real_, #engine volume
                       eng_pow = NA_real_, #engine power
                       fuel_type = NA_real_, #type of fuel
                       fuel_cons = NA_real_, #fuel consumption
                       co2_em = NA_real_, #CO2 emmissions
                       seats = NA_real_, #Number of seats
                       doors = NA_real_, #Number of doors
                       transm = NA_real_, #Transmission
                       co2_class = NA_real_, #Emission class
                       co2_sticker = NA_real_, #Emission sticker
                       fr = NA_real_, #first registration
                       hu = NA_real_, #HU
                       clima = NA_real_, #Climatisation
                       extra = NA_real_,  #Extra features
                       stringsAsFactors = FALSE)

extract_data <- function(i){
  #extract_data takes in a row from cars_id data frame, creates link to go over and extracts data from that link
  cat(i, ",")
  pg <- read_html(paste0(url_base_car, cars_id$id[i]))
  
  data.frame(id = cars_id$id[i],
             carmake = cars_id$car_maker[i],
             price = pg %>% html_node(".rbt-prime-price") %>% html_text(),
             title = pg %>% html_node("#rbt-ad-title") %>% html_text(),
             owners = pg %>% html_node("#rbt-numberOfPreviousOwners-v") %>% html_text(),
             envir = pg %>% html_node("#rbt-envkv") %>% html_text(),
             category = pg %>% html_node(".rbt-sl") %>% html_text(), #category
             mileage = pg %>% html_node("#rbt-mileage-v") %>% html_text(), #mileage
             eng_vol = pg %>% html_node("#rbt-cubicCapacity-v") %>% html_text(), #engine volume
             eng_pow = pg %>% html_node("#rbt-power-v") %>% html_text(), #engine power
             fuel_type = pg %>% html_node("#rbt-fuel-v") %>% html_text(), #type of fuel
             fuel_cons = pg %>% html_node("#rbt-envkv\\.consumption-v") %>% html_nodes("div") %>% html_text() %>% paste(collapse = ";"), #fuel consumption
             co2_em = pg %>% html_node("#rbt-envkv\\.emission-v") %>% html_text(), #CO2 emmissions
             seats = pg %>% html_node("#rbt-numSeats-v") %>% html_text(), #Number of seats
             doors = pg %>% html_node("#rbt-doorCount-v") %>% html_text(), #Number of doors
             transm = pg %>% html_node("#rbt-transmission-v") %>% html_text(), #Transmission
             co2_class = pg %>% html_node("#rbt-emissionClass-v") %>% html_text(), #Emission class
             co2_sticker = pg %>% html_node("#rbt-emissionsSticker-v") %>% html_text(), #Emission sticker
             fr = pg %>% html_node("#rbt-firstRegistration-v") %>% html_text(), #first registration
             hu = pg %>% html_node("#rbt-hu-v") %>% html_text(), #HU
             clima = pg %>% html_node("#rbt-climatisation-v") %>% html_text(), #Climatisation
             extra = pg %>% html_node("#rbt-features") %>% html_nodes("p") %>% html_text() %>% paste(collapse = ";"),  #Extra features
             stringsAsFactors = FALSE)
}

cars_df <- map_df(1:nrow(cars_id), possibly(extract_data, dummy_df)) #593 ads were removed from mobile.de
saveRDS(cars_df, file = "first_pass.Rds") 

cars_id %>% group_by(car_maker) %>% tally()

# Load libraries ----------------------------------------------------------

library(dplyr)
library(sbtools)
library(stringr)

# Read data from ScienceBase ----------------------------------------------
# link to data: https://www.sciencebase.gov/catalog/item/60afb5e5d34e4043c8564830

sb_id <- '60afb5e5d34e4043c8564830'
sbtools::item_get(sb_id = sb_id)

sb_filenames <- item_list_files(sb_id = sb_id)

originalData <-readr::read_csv(file = sb_filenames$url[1])

# Parent Event (Cruise Info) --------------------------------------------------------------

parentCruise <- originalData %>% 
  rename(
    eventID = Cruise
  ) %>% 
  
  mutate(
    eventDate = "2018-10-12/2018-11-07",
    samplingProtocol = "remotely occupied vehicle"
  ) %>% 
  
  select(
    eventID,
    eventDate,
    samplingProtocol
  ) %>% 
  
  distinct()


# Modify Dataset from the Same Cruise to Provide Dates ---------

# event dates pulled from doi 10.5066/P99DIQZ5, a different set of samples from the same cruise
# aligning site names to that of main data so I can join by Site Name

cruiseWithSamplingDates <- readr::read_csv("cruise2018-SH-12_with_dates.csv")

cruiseWithSamplingDates <- cruiseWithSamplingDates %>% 
  mutate(
    locality = `Site Name`,
    locality = recode(locality,
                      "Daisy Bank 1" = "Daisy Bank",
                      "Mendocino" = "Mendocino Ridge",
                      "Santa Lucia I" = "Santa Lucia Bank",
                      "Sverdup" = "Sverdrup Bank 1",
                      "Sverdup 2" = "Sverdrup Bank 2",
                      "Wind Farm  (south)" = "Wind Farm south",
                      "Pt. Arena" = "Point Arena",
                      "Bandon High" = "Bandon High Spot"
                      ),
    eventDate = Date %>% 
      as.Date("%m/%d/%y"),
    # two sampling dates, so mutating to include both
     eventDate = case_when (`locality` == "Point Arena" ~ paste("2018-10-21","2018-10-22", sep = "/"),
                           `locality` == "La Cruz Canyon" ~ paste("2018-10-27","2018-10-28", sep = "/"),
                           .default = as.character(eventDate)
                                              )
  )




# Child Events (Station-Level Collection) ---------------------------------


# remove non-occurrence sampling events
stationEvents <- originalData %>% 
  filter(
  !TaxaCategory == "POM",
  !TaxaCategory == "Other"
  ) %>% 
  
  select(
    Cruise,
    Location,
    Station,
    Latitude,
    Longitude,
    Depth_m
  ) %>% 
  
  rename(
    parentEventID = Cruise,
    locality = Location
  ) %>% 
    
  mutate(
    geodeticDatum = "WGS84",
    decimalLatitude = Latitude,
    decimalLongitude = Longitude,
    verbatimLatitude = Latitude,
    verbatimLongitude = Longitude,
    minimumDepthInMeters = Depth_m,
    maximumDepthInMeters = Depth_m,
    countryCode = "US",
    samplingProtocol = "remotely occupied vehicle"
  ) %>% 
  
  distinct() %>% 
  
  group_by(Station) %>%
  mutate(
    eventID = paste(Station, row_number(), sep = "_" %>%
                    stringr::str_remove_all(pattern = "-"))
    )%>%
  ungroup()
  
#joining station events with the site visit dates table
  stationEventsNew <- 
    left_join(stationEvents, cruiseWithSamplingDates, by = "locality", relationship = "many-to-many") %>%
    mutate(eventDate = case_when (`locality` == "Heceta Bank" ~ paste("2018-10-12","2018-11-07", sep = "/"),
                          .default = as.character(eventDate))
    )
    
#creating final table with parent cruise and sampling events
  Shimada_Event <- bind_rows(parentCruise, stationEventsNew) %>%   
    select(
      eventID,
      parentEventID,
      eventDate,
      locality,
      geodeticDatum,
      decimalLatitude,
      decimalLongitude,
      countryCode,
      minimumDepthInMeters,
      maximumDepthInMeters,
      samplingProtocol,
      verbatimLatitude,
      verbatimLongitude
      ) %>% 
    distinct()
  

# Export to csv -----------------------------------------------------------

  Infauna_Event %>% 
    write.csv(
      paste0("data/Shimada_2018_expedition_event_", Sys.Date(), ".csv"),
      na = "",
      fileEncoding = "UTF-8", 
      row.names = FALSE
    )

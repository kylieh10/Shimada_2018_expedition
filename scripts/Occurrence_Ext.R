# Load libraries ----------------------------------------------------------

library(dplyr)
library(sbtools)
library(stringr)
library(worrms)

# Read data from ScienceBase ----------------------------------------------
# link to data: https://www.sciencebase.gov/catalog/item/60afb5e5d34e4043c8564830

sb_id <- '60afb5e5d34e4043c8564830'
sbtools::item_get(sb_id = sb_id)

sb_filenames <- item_list_files(sb_id = sb_id)

originalData <-readr::read_csv(file = sb_filenames$url[1])

# make sure to run Event_Core script first to join with occurrences in this script
# Pulling unique occurrences ----------------------------------------------

distinctOcc <- originalData %>% 
  distinct(AlternativeAssociatedID, SpeciesID, Latitude, .keep_all = TRUE) %>% 
  filter(
    !TaxaCategory == "POM",
    !TaxaCategory == "Other"
  )

# Occurrence Table --------------------------------------------------------

occ_ext <- distinctOcc %>% 
  rename(
    locality = Location,
    decimalLatitude = Latitude,
    decimalLongitude = Longitude
  ) %>% 
  mutate(
    basisOfRecord = "humanObservation",
    occurrenceID = case_when(AlternativeAssociatedID != "ND" ~ AlternativeAssociatedID,
                             AlternativeAssociatedID == "ND" ~ IsoNum,
                             AlternativeAssociatedID == "SH_18_12-245" ~ paste(AlternativeAssociatedID, row_number())),
    occurrenceStatus = "present",
    verbatimIdentification = SpeciesID,
    vernacularName = TaxaCategory,
    individualCount = "1"
  )
  
occ_ext_dates <- left_join (occ_ext, Shimada_Event, by = c("locality", "decimalLatitude", "decimalLongitude"),
                      relationship = "many-to-many") %>% 
  mutate(
    identificationQualifier = 
      case_when(grepl("cf\\.", SpeciesID) ~ gsub(".*cf\\.(.+)", "inc\\.\\1", SpeciesID),
                grepl("sp\\.", SpeciesID) ~ "indet.")
# replace inside speciesID with text in the matching group (.+)
    )%>% 
  select(
    occurrenceID,
    eventID,
    eventDate,
    occurrenceStatus,
    basisOfRecord,
    individualCount,
    locality,
    countryCode,
    SpeciesID,
    verbatimIdentification,
    TaxaCategory,
    vernacularName,
    identificationQualifier
  )

# Pulling WoRMS taxonomic info --------------------------------------------

speciesList <- distinctOcc$SpeciesID %>% na.omit() %>% unique()
speciesList[speciesList=="Tubeworm"] <- "Polychaeta"
speciesList[speciesList=="Starfish"] <- "Asteroidea"

myAphiaID <- wm_records_taxamatch(speciesList) %>% 
  data.table::rbindlist()

uniqueAphiaSelectColumns <- select(.data = myAphiaID,
                                   scientificname,
                                   rank,
                                   kingdom,
                                   phylum,
                                   class,
                                   order,
                                   family,
                                   genus,
                                   lsid,
                                   AphiaID) %>%
  rename(
    scientificName = scientificname,
    taxonRank = rank,
    scientificNameID = lsid
  )

Shimada_occ <- left_join(occ_ext_dates, uniqueAphiaSelectColumns)


# Export to csv --------------------------------------------------------------

Shimada_occ %>% 
  write.csv(
    paste0("data/Shimada_2018_expedition_occurrences_", Sys.Date(), ".csv"),
    na = "",
    fileEncoding = "UTF-8", 
    row.names = FALSE
  )
  
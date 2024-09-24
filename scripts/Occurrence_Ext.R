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
    occurrenceID = case_when(!AlternativeAssociatedID == "ND" ~ AlternativeAssociatedID,
                             AlternativeAssociatedID == "ND" ~ IsoNum,
                             AlternativeAssociatedID == "SH_18_12-245" ~ paste(AlternativeAssociatedID, row_number())),
    occurrenceStatus = "present",
    verbatimIdentification = SpeciesID,
    vernacularName = TaxaCategory
  )
  
occ_ext <- left_join (occ_ext, Shimada_Event, by = c("locality", "decimalLatitude", "decimalLongitude"),
                      relationship = "many-to-many") %>% 
  select(
    occurrenceID,
    eventID,
    eventDate,
    occurrenceStatus,
    basisOfRecord,
    locality,
    countryCode,
    
  )

# Pulling WoRMS taxonomic info --------------------------------------------

speciesList <- distinctOcc$SpeciesID %>% na.omit() %>% unique()

myAphiaID <- wm_records_names(speciesList) %>% 
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

  
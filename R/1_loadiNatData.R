
source("R/0_setup.R")


# Load gbif data --------

#  GBIF.org (25 March 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.qrvnyw
df_gbif <- fread(file.path("data", "iNat_data", "0030828-240321170329656.csv")) %>% 
  dplyr::filter(grepl("StillImage", mediaType)) %>% 
  dplyr::mutate(
    date = as_date(paste(year, month, day, sep = "-")),
    yday = lubridate::yday(date)
    )
df_focal <- df_gbif %>% 
  dplyr::filter(genus %in% c("Lasiurus", "Lasionycteris", "Aeorestes", "Nycticeius")) %>% 
  dplyr::mutate(
    species = gsub("Aeorestes", "Lasiurus", species),
    genus = gsub("Aeorestes", "Lasiurus", genus),
    ID = paste0("gbif_", gbifID)
    )

# Identify photos associated with each observation.
#  GBIF.org (25 March 2024) GBIF Occurrence Download https://doi.org/10.15468/dl.mf4pa3
gbif_photos <- fread(file.path("data", "iNat_data", "0031166-240321170329656", "multimedia.txt")) %>% 
  dplyr::filter(gbifID %in% df_focal$gbifID, type == "StillImage") %>% 
  dplyr::mutate(
    ID = paste0("gbif_", gbifID),
    photo_id = gsub("https://inaturalist-open-data.s3.amazonaws.com/photos/","", identifier),
    photo_id = tools::file_path_sans_ext(photo_id),
    photo_id = gsub("/original","", photo_id),
    photo_id = as.numeric(photo_id),
    fileext = tools::file_ext(identifier),
    file = paste0(photo_id, ".", fileext)
  )

# Pull out GBIF tax backbone
gbif_genusSpecies <- df_focal %>% 
  dplyr::select(species, genus) %>% 
  distinct

# Use iNat API to fill in some gaps ---------

library(rinat)

rinatPath <- file.path("data", "dat_inat_api.csv")
if(!file.exists(rinatPath)) {
  # Load by genera.
  df_lasiurus      <- rinat::get_inat_obs(place_id = 97394, taxon_id = 40516, quality = "research", maxresults = 10000)
  df_lasionycteris <- rinat::get_inat_obs(place_id = 97394, taxon_id = 40628, quality = "research", maxresults = 10000)
  df_nycticieus    <- rinat::get_inat_obs(place_id = 97394, taxon_id = 40563, quality = "research", maxresults = 10000)
  
  dat_inat_api <- bind_rows(df_lasiurus, df_lasionycteris, df_nycticieus)
  fwrite(dat_inat_api, rinatPath, row.names = F)
} else {
  dat_inat_api <- fread(rinatPath)
}

# Bind and get into similar format to GBIF data.
dat_inat <- dat_inat_api %>% 
  dplyr::mutate(
    ID = paste0("inat_", id),
    fileext = tools::file_ext(image_url),
    photo_id = gsub("/medium","", image_url),
    photo_id = gsub("https://inaturalist-open-data.s3.amazonaws.com/photos/","", photo_id),
    photo_id = gsub("https://static.inaturalist.org/photos/","", photo_id),
    photo_id = tools::file_path_sans_ext(photo_id),
    photo_id = as.numeric(photo_id),
    file = paste0(photo_id, ".", fileext),
    identifier = gsub("medium", "original", image_url)
  ) %>% 
  # Then exclude observations that we already covered from the GBIF dataset.
  dplyr::filter(!url%in%df_gbif$occurrenceID) %>% 
  # Then do some formatting to get column names to match up
  dplyr::rename(
    decimalLongitude = longitude,
    decimalLatitude  = latitude,
    species = scientific_name
  ) %>% 
  dplyr::mutate(
    observed_on = lubridate::ymd(observed_on),
    yday = yday(observed_on)
  ) %>% 
  left_join(., gbif_genusSpecies) %>% 
  # Exclude some manual flags 
  dplyr::filter(
    identifier != "",
    yday != 1
  )

  
# Combine data sources ----------------------------------------------------

df_all <- df_focal %>% 
  bind_rows(dat_inat) %>% 
  dplyr::mutate(
    floor_lat = floor(decimalLatitude),
    floor_lon = floor(decimalLongitude),
    lat_bin2 = cut(decimalLatitude, breaks = seq(0,90,by=2)),
    lon_bin2 = cut(decimalLongitude, breaks = seq(-180,180,by=2)),
    lat_bin5 = cut(decimalLatitude, breaks = seq(0,90,by=5)),
    lon_bin5 = cut(decimalLongitude, breaks = seq(-180,180,by=5))
  ) %>% 
  dplyr::select(ID, genus, species, date, yday, starts_with("decimal"), starts_with("floor"), starts_with("lat"),  starts_with("lon"))

photos_all <- bind_rows(gbif_photos, dat_inat)
photos_all <- photos_all %>% 
  dplyr::mutate(
    file = paste0( photos_all$photo_id, ".", photos_all$fileext)
  ) %>% 
  dplyr::select(ID, photo_id, fileext, identifier, file)

# Download photos --------
pb = txtProgressBar(min = 0, max = nrow(photos_all), initial = 0, style =3 ) 
for(i in 1:nrow(photos_all)) {
  
  myext <- file.path("data/bat_images", photos_all$file[i] )
  if(file.exists(myext)) next
  
  download.file(photos_all$identifier[i], myext , quiet = T)
  
  Sys.sleep(1.05)
  setTxtProgressBar(pb,i)
  if(i %% 100 == 0) { writeLines(paste("\nWorking on", i, "of", nrow(photos_all), "at", format(Sys.time(), "%b %d %X") )) }
  
}

#### ImageAnt analyses happen here #######

# logfile <- fread(file.path("img_annotation", "bat_annotations.log"),  quote="")
# fwrite(logfile, file = file.path("img_annotation", "bat_annotations2.log"))
# 

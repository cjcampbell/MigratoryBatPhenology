# source("R/1_loadiNatData.R")
theme_set(theme_minimal())

ann <-  read.csv(file.path("img_annotation", "bat_annotations.csv")) %>% 
  left_join(., photos_all, by = "file") %>% 
  inner_join(., df_all, by = "ID") 

annDat <- ann %>% 
  dplyr::filter(how_observed != "U") %>% 
  group_by(ID) %>% 
  dplyr::reframe(
    how_observed = list(sort(unique(how_observed))),
    l = length(unlist(how_observed)),
    how_observed_final = case_when(
      l == 1 ~ unlist(how_observed),
      any(unlist(how_observed) == "spectro")      ~ "spectro",
      any(unlist(how_observed) == "very dead")     ~ "very dead",
      any(unlist(how_observed) == "recently dead") ~ "recently dead",
      any(unlist(how_observed) == "captured")      ~ "captured",
      all(unlist(how_observed) == c("grounded", "roosting")) ~ "grounded",
      all(unlist(how_observed) == c("flight", "grounded")) ~ "grounded",
      all(unlist(how_observed) == c("flight", "roosting")) ~ "flight",
      TRUE ~ NA
    )
  ) %>% 
  distinct() %>% 
  # Manual changes
  dplyr::mutate(
    how_observed_final = case_when(
      ID == "gbif_2518027607" ~ "roosting",
      TRUE ~ how_observed_final
    )
  ) %>% 
  left_join(., df_all, by = "ID") %>% 
  dplyr::select(-how_observed) %>% 
  dplyr::rename(how_observed = how_observed_final)

fwrite(annDat, file.path("out","iNat_dat.csv"), row.names = F)

annDat %>% 
  ggplot() + 
  geom_bar(aes(species, fill = how_observed))

annDat %>% 
  ggplot() + 
  geom_bar(aes(yday, fill = how_observed)) +
  facet_grid(how_observed~species, scales = "free_y")

whichToInclude <- c("grounded", "flight", "recently dead", "roosting", "mating")

annDat %>%
  dplyr::filter(how_observed %in% whichToInclude, species == "Lasiurus borealis") %>% 
  arrange(desc(lat_bin2)) %>% 
  {mutate(., lat_bin2 = factor(lat_bin2, levels = unique(.$lat_bin2))) } %>% 
  ggplot() + 
  geom_histogram(aes(yday, fill = species), binwidth = 14) +
  facet_grid(lat_bin2~lon_bin2, scales = "free_y")

annDat %>% 
  dplyr::filter(how_observed %in% whichToInclude, species == "Lasionycteris noctivagans") %>% 
  arrange(desc(lat_bin2)) %>% 
  {mutate(., lat_bin2 = factor(lat_bin2, levels = unique(.$lat_bin2))) }%>% 
  ggplot() + 
  geom_histogram(aes(yday, fill = species), binwidth = 14) +
  facet_grid(lat_bin2~lon_bin2, scales = "free_y")

annDat %>% 
  dplyr::filter(how_observed %in% whichToInclude, species == "Lasiurus borealis") %>% 
  arrange(desc(lat_bin5)) %>% 
  {mutate(., lat_bin5 = factor(lat_bin5, levels = unique(.$lat_bin5))) }%>% 
  ggplot() + 
  geom_histogram(aes(yday, fill = species), binwidth = 14) +
  facet_grid(lat_bin5~lon_bin5, scales = "free_y")


### Subset to labo ----
labo_subset <- annDat %>% 
  dplyr::filter(how_observed %in% whichToInclude, species == "Lasiurus borealis") %>% 
  count( lat_bin5, lon_bin5) %>% 
  dplyr::filter(n >= 60) %>% 
  dplyr::select(-n) %>% 
  inner_join(., annDat) %>% 
  dplyr::filter(how_observed %in% whichToInclude, species == "Lasiurus borealis") %>% 
  arrange(desc(lat_bin5)) %>% 
  {mutate(., lat_bin5 = factor(lat_bin5, levels = unique(.$lat_bin5))) }

labo_subset %>% 
  dplyr::mutate(before172 = case_when(yday >= 172 ~ "no", TRUE ~ "yes")) %>% 
  ggplot() + 
  geom_histogram(aes(yday, fill = before172), binwidth = 14) +
  facet_grid(lat_bin5~lon_bin5) +
  ggtitle("Observations of LABO")

# Filter out duplicate dates.
labo_subset %>% 
  dplyr::mutate(before172 = case_when(yday >= 172 ~ "no", TRUE ~ "yes")) %>% 
  group_by(species, how_observed, date, floor_lat, floor_lon, lat_bin5, lon_bin5) %>% 
  slice(1) %>% 
  ggplot() + 
  geom_histogram(aes(yday, fill = before172), binwidth = 14) +
  facet_grid(lat_bin5~lon_bin5) +
  ggtitle("Observations of LABO, one obs/day/degree")


### Subset to laci ----
laci_subset <- annDat %>% 
  dplyr::filter(how_observed %in% whichToInclude, species == "Lasiurus cinereus") %>% 
  count( lat_bin5, lon_bin5) %>% 
  dplyr::filter(n >= 20) %>% 
  dplyr::select(-n) %>% 
  inner_join(., annDat) %>% 
  dplyr::filter(how_observed %in% whichToInclude, species == "Lasiurus cinereus") %>% 
  arrange(desc(lat_bin5)) %>% 
  {mutate(., lat_bin5 = factor(lat_bin5, levels = unique(.$lat_bin5))) }

laci_subset %>% 
  dplyr::mutate(before172 = case_when(yday >= 172 ~ "no", TRUE ~ "yes")) %>% 
  ggplot() + 
  geom_histogram(aes(yday, fill = before172), binwidth = 14) +
  facet_grid(lat_bin5~lon_bin5) +
  ggtitle("Observations of laci")

# Filter out duplicate dates.
laci_subset %>% 
  dplyr::mutate(before172 = case_when(yday >= 172 ~ "no", TRUE ~ "yes")) %>% 
  group_by(species, how_observed, date, floor_lat, floor_lon, lat_bin5, lon_bin5) %>% 
  slice(1) %>% 
  ggplot() + 
  geom_histogram(aes(yday, fill = before172), binwidth = 14) +
  facet_grid(lat_bin5~lon_bin5) +
  ggtitle("Observations of laci, one obs/day/degree")

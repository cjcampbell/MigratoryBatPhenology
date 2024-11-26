
library(patchwork)
library(ggpubr)

dat_ba_init <- read_csv(file.path("data", "batAMP_data", "batamp_lano_laci_labo__04_01_2024.csv")) %>% 
  dplyr::select(lat, lon, presence_only, night, lano, labo, laci, dayofyear)

dat_ba <- dat_ba_init %>% 
  pivot_longer(cols = c("lano", "labo", "laci"), names_to = "species", values_to = "calls") %>% 
  dplyr::mutate(
    night = ymd(night),
    yday = yday(night),
    detected = case_when( calls >= 1 ~ 1, calls == 0 ~ 0 , TRUE ~ as.numeric(NA)),
    lat_bin2 = cut(lat, breaks = seq(0,90,by=2)),
    lon_bin2 = cut(lon, breaks = seq(-180,180,by=2)),
    lat_bin5 = cut(lat, breaks = seq(0,90,by=5)),
    lon_bin5 = cut(lon, breaks = seq(-180,180,by=5))
  ) %>% 
  dplyr::filter(!is.na(calls))

dat_ba %>% count(night) %>% nrow
dat_ba %>% count(lat, lon) %>% nrow
dat_ba %>% count(lat, lon, night) %>% nrow
dat_ba %>% count(species, presence_only, detected)
dat_ba %>% count(presence_only, detected, calls)
dat_ba %>% count(lat, lon, detected)

pa_site <- dat_ba %>% 
  group_by(lat,lon, lat_bin2, lon_bin2, lat_bin5, lon_bin5, species) %>% 
  arrange(desc(detected)) %>% 
  slice(1)

ggplot(pa_site) +
  geom_point(aes(x=lon, y=lat, color = factor(detected))) +
  facet_wrap(~species) +
  coord_quickmap()

pa_site_yday <- dat_ba %>% 
  group_by(lat,lon, lat_bin2, lon_bin2, lat_bin5, lon_bin5, yday, species) %>% 
  arrange(desc(detected)) %>% 
  slice(1) %>% 
  ungroup %>% 
  arrange(desc(lat_bin5)) %>% 
  {mutate(., lat_bin5 = factor(lat_bin5, levels = unique(.$lat_bin5))) }

pa_site_yday %>% 
  dplyr::filter(species == "labo" ) %>%
  ggplot() +
  geom_jitter(aes(x=yday, y = factor(detected), color = factor(detected)), width = 0, height = 0.1, alpha = 0.05) +
  facet_grid(lat_bin5~lon_bin5) +
  scale_x_continuous(limits = c(1,365)) +
  theme(
    panel.grid.major.x = element_line()
  )

# Plot detections and nondetections ---------
dplyr::filter(pa_site_yday, species == "laci") %>% 
ggplot() +
  geom_histogram(aes(x=yday, fill = factor(detected)), position = "stack") +
  facet_grid(lat_bin5~lon_bin5) +
  scale_x_continuous(limits = c(1,365)) +
  theme(
    panel.grid.major.x = element_line()
  ) +
  ggtitle("LACI")

dplyr::filter(pa_site_yday, species == "labo") %>% 
  ggplot() +
  geom_histogram(aes(x=yday, fill = factor(detected)), position = "stack") +
  facet_grid(lat_bin5~lon_bin5) +
  scale_x_continuous(limits = c(1,365)) +
  theme(
    panel.grid.major.x = element_line()
  ) +
  ggtitle("LABO")

dplyr::filter(pa_site_yday, species == "lano") %>% 
  ggplot() +
  geom_histogram(aes(x=yday, fill = factor(detected)), position = "stack") +
  facet_grid(lat_bin5~lon_bin5) +
  scale_x_continuous(limits = c(1,365)) +
  theme(
    panel.grid.major.x = element_line()
  ) +
  ggtitle("LANO")


# Plot detections / unit effort -------

effort <- pa_site_yday %>%
  count(lat_bin5, lon_bin5, species, yday, name = "detectorNights")
detections <- pa_site_yday %>%
  dplyr::filter(detected == 1) %>% 
  count(lat_bin5, lon_bin5, species, yday, name = "detections")
batAMP_det <- full_join(detections, effort) %>% 
  dplyr::mutate(detections_per_detectorNights = detections/detectorNights)

fwrite(batAMP_det, file = file.path("out", "batAMP_detections.csv"), row.names = F)


dplyr::filter(batAMP_det, species == "laci") %>% 
  ggplot() +
  geom_col(aes(x=yday, y = detections_per_detectorNights, fill = log10(detectorNights)), position = "stack") +
  scale_fill_gradient2(low = "grey95", mid = "grey50", high = "black", midpoint = 1) +
  facet_grid(lat_bin5~lon_bin5) +
  scale_x_continuous(limits = c(1,365)) +
  theme(
    panel.grid.major.x = element_line()
  ) +
  ggtitle("LACI")

dplyr::filter(batAMP_det, species == "labo") %>% 
  ggplot() +
  geom_col(aes(x=yday, y = detections_per_detectorNights, fill = log10(detectorNights)), position = "stack") +
  scale_fill_gradient2(low = "grey95", mid = "grey50", high = "black", midpoint = 1) +
  facet_grid(lat_bin5~lon_bin5) +
  scale_x_continuous(limits = c(1,365)) +
  theme(
    panel.grid.major.x = element_line()
  ) +
  ggtitle("LABO")

dplyr::filter(batAMP_det, species == "lano") %>% 
  ggplot() +
  geom_col(aes(x=yday, y = detections_per_detectorNights, fill = log10(detectorNights)), position = "stack") +
  scale_fill_gradient2(low = "grey95", mid = "grey50", high = "black", midpoint = 1) +
  facet_grid(lat_bin5~lon_bin5) +
  scale_x_continuous(limits = c(1,365)) +
  theme(
    panel.grid.major.x = element_line()
  ) +
  ggtitle("LANO")




# Plot sampling density on map --------------------------------------------

US <- geodata::gadm("USA",path = "/Users/ccampbell/Library/CloudStorage/Box-Box/- Missions & Programs/Research & Development/Data Products", resolution = 2) %>% 
  st_as_sf() %>% 
  dplyr::filter(NAME_1 != "Alaska", NAME_1 != "Hawaii")
mygrid <- st_make_grid(US, cellsize = 5, offset = c(-125, 25)) %>% 
  st_as_sf() %>% 
  dplyr::mutate(
    coords = as.character(x),
    coords = gsub("))", "",coords),
    coords = gsub("list\\(c\\(", "", coords),
    lon_bin5 = as.character(gsub(" ", "", paste0("(", paste0(stringr::word(coords, 1,2, sep = ",")), "]"))),
    lat_bin5 = as.character(gsub(" ", "", paste0("(", paste0(stringr::word(coords, 7,8, sep = ",")), "]")))
  )


grid_count <- dat_ba %>% 
  count(night, lat_bin5, lon_bin5) %>% 
  dplyr::select(-n) %>% 
  count(lon_bin5, lat_bin5) %>% 
  right_join(., mygrid) %>% 
  st_as_sf() 

cols <- viridis::turbo(10)
p_mapGrid2 <- ggplot() + 
  geom_sf(US, mapping = aes()) + 
  geom_sf(grid_count, mapping = aes(fill = n/1000), alpha = 0.5, linewidth = 0.5) +
  scale_fill_gradientn("Nights surveyed\n(thousand)", colors = c(cols, rep(cols[10], 5)), na.value = NA) +
  theme_void() +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme(
    plot.background = element_rect(color = NA, fill = "black"),
    legend.position = c(0.9,0.2),
    legend.direction="horizontal",
    legend.title = element_text(hjust=0.5),
    text = element_text(color = "white")
  )
p_mapGrid2
ggsave(p_mapGrid2, file = "figs/map_grid_batAMP.png", dpi = 600, width = 12, height = 6)



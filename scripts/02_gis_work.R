# **************************************** #
# MHDP project
# Script for generating tables and charts
# Oct 2025
# **************************************** #

p_colors <- c("#4E79A7", "#00868B","#36648B", "#A0CBE8", "#FFBE7D", "#59A14F",
              "#2F4F4F")
 #ref: https://r-charts.com/colors/


# Load the necessary packages  ----------------------------
pacman::p_load(
  tidyverse,  # data wrangling and visualization
  table1,     # for summary EDA tables
  here,       # project oriented workflow
  scales,     # easily convert proportions to percents
  gtsummary,  # summary statistics and tests
  ggthemes,   # additional themes for ggplot2
  ggthemr,    # more themes for ggplot2 form ggthemr
  DT,
  sf,
  tmap,
  ggspatial,
  readxl,
  skimr,
  janitor,
  patchwork,
  viridis,
  gt
) 


# Read the Ug districts shape file from the zip folder -------------
# download dataset of regional referral hosiptals with their co-ordinates from
# (https://data.humdata.org/dataset/ugandan-regional-and-general-hospitals/resource/7b4d01f7-8d4d-4255-a69a-8b54232c0d69)
# Add on the details for Entebbe, Yumbe and Butabika hospitals manually as they are missing in the dataset
# Entebbe Regional Referral Hospital, Entebbe, Wakiso, 0.06389, 32.47167
# Yumbe Regional Referral Hospital, Yumbe, Yumbe, 3.4650, 31.2450
# Butabika National Referral Hospital, Kampala, Kampala, 0.31587, 32.6579

# unzip(here::here("gadm41_UGA_shp.zip"), exdir = here("geo_data")) do this once to extract the files
ug_dist <- read_sf(here("data/geo_data", "gadm41_UGA_2.shp"))
geo_cord <- read_excel(here("data/geo_data", "rrh-with-coordinates-hashed.xls"), skip = 1, col_names = F)

geo_cord <- geo_cord %>%
  select(1, 2, 6, 8, 9) %>%
  setNames(c("gps", "hospi", "organisation_unit_name", "region", "district")) %>%
  mutate(
    organisation_unit_name = gsub(
      pattern = "RRH",
      replacement = "Regional Referral Hospital",
      x = organisation_unit_name
    ),
    organisation_unit_name = if_else(organisation_unit_name == "Naguru Regional Referral Hospital",
      "China Uganda Friendship (Naguru) Regional Referral Hospital",
      organisation_unit_name
    )
  ) %>%
  separate(gps, into = c("long", "lat"), sep = ",", convert = TRUE)

# Generate a dataset with co-ordinates for the missing hospitals 

long <- c(32.47167, 31.2450, 32.6579)
lat <- c(0.06389, 3.4650, 0.31587)
hospi <- c("Entebbe Regional RH", "Yumbe Regional RH", "Butabika National RH")
organisation_unit_name <- c("Entebbe Regional Referral Hospital", "Yumbe Regional Referral Hospital", "Butabika National Referral Hospital")
region <- c("Central", "Northern", "Central")
district <- c("Wakiso", "Yumbe", "Kampala")
new_hosp <- data.frame(long, lat, hospi, organisation_unit_name, region, district)

# Combine with the existing geo_cord dataframe
geo_cord <- bind_rows(geo_cord, new_hosp)

# Convert to shape file/ spatial object
geo_cord_sf <- st_as_sf(geo_cord, coords = c("long", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = ug_dist, fill = "gray95", color = "lightblue") +
  geom_sf(data = geo_cord_sf, color = "darkgreen", size = 5, pch = 3, stroke = 2) +
  labs(
    title = "Regional Referral Hospitals in Uganda",
    caption = "Source of co-ordinates data: data.humdata.org & GADM \n
    dataset: MHDP Uganda Dataset"
  ) +
  theme_dark()

# Remember (pch = 3 (+ shape), pch = 1 (circle shape))

# Plotting the map of Uganda with patients by facility---------------------

# Aggregate MH cases by facility
table1::table1(~mh_organisation_unit_name, data = target_data)

mh_cases_by_unit <- target_data %>%
  group_by(mh_organisation_unit_name) %>%
  summarise(total_cases = n())

# Merge MH cases data with shape file
uganda_mh_map <- geo_cord_sf %>%
  rename(mh_organisation_unit_name = organisation_unit_name) %>%
  left_join(mh_cases_by_unit, by = "mh_organisation_unit_name")

# plot the map with total_cases by facility

mh_map1 <-
  ggplot() +
  geom_sf(data = ug_dist, fill = "gray95", color = "blue") +
  geom_sf(data = uganda_mh_map[1:16,], color = "darkgreen", alpha = 0.7, size = 5) +
  #geom_sf(data = uganda_mh_map, color = "red", alpha = 0.6, size = 4, pch=3) +
  geom_sf_text(
    data = uganda_mh_map[1:16,],
    aes(label = total_cases),
    size = 3,
    nudge_y = -0.15,
    fontface = "bold",
    color = "black"
  ) +
  labs(
    #title = "Mental health patients by facility in Uganda",
    caption = "Source data: MHDP Uganda Dataset \n
    Co-ordinates data: data.humdata.org & GADM"
  ) +
  theme_dark() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  #add compass on the bottom right
  annotation_north_arrow(
    location = "br",
    which_north = "true", 
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"), 
    style = north_arrow_fancy_orienteering
  )
  
mh_map1

# Save the map as an image
ggsave(
  filename = "outputs/uganda_mh_map.png",
  plot = mh_map1,
  width = 12,
  height = 8,
  dpi = 300,
  bg ="transparent" 
)

# Option 2 , Include the names of the facilities on the map ------------------

uganda_mh_map <- uganda_mh_map %>%
  mutate(hospital = gsub("Regional Referral Hospital", "RRH", mh_organisation_unit_name),
         hospital = gsub("National Referral Hospital", "NRH", hospital)
         ) %>% 
  mutate(
    hospital = case_when(
      hospital == "China Uganda Friendship (Naguru) RRH" ~ "Naguru RRH",
      TRUE ~ hospital
    )
  )

ggplot() +
  geom_sf(data = ug_dist, fill = "gray95", color = "lightblue") +
  geom_sf(data = uganda_mh_map[1:16,], color = "darkgreen", alpha = 0.6, size = 5) +
  #geom_sf_text(
  #  data = uganda_mh_map,
  #  aes(label = hospital),
  #  size = 3,
  #  nudge_x = 0.5,
  #  fontface = "bold",
  #  color = "black"
  #) +
  geom_sf_text(
    data = uganda_mh_map[1:16,],
    aes(label = glue("{hospital}\n({total_cases})")),
    size = 3,
    nudge_y = -0.17,
    fontface = "bold",
    color = "black"
  ) +
  labs(
    title = "How many patient records per facility?",
    caption = "Source: OPD MH Dataset"
  ) +
  theme_dark() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )+
#add compass on the bottom right
annotation_north_arrow(
  location = "br",
  which_north = "true", 
  pad_x = unit(0.1, "in"),
  pad_y = unit(0.1, "in"), 
  style = north_arrow_fancy_orienteering
)


# Plot the map with bubble size relative to the patient numbers------------------
ggplot() +
  geom_sf(data = ug_dist, fill = "gray95", color = "lightblue") +
  geom_sf(data = uganda_mh_map, aes(size = total_cases), color = "red", alpha = 0.6) +
  scale_size_continuous(range = c(1, 10), name = "Total MH Cases") +
  labs(
    title = "Mental Health Cases by Facility in Uganda",
    caption = "Source: MH Dataset"
  ) +
  theme_gray() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Save the map as an image
tmap_save(tm = last_map(), filename = here::here("mh_cases_uganda_map.png"))

# PCN location maps

library("dplyr")
library("BSol.mapR")

source("~/Main work/MiscCode/postcode_functions/postcode_functions.R")

ward_localities <- read.csv("../data/ward_to_locality.csv") %>%
  mutate(Ward = AreaName) %>%
  select(Ward, Locality) 

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

gp_info <- read.csv(
  "../data/2024-07-15-BSol-GP-list.csv"
) %>%
  filter(
    Type == "Main"
  )

gp_pops <- read.csv(
  "../data/BSol-GP-pops-2024.csv"
) 
  
gp_data <- gp_info %>%
  left_join(
    gp_pops,
    by = join_by("Practice.Code")
  )

# Check if any GPs got missed
problem_GPs <- gp_data %>%
  filter(
    is.na(total_pop)
  )

if (nrow(problem_GPs) > 0) {
  error_msg <- paste(
    "Error: Some GPs not found:",
    problem_GPs$Practice.Code
  )
  stop(error_msg)
}

# Use API to get longitude and latitude of each GP
# ~~~~~~~~ DOESN'T WORK WITH NETMOTION ON ~~~~~~~~
gp_data <- gp_data %>%
  mutate(
    LONG := vapply(gp_data$Postcode, get_coord, coord = "long", FUN.VALUE = numeric(1)),
    LAT := vapply(gp_data$Postcode, get_coord, coord = "lat", FUN.VALUE = numeric(1))
    ) 

# Check that all coordinates were found correctly
if (any(is.na(gp_data$LAT)) | any(is.na(gp_data$LONG)) ) {
  stop("Error: Failed to get one or more GP coordinates.")
}

# Calculate weighted average PCN position
PCN_data <- gp_data %>% 
  group_by(PCN) %>%
  summarise(
    LONG = sum(LONG*total_pop)/sum(total_pop),
    LAT = sum(LAT*total_pop)/sum(total_pop),
    Locality = Mode(Locality)
  )

################################################################## 
#                 Plot PCNs across all of BSol                   #
##################################################################

map <- plot_empty_map(
) 


map <- add_points(
  map,
  PCN_data,
  color = "hotpink",
  size = 0.15
)

map

save_map(map, "../output/all-pcns.png")

##### Locality Zoom-in #####

palette <- ggpubr::get_palette((c("#FFFFFF", "lightblue")), 20)

for (Locality_i in names(locality_lookup)){
  print(Locality_i)
  save_name <- paste0(
    "../output/", 
    tolower(Locality_i),
    ".png"
    )
  
  # Filter PCNs for this locality
  pcn_locs_i <- PCN_data %>%
    filter(
      Locality == Locality_i
    )

  wards_i <- ward_localities %>%
    mutate(
      highlight = case_when(
        Locality == Locality_i ~ 1,
        TRUE ~ 0
      )
    )
  
  loc_map <- plot_map(
    wards_i,
    "highlight",
    "Ward",
    style = "fixed",
    breaks = c(0,0.5,1),
    labels = c("",Locality_i),
    palette = palette
  )
  
  loc_map <- add_points(
    loc_map,
    pcn_locs_i,
    color = "hotpink",
    size = 0.15
  )
  
  save_map(
    loc_map,
    save_name
  )
  
}

# PCN location maps
library("dplyr")
library("BSol.mapR")
library("tmap")
library("stringr")
library("grid")
library("gridExtra")
source("~/Main work/MiscCode/postcode_functions/postcode_functions.R")

fill_colour <- "#6699ff"

ward_localities <- read.csv("../data/ward-to-locality.csv") %>%
  mutate(
    Ward = AreaName,
    Ward_label = str_wrap(Ward, 14)
    ) %>%
  select(Ward, Locality, Ward_label) 

const_localities <- read.csv("../data/const-to-locality.csv")

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

palette <- ggpubr::get_palette((c("white", fill_colour)), 20)

for (Locality_i in unique(ward_localities$Locality)){
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
    ) %>%
    mutate(
      label = paste(Locality_i, "PCNs")
    )

  wards_i <- ward_localities %>%
    mutate(
      highlight = case_when(
        Locality == Locality_i ~ 1,
        TRUE ~ 0
      )
    )
  
  # Filter locality shape
  locality_shape_i <- subset(
    Locality,
    Locality == Locality_i
  ) 
  
  # Filter constituency shape
  const_shape_i <- subset(
    Constituency,
    Cnsttnc %in% const_localities$Constituency[
      const_localities$Locality == Locality_i
    ]
  )
  
  # Filter ward shape
  ward_shape_i <- subset(
    Ward,
    Ward %in% wards_i$Ward[wards_i$Locality == Locality_i]
  ) 
  ward_shape_i@data <- ward_shape_i@data %>%
    left_join(
      wards_i,
      by = join_by(Ward)
    )
  
  loc_map <- plot_empty_map(
    const_names = F
  ) +
    tm_shape(
      locality_shape_i
    ) + 
    tm_fill(
      col = fill_colour
    ) +
    tm_shape(
      ward_shape_i
    ) + 
    tm_borders(
      col = "grey80", lwd = 0.4
    ) +
    tm_shape(
      const_shape_i
    ) +
    tm_borders(
      col = "grey40", lwd = 1.5
    ) +
    tm_text(
      text = "Cnsttnc",
      size = 0.8
    )
  
  loc_map <- add_points(
    loc_map,
    pcn_locs_i,
    color = "hotpink",
    size = 0.2,
    alpha = 0.6
  ) +
    tm_layout(frame = FALSE,
              inner.margins = c(0.12,0,0.1,0.12),
              bg.color = "transparent",
              legend.position = c("LEFT", "TOP"),
              legend.text.size = 0.6,
              #title.position = c('LEFT', 'TOP'),
              legend.width = 2,
              legend.height = 3)
  


  
  zoom_map_i <- tm_shape(locality_shape_i) +
    # Invisible base layer to fix map zoom
    tm_borders(lwd = 0) +
    tm_fill(
      col = fill_colour
      ) +
    # Add ward borders
    tm_shape(ward_shape_i) +
    tm_borders(lwd = 0.7) +
    # Add thicker locality borders on top
    tm_shape(locality_shape_i) +
    tm_borders(lwd = 1.5, col = "grey40") +
    # Overlay ward names
    tm_shape(ward_shape_i) +
    tm_text(
      text = "Ward_label",
      size = 0.4
      ) +
    tm_layout(frame = FALSE,
              inner.margins = c(0.15,0.15,0.15,0.0),
              bg.color = "transparent",
              legend.position = c(0.2, 0.9),
              legend.text.size = 0.6,
              title.position = c('LEFT', 'TOP'),
              legend.width = 2,
              legend.height = 3) 
  
  zoom_map_i <- add_points(
    zoom_map_i, 
    pcn_locs_i,
    color = "label",
    palette = c("hotpink"),
    size = 0.2,
    alpha = 0.6
  ) +
    tm_layout(
      legend.title.color ="white"
      )
  
  

  
  combined_map <- cowplot::plot_grid(
    tmap_grob(zoom_map_i),
    tmap_grob(loc_map),
    ncol = 2
  )
  
  comb_title <- paste(
    Locality_i,
    case_when(
      Locality_i != "Solihull" ~ "Locality",
      TRUE ~ ""
    ),
    "PCNs, Wards, and Constituencies"
  )
  
  x.grob <- textGrob(comb_title, gp=gpar(fontsize=15))
  
  #add to plot
  combined_map <- grid.arrange(
    arrangeGrob(
      combined_map, 
      top = x.grob
      )
    )
  
  combined_map
  
  ggplot2::ggsave(combined_map, 
                  filename = paste("../output/png/",Locality_i,"_PCN_map.png",sep = ""),
                  width = 10, height = 5.5 ,bg = "white", dpi=900)
  
  ggplot2::ggsave(combined_map, 
                  filename = paste("../output/svg/",Locality_i,"_PCN_map.png",sep = ""),
                  width = 10, height = 5.5 ,bg = "white", dpi=900)
  
}

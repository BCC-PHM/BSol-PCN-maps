# PCN location maps

library("dplyr")
library("rgdal")
library("tmap")
source("~/Main work/MiscCode/postcode_functions/postcode_functions.R")

credits <- "Produced by Birmingham Public Health Knowledge Team (19th September 2023)
Contains OS data \u00A9 Crown copyright and database right 2020. Source:
Office for National Statistics licensed under the Open Government Licence v.3.0."


ward_localities <- readxl::read_excel("../data/BSol_ward_info.xlsx") %>%
  select(c("AreaCode", "Locality"))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Load and clean up GP-level data from Chung
gp_data <- read.csv("../data/gp-pop-and-pcn.csv", header=TRUE, 
                     stringsAsFactors=FALSE) %>%
  mutate(Postcode = Post.Code,
         `Registered Population` = Pop,
         `Practice Name` = PRAC.NAME,
         `Practice Code` = Area.Code) %>%
  arrange(PCN) %>%
  select(c(PCN, `Practice Code`, `Practice Name`, Postcode, 
           `Registered Population`,Locality)) %>%
  filter(!is.na(PCN))

# Use API to get longitude and latitude of each GP
# ~~~~~~~~ DOESN'T WORK WITH NETMOTION ON ~~~~~~~~
longitudes <- vapply(gp_data$Postcode, get_coord, coord = "long", FUN.VALUE = numeric(1))
latitudes <- vapply(gp_data$Postcode, get_coord, coord = "lat", FUN.VALUE = numeric(1))

gp_data <- gp_data %>%
  mutate(longitude := longitudes,
         latitude := latitudes) 
head(gp_data)


# Calculate weighted average PCN position
PCN_data <- gp_data %>% 
  group_by(PCN) %>%
  summarise(
    longitude = sum(longitude*`Registered Population`)/sum(`Registered Population`),
    latitude = sum(latitude*`Registered Population`)/sum(`Registered Population`),
    Locality = Mode(Locality)
  )
head(PCN_data)

# Load shape files
path = "//SVWCCG111/publichealth$/2.0 KNOWLEDGE EVIDENCE & GOVERNANCE - KEG/2.8 GIS RESOURCES/Birmingham Geometries/Shape Files/"

const_shape <- readOGR(
  paste(path, "constituencies", sep = ""),
  "constituencies",
  verbose = FALSE
)

ward_shape <- readOGR(
  paste(path, "wards", sep = ""),
  "wards",
  verbose = FALSE
)

const_shape@data$PCON13NM = gsub("Birmingham, ", "", const_shape@data$PCON13NM)

# Create new shape with defib points
pcn_locs <- SpatialPointsDataFrame(data.frame(PCN_data$longitude, PCN_data$latitude),
                                  data.frame(row.names=row.names(PCN_data$longitude),
                                             PCN=PCN_data$PCN,
                                             Locality=PCN_data$Locality),
                                  proj4string=CRS("+proj=longlat +datum=WGS84"))
pcn_locs <- spTransform(pcn_locs, "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs")


# Plot PCNs across all of Birmingham
BrumPCNs <- tm_shape(const_shape) +
  tm_borders(col = "grey30", lwd = 0.65) +
  tm_layout(legend.position = c("LEFT", "TOP"),
            legend.width = 0.5,
            legend.height = 0.5,
            legend.frame =FALSE,
            inner.margins = 0.1) +
  tm_shape(pcn_locs) +
  tm_dots(size = 0.15, col = "black") +
  tm_shape(pcn_locs) +
  tm_dots(size = 0.05, col = "lightblue") +
  tm_credits(credits, size = 0.8, position = c(0, 0))
BrumPCNs

##### Locality Zoom-in #####

localities = c(
  "East",
  "West", 
  "Central", 
  "North", 
  "South"
)
constituencies = list(
  "East"    = c("Hodge Hill", "Yardley"),
  "West"    = c("Ladywood",   "Perry Barr"),
  "Central" = c("Selly Oak",  "Hall Green"),
  "North"   = c("Erdington",  "Sutton Coldfield"),
  "South"   = c("Northfield", "Edgbaston")
)

for (locality in localities){
  print(locality)
  
  # Filter PCNs for this locality
  pcn_locs_i <- pcn_locs[pcn_locs@data$Locality == locality,]

  const_shape@data <- const_shape@data %>%
    mutate(
      `Area` = case_when(
        PCON13NM %in% constituencies[[locality]] ~ paste(locality, "Birmingham"),
        TRUE ~ ""
      )
    )
  
  main_map <- tm_shape(const_shape) +
    tm_fill(
      "Area",
      #style = "fixed",
      palette = c("#FFFFFF", "#D58CF7"),
      labels = c("", ""),
      legend.show = FALSE
    ) +
    tm_shape(ward_shape) +
    tm_borders(col = "grey80", lwd = 0.65) + 
    tm_shape(const_shape) +
    tm_borders(col = "grey30", lwd = 1.3) +
    tm_shape(const_shape) +
    tm_text("PCON13NM", size = 0.5)  + 

    tm_add_legend("line", 
                  col = c("grey80", "white","grey30"), 
                  lwd = c(0.65, 0,1.3), 
                  labels = c("Wards", "","Constituencies"),
                  size = 0.8) +
    tm_add_legend("fill", 
                  col = c("#D58CF7"), 
                  labels = c(paste(locality, "Birmingham")),
                  title = "",
                  size = 0.8) +
    tm_layout(frame = FALSE,
              bg.color = "transparent",
              inner.margins = c(0.12, 0.05, 0.05, 0.05),
              legend.position = c("LEFT", "TOP"), 
              title.position = c('LEFT', 'TOP'),
              legend.width = 1.5) + 
    tm_credits(credits, size = 0.3, position = c("LEFT", "bottom"))
  
  # Filter for zoom in
  const_shape_i <- const_shape[const_shape@data$PCON13NM %in% constituencies[[locality]],]
  
  ward_shape_i <- ward_shape
  ward_shape_i@data <- ward_shape_i@data %>%
    left_join(
      ward_localities,
      by = c("code" = "AreaCode")
    )
  ward_shape_i = ward_shape_i[ward_shape_i@data$Locality == locality,]
  
  ward_shape_i@data$ward_name = gsub("&", "&\n", ward_shape_i@data$ward_name)
  ward_shape_i@data$ward_name = gsub("Yardley East", "\n\n\nYardley\nEast", ward_shape_i@data$ward_name)
  ward_shape_i@data$ward_name = gsub("Garretts Green", "Garretts\nGreen", ward_shape_i@data$ward_name)

  
  
  zoom_map <- tm_shape(const_shape_i) +
    tm_borders(col = "grey30", lwd = 1.3) +
    tm_fill(
      "Area",
      palette = c("#D58CF7"),
      legend.show = FALSE
    ) +
    tm_shape(ward_shape_i) +
    tm_borders(col = "grey80", lwd = 0.65) + 
    tm_shape(const_shape_i) +
    tm_borders(col = "grey30", lwd = 1.3) + 
    tm_shape(ward_shape_i) +
    tm_text("ward_name", size = 0.4) +
    tm_shape(pcn_locs_i) +
    tm_dots(size = 0.15, col = "PCN") +
    tm_layout(frame = FALSE,
              inner.margins = c(0.05,0.1,0.2,0.1),
              bg.color = "transparent",
              legend.position = c("LEFT", "TOP"),
              legend.text.size = 0.6,
              title.position = c('LEFT', 'TOP'),
              legend.width = 2,
              legend.height = 3) +
    tm_compass(type = "8star", size = 4,
               position = c("RIGHT", "BOTTOM"))
  tmap_save(zoom_map, paste("../figures/", locality, "test.png", sep = ""))
  
  combined_map <- cowplot::plot_grid(
    tmap_grob(main_map),
    tmap_grob(zoom_map),
    ncol = 2,
    labels = "Birmingham Constituencies and Wards", 
    label_y = 1,
    label_x = -0.2
  )
  
  # ggplot2::ggsave(combined_map, 
  #                 filename = paste("../figures/",locality,"_PCN_map.svg",sep = ""),
  #                 width = 6, height = 4 ,bg = "white")
  ggplot2::ggsave(combined_map, 
                  filename = paste("../figures/",locality,"_PCN_map.png",sep = ""),
                  width = 6, height = 4 ,bg = "white", dpi=900)
  
}
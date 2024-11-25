library(dplyr)


################################################################## 
#          Download Population data from FingerTips              #
##################################################################

BSol_LA_codes = list(
  "Birmingham" = "E08000025",
  "Solihull" = "E08000029"
)

path_prefix <- "../data/tmp-"

for (LA in names(BSol_LA_codes)) {
  url_i <- paste0(
    "https://fingertips.phe.org.uk/api/population_age_distribution/csv?area_type_id=502&area_code=",
    BSol_LA_codes[[LA]]
  )
  
  tmp_file_i <- paste0(path_prefix, LA, ".csv")
  download.file(url_i, tmp_file_i)
}

################################################################## 
#               Combine Birmingham and Solihull                 #
##################################################################

# Create BSol GP population data (by age)
BSol_GP_age_pop <- do.call(
  rbind, 
  lapply(
    names(BSol_LA_codes), 
    function(region) {
      read.csv(paste0(path_prefix, region, ".csv"))
      }
    )
  )

# Delete temp files
invisible(
  lapply(paste0(path_prefix, names(BSol_LA_codes), ".csv"), file.remove)
)

################################################################## 
#                    Process and Save Data                       #
##################################################################

# Calculate total population for each GP
BSol_GP_total_pops <- BSol_GP_age_pop %>%
  mutate(
    Practice.Code = Area.Code,
    Practice.Name = Area.Name
  ) %>%
  group_by(
    Practice.Code, Practice.Name
  ) %>%
  summarise(
    total_pop = sum(Value)
  )

write.csv(
  BSol_GP_total_pops,
  "../data/BSol-GP-pops.xlsx"
  )
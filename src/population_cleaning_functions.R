load_population <- function(datadir, product, year) {
  library(tidyverse)

  # check that product is incorporated 
  product_list <- c("landscan")
  if (!(product %in% product_list)) {
    no_product_error <- paste0(
      "'", product, "' is not a viable option. Choose a product in: ",
      paste0(product_list, collapse = ", "), "."
    )
    stop(no_product_error)
  }
  
  # call product-specific load function 
  if (product == "landscan") {
    load_landscan_population(datadir, product, year)
  }
  
}

# Product-specific functions ----
load_landscan_population <- function(datadir, year) {
  
  # check that year has been downloaded 
  landscan_years <- list.files(
    paste0(datadir, "data/landscan/"), 
    recursive = T,
    full.names = T,
    pattern = "\\d+.tif$") %>%  
    str_extract("landscan-global-(\\d+)-assets", group = 1)
  
  if (!(year %in% landscan_years)) {
    no_year_error <- paste0(
      "'", year, "' is not a viable landscan year. Choose a year in: ",
      paste0(landscan_years, collapse = ", "), "."
    )
    stop(no_year_error)
  }
  
  terra::rast(
    str_c(
      datadir, "data/landscan-global-", year, "-assets/landscan-global-", year, ".tif"
    )
  )

}




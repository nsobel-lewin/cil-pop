# Population loading functions ----
load_population <- function(datadir, product, year) {
  
  # Convert year to character in case it has been supplied as an integer type
  year <- as.character(year)

  # check that product is incorporated 
  product_list <- str_extract(list.dirs(paste0(datadir, "data/products/"), recursive = F), "/(\\w+)$", group = 1)
  
  if (!(product %in% product_list)) {
    no_product_error <- paste0(
      "'", product, "' is not a viable option. Choose a product in: ",
      paste0(product_list, collapse = ", "), "."
    )
    stop(no_product_error)
  }
  
  # call product-specific load function 
  if (product == "landscan") {
    spat_rast <- load_landscan_population(datadir, year)
  }
  
  return(spat_rast)
}

## Product-specific loading functions ----
load_landscan_population <- function(datadir, year) {
  
  # check that year has been downloaded 
  landscan_years <- list.files(
    paste0(datadir, "data/products/landscan/"), 
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

  # Load raster data
  spat_rast <- terra::rast(
    str_c(
      datadir, "data/products/landscan/landscan-global-", year, 
      "-assets/landscan-global-", year, "-lzw.tif"
    )
  )
  
  return(spat_rast)

}

# Aggregation and scaling IR-level populations functions ----
aggregate_pop_to_ir <- function(datadir, product, year) {
  
  shps <- load_shapefile(datadir)
  
  gridded_pops <- load_population(datadir, product, year)

  # Convert polygon CRS to raster CRS
  sf::st_crs(shps) <- sf::st_crs(gridded_pops)
  
  # Compute population in each polygon
  aggregated_pop <- 
    bind_cols(
      # Product identifiers
      year = year,
      product = product,
      rescaled = F,
      
      # IR identifiers
      sf::st_drop_geometry(shps),

      # Population within each IR
      pop = 
        exactextractr::exact_extract(
          x = gridded_pops, 
          y = shps, 
          fun = "sum",
          progress = F
      )
    )
    
  return(aggregated_pop)
  
}
  
scale_ir_pop <- function(datadir, ir_pop, skip_missing_data_check = F) {
  
  year <- ir_pop$year[1]
  un_pop <- load_un_population(datadir, year) %>% 
    rename(un_pop = pop) %>% 
    select(ISO, un_pop)
  
  scale_factor <- ir_pop %>% 
    select(ISO, pop) %>% 
    summarize(ir_pop = sum(pop), .by = ISO) %>% 
    left_join(un_pop, by = "ISO") %>% 
    # UN Pop data is missing for a few small islands, so set scale to 1
    mutate(scale_factor = if_else(is.na(un_pop), 1, un_pop / ir_pop))
  
  # Test that countries missing UN population data are small
  if (!skip_missing_data_check) {
    if (max(scale_factor$ir_pop[is.na(scale_factor$un_pop)]) > 1e6) {
      stop("A country with more than 1m people is missing UN population data.\nTo skip this test set `skip_missing_data_check = T`")
    }
  }

  ir_pop %>% 
    left_join(scale_factor, by = "ISO") %>% 
    mutate(pop = pop * scale_factor, rescaled = T, .keep = "unused") %>% 
    select(-c(ir_pop, un_pop)) %>% 
    return()
  
}

## Other loading functions ----
load_shapefile <- function(datadir) {
  
  shp_file_path <- str_c(datadir, "data/shapefiles/agglomerated-world-new.shp")
  shp <- sf::st_read(shp_file_path, quiet = T) %>% 
    select(gadmid, hierid, ISO)
  
  # Fix bounding box
  attr(sf::st_geometry(shp), "bbox") <- 
    structure(
      c(-180, -90, 180, 90),
      names = c("xmin", "ymin", "xmax", "ymax"),
      class = "bbox")
  
  sf::st_crs(shp) <- 4326
  
  return(shp)
}

load_un_population <- function(datadir, year, Variant = NULL) {
  
  un_file_path <- str_c(datadir, "data/UN/WPP2024_TotalPopulationBySex.csv")
  
  un <- read_csv(un_file_path, col_select = c(ISO3_code, Time, Variant, PopTotal),
                 show_col_types = F) %>% 
    rename(ISO = ISO3_code, pop = PopTotal, year = Time) %>% 
    # Variant is used for projections
    filter(!is.na(ISO) & year == !!year) %>%
    {if (!is.null(Variant)) filter(., Variant == !!Variant) else .} %>% 
    select(ISO, year, pop) %>% 
    mutate(
      # Convert pop from 1,000s to 1s 
      pop = pop * 1e3,
      # Convert Kosovo ISO to adhere to CIL standard
      ISO = if_else(ISO == "XKX", "KO-", ISO))
  
  duplicated_iso_error <- str_c(
    "ISOs are duplicated for ", year, 
    ". The UN population counts are likely projections. 
    Use the 'Variant' argument to choose a projection."
  )
  
  if(any(duplicated(un$ISO))) stop(duplicated_iso_error)
  
  return(un)
}
  

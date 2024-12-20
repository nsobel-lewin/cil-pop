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
  
# Projection functions ----
create_ir_ssp_projections <- function(datadir, product, year) {
  
  browser
  ir_level_pop <- read_csv(str_c(datadir, "processed/ir_population_data.csv"),
                           show_col_types = F) %>% 
    filter(product == !!product & year == !!year & rescaled_to_sum_to_un_pop == T) %>% 
    select(gadmid, hierid, ISO, pop)
  
  ssp_projections <- readxl::read_xlsx(
    str_c(datadir, "data/SSP/SSP_data_2023.xlsx"),
    guess_max = 10e3) %>% 
    select(Region, Scenario, matches("^\\d+")) %>% 
    filter(!is.na(Region) & !is.na(Scenario)) %>% 
    pivot_longer(
      matches("^\\d+"), 
      names_to = "year", 
      names_transform = as.integer,
      values_to = "pop"
    ) %>% 
    mutate(
      # Fill in years prior to 2022 with historical data 
      pop = if_else(year < 2020, max(pop[Scenario == "Historical Reference"]), pop),
      .by = c(Region, year)
    ) %>% 
    filter(str_detect(Scenario, "SSP\\d")) %>% 
    mutate(
      Region = case_when(
        Region == "Micronesia" ~ "Micronesia (Federated States of)",
        Region == "Cura?ao" ~ "Curaçao",
        Region == "R?union" ~ "Réunion",
        T ~ Region
      ),
      ISO = countrycode::countrycode(
        Region, origin = "country.name", destination = "iso3c")
    ) %>% 
    rename(country = Region, SSP = Scenario) %>% 
    select(ISO, country, SSP, year, pop) %>% 
    # Convert population to millions
    mutate(pop = pop * 1e6) %>% 
    # Add non-decadal years
    bind_rows(
      select(., c(ISO, country, SSP)) %>% 
        unique() %>% 
        expand(year = setdiff(2010:2100, seq(2010, 2100, 5)), nesting(ISO, country, SSP))
    ) %>% 
    arrange(ISO, country, SSP, year) %>% 
    # Interpolate population
    mutate(pop = approx(year, pop, year)$y, .by = c(ISO, country, SSP)) %>% 
    # Compute SSP rescale factor
    nest(.by = c(ISO, country, SSP)) %>% 
    mutate(data = map(data, compute_rescale_factor, year = !!year)) %>% 
    unnest(everything()) %>% 
    # Create a CONSTANT country
    bind_rows(
      ., 
      mutate(., country = "CONSTANT", ISO = "CONSTANT", rescale_factor = 1) %>% 
        unique()
    )

  # TODO: NOT ALL ISOs have corresponding SSP data! 
  # For now inner join, but need to resolve this
  output_data <- ir_level_pop %>% 
    # Some ISOs are not included in the SSP database, 
    # compute projections based on nearby countries
    mutate(
      ISO = case_when(
        ISO == "AIA" ~ "ATG", # Anguilla (Use Antigua and Barbuda)
        ISO == "ALA" ~ "FIN", # Åland Islands (Use Finland)
        ISO == "AND" ~ "ESP", # Andorra (Use Spain)
        ISO == "ASM" ~ "WSM", # American Samoa (Use Samoa)
        ISO == "ATA" ~ "CONSTANT", # Antarctica (Use CONSTANT)
        ISO == "ATF" ~ "CONSTANT", # French Southern Territories (Use CONSTANT)
        ISO == "BES" ~ "CUW", # Caribbean Netherlands (Use Curacao)
        ISO == "BLM" ~ "ATG", # St. Barthélemy (Use Antigua and Barbuda)
        ISO == "BMU" ~ "VIR", # Bermuda (Use U.S. Virgin Islands)
        ISO == "BVT" ~ "CONSTANT", # Bouvet Island (Use CONSTANT)
        ISO == "CA-" ~ "CONSTANT", # Caspian Sea (Use CONSTANT)
        ISO == "CCK" ~ "AUS", # Cocos (Keeling) Islands (Use Australia)
        ISO == "CL-" ~ "CONSTANT", # Clipperton Island (Use CONSTANT)
        ISO == "COK" ~ "PYF", # Cook Islands (Use French Polynesia)
        ISO == "CXR" ~ "AUS", # Christmas Island (Use Australia)
        ISO == "CYM" ~ "CUB", # Cayman Islands (Use Cuba)
        ISO == "DMA" ~ "MTQ", # Dominica (Use Martinique)
        ISO == "FLK" ~ "GBR", # Falkland Islands (Use Britain)
        ISO == "FRO" ~ "DNK", # Faroe Islands (Use Denmark)
        ISO == "GGY" ~ "GBR", # Guernsey (Use Britain)
        ISO == "GIB" ~ "ESP", # Gibraltar (Use Spain)
        ISO == "GRL" ~ "DNK", # Greenland (Use Denmark)
        ISO == "HMD" ~ "CONSTANT", # Heard & McDonald Islands (Use CONSTANT)
        ISO == "IMN" ~ "GBR", # Isle of Man (Use Britain)
        ISO == "IOT" ~ "CONSTANT", # British Indian Ocean Territory (Use CONSTANT)
        ISO == "JEY" ~ "GBR", # Jersey (Use Britain)
        ISO == "KNA" ~ "ATG", # St. Kitts & Nevis (Use Antigua and Barbuda)
        ISO == "KO-" ~ "CONSTANT", # Kosovo (Use CONSTANT)
        ISO == "LIE" ~ "CHE", # Liechtenstein (Use Switzerland)
        ISO == "MAF" ~ "ATG", # Saint Martin (French part) (Use Antigua and Barbuda)
        ISO == "MCO" ~ "FRA", # Monaco (Use France)
        ISO == "MHL" ~ "CONSTANT", # Marshall Islands (Use CONSTANT)
        ISO == "MNP" ~ "GUM", # Northern Mariana Islands (Use Guam)
        ISO == "MSR" ~ "ATG", # Montserrat (Use Antigua and Barbuda)
        ISO == "NFK" ~ "AUS", # Norfolk Island (Use Australia)
        ISO == "NIU" ~ "WSM", # Niue (Use Samoa)
        ISO == "NRU" ~ "CONSTANT", # Nauru (Use CONSTANT)
        ISO == "PCN" ~ "CONSTANT", # Pitcairn Islands (Use CONSTANT)
        ISO == "PLW" ~ "CONSTANT", # Palau (Use CONSTANT)
        ISO == "SGS" ~ "CONSTANT", # South Georgia & South Sandwich Islands (Use CONSTANT)
        ISO == "SHN" ~ "CONSTANT", # St. Helena (Use CONSTANT)
        ISO == "SJM" ~ "NOR", # Svalbard & Jan Mayen (Use Norway)
        ISO == "SMR" ~ "ITA", # San Marino (Use Italy)
        ISO == "SMX" ~ "ATG", # Sint Maarten (Use Antigua and Barbuda)
        ISO == "SP-" ~ "CONSTANT", # Spratly islands (Use CONSTANT)
        ISO == "SPM" ~ "CAN", # St. Pierre & Miquelon (Use Canada)
        ISO == "TCA" ~ "BHS", # Turks & Caicos Islands (Use Bahamas)
        ISO == "TKL" ~ "WSM", # Tokelau (Use Samoa)
        ISO == "TUV" ~ "WSM", # Tuvalu (Use Samoa)
        ISO == "UMI" ~ "CONSTANT", # United States Minor Outlying Islands (the) (Use CONSTANT)
        ISO == "VAT" ~ "ITA", # Vatican City (Use Italy)
        ISO == "VGB" ~ "VIR", # British Virgin Islands (Use US Virgin islands)
        ISO == "WLF" ~ "WSM", # Wallis & Futuna (Use Samoa)
      )
    )
    
    
    left_join(
    ir_level_pop, ssp_projections, by = "ISO", relationship = "many-to-many") %>% 
    select(-ISO) %>% 
    mutate(pop = pop * rescale_factor, .keep = "unused") %>%
    select(gadmid, hierid, SSP, year, pop) %>% 
    nest(.by = c(SSP)) 
  
  output_paths <- output_data %>% 
    mutate(path = map2(data, SSP, ~ save_ir_pop(.x, .y, product, year, datadir))) %>% 
    pull(path)
  
  return(output_paths)
  
}

compute_rescale_factor <- function(df, year) {
  base_value <- df$pop[df$year == year]
  df %>% 
    mutate(rescale_factor = pop / base_value, .keep = "unused") %>% 
    return()
}

save_ir_pop <- function(df, SSP, product, year, datadir) {
  output_dir <- str_c(datadir, "processed", SSP, 
                       product, year, "", sep = "/")
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = T)
  
  output_path <- str_c(output_dir, "ir_pop.csv")
  
  write_csv(df, output_path)
  return(output_path)
}


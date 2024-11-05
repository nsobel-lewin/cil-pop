# Plotting functions ----
make_pop_diff_density_plot <- function(datadir, product, year) {
  
  ir_pop <- read_csv(str_c(datadir, "processed/ir_population_data.csv"),
                     show_col_types = F) %>% 
    filter(year == !!year & source == product) 
  
  plot_data <- ir_pop %>% 
    pivot_wider(values_from = pop, names_from = rescaled_to_sum_to_un_pop, 
                names_prefix = "rescaled_") %>% 
    mutate(diff = rescaled_TRUE - rescaled_FALSE) %>% 
    select(diff)
  
  plot <- ggplot(plot_data, aes(x = diff)) +
    geom_density() +
    scale_x_continuous(
      "Scaled pop. - unscaled pop.",
      limits = c(-1e5, 1e5),
      breaks = c(-1e5, -5e4, 0, 5e4, 1e5),
      labels = c("<-100,000", "50,000", "0", "50,000", ">100,000"),
      oob = scales::squish
    ) +
    scale_y_continuous(
      expand = c(5e-6, 5e-6)
    ) +
    labs(title = str_c(str_to_sentence(product), " - ", year)) +
    theme_classic() +
    theme(axis.title.y = element_blank())

  
  return(plot)
}


make_rescale_factor_density_plot <- function(datadir, product, year) {
  
  ir_pop <- read_csv(str_c(datadir, "processed/ir_population_data.csv"),
                     show_col_types = F) %>% 
    filter(year == !!year & source == product) 
  
  plot_data <- ir_pop %>% 
    pivot_wider(values_from = pop, names_from = rescaled_to_sum_to_un_pop, 
                names_prefix = "rescaled_") %>% 
    mutate(rescale_factor = rescaled_TRUE / rescaled_FALSE) %>% 
    select(ISO, rescale_factor) %>% 
    unique()
  
  plot <- ggplot(plot_data, aes(x = rescale_factor)) +
    geom_density() +
    scale_x_continuous(
      "Rescale factor",
      breaks = seq(0.8, 1.6, 0.2),
      labels = c("<0.8", "1.0", "1.2", "1.4", ">1.6"),
      limits = c(0.8, 1.6),
      oob = scales::squish
    ) +
    labs(title = str_c(str_to_sentence(product), " - ", year)) +
    theme_classic() +
    theme(axis.title.y = element_blank())
  
  return(plot)
}


make_pop_plot <- function(datadir, simplified_shp, product, year, rescaled) {

  ir_pop <- read_csv(str_c(datadir, "processed/ir_population_data.csv"),
                     show_col_types = F) %>% 
    filter(year == !!year & source == product & rescaled_to_sum_to_un_pop == rescaled) %>% 
    select(gadmid, pop)
  
  plot_data <- simplified_shp %>% 
    full_join(ir_pop, by = "gadmid") %>% 
    # Drop Antarctica
    filter(gadmid != 2836) %>% 
    select(-gadmid) %>% 
    mutate(pop = if_else(pop == 0, -1, log10(pop)))
  
  plot <- ggplot(plot_data, aes(fill = pop)) +
    geom_sf(linewidth = 0) +
    scale_fill_viridis_c(
      begin = 0.05, 
      end = 0.98,
      breaks = c(3, 4, 5, 6),
      labels = c("<1,000", "10,000", "100,000", ">1,000,000"),
      limits = c(3, 6),
      oob = scales::squish) +
    labs(title = str_c(str_to_sentence(product), " - ", year, 
                       if_else(rescaled, " - rescaled", ""))) +
    theme_void() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.key.width = unit(2.5, "cm"),
          legend.position = "bottom") 
  
  return(plot)
}

# Helper functions ----
load_simplified_shapefile <- function(datadir) {
  
  shp <- load_shapefile(datadir) %>% 
    select(gadmid, geometry)
  
  sf::sf_use_s2(FALSE)
  simplified_shp <- sf::st_simplify(shp, dTolerance = 0.05)
  
  return(simplified_shp)
  
}



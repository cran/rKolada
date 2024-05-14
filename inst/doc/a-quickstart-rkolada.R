## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library("rKolada")

## ----echo = FALSE-------------------------------------------------------------
kpis <- rKolada:::kpi_df
munic <- rKolada:::munic

## ----eval = FALSE-------------------------------------------------------------
#  kpis <- get_kpi(cache = FALSE)
#  munic <- get_municipality(cache = FALSE)

## -----------------------------------------------------------------------------
dplyr::glimpse(kpis)

## -----------------------------------------------------------------------------
# Get a list KPIs matching a search for "BRP" (Gross regional product)
kpi_res <- kpis %>%
  kpi_search("BRP") %>%
  # Keep only KPIs with data for the municipality level
  kpi_search("K", column = "municipality_type") %>%
  kpi_minimize(remove_undocumented_columns = TRUE, remove_monotonous_data = TRUE)

dplyr::glimpse(kpi_res)

## -----------------------------------------------------------------------------
munic_res <- munic %>% 
  # Only keep municipalities (drop regions)
  municipality_search("K", column = "type") %>% 
  # Only keep Stockholm, Gothenburg and Malmö
  municipality_search(c("Stockholm", "Göteborg", "Malmö"))

dplyr::glimpse(munic_res)

## ----echo = TRUE, results='asis'----------------------------------------------
kpi_res %>%
  kpi_bind_keywords(n = 4) %>% 
  kpi_describe(max_n = 1, format = "md", heading_level = 4, sub_heading_level = 5)

## ----echo = FALSE-------------------------------------------------------------
kld_data <- rKolada:::kld_data

## ----eval = FALSE-------------------------------------------------------------
#  kld_data <- get_values(
#    kpi = kpi_extract_ids(kpi_res),
#    municipality = municipality_extract_ids(munic_res),
#    period = 1990:2019,
#    simplify = TRUE
#  )

## -----------------------------------------------------------------------------
# Visualise results
library("ggplot2")

ggplot(kld_data, aes(x = year, y = value)) +
  geom_line(aes(color = municipality)) +
  facet_grid(kpi ~ .) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Gross Regional Product",
    subtitle = "Yearly development in Sweden's three\nmost populous municipalities",
    x = "Year",
    y = "",
    caption = values_legend(kld_data, kpis)
  )


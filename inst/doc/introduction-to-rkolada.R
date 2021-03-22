## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("tidyverse")
#  install.packages("rKolada")

## ----setup--------------------------------------------------------------------
library("rKolada")

## -----------------------------------------------------------------------------
n00945 <- get_values(
  kpi = "N00945",
  municipality = c("0180", "1480", "1280"),
  period = 1970:2020
)

n00945

## -----------------------------------------------------------------------------
# Download all KPI metadata as a tibble (kpi_df)
kpi_df <- get_kpi()

kpi_df

## -----------------------------------------------------------------------------
# Search for KPIs with the term "BRP" in their description or title
kpi_filter <- kpi_df %>% kpi_search("skola", column = c("description", "title"))
kpi_filter

# Search for municipality groups containing the name "Arboga"
munic_g <- get_municipality_groups()
arboga_groups <- munic_g %>% municipality_grp_search("Arboga")
arboga_groups

## ---- results='asis'----------------------------------------------------------
kpi_filter %>% kpi_describe(max_n = 2, format = "md", heading_level = 4)

## -----------------------------------------------------------------------------
# Add keywords to a KPI table
kpis_with_keywords <- kpi_filter %>% kpi_bind_keywords(n = 4)

# count keywords
kpis_with_keywords %>%
  tidyr::pivot_longer(dplyr::starts_with("keyword"), values_to = "keyword") %>%
  dplyr::count(keyword, sort = TRUE)

## -----------------------------------------------------------------------------
# Top 10 rows of the table
kpi_filter %>% dplyr::slice(1:10)

# Top 10 rows of the table, with non-distinct data removed
kpi_filter %>% dplyr::slice(1:10) %>% kpi_minimize()

## -----------------------------------------------------------------------------
# Get KPIs describing Gross Regional Product of municipalities
kpi_filter <- get_kpi() %>% 
  kpi_search("BRP")
# Creates a table with two rows

# Get a suitable group of municipalities
munic_grp_filter <- get_municipality_groups() %>% 
  municipality_grp_search("Liknande kommuner socioekonomi, Arboga")
# Creates a table with one group of 7 municipalities

# Also include Arboga itself
arboga <- get_municipality() %>% municipality_search("Arboga")

# Get data
grp_data <- get_values(
  kpi = kpi_extract_ids(kpi_filter),
  municipality = c(
    municipality_grp_extract_ids(munic_grp_filter),
    municipality_extract_ids(arboga)
  )
)

# Visualize results
library("ggplot2")
ggplot(grp_data, aes(year, value, color = municipality)) +
  geom_line(aes(linetype = municipality)) +
  facet_grid(kpi ~ ., scales = "free") +
  labs(
    title = "Gross Regional Product per capita 2012-2018",
    subtitle = "Swedish municipalities similar to Arboga",
    caption = values_legend(grp_data, kpi_filter)
  ) +
  scale_color_viridis_d(option = "B") +
  scale_y_continuous(labels = scales::comma)


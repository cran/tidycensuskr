## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(tidycensuskr)
library(spdep)

## ----load-data_1--------------------------------------------------------------

# Load 2020 boundaries
data(adm2_sf_2020)

# Load 2020 economy data
df_2020_economy <- anycensus(year = 2020,
                             type = "economy")

# Merge with spatial data
adm2_sf_2020_economy <- adm2_sf_2020 |>
  dplyr::inner_join(df_2020_economy, by = "adm2_code")

# Variable of interest: number of companies
var <- adm2_sf_2020_economy$company_total_cnt

## ----global_moran_1-----------------------------------------------------------
# Build neighbors (queen contiguity) and spatial weights
nb <- poly2nb(adm2_sf_2020_economy, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Global Moran's I test
global_moran <- moran.test(var, lw, zero.policy = TRUE)
global_moran

## ----local_moran_1, , fig.width=7, fig.height=5-------------------------------
# Local Moran's I
local_moran <- localmoran(var, lw, zero.policy = TRUE)

# Bind results back to sf object
adm2_sf_2020_economy <- adm2_sf_2020_economy |>
  mutate(
    Ii   = local_moran[, "Ii"],
    pval = local_moran[, "Pr(z != E(Ii))"]
  )

mean_var <- mean(var, na.rm = TRUE)

adm2_sf_2020_economy <- adm2_sf_2020_economy |>
  mutate(
    cluster = case_when(
      var > mean_var & Ii > 0 & pval <= 0.05 ~ "High-High",
      var < mean_var & Ii > 0 & pval <= 0.05 ~ "Low-Low",
      var > mean_var & Ii < 0 & pval <= 0.05 ~ "High-Low",
      var < mean_var & Ii < 0 & pval <= 0.05 ~ "Low-High",
      TRUE ~ "Not significant"
    )
  )

ggplot(adm2_sf_2020_economy) +
  geom_sf(aes(fill = cluster), color = "grey70", size = 0.05) +
  scale_fill_manual(
    values = c(
      "High-High"     = "red",
      "Low-Low"       = "blue",
      "High-Low"      = "pink",
      "Low-High"      = "lightblue",
      "Not significant" = "white"
    )
  ) +
  labs(title = "LISA Cluster Map of Company units (2020)") +
  theme_minimal()

## ----pop-change-1, fig.width = 8.2, fig.height = 10---------------------------
# load packages
library(geofacet)

# load bundled data in tidycensuskr
data(censuskor)
data(adm2_sf_2020)
data(kr_grid_adm2_sgis_2020)

# prepare geofacet grid data
# Use the newest adm2_code and name if one got its name changed or promoted
pop <- censuskor |>
  dplyr::filter(
    type == "population" & class1 == "all households"
  ) |>
  dplyr::rename(code = adm2_code) |>
  dplyr::filter(class1 == "all households", class2 != "total") |>
  dplyr::mutate(
    value = value / 1000,
    code = dplyr::case_when(
      # Michuhol-gu (i.e., 23030 to 23090)
      code == 23030 ~ 23090,
      # Yeoju-si
      code == 31320 ~ 31280,
      # Dangjin-si
      code == 34390 ~ 34080,
      TRUE ~ code
    )
  ) |>
  dplyr::arrange(code, class2, -year) |>
  dplyr::group_by(code) |>
  dplyr::mutate(adm2 = adm2[which.max(year)]) |>
  dplyr::ungroup()

head(pop)

## ----pop-change-2, fig.width = 8.2, fig.height = 10---------------------------
# for a geofacet plot
# map codes to district names for facet labels
pop_name_map <- pop %>%
  dplyr::distinct(code, adm2) %>%
  { setNames(.$adm2, .$code) }

pop_labels <- pop %>% dplyr::distinct(code, adm2)

# Adjust for the identical district names in different provinces
kr_grid_adm2_sgis_2020 <-
  kr_grid_adm2_sgis_2020 |>
  dplyr::mutate(
    name = dplyr::case_when(
      grepl("^32", code) & name == "Goseong-gun" ~ "Goseong-gun (GW)",
      grepl("^38", code) & name == "Goseong-gun" ~ "Goseong-gun (GN)",
      grepl("^11", code) & name == "Jung-gu" ~ "Jung-gu (SE)",
      grepl("^21", code) & name == "Jung-gu" ~ "Jung-gu (BU)",
      grepl("^22", code) & name == "Jung-gu" ~ "Jung-gu (DG)",
      grepl("^23", code) & name == "Jung-gu" ~ "Jung-gu (IC)",
      grepl("^25", code) & name == "Jung-gu" ~ "Jung-gu (DJ)",
      grepl("^26", code) & name == "Jung-gu" ~ "Jung-gu (UL)",
      grepl("^21", code) & name == "Seo-gu" ~ "Seo-gu (BU)",
      grepl("^22", code) & name == "Seo-gu" ~ "Seo-gu (DG)",
      grepl("^23", code) & name == "Seo-gu" ~ "Seo-gu (IC)",
      grepl("^24", code) & name == "Seo-gu" ~ "Seo-gu (GJ)",
      grepl("^25", code) & name == "Seo-gu" ~ "Seo-gu (DJ)",
      grepl("^26", code) & name == "Seo-gu" ~ "Seo-gu (UL)",
      grepl("^21", code) & name == "Nam-gu" ~ "Nam-gu (BU)",
      grepl("^22", code) & name == "Nam-gu" ~ "Nam-gu (DG)",
      grepl("^24", code) & name == "Nam-gu" ~ "Nam-gu (GJ)",
      grepl("^26", code) & name == "Nam-gu" ~ "Nam-gu (UL)",
      grepl("^21", code) & name == "Dong-gu" ~ "Dong-gu (BU)",
      grepl("^22", code) & name == "Dong-gu" ~ "Dong-gu (DG)",
      grepl("^23", code) & name == "Dong-gu" ~ "Dong-gu (IC)",
      grepl("^24", code) & name == "Dong-gu" ~ "Dong-gu (GJ)",
      grepl("^25", code) & name == "Dong-gu" ~ "Dong-gu (DJ)",
      grepl("^26", code) & name == "Dong-gu" ~ "Dong-gu (UL)",
      grepl("^21", code) & name == "Buk-gu" ~ "Buk-gu (BU)",
      grepl("^22", code) & name == "Buk-gu" ~ "Buk-gu (DG)",
      grepl("^24", code) & name == "Buk-gu" ~ "Buk-gu (GJ)",
      grepl("^26", code) & name == "Buk-gu" ~ "Buk-gu (UL)",
      grepl("^11", code) & name == "Gangseo-gu" ~ "Gangseo-gu (SE)",
      grepl("^21", code) & name == "Gangseo-gu" ~ "Gangseo-gu (BU)",
      TRUE ~ name
    )
  )

## ----pop-change-3, fig.width = 8.2, fig.height = 10---------------------------

ggplot(data = pop) +
  geom_line(
    aes(x = year, y = value, group = interaction(adm2, class2), color = class2),
    alpha = 0.5,
    linewidth = 1.5
  ) +
  facet_geo(~ code, grid = kr_grid_adm2_sgis_2020, label = "name", scale = "free_y") +
  labs(
    title = "Population Trends in South Korea by Sex and District",
    x = "Year",
    y = "",
    color = "Population Class",
    caption = "Y-axis values are not commensurate with the original scale"
  ) +
  scale_color_manual(values = c(female = "#F44336", male = "#2196F3")) +
  theme_void() +
  scale_x_continuous(
    breaks = sort(unique(pop$year)),
    labels = function(x) sprintf("%d", as.integer(x))
  ) +
  theme(
    strip.text = element_text(
      size = 5,
      margin = margin(0.05, 0.05, 0.05, 0.05, "cm")
    ),
    strip.background = element_blank(),
    axis.text.x = element_text(size = 6, angle = 90, hjust = 1),
    axis.text.y = element_blank(),
    panel.spacing = grid::unit(1, "pt"),
    plot.margin = margin(1, 1, 1, 1, "mm")
  )



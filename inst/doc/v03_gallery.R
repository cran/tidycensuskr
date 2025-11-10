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


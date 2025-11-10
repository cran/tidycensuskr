## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)
library(tidycensuskr)
library(biscale)
library(cowplot)
library(grid)
library(ggspatial)

## ----load-data_1--------------------------------------------------------------

# Load 2020 boundaries
data(adm2_sf_2020)


## ----filter-merge_1-----------------------------------------------------------
# Load census population data for 2010 and 2020
df_2010_pop <- anycensus(year = 2010, 
                         codes = c("Gyeongsangnam-do", "Busan", "Ulsan"), 
                         type = "population")
df_2020_pop <- anycensus(year = 2020, 
                         codes = c("Gyeongsangnam-do", "Busan", "Ulsan"), 
                         type = "population")

# Merge with spatial data and compute population change
sf_target <- adm2_sf_2020 |>
  inner_join(df_2010_pop, by = "adm2_code") |>
  inner_join(df_2020_pop, by = "adm2_code") |>
  mutate(change = `all households_total_prs.y` - `all households_total_prs.x`)


## ----choropleth_1, fig.width=7, fig.height=5----------------------------------
# Choropleth map for population change
map <- ggplot(sf_target) +
  geom_sf(aes(fill = change), color = "gray80", size = 0.1) +
  labs(title = "Population change between 2010 and 2020") +
  scale_fill_gradient2(
    low = "#2C7BB6", mid = "white", high = "#D7191C",
    midpoint = 0,                 
    name = "Change"
  ) +
  theme_void() +
  annotation_scale(location = "tr", width_hint = 0.25, text_cex = 0.7, line_width = 0.7)

# National boundary (union of all districts)
sf_korea_boundary <- adm2_sf_2020 |>
  summarise(geometry = st_union(geometry))

# Target region boundary (union of selected provinces/cities)
sf_target_boundary <- sf_target |>
  summarise(geometry = st_union(geometry))

# Inset map: whole Korea + highlighted target region
korea_inset <- ggplot() +
  geom_sf(data = sf_korea_boundary, fill = "grey90", color = "grey90") +
  geom_sf(data = sf_target_boundary, fill = "grey30", color = "grey30") +
  theme_void()

# Combine main map and inset
cowplot::ggdraw() +
  cowplot::draw_plot(map) +
  cowplot::draw_plot(korea_inset, x = 0.7, y = 0.05, width = 0.25, height = 0.25) +
  draw_grob(grid::rectGrob(gp = gpar(col = "black", fill = NA, lwd = 0.6)),
            x = 0.7, y = 0.05, width = 0.25, height = 0.25)

## ----filter-merge_2-----------------------------------------------------------
# Load census data
df_2020_pop <- anycensus(year = 2020, 
                         type = "population")
df_2020_tax <- anycensus(year = 2020, 
                         type = "tax")

# Merge population with boundaries
adm2_sf_2020_pop <- adm2_sf_2020 |>
  left_join(df_2020_pop, by = "adm2_code") |>
  mutate(
    adm2_code_chr = as.character(adm2_code),
    adm2_prefix4  = substr(adm2_code_chr, 1, 4),
    last_digit    = substr(adm2_code_chr, 5, 5)
  )

# Aggregate smaller units (adm2_code ending not with 0) into 4-digit groups
sf_union_needed <- adm2_sf_2020_pop |>
  filter(last_digit != "0") |>
  group_by(adm2_prefix4) |>
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    geometry = st_union(geometry),
    .groups = "drop"
  ) |>
  mutate(adm2_code = as.numeric(paste0(adm2_prefix4, "0")))

# Combine aggregated units with existing "0"-ending districts
adm2_sf_2020_unioned <- adm2_sf_2020_pop |>
  filter(last_digit == "0") |>
  bind_rows(sf_union_needed)

# Join with tax data
sf_final <- adm2_sf_2020_unioned |>
  left_join(df_2020_tax, by = "adm2_code")


## ----bivariate_2, fig.width=7, fig.height=5-----------------------------------
# Create 3x3 bivariate classes (population vs tax)
bi_data <- bi_class(
  sf_final,
  x = `all households_total_prs`,
  y = income_general_mkr,
  style = "quantile",
  dim = 3
)

# Bivariate legend
legend <- bi_legend(
  pal = "DkCyan", dim = 3,
  xlab = "Low to High population",
  ylab = "Low to High tax",
  size = 7
)

# Mapping
cowplot::ggdraw() +
  cowplot::draw_plot(
    ggplot() +
      geom_sf(data = bi_data, aes(fill = bi_class), color = NA) +
      bi_scale_fill(pal = "DkCyan", dim = 3, guide = "none") +
      bi_theme() +
      labs(title = "Population vs Tax (2020)") +
      theme(plot.title = element_text(size = 10))
  ) +
  cowplot::draw_plot(legend, x = 0.7, y = 0.1, width = 0.3, height = 0.3)

## ----data_load_3, include=TRUE------------------------------------------------
# Load population data for the Seoul Metropolitan Area (SMA)
df_sma <- anycensus(
  year  = 2020,
  codes = c("Seoul", "Gyeonggi-do", "Incheon"),
  type  = "population"
)

# Calculate sex ratio (males per 100 females)
df_sma <- df_sma |>
  mutate(sex_ratio = `all households_male_prs` / `all households_female_prs` * 100)

# Extract overall distribution (all SMA combined)
df_all <- df_sma |>
  select(sex_ratio)

## ----histogram_3, include=TRUE, fig.width=7, fig.height=3---------------------
ggplot() +
  # Background: overall distribution across all SMA
  geom_histogram(
    data = df_all,
    aes(x = sex_ratio, y = after_stat(density)),
    bins = 20, fill = "grey80", color = NA, alpha = 1
  ) +
  # Regional distributions
  geom_histogram(
    data = df_sma,
    aes(x = sex_ratio, y = after_stat(density), fill = adm1),
    bins = 20, alpha = 0.7, color = NA, position = "identity"
  ) +
  facet_wrap(~ adm1, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c(
    "Seoul" = "darkorange",
    "Incheon" = "purple",
    "Gyeonggi-do" = "cyan4"
  )) +
  labs(
    title = "Sex ratio distribution by region (SMA, 2020)",
    x = "Sex ratio (male per 100 females)",
    y = "Density"
  ) +
  theme_bw()

## ----choropleth_3, fig.width=7, fig.height=3.5--------------------------------

# Merge SMA population data with boundaries
adm2_sf_2020_sma <- adm2_sf_2020 |>
  inner_join(df_sma, by = "adm2_code")

# Choropleth map for sex ratio
ggplot(adm2_sf_2020_sma) +
  geom_sf(aes(fill = sex_ratio), color = "gray", size = 0.01) +
  scale_fill_gradient2(
    low = "#2C7BB6", mid = "white", high = "#D7191C",
    midpoint = 100,                        # 100 = equal male/female
    name = "Sex ratio\n(males per 100 females)"
  ) +
  labs(title = "Sex ratio in the Seoul Metropolitan Area (2020)") +
  theme_void()


## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  out.width = "100%"
)

library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)

## ----include = TRUE-----------------------------------------------------------
library(tidycensuskr)


## ----include = TRUE-----------------------------------------------------------
data(adm2_sf_2020)
print(length(unique(adm2_sf_2020$adm2_code)))


## ----include = TRUE-----------------------------------------------------------
df_2020 <- anycensus(year = 2020, 
                     type = "mortality",
                     level = "adm2")
head(df_2020)


## ----include = TRUE-----------------------------------------------------------
df_2020_sido <- anycensus(year = 2020, 
                          type = "mortality",
                          level = "adm1",
                          aggregator = sum,
                          na.rm = TRUE)
head(df_2020_sido)


## ----include = TRUE-----------------------------------------------------------
data(censuskor)
head(censuskor)


## ----include = TRUE, fig.width=7, fig.height=4--------------------------------
ggplot(df_2020, aes(x = `all causes_male_p1p`, y = `all causes_female_p1p`)) +
  geom_point() +
  labs(
    x = "Male mortality (per 100,000 population)",
    y = "Female mortality (per 100,000 population)",
    title = "Male vs. Female Age-standardized Mortality Rates in South Korea (2020)"
  ) +
  theme_minimal(base_size = 10)



# Data collection for exercise 5 ----

# Libraries ----
library(dplyr)
library(readr)
library(stringr)

# Age-specific death rates for Russia in 1997 ----
rus_mxi <- readr::read_csv("raw/RUS_m_short_idr.csv") %>%
  dplyr::filter(sex == 3, year == 1997) %>%
  dplyr::mutate(disease_type = dplyr::case_when(
    cause == 0 ~ "all",
    cause == 7 ~ "heart",
    cause %in% c(1:6, 8:16) ~ "other"
  )) %>%
  dplyr::group_by(disease_type) %>%
  dplyr::summarize_at(vars(m0:m85p), ~sum(.)/1e6) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_longer(
    cols = m0:m85p,
    names_prefix = "m",
    names_to = "x",
    values_to = "mx"
  ) %>%
  tidyr::pivot_wider(
    id_cols = x,
    names_from = disease_type,
    values_from = mx,
    names_prefix = "mx_"
  ) %>%
  dplyr::mutate(
    x = as.numeric(stringr::str_replace_all(x, "p", ""))
  ) %>%
  dplyr::select(-mx_all)
saveRDS(rus_mxi, "data/rus_mxi.rds")

# Age-specific death rates for Japan in 1997 ----
jpn_mxi <- readr::read_csv("raw/JPN_m_short_idr.csv") %>%
  dplyr::filter(sex == 3, year == 1997) %>%
  dplyr::mutate(disease_type = dplyr::case_when(
    cause == 0 ~ "all",
    cause == 7 ~ "heart",
    cause %in% c(1:6, 8:16) ~ "other"
  )) %>%
  dplyr::group_by(disease_type) %>%
  dplyr::summarize_at(vars(m0:m85p), ~sum(.)/1e6) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_longer(
    cols = m0:m85p,
    names_prefix = "m",
    names_to = "x",
    values_to = "mx"
  ) %>%
  tidyr::pivot_wider(
    id_cols = x,
    names_from = disease_type,
    values_from = mx,
    names_prefix = "mx_"
  ) %>%
  dplyr::mutate(
    x = as.numeric(stringr::str_replace_all(x, "p", ""))
  ) %>%
  dplyr::select(-mx_all)
saveRDS(jpn_mxi, "data/jpn_mxi.rds")
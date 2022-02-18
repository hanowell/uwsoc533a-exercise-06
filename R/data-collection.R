# Data collection for exercise 5 ----

# Libraries ----
library(dplyr)
library(readr)
library(stringr)

# Age-specific death rates and death counts for Russia in 1997 ----
rus_Mxi <- readr::read_csv("raw/RUS_m_short_idr.csv") %>%
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
    values_to = "Mx"
  ) %>%
  tidyr::pivot_wider(
    id_cols = x,
    names_from = disease_type,
    values_from = Mx,
    names_prefix = "Mx_"
  ) %>%
  dplyr::mutate(
    x = as.numeric(stringr::str_replace_all(x, "p", ""))
  ) %>%
  dplyr::select(-Mx_all)
rus_Dxi <- readr::read_csv("raw/RUS_d_short_idr.csv") %>%
  dplyr::filter(sex == 3, year == 1997) %>%
  dplyr::mutate(disease_type = dplyr::case_when(
    cause == 0 ~ "all",
    cause == 7 ~ "heart",
    cause %in% c(1:6, 8:16) ~ "other"
  )) %>%
  dplyr::group_by(disease_type) %>%
  dplyr::summarize_at(vars(d0:d85p), sum) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_longer(
    cols = d0:d85p,
    names_prefix = "d",
    names_to = "x",
    values_to = "dx"
  ) %>%
  tidyr::pivot_wider(
    id_cols = x,
    names_from = disease_type,
    values_from = dx,
    names_prefix = "Dx_"
  ) %>%
  dplyr::mutate(
    x = as.numeric(stringr::str_replace_all(x, "p", ""))
  ) %>%
  dplyr::select(-Dx_all)
rus_Mxi_Dxi <- rus_Mxi %>% dplyr::left_join(rus_Dxi)
saveRDS(rus_Mxi_Dxi, "data/rus_Mxi_Dxi.rds")

# Age-specific death rates and death counts for Japan in 1997 ----
jpn_Mxi <- readr::read_csv("raw/jpn_m_short_idr.csv") %>%
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
    values_to = "Mx"
  ) %>%
  tidyr::pivot_wider(
    id_cols = x,
    names_from = disease_type,
    values_from = Mx,
    names_prefix = "Mx_"
  ) %>%
  dplyr::mutate(
    x = as.numeric(stringr::str_replace_all(x, "p", ""))
  ) %>%
  dplyr::select(-Mx_all)
jpn_Dxi <- readr::read_csv("raw/jpn_d_short_idr.csv") %>%
  dplyr::filter(sex == 3, year == 1997) %>%
  dplyr::mutate(disease_type = dplyr::case_when(
    cause == 0 ~ "all",
    cause == 7 ~ "heart",
    cause %in% c(1:6, 8:16) ~ "other"
  )) %>%
  dplyr::group_by(disease_type) %>%
  dplyr::summarize_at(vars(d0:d85p), sum) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_longer(
    cols = d0:d85p,
    names_prefix = "d",
    names_to = "x",
    values_to = "dx"
  ) %>%
  tidyr::pivot_wider(
    id_cols = x,
    names_from = disease_type,
    values_from = dx,
    names_prefix = "Dx_"
  ) %>%
  dplyr::mutate(
    x = as.numeric(stringr::str_replace_all(x, "p", ""))
  ) %>%
  dplyr::select(-Dx_all)
jpn_Mxi_Dxi <- jpn_Mxi %>% dplyr::left_join(jpn_Dxi)
saveRDS(jpn_Mxi_Dxi, "data/jpn_Mxi_Dxi.rds")
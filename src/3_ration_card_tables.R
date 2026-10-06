rm(list = ls())

rq_packages <- c(
  "tidyverse",
  "dplyr",
  "readr",
  "srvyr",
  "ggplot2",
  "tidyr",
  "ggridges",
  "gt",
  "haven",
  "foreign",
  "tmap",
  "sf",
  "rmapshaper",
  "readxl",
  "hrbrthemes",
  "wesanderson",
  "treemap",
  "treemapify"
)

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}
lapply(rq_packages, require, character.only = T)
rm(list = c("rq_packages", "installed_packages"))

source("functions/aggregated_inadequacy.R")
source("functions/general_inadequacy.R")
# source("functions/get_har.R")

#---------------------------------------------------------------------------

# set paths
figure_path <- "figures/"
raw_path <- "data/raw/"
processed_path <- "data/processed/"

file_list = list.files("data/raw/HCES_2022_23/")

data_list <- lapply(
  paste0("data/raw/HCES_2022_23/", file_list[3:4]),
  haven::read_dta
)
names(data_list) <- tools::file_path_sans_ext(file_list[3:4])

level04 <- data_list$level04
level03 <- data_list$level03

rm(data_list)
# household intake
hh_mn_intake <- readRDS(paste0(processed_path, "ind_nss2223_base_case.rds"))
ind_nss2223_hh_info <- read_csv(paste0(processed_path,'ind_nss2223_hh_info.csv'))
ind_nss2223_base_ai <- read_csv(paste0(processed_path,'ind_nss2223_base_ai.csv'))
ind_nss2223_food_consumption <- readRDS(paste0(processed_path, "ind_nss2223_food_consumption.rds")) 

nss_states <- tibble::tibble(
  adm1 = sprintf("%02d", 1:37),
  state_name = c(
    "Jammu & Kashmir",
    "Himachal Pradesh",
    "Punjab",
    "Chandigarh",
    "Uttarakhand",
    "Haryana",
    "Delhi",
    "Rajasthan",
    "Uttar Pradesh",
    "Bihar",
    "Sikkim",
    "Arunachal Pradesh",
    "Nagaland",
    "Manipur",
    "Mizoram",
    "Tripura",
    "Meghalaya",
    "Assam",
    "West Bengal",
    "Jharkhand",
    "Odisha",
    "Chhattisgarh",
    "Madhya Pradesh",
    "Gujarat",
    "Dadra & Nagar Haveli",
    "",
    "Maharashtra",
    "Andhra Pradesh",
    "Karnataka",
    "Goa",
    "Lakshadweep",
    "Kerala",
    "Tamil Nadu",
    "Puducherry",
    "Andaman & Nicobar Islands",
    "Telangana",
    "Ladakh"
  )
)


state_order <- tribble(
  ~region                  , ~state                      ,
  # ---- Northern ----
  "Region - Northern"      , "Chandigarh"                ,
  "Region - Northern"      , "Delhi"                     ,
  "Region - Northern"      , "Haryana"                   ,
  "Region - Northern"      , "Himachal Pradesh"          ,
  "Region - Northern"      , "Jammu & Kashmir"           ,
  "Region - Northern"      , "Ladakh"                    ,
  "Region - Northern"      , "Punjab"                    ,
  "Region - Northern"      , "Rajasthan"                 ,
  "Region - Northern"      , "Uttarakhand"               ,

  # ---- Central ----
  "Region - Central"       , "Chhattisgarh"              ,
  "Region - Central"       , "Madhya Pradesh"            ,
  "Region - Central"       , "Uttar Pradesh"             ,

  # ---- Eastern ----
  "Region - Eastern"       , "Andaman & Nicobar Islands" ,
  "Region - Eastern"       , "Bihar"                     ,
  "Region - Eastern"       , "Jharkhand"                 ,
  "Region - Eastern"       , "Odisha"                    ,
  "Region - Eastern"       , "West Bengal"               ,

  # ---- North-Eastern ----
  "Region - North-Eastern" , "Arunachal Pradesh"         ,
  "Region - North-Eastern" , "Assam"                     ,
  "Region - North-Eastern" , "Manipur"                   ,
  "Region - North-Eastern" , "Meghalaya"                 ,
  "Region - North-Eastern" , "Mizoram"                   ,
  "Region - North-Eastern" , "Nagaland"                  ,
  "Region - North-Eastern" , "Sikkim"                    ,
  "Region - North-Eastern" , "Tripura"                   ,

  # ---- Western ----
  "Region - Western"       , "Dadra & Nagar Haveli"      ,
  "Region - Western"       , "Goa"                       ,
  "Region - Western"       , "Gujarat"                   ,
  "Region - Western"       , "Maharashtra"               ,

  # ---- Southern ----
  "Region - Southern"      , "Andhra Pradesh"            ,
  "Region - Southern"      , "Karnataka"                 ,
  "Region - Southern"      , "Kerala"                    ,
  "Region - Southern"      , "Lakshadweep"               ,
  "Region - Southern"      , "Puducherry"                ,
  "Region - Southern"      , "Tamil Nadu"                ,
  "Region - Southern"      , "Telangana"
) %>%
  mutate(
    region = factor(region, levels = unique(region)),
    state = factor(state, levels = state)
  )

ind_nss2223_hh_info |> filter(adm1 == 27)
ind_nss2223_hh_info <- ind_nss2223_hh_info |> left_join(nss_states)
# rearrange dfs
use_of_rc <- level04 |>
  rename(hhid = common_id)
rm(level04)

type_rc <- level03 |>
  rename(hhid = common_id) |>
  select(hhid, ration_card_type) |>
  mutate(
    ration_card_type = case_match(
      ration_card_type,
      '0' ~ "None",
      '1' ~ "AAY",
      '2' ~ "BPL",
      '3' ~ "APL",
      '4' ~ "PHH",
      '5' ~ "SFSS",
      '9' ~ "Others"
    )
  )

rm(level03)

ration_card_total <- type_rc |>
  left_join(
    use_of_rc |>
      select(
        hhid,
        hh_used_ration_card_30days,
        ration_card_item_rice,
        ration_card_item_wheat
      ),
    by = 'hhid'
  ) |>
  mutate(
    hh_used_ration_card_30days = case_match(
      hh_used_ration_card_30days,
      '1' ~ "Yes",
      '2' ~ "No"
    ),
    ration_card_item_rice = case_match(
      ration_card_item_rice,
      '1' ~ "Yes",
      '' ~ "No"
    ),
    ration_card_item_wheat = case_match(
      ration_card_item_wheat,
      '1' ~ "Yes",
      '' ~ "No"
    ),
    aay_phh = case_match(
      ration_card_type,
      "AAY" ~ "Yes",
      "PHH" ~ "Yes",
      .default = "No"
    ),
    any_card = case_match(
      ration_card_type,
      "APL" ~ "Yes",
      "AAY" ~ "Yes",
      "PHH" ~ "Yes",
      "BPL" ~ "Yes",
      'SFSS' ~ "Yes",
      .default = "No"
    )
  )

ind_nss2223_base_ai <- ind_nss2223_base_ai |>
  select(hhid, fe_mg, folate_mcg, vitb12_mcg)


# Objective 1 #############################

all_intake <- ind_nss2223_base_ai |>
  left_join(ind_nss2223_hh_info) |>
  left_join(ration_card_total) |>
  mutate(res_quintile = paste(res, res_quintile))

all_intake |> filter(adm1 == 25)

aay_phh <- all_intake |>
  filter(aay_phh == "Yes")

any_card <- all_intake |>
  filter(any_card == "Yes")

ear_table <- data.frame(
  nutrient = c("folate_mcg", "vitb12_mcg"),
  ear_value = c(180, 2)
)
# state inad

make_nutrient_gt_o1_1 <- function(df, elgible) {
  if (elgible) {
    df_fmt <- df |>
      filter(!is.na(region)) |>
      mutate(
        elgible = sprintf("%.1f (%.1f)", elgible, elgible_se),
        fe = sprintf("%.1f (%.1f)", fe_inad, fe_inad_se),
        folate = sprintf("%.1f (%.f)", folate_mcg_inad, folate_mcg_inad_se),
        vitb12 = sprintf("%.1f (%.1f)", vitb12_mcg_inad, vitb12_mcg_inad_se)
      ) |>
      select(region, state_name, elgible, fe, folate, vitb12) |>
      arrange(region, state_name)

    df_fmt |>
      gt(
        rowname_col = "state_name",
        groupname_col = "region"
      ) |>
      cols_label(
        elgible = "Proportion elgibile \n for PDS",

        fe = "Iron (%)",
        folate = "Folate (%)",
        vitb12 = "Vitamin B12 (%)"
      ) |>
      cols_hide(region)
  } else {
    df_fmt <- df |>
      filter(!is.na(region)) |>
      mutate(
        fe = sprintf("%.1f (%.1f)", fe_inad, fe_inad_se),
        folate = sprintf("%.1f (%.1f)", folate_mcg_inad, folate_mcg_inad_se),
        vitb12 = sprintf("%.1f (%.1f)", vitb12_mcg_inad, vitb12_mcg_inad_se)
      ) |>
      select(region, state_name, fe, folate, vitb12) |>
      arrange(region, state_name)

    df_fmt |>
      gt(
        rowname_col = "state_name",
        groupname_col = "region"
      ) |>
      cols_label(
        fe = "Iron (%)",
        folate = "Folate (%)",
        vitb12 = "Vitamin B12 (%)"
      ) |>
      cols_hide(region)
  }
}

total_inad <- general_inadequacy(all_intake, group = adm1, ear_table) |>
  left_join(nss_states) %>%
  left_join(state_order, by = c("state_name" = "state")) %>%
  arrange(region, state_name)

general_inadequacy(all_intake, group = NULL, ear_table)
general_inadequacy(aay_phh, group = NULL, ear_table)
general_inadequacy(any_card, group = NULL, ear_table)


gtsave(
  make_nutrient_gt_o1_1(total_inad, elgible = FALSE),
  filename = paste0(figure_path, "/objective_1/total_inad.html")
)

all_intake |>
  as_survey_design(ids = ea, strata = res, weights = survey_wgt) |>
  summarise(
    elgible = survey_mean(any_card == "Yes", proportion = T, na.rm = T) * 100
  )


phh_aay_inad <- general_inadequacy(
  all_intake |> filter(aay_phh == "Yes"),
  adm1,
  ear_table
) |>
  left_join(nss_states) %>%
  left_join(state_order, by = c("state_name" = "state")) %>%
  arrange(region, state_name) |>
  left_join(
    all_intake |>
      as_survey_design(ids = ea, strata = res, weights = survey_wgt) |>
      group_by(adm1) |>
      summarise(
        elgible = survey_mean(aay_phh == "Yes", proportion = T, na.rm = T) * 100
      )
  )

gtsave(
  make_nutrient_gt_o1_1(phh_aay_inad, elgible = TRUE),
  filename = paste0(figure_path, "/objective_1/phh_aay_inad.html")
)


any_card_inad <- general_inadequacy(
  all_intake |> filter(any_card == "Yes"),
  adm1,
  ear_table
) |>
  left_join(nss_states) %>%
  left_join(state_order, by = c("state_name" = "state")) %>%
  arrange(region, state_name) |>
  left_join(
    all_intake |>
      as_survey_design(ids = ea, strata = res, weights = survey_wgt) |>
      group_by(adm1) |>
      summarise(
        elgible = survey_mean(any_card == "Yes", proportion = T, na.rm = T) *
          100
      )
  )

gtsave(
  make_nutrient_gt_o1_1(any_card_inad, elgible = TRUE),
  filename = paste0(figure_path, "/objective_1/any_card_inad.html")
)


make_nutrient_gt_o1_2 <- function(df) {
  df_fmt <- df |>
    filter(!is.na(region)) |>
    mutate(
      AAY = sprintf("%.1f (%.1f)", AAY, AAY_se),
      PHH = sprintf("%.1f (%.1f)", PHH, PHH_se),
      BPL = sprintf("%.1f (%.1f)", BPL, BPL_se),
      APL = sprintf("%.1f (%.1f)", APL, APL_se),
      SFSS = sprintf("%.1f (%.1f)", SFSS, SFSS_se)
    ) |>
    select(region, state_name, AAY, PHH, BPL, APL, SFSS) |>
    arrange(region, state_name)

  df_fmt |>
    gt(
      rowname_col = "state_name",
      groupname_col = "region"
    ) |>
    # cols_label(
    #   elgible = "Proportion elgibile \n for PDS",

    #   fe     = "Iron (%)",
    #   folate = "Folate (%)",
    #   vitb12 = "Vitamin B12 (%)"
    # ) |>
    cols_hide(region)
}


state_rc <- all_intake |>
  as_survey_design(ids = ea, strata = res, weights = survey_wgt) |>
  group_by(adm1) |>
  summarise(
    AAY = survey_mean(ration_card_type == "AAY", proportion = TRUE, na.rm = T) *
      100,
    PHH = survey_mean(ration_card_type == "PHH", proportion = TRUE, na.rm = T) *
      100,
    BPL = survey_mean(ration_card_type == "BPL", proportion = TRUE, na.rm = T) *
      100,
    APL = survey_mean(ration_card_type == "APL", proportion = TRUE, na.rm = T) *
      100,
    SFSS = survey_mean(
      ration_card_type == "SFSS",
      proportion = TRUE,
      na.rm = T
    ) *
      100
  ) |>
  left_join(nss_states) %>%
  left_join(state_order, by = c("state_name" = "state")) %>%
  arrange(region, state_name)


gtsave(
  make_nutrient_gt_o1_2(state_rc),
  filename = paste0(figure_path, "/objective_1/ration_card.html")
)


nat_rc <- all_intake |>
  as_survey_design(ids = ea, strata = res, weights = survey_wgt) |>
  summarise(
    AAY = survey_mean(ration_card_type == "AAY", proportion = TRUE, na.rm = T) *
      100,
    PHH = survey_mean(ration_card_type == "PHH", proportion = TRUE, na.rm = T) *
      100,
    BPL = survey_mean(ration_card_type == "BPL", proportion = TRUE, na.rm = T) *
      100,
    APL = survey_mean(ration_card_type == "APL", proportion = TRUE, na.rm = T) *
      100,
    SFSS = survey_mean(
      ration_card_type == "SFSS",
      proportion = TRUE,
      na.rm = T
    ) *
      100
  )

nat_rc

# Objective 2 #############################

# ---- Global spec choice ----------------------------------------------------
# Change this one value to switch the whole pipeline between India and WFP specs.
FORT_SPEC <- "WFP" # "India" or "WFP"
FORT_SPEC_SLUG <- tolower(FORT_SPEC) # used in output filenames


# ---- Fortification specs ---------------------------------------------------
ind_fort_spec <- data.frame(
  commodity = c("rice", "rice", "wheat", "wheat"),
  specs = c("India", "WFP", "India", "WFP"),
  fe_mg = c(3.525, 7, 1.7625, 2),
  folate_mcg = c(10, 130, 8.3, 130 * 0.83),
  vitb12_mcg = c(0.1, 1, 0.085, 0.85)
)

get_spec <- function(commodity_name, spec_name = FORT_SPEC) {
  row <- ind_fort_spec |>
    filter(commodity == commodity_name, specs == spec_name)

  if (nrow(row) != 1) {
    stop(sprintf(
      "Expected 1 row for commodity='%s' and specs='%s', got %d.",
      commodity_name,
      spec_name,
      nrow(row)
    ))
  }

  as.list(row)
}

rice_spec <- get_spec("rice")
wheat_spec <- get_spec("wheat")

# ---- Build commodity-level household data ---------------------------------
build_commodity <- function(food_df, hh_df, item_codes) {
  food_df |>
    filter(item_code %in% item_codes) |>
    group_by(hhid) |>
    summarise(quantity_g = sum(quantity_g), .groups = "drop") |>
    right_join(hh_df, by = "hhid") |>
    mutate(
      quantity_g = quantity_g / afe,
      consumed = if_else(is.na(quantity_g), 0, 1),
      # zero-filled version for full-population mean (used for projection)
      quantity_g_full = if_else(consumed == 1, quantity_g, 0)
    )
}

rice <- build_commodity(
  ind_nss2223_food_consumption,
  ind_nss2223_hh_info,
  c(61, 101)
)
wheat <- build_commodity(
  ind_nss2223_food_consumption,
  ind_nss2223_hh_info,
  c(62, 107)
)


# ---- Summary helper --------------------------------------------------------
# - reach + mean quantity across the FULL population (powers the projection)
# - median + IQR of quantity AMONG CONSUMERS (for display)
# - projected Fe / folate / B12 intake from the population mean
# ---- Summary helper --------------------------------------------------------
# - reach                                          (population)
# - mean quantity_g_full                           (population, for projection)
# - Q25 / median / Q75 of quantity_g_full          (population, for projection)
# - Q25 / median / Q75 of quantity_g               (consumers only, for display)
# - micronutrient projections from EACH of the 4 population quantity bases

commodity_summary <- function(df, spec, by_adm1 = TRUE) {
  des <- df |>
    as_survey_design(ids = ea, strata = res, weights = survey_wgt)

  if (by_adm1) {
    des_pop <- des |> group_by(adm1)
    des_con <- des |> filter(consumed == 1) |> group_by(adm1)
  } else {
    des_pop <- des
    des_con <- des |> filter(consumed == 1)
  }

  # Full-population stats on zero-filled quantity
  pop_stats <- des_pop |>
    summarise(
      reach = survey_mean(consumed == 1, proportion = TRUE, na.rm = TRUE) * 100,
      quantity_g = survey_mean(quantity_g_full, na.rm = TRUE),
      quantity_full = survey_quantile(
        quantity_g_full,
        quantiles = c(0.25, 0.5, 0.75),
        na.rm = TRUE
      )
    )
  # -> quantity_full_q25, quantity_full_q25_se,
  #    quantity_full_q50, quantity_full_q50_se,
  #    quantity_full_q75, quantity_full_q75_se

  # Consumer-only quantiles (for display in the table)
  consumer_stats <- des_con |>
    summarise(
      quantity_con = survey_quantile(
        quantity_g,
        quantiles = c(0.25, 0.5, 0.75),
        na.rm = TRUE
      )
    )
  # -> quantity_con_q25 / _q50 / _q75 (+ _se)

  out <- if (by_adm1) {
    pop_stats |> left_join(consumer_stats, by = "adm1")
  } else {
    bind_cols(pop_stats, consumer_stats)
  }

  # Helper: add 6 projection columns (fe/folate/b12 value + SE) for a given
  # quantity basis (mean / q25 / median / q75), with a suffix on the output names.
  project_nutrients <- function(df, q, q_se, suffix, spec) {
    df |>
      mutate(
        !!paste0("fe_mg", suffix) := .data[[q]] * spec$fe_mg / 100,
        !!paste0("fe_mg", suffix, "_se") := .data[[q_se]] * spec$fe_mg / 100,
        !!paste0("folate_mcg", suffix) := .data[[q]] * spec$folate_mcg / 100,
        !!paste0("folate_mcg", suffix, "_se") := .data[[q_se]] *
          spec$folate_mcg /
          100,
        !!paste0("vitb12_mcg", suffix) := .data[[q]] * spec$vitb12_mcg / 100,
        !!paste0("vitb12_mcg", suffix, "_se") := .data[[q_se]] *
          spec$vitb12_mcg /
          100
      )
  }

  out <- out |>
    project_nutrients("quantity_g", "quantity_g_se", "", spec) |> # mean
    project_nutrients(
      "quantity_full_q25",
      "quantity_full_q25_se",
      "_q25",
      spec
    ) |>
    project_nutrients(
      "quantity_full_q50",
      "quantity_full_q50_se",
      "_med",
      spec
    ) |>
    project_nutrients("quantity_full_q75", "quantity_full_q75_se", "_q75", spec)

  if (by_adm1) {
    out <- out |>
      left_join(nss_states, by = "adm1") |>
      left_join(state_order, by = c("state_name" = "state")) |>
      arrange(region, state_name)
  }

  out
}


# ---- State-level and national summaries ------------------------------------
wheat_summary <- commodity_summary(wheat, wheat_spec, by_adm1 = TRUE)
wheat_summary_nat <- commodity_summary(wheat, wheat_spec, by_adm1 = FALSE)

rice_summary <- commodity_summary(rice, rice_spec, by_adm1 = TRUE)
rice_summary_nat <- commodity_summary(rice, rice_spec, by_adm1 = FALSE)


# ---- gt table --------------------------------------------------------------
make_nutrient_gt_o2 <- function(df) {
  df |>
    filter(!is.na(region)) |>
    mutate(
      reach = sprintf("%.0f (%.0f)", reach, reach_se),
      pc_consumption = sprintf(
        "%.0f (%.0f, %.0f)",
        quantity_con_q50,
        quantity_con_q25,
        quantity_con_q75
      ),
      fe = sprintf("%.1f (%.1f, %.1f)", fe_mg_med, fe_mg_q25, fe_mg_q75),
      folate = sprintf(
        "%.0f (%.0f, %.0f)",
        folate_mcg_med,
        folate_mcg_q25,
        folate_mcg_q75
      ),
      vitb12 = sprintf(
        "%.1f (%.1f, %.1f)",
        vitb12_mcg_med,
        vitb12_mcg_q25,
        vitb12_mcg_q75
      )
    ) |>
    select(region, state_name, reach, pc_consumption, fe, folate, vitb12) |>
    arrange(region, state_name) |>
    gt(rowname_col = "state_name", groupname_col = "region") |>
    cols_label(
      reach = "Reach (%)",
      pc_consumption = "Quantity, consumers only (g/d) — median (Q25, Q75)",
      fe = "Iron (mg) — median (Q25, Q75)",
      folate = "Folate (µg) — median (Q25, Q75)",
      vitb12 = "Vitamin B12 (µg) — median (Q25, Q75)"
    ) |>
    cols_hide(region)
}

# ---- Save ------------------------------------------------------------------
gtsave(
  make_nutrient_gt_o2(wheat_summary),
  filename = paste0(
    figure_path,
    "/objective_2/reach_wheat_",
    FORT_SPEC_SLUG,
    ".html"
  )
)

gtsave(
  make_nutrient_gt_o2(rice_summary),
  filename = paste0(
    figure_path,
    "/objective_2/reach_rice_",
    FORT_SPEC_SLUG,
    ".html"
  )
)

# Objective 3 #############################

# ── Fortification specs -----------------------------------------------------

ind_fort_spec <- data.frame(
  commodity = c("rice", "rice", "wheat", "wheat"),
  specs = c("India", "WFP", "India", "WFP"),
  fe_mg = c(3.525, 7, 1.7625, 2),
  folate_mcg = c(10, 130, 10*0.83, 130 * 0.83),
  vitb12_mcg = c(0.1, 1, 0.085, 0.85)
)

get_spec <- function(commodity_name, spec_name) {
  row <- ind_fort_spec |>
    dplyr::filter(commodity == commodity_name, specs == spec_name)
  as.list(row)
}

# ── Commodity construction --------------------------------------------------

build_commodity <- function(food_df, hh_df, item_codes) {
  food_df |>
    dplyr::filter(item_code %in% item_codes) |>
    dplyr::group_by(hhid) |>
    dplyr::summarise(quantity_g = sum(quantity_g), .groups = "drop") |>
    dplyr::right_join(hh_df, by = "hhid") |>
    dplyr::mutate(
      quantity_g = quantity_g / afe,
      quantity_g = ifelse(is.na(quantity_g), 0, quantity_g)
    )
}

rice <- build_commodity(ind_nss2223_food_consumption,
                        ind_nss2223_hh_info,
                        c(61, 101))

wheat <- build_commodity(ind_nss2223_food_consumption,
                         ind_nss2223_hh_info,
                         c(62, 107))

# ── Fortification contributions (BOTH specs) --------------------------------

fort_contributions <- wheat |>
  dplyr::select(hhid, quantity_g) |>
  dplyr::mutate(
    fe_mg_fort_wf_india = quantity_g * get_spec("wheat","India")$fe_mg / 100,
    fe_mg_fort_wf_wfp   = quantity_g * get_spec("wheat","WFP")$fe_mg / 100,
    folate_mcg_fort_wf_india = quantity_g * get_spec("wheat","India")$folate_mcg / 100,
    folate_mcg_fort_wf_wfp   = quantity_g * get_spec("wheat","WFP")$folate_mcg / 100,
    vitb12_mcg_fort_wf_india = quantity_g * get_spec("wheat","India")$vitb12_mcg / 100,
    vitb12_mcg_fort_wf_wfp   = quantity_g * get_spec("wheat","WFP")$vitb12_mcg / 100
  ) |>
  dplyr::left_join(ind_nss2223_base_ai, by = "hhid") |>
  dplyr::left_join(
    rice |>
      dplyr::select(hhid, quantity_g) |>
      dplyr::mutate(
        fe_mg_fort_rice_india = quantity_g * get_spec("rice","India")$fe_mg / 100,
        fe_mg_fort_rice_wfp   = quantity_g * get_spec("rice","WFP")$fe_mg / 100,
        folate_mcg_fort_rice_india = quantity_g * get_spec("rice","India")$folate_mcg / 100,
        folate_mcg_fort_rice_wfp   = quantity_g * get_spec("rice","WFP")$folate_mcg / 100,
        vitb12_mcg_fort_rice_india = quantity_g * get_spec("rice","India")$vitb12_mcg / 100,
        vitb12_mcg_fort_rice_wfp   = quantity_g * get_spec("rice","WFP")$vitb12_mcg / 100
      ) |>
      dplyr::select(-quantity_g),
    by = "hhid"
  ) |>
  dplyr::mutate(across(-hhid, ~ ifelse(is.na(.), 0, .)))

# ── Scenario construction ---------------------------------------------------

df_long <- fort_contributions |>
  dplyr::mutate(
    # base
    fe_mg_base = fe_mg,
    folate_mcg_base = folate_mcg,
    vitb12_mcg_base = vitb12_mcg,
    
    # rice
    fe_mg_rice_india = fe_mg + fe_mg_fort_rice_india,
    fe_mg_rice_wfp   = fe_mg + fe_mg_fort_rice_wfp,
    folate_mcg_rice_india = folate_mcg + folate_mcg_fort_rice_india,
    folate_mcg_rice_wfp   = folate_mcg + folate_mcg_fort_rice_wfp,
    vitb12_mcg_rice_india = vitb12_mcg + vitb12_mcg_fort_rice_india,
    vitb12_mcg_rice_wfp   = vitb12_mcg + vitb12_mcg_fort_rice_wfp,
    
    # wheat
    fe_mg_wheat_india = fe_mg + fe_mg_fort_wf_india,
    fe_mg_wheat_wfp   = fe_mg + fe_mg_fort_wf_wfp,
    folate_mcg_wheat_india = folate_mcg + folate_mcg_fort_wf_india,
    folate_mcg_wheat_wfp   = folate_mcg + folate_mcg_fort_wf_wfp,
    vitb12_mcg_wheat_india = vitb12_mcg + vitb12_mcg_fort_wf_india,
    vitb12_mcg_wheat_wfp   = vitb12_mcg + vitb12_mcg_fort_wf_wfp,
    
    # both
    fe_mg_both_india = fe_mg + fe_mg_fort_rice_india + fe_mg_fort_wf_india,
    fe_mg_both_wfp   = fe_mg + fe_mg_fort_rice_wfp + fe_mg_fort_wf_wfp,
    folate_mcg_both_india = folate_mcg + folate_mcg_fort_rice_india + folate_mcg_fort_wf_india,
    folate_mcg_both_wfp   = folate_mcg + folate_mcg_fort_rice_wfp + folate_mcg_fort_wf_wfp,
    vitb12_mcg_both_india = vitb12_mcg + vitb12_mcg_fort_rice_india + vitb12_mcg_fort_wf_india,
    vitb12_mcg_both_wfp   = vitb12_mcg + vitb12_mcg_fort_rice_wfp + vitb12_mcg_fort_wf_wfp
  )

# ── Reshape -----------------------------------------------------------------


fort_scenarios <- df_long |>
  dplyr::select(
    hhid,
    dplyr::matches("fe_mg|folate_mcg|vitb12_mcg")
  ) |>
  dplyr::left_join(ind_nss2223_hh_info, by = "hhid") |>
  dplyr::left_join(ration_card_total, by = "hhid")


# ── Inadequacy prep ---------------------------------------------------------

prep_inadequacy <- function(df) {
  scen_label <- unique(df$scenario)
  
  national <- df |>
    general_inadequacy(ear_table = ear_table) |>
    dplyr::mutate(state_name = "National", region = "National")
  
  states <- df |>
    general_inadequacy(group = adm1, ear_table = ear_table) |>
    dplyr::left_join(nss_states, by = "adm1") |>
    dplyr::left_join(state_order, by = c("state_name" = "state")) |>
    dplyr::arrange(region, state_name)
  
  dplyr::bind_rows(national, states) %>% 
    select(-national,-adm1)
}

# ── Table builder -----------------------------------------------------------

make_gt_for_group <- function(scen, card_status = NULL) {
  
  df <- fort_scenarios |>
    dplyr::filter(
      if (!is.null(card_status)) any_card == card_status else TRUE
    )
  
  compute <- function(scen, spec) {
    
    suffix <- if (spec == "base") {
      "base"
    } else {
      paste0(scen, "_", spec)
    }
    
    df_tmp <- df |>
      dplyr::mutate(
        fe_mg = .data[[paste0("fe_mg_", suffix)]],
        folate_mcg = .data[[paste0("folate_mcg_", suffix)]],
        vitb12_mcg = .data[[paste0("vitb12_mcg_", suffix)]]
      )
    
    prep_inadequacy(df_tmp)
  }
  
  
  base  <- compute("base",  "base")
  india <- compute(scen,   "india")
  wfp   <- compute(scen,   "wfp")
  
  
  
  merged <- base |>
    dplyr::rename(
      folate_base  = folate_mcg_inad,
      b12_base     = vitb12_mcg_inad,
      iron_base    = fe_inad
    ) |>
    dplyr::left_join(
      india |>
        dplyr::rename(
          folate_india  = folate_mcg_inad,
          b12_india     = vitb12_mcg_inad,
          iron_india    = fe_inad
        ),
      by = c("state_name", "region")
    ) |>
    dplyr::left_join(
      wfp |>
        dplyr::rename(
          folate_wfp  = folate_mcg_inad,
          b12_wfp     = vitb12_mcg_inad,
          iron_wfp    = fe_inad
        ),
      by = c("state_name", "region")
    )
  
  
  
  names(merged)
  
  
  
  merged |>
    dplyr::transmute(
      region,
      state_name,
      
      folate_base  = round(folate_base),
      folate_india = round(folate_india),
      folate_wfp   = round(folate_wfp),
      
      b12_base  = round(b12_base),
      b12_india = round(b12_india),
      b12_wfp   = round(b12_wfp),
      
      iron_base  = round(iron_base),
      iron_india = round(iron_india),
      iron_wfp   = round(iron_wfp)
    )|>
    gt::gt(
      rowname_col = "state_name",
      groupname_col = "region"
    ) |>
    gt::cols_label(
      folate_base  = "Base",
      folate_india = "India",
      folate_wfp   = "WFP",
      
      b12_base  = "Base",
      b12_india = "India",
      b12_wfp   = "WFP",
      
      iron_base  = "Base",
      iron_india = "India",
      iron_wfp   = "WFP"
    ) |>
    
    gt::tab_spanner(
      label = "Iron",
      columns = c(iron_base, iron_india, iron_wfp) 
    ) %>% 
    gt::tab_spanner(
      label = "Folate",
      columns = c(folate_base, folate_india, folate_wfp)
    ) |>
    gt::tab_spanner(
      label = "Vitamin B12",
      columns = c(b12_base, b12_india, b12_wfp)
    ) |>
    gt::tab_header(
      title = paste0(tools::toTitleCase(scen), " fortification scenario"),
      subtitle = ifelse(
        is.null(card_status),
        "All households",
        "Ration card households"
      )
    )
  
}

# ── Final tables ------------------------------------------------------------

scenarios <- c("rice","wheat","both")

tables <- tidyr::expand_grid(
  scenario = scenarios,
  group = c("all","card")
) |>
  dplyr::mutate(
    gt = purrr::map2(
      scenario,
      group,
      ~make_gt_for_group(.x,
                         if(.y=="card") "Yes" else NULL)
    )
  )

tables$gt

type

# ---- Save to HTML ---------------------------------------------------------
library(flextable)
library(officer)
library(htmltools)

gt_to_html <- function(gt_tbl) {
  HTML(as_raw_html(gt_tbl))
}

tables |>
  rowwise() |>
  mutate(
    html_file = paste0(
      figure_path,
      "objective_3/",
      scenario, "_", group, "_", FORT_SPEC_SLUG, ".html"
    ),
    docx_file = sub("\\.html$", ".docx", html_file)
  ) |>
  group_walk(~ {
    
    # ---- Save HTML --------------------------------------------------------
    html_doc <- tagList(
      tags$h1(paste0(.x$scenario, " — ", .x$group, " — ", FORT_SPEC, " spec")),
      gt_to_html(.x$gt[[1]])
    )
    
    save_html(html_doc, file = .x$html_file)
    
    # ---- Convert to Word --------------------------------------------------
    
    rmarkdown::pandoc_convert(
      input  = normalizePath(.x$html_file),
      output = normalizePath(.x$docx_file, mustWork = FALSE)
    )
    
    
  })

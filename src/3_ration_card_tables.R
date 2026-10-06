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
source("functions/get_har.R")
# source("src/7_clean_for_db.R")  # NOTE: this script has been removed; ind_nss2223_hh_info and ind_nss2223_base_ai must be loaded separately

ind_nss2223_hh_info <- read_csv("data/ind_nss2223_hh_info.csv")

ind_nss2223_base_ai <- read_rds('data/processed/ind_nss2223_base_case.rds') |>
  rename(
    hhid = common_id,
    fe_mg = iron_mg,
    folate_mcg = folate_ug,
    vitb12_mcg = vitaminb12_in_mcg
  )

ind_nss2223_food_consumption <- read_rds(
  'data/processed/ind_nss2223_food_consumption.rds'
) |>
  rename(
    hhid = common_id,
    item_code = Item_Code,
    quantity_g = Total_Consumption_Quantity
  )
#---------------------------------------------------------------------------

# set paths
figure_path <- "figures/ration_cards/"
raw_path <- "data/raw/"
processed_path <- "data/processed/"

for (obj in c("objective_1", "objective_2", "objective_3")) {
  dir.create(paste0(figure_path, obj), recursive = TRUE, showWarnings = FALSE)
}

file_list = list.files("data/raw/HCES_2022_23/")

data_list <- lapply(
  paste0("data/raw/HCES_2022_23/", file_list[1:2]),
  haven::read_dta
)
names(data_list) <- tools::file_path_sans_ext(file_list[1:2])

level04 <- data_list$level04
level03 <- data_list$level03

rm(data_list)
# household intake
hh_mn_intake <- readRDS(paste0(processed_path, "ind_nss2223_base_case.rds"))


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
      "AAY" ~ "Yes",
      "PHH" ~ "Yes",
      "BPL" ~ "Yes",
      'SFSS' ~ "Yes",
      .default = "No"
    ),
    # "Yes"/"No" preserving NA (missing/refused) rather than folding it into
    # "No" - used for the coverage table so missingness is excluded from
    # both numerator and denominator (it gets its own table instead).
    any_card_incl_apl = case_when(
      is.na(ration_card_type) ~ NA_character_,
      ration_card_type == "None" ~ "No",
      TRUE ~ "Yes"
    ),
    aay_phh_comb = case_when(
      is.na(ration_card_type) ~ NA_character_,
      ration_card_type %in% c("AAY", "PHH") ~ "Yes",
      TRUE ~ "No"
    ),
    card_missing = is.na(ration_card_type)
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
        elgible = sprintf("%.0f (%.0f)", elgible, elgible_se),
        fe = sprintf("%.0f (%.0f)", fe_inad, fe_inad_se),
        folate = sprintf("%.0f (%.0f)", folate_mcg_inad, folate_mcg_inad_se),
        vitb12 = sprintf("%.0f (%.0f)", vitb12_mcg_inad, vitb12_mcg_inad_se)
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
        fe = sprintf("%.0f (%.0f)", fe_inad, fe_inad_se),
        folate = sprintf("%.0f (%.0f)", folate_mcg_inad, folate_mcg_inad_se),
        vitb12 = sprintf("%.0f (%.0f)", vitb12_mcg_inad, vitb12_mcg_inad_se)
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

# National row + formatted version of phh_aay_inad, for the Word summary docs
# below (the gt/HTML table above is state-only, matching the other Objective 1
# tables' convention; the summary docs add a bold National row like Tables 1-4).
phh_aay_inad_nat <- bind_cols(
  general_inadequacy(
    all_intake |> filter(aay_phh == "Yes"),
    group = NULL,
    ear_table
  ) |>
    select(-national),
  all_intake |>
    as_survey_design(ids = ea, strata = res, weights = survey_wgt) |>
    summarise(
      elgible = survey_mean(aay_phh == "Yes", proportion = T, na.rm = T) * 100
    )
) |>
  mutate(state_name = "National", region = "National")

phh_aay_inad_fmt <- bind_rows(phh_aay_inad_nat, phh_aay_inad) |>
  filter(!is.na(region)) |>
  mutate(
    elgible = sprintf("%.0f (%.0f)", elgible, elgible_se),
    fe = sprintf("%.0f (%.0f)", fe_inad, fe_inad_se),
    folate = sprintf("%.0f (%.0f)", folate_mcg_inad, folate_mcg_inad_se),
    vitb12 = sprintf("%.0f (%.0f)", vitb12_mcg_inad, vitb12_mcg_inad_se)
  ) |>
  select(region, state_name, elgible, fe, folate, vitb12)


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
      AAY = sprintf("%.0f (%.0f)", AAY, AAY_se),
      PHH = sprintf("%.0f (%.0f)", PHH, PHH_se),
      BPL = sprintf("%.0f (%.0f)", BPL, BPL_se),
      APL = sprintf("%.0f (%.0f)", APL, APL_se),
      SFSS = sprintf("%.0f (%.0f)", SFSS, SFSS_se)
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

# Objective 1b: coverage, missingness, and no-card inadequacy tables ########

# Table 1: proportion holding any ration card (incl. APL) vs. AAY/PHH -------
card_coverage_nat <- all_intake |>
  as_survey_design(ids = ea, strata = res, weights = survey_wgt) |>
  summarise(
    any_card = survey_mean(
      any_card_incl_apl == "Yes",
      proportion = TRUE,
      na.rm = TRUE
    ) *
      100,
    aay_phh = survey_mean(
      aay_phh_comb == "Yes",
      proportion = TRUE,
      na.rm = TRUE
    ) *
      100
  ) |>
  mutate(state_name = "National", region = "National")

card_coverage_state <- all_intake |>
  as_survey_design(ids = ea, strata = res, weights = survey_wgt) |>
  group_by(adm1) |>
  summarise(
    any_card = survey_mean(
      any_card_incl_apl == "Yes",
      proportion = TRUE,
      na.rm = TRUE
    ) *
      100,
    aay_phh = survey_mean(
      aay_phh_comb == "Yes",
      proportion = TRUE,
      na.rm = TRUE
    ) *
      100
  ) |>
  left_join(nss_states, by = "adm1") |>
  left_join(state_order, by = c("state_name" = "state")) |>
  arrange(region, state_name)

card_coverage <- bind_rows(card_coverage_nat, card_coverage_state)

card_coverage_fmt <- card_coverage |>
  filter(!is.na(region)) |>
  mutate(
    any_card = sprintf("%.0f (%.0f)", any_card, any_card_se),
    aay_phh = sprintf("%.0f (%.0f)", aay_phh, aay_phh_se)
  ) |>
  select(region, state_name, any_card, aay_phh)

make_card_coverage_gt <- function(df_fmt) {
  df_fmt |>
    gt(rowname_col = "state_name", groupname_col = "region") |>
    cols_label(
      any_card = "Any ration card, incl. APL (%)",
      aay_phh = "AAY or PHH (%)"
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = state_name == "National")
    ) |>
    cols_hide(region)
}

gtsave(
  make_card_coverage_gt(card_coverage_fmt),
  filename = paste0(figure_path, "/objective_1/card_coverage.html")
)


# Table 2: proportion missing/refused on ration card type -------------------
card_missing_nat <- all_intake |>
  as_survey_design(ids = ea, strata = res, weights = survey_wgt) |>
  summarise(
    missing = survey_mean(card_missing, proportion = TRUE, na.rm = TRUE) * 100
  ) |>
  mutate(state_name = "National", region = "National")

card_missing_state <- all_intake |>
  as_survey_design(ids = ea, strata = res, weights = survey_wgt) |>
  group_by(adm1) |>
  summarise(
    missing = survey_mean(card_missing, proportion = TRUE, na.rm = TRUE) * 100
  ) |>
  left_join(nss_states, by = "adm1") |>
  left_join(state_order, by = c("state_name" = "state")) |>
  arrange(region, state_name)

card_missing <- bind_rows(card_missing_nat, card_missing_state)

card_missing_fmt <- card_missing |>
  filter(!is.na(region)) |>
  mutate(missing = sprintf("%.0f (%.0f)", missing, missing_se)) |>
  select(region, state_name, missing)

make_card_missing_gt <- function(df_fmt) {
  df_fmt |>
    gt(rowname_col = "state_name", groupname_col = "region") |>
    cols_label(missing = "Missing / refused ration card type (%)") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = state_name == "National")
    ) |>
    cols_hide(region)
}

gtsave(
  make_card_missing_gt(card_missing_fmt),
  filename = paste0(figure_path, "/objective_1/card_missing.html")
)


# Table 2b: % with no ration card ("None") vs. % with an "Other" card type --
# Unlike card_missing above (non-response), this covers households that
# affirmatively reported having no ration card, plus a companion column for
# the "Other" (SFSS/miscellaneous, code 9) card type for a complete picture.
card_none_other_nat <- all_intake |>
  as_survey_design(ids = ea, strata = res, weights = survey_wgt) |>
  summarise(
    none = survey_mean(
      ration_card_type == "None",
      proportion = TRUE,
      na.rm = TRUE
    ) *
      100,
    others = survey_mean(
      ration_card_type == "Others",
      proportion = TRUE,
      na.rm = TRUE
    ) *
      100
  ) |>
  mutate(state_name = "National", region = "National")

card_none_other_state <- all_intake |>
  as_survey_design(ids = ea, strata = res, weights = survey_wgt) |>
  group_by(adm1) |>
  summarise(
    none = survey_mean(
      ration_card_type == "None",
      proportion = TRUE,
      na.rm = TRUE
    ) *
      100,
    others = survey_mean(
      ration_card_type == "Others",
      proportion = TRUE,
      na.rm = TRUE
    ) *
      100
  ) |>
  left_join(nss_states, by = "adm1") |>
  left_join(state_order, by = c("state_name" = "state")) |>
  arrange(region, state_name)

card_none_other <- bind_rows(card_none_other_nat, card_none_other_state)

card_none_other_fmt <- card_none_other |>
  filter(!is.na(region)) |>
  mutate(
    none = sprintf("%.0f (%.0f)", none, none_se),
    others = sprintf("%.0f (%.0f)", others, others_se)
  ) |>
  select(region, state_name, none, others)

make_card_none_other_gt <- function(df_fmt) {
  df_fmt |>
    gt(rowname_col = "state_name", groupname_col = "region") |>
    cols_label(
      none = "No ration card (%)",
      others = "\"Other\" ration card type (%)"
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = state_name == "National")
    ) |>
    cols_hide(region)
}

gtsave(
  make_card_none_other_gt(card_none_other_fmt),
  filename = paste0(figure_path, "/objective_1/card_none_other.html")
)


# Table 3: micronutrient inadequacy among households with NO ration card ----
no_card <- all_intake |>
  filter(ration_card_type == "None")

no_card_inad_nat <- general_inadequacy(no_card, group = NULL, ear_table) |>
  mutate(state_name = "National", region = "National")

no_card_inad_state <- general_inadequacy(no_card, group = adm1, ear_table) |>
  left_join(nss_states, by = "adm1") |>
  left_join(state_order, by = c("state_name" = "state")) |>
  arrange(region, state_name)

no_card_inad <- bind_rows(no_card_inad_nat, no_card_inad_state)

no_card_inad_fmt <- no_card_inad |>
  filter(!is.na(region)) |>
  mutate(
    fe = sprintf("%.0f (%.0f)", fe_inad, fe_inad_se),
    folate = sprintf("%.0f (%.0f)", folate_mcg_inad, folate_mcg_inad_se),
    vitb12 = sprintf("%.0f (%.0f)", vitb12_mcg_inad, vitb12_mcg_inad_se)
  ) |>
  select(region, state_name, fe, folate, vitb12)

make_no_card_inad_gt <- function(df_fmt) {
  df_fmt |>
    gt(rowname_col = "state_name", groupname_col = "region") |>
    cols_label(
      fe = "Iron inadequacy (%)",
      folate = "Folate inadequacy (%)",
      vitb12 = "Vitamin B12 inadequacy (%)"
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = state_name == "National")
    ) |>
    cols_hide(region)
}

gtsave(
  make_no_card_inad_gt(no_card_inad_fmt),
  filename = paste0(figure_path, "/objective_1/no_card_inadequacy.html")
)

# Table 6: full ration card type breakdown, national + every state ----------
# All 7 mutually exclusive categories (AAY, PHH, BPL, APL, SFSS, no card,
# "Other") reusing state_rc/nat_rc (Objective 1) and card_none_other (Table
# 2b) so nothing is recomputed. Since the categories partition the
# non-missing sample, the "total" column is a sanity check: it should read
# ~100 for every row.
card_type_full <- bind_rows(
  nat_rc |> mutate(state_name = "National", region = "National"),
  state_rc
) |>
  left_join(
    card_none_other |> select(state_name, none, none_se, others, others_se),
    by = "state_name"
  ) |>
  mutate(total_pct = AAY + PHH + BPL + APL + SFSS + none + others)

card_type_full_fmt <- card_type_full |>
  filter(!is.na(region)) |>
  mutate(
    AAY = sprintf("%.0f (%.0f)", AAY, AAY_se),
    PHH = sprintf("%.0f (%.0f)", PHH, PHH_se),
    BPL = sprintf("%.0f (%.0f)", BPL, BPL_se),
    APL = sprintf("%.0f (%.0f)", APL, APL_se),
    SFSS = sprintf("%.0f (%.0f)", SFSS, SFSS_se),
    none = sprintf("%.0f (%.0f)", none, none_se),
    others = sprintf("%.0f (%.0f)", others, others_se),
    total = sprintf("%.0f", total_pct)
  ) |>
  select(region, state_name, AAY, PHH, BPL, APL, SFSS, none, others, total)

make_card_type_full_gt <- function(df_fmt) {
  df_fmt |>
    gt(rowname_col = "state_name", groupname_col = "region") |>
    cols_label(
      AAY = "AAY (%)",
      PHH = "PHH (%)",
      BPL = "BPL (%)",
      APL = "APL (%)",
      SFSS = "SFSS (%)",
      none = "No card (%)",
      others = "Other (%)",
      total = "Total (%)"
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = state_name == "National")
    ) |>
    cols_hide(region)
}

gtsave(
  make_card_type_full_gt(card_type_full_fmt),
  filename = paste0(figure_path, "/objective_1/card_type_full.html")
)

# ---- Combined Word document for the three tables above ---------------------
library(flextable)
library(officer)

make_word_flextable <- function(df_fmt, col_labels) {
  names(df_fmt)[match(names(col_labels), names(df_fmt))] <- unname(col_labels)

  df_fmt |>
    rename(Region = region, State = state_name) |>
    as_grouped_data(groups = "Region") |>
    as_flextable() |>
    bold(i = ~ State == "National") |>
    autofit()
}

# Simple note: raw counts of households recording "Other" ration card type.
other_card_counts <- data.frame(
  Metric = c(
    "Households reporting \"Other\" ration card type (analysis sample, unweighted)",
    "Households reporting \"Other\" ration card type (raw ration-card file, before matching to analysis sample)"
  ),
  Value = c(
    as.character(sum(all_intake$ration_card_type == "Others", na.rm = TRUE)),
    as.character(sum(type_rc$ration_card_type == "Others", na.rm = TRUE))
  )
)

make_note_flextable <- function(df) {
  df |> flextable() |> autofit()
}

phh_aay_inad_col_labels <- c(
  elgible = "Eligible for PDS (AAY/PHH) (%)",
  fe = "Iron inadequacy (%)",
  folate = "Folate inadequacy (%)",
  vitb12 = "Vitamin B12 inadequacy (%)"
)

ration_card_summary_doc <- read_docx() |>
  body_add_par(
    "Ration card coverage, missingness, card composition, and no-card inadequacy",
    style = "heading 1"
  ) |>
  body_add_par(
    "Table 1. Proportion holding a ration card, by state",
    style = "heading 2"
  ) |>
  body_add_flextable(make_word_flextable(
    card_coverage_fmt,
    c(
      any_card = "Any ration card, incl. APL (%)",
      aay_phh = "AAY or PHH (%)"
    )
  )) |>
  body_add_break() |>
  body_add_par(
    "Table 2. Missing or refused ration card type, by state",
    style = "heading 2"
  ) |>
  body_add_flextable(make_word_flextable(
    card_missing_fmt,
    c(missing = "Missing / refused ration card type (%)")
  )) |>
  body_add_break() |>
  body_add_par(
    "Table 3. No ration card vs. \"Other\" ration card type, by state",
    style = "heading 2"
  ) |>
  body_add_par(
    "Note: raw counts of households recording \"Other\" ration card type",
    style = "heading 3"
  ) |>
  body_add_flextable(make_note_flextable(other_card_counts)) |>
  body_add_flextable(make_word_flextable(
    card_none_other_fmt,
    c(
      none = "No ration card (%)",
      others = "\"Other\" ration card type (%)"
    )
  )) |>
  body_add_break() |>
  body_add_par(
    "Table 4. Micronutrient inadequacy among households with no ration card, by state",
    style = "heading 2"
  ) |>
  body_add_flextable(make_word_flextable(
    no_card_inad_fmt,
    c(
      fe = "Iron inadequacy (%)",
      folate = "Folate inadequacy (%)",
      vitb12 = "Vitamin B12 inadequacy (%)"
    )
  )) |>
  body_add_break() |>
  body_add_par(
    "Table 5. Micronutrient inadequacy among AAY/PHH (\"priority eligible\") households, by state",
    style = "heading 2"
  ) |>
  body_add_flextable(make_word_flextable(
    phh_aay_inad_fmt,
    phh_aay_inad_col_labels
  )) |>
  body_add_break() |>
  body_add_par(
    "Table 6. Full ration card type breakdown, by state (incl. \"Total\" as a sum check)",
    style = "heading 2"
  ) |>
  body_add_flextable(make_word_flextable(
    card_type_full_fmt,
    c(
      AAY = "AAY (%)",
      PHH = "PHH (%)",
      BPL = "BPL (%)",
      APL = "APL (%)",
      SFSS = "SFSS (%)",
      none = "No card (%)",
      others = "Other (%)",
      total = "Total (%)"
    )
  ))

print(
  ration_card_summary_doc,
  target = paste0(figure_path, "/objective_1/ration_card_summary_tables.docx")
)

# ---- Standalone "changes" document: only the items from the 14 Sep 2026 ask ----
changes_doc <- read_docx() |>
  body_add_par(
    "Ration card tables - changes (14 September 2026)",
    style = "heading 1"
  ) |>
  body_add_par(
    "1. Households recording \"Other\" ration card type",
    style = "heading 2"
  ) |>
  body_add_flextable(make_note_flextable(other_card_counts)) |>
  body_add_break() |>
  body_add_par(
    "2. No ration card vs. \"Other\" ration card type, by nation and state",
    style = "heading 2"
  ) |>
  body_add_flextable(make_word_flextable(
    card_none_other_fmt,
    c(
      none = "No ration card (%)",
      others = "\"Other\" ration card type (%)"
    )
  )) |>
  body_add_break() |>
  body_add_par(
    "3. Risk of inadequacy among AAY/PHH (\"priority eligible\") households, by state",
    style = "heading 2"
  ) |>
  body_add_par(
    "Ladakh (previously missing) is highlighted below.",
    style = "Normal"
  ) |>
  body_add_flextable(
    make_word_flextable(phh_aay_inad_fmt, phh_aay_inad_col_labels) |>
      bg(i = ~ State == "Ladakh", bg = "#FFF3B0", part = "body")
  )

print(
  changes_doc,
  target = paste0(figure_path, "/objective_1/changes_14092026.docx")
)

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
# ---- Per-household fortification contributions ----------------------------
fort_contributions <- wheat |>
  select(hhid, quantity_g) |>
  mutate(
    fe_mg_fort_wf = quantity_g * wheat_spec$fe_mg / 100,
    folate_mcg_fort_wf = quantity_g * wheat_spec$folate_mcg / 100,
    vitb12_mcg_fort_wf = quantity_g * wheat_spec$vitb12_mcg / 100
  ) |>
  left_join(ind_nss2223_base_ai, by = "hhid") |>
  left_join(
    rice |>
      select(hhid, quantity_g) |>
      mutate(
        fe_mg_fort_rice = quantity_g * rice_spec$fe_mg / 100,
        folate_mcg_fort_rice = quantity_g * rice_spec$folate_mcg / 100,
        vitb12_mcg_fort_rice = quantity_g * rice_spec$vitb12_mcg / 100
      ) |>
      select(-quantity_g),
    by = "hhid"
  ) |>
  mutate(across(-c(hhid, quantity_g), ~ ifelse(is.na(.), 0, .)))


# ---- Scenario nutrient totals (base / rice / wheat / both) -----------------
df_long <- fort_contributions |>
  mutate(
    # Base
    fe_mg_base = fe_mg,
    folate_mcg_base = folate_mcg,
    vitb12_mcg_base = vitb12_mcg,

    # Rice fortified
    fe_mg_rice = fe_mg + fe_mg_fort_rice,
    folate_mcg_rice = folate_mcg + folate_mcg_fort_rice,
    vitb12_mcg_rice = vitb12_mcg + vitb12_mcg_fort_rice,

    # Wheat fortified
    fe_mg_wheat = fe_mg + fe_mg_fort_wf,
    folate_mcg_wheat = folate_mcg + folate_mcg_fort_wf,
    vitb12_mcg_wheat = vitb12_mcg + vitb12_mcg_fort_wf,

    # Both fortified
    fe_mg_both = fe_mg + fe_mg_fort_rice + fe_mg_fort_wf,
    folate_mcg_both = folate_mcg + folate_mcg_fort_rice + folate_mcg_fort_wf,
    vitb12_mcg_both = vitb12_mcg + vitb12_mcg_fort_rice + vitb12_mcg_fort_wf
  ) |>
  select(
    hhid,
    matches("^(fe_mg|folate_mcg|vitb12_mcg)_(base|rice|wheat|both)$")
  )

df_long |>
  summarise(
    mean(fe_mg_base),
    mean(fe_mg_rice),
    mean(fe_mg_wheat),
    mean(fe_mg_both)
  )


fort_scenarios <- df_long |>
  pivot_longer(
    cols = matches("^(fe_mg|folate_mcg|vitb12_mcg)_(base|rice|wheat|both)$"),
    names_to = c("nutrient", "scenario"),
    names_pattern = "(.*)_(base|rice|wheat|both)",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = nutrient,
    values_from = value
  ) |>
  left_join(ind_nss2223_hh_info, by = "hhid") |>
  left_join(ration_card_total, by = "hhid") |>
  mutate(res_quintile = paste(res, res_quintile))


fort_scenarios |>
  filter(scenario == "both") |>
  summarise(mean(fe_mg))


# ── Shared data prep pipeline ──────────────────────────────────────────────────

prep_inadequacy <- function(df) {
  scen_label <- unique(df$scenario)

  national_row <- df |>
    general_inadequacy(ear_table = ear_table) |>
    mutate(
      scenario = scen_label,
      state_name = "National",
      region = "National"
    )

  state_rows <- df |>
    general_inadequacy(group = adm1, ear_table = ear_table) |>
    mutate(scenario = scen_label) |>
    left_join(nss_states, by = "adm1") |>
    left_join(state_order, by = c("state_name" = "state")) |>
    arrange(region, state_name)

  bind_rows(national_row, state_rows)
}


# ── gt formatter ──────────────────────────────────────────────────────────────

make_inad_gt <- function(df) {
  df |>
    mutate(
      folate = sprintf("%.0f (%.0f)", folate_mcg_inad, folate_mcg_inad_se),
      vitb12 = sprintf("%.0f (%.0f)", vitb12_mcg_inad, vitb12_mcg_inad_se),
      fe = sprintf("%.0f (%.0f)", fe_inad, fe_inad_se)
    ) |>
    select(region, state_name, folate, vitb12, fe) |>
    filter(!is.na(region)) |>
    gt(rowname_col = "state_name", groupname_col = "region") |>
    cols_label(
      folate = "Folate inadequacy (%)",
      vitb12 = "Vitamin B12 inadequacy (%)",
      fe = "Iron inadequacy (%)"
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = state_name == "National")
    ) |>
    cols_hide("region")
}


# ── Single-table builder ───────────────────────────────────────────────────────

make_gt_for_group <- function(scen, card_status = NULL) {
  label <- if (is.null(card_status)) "All" else paste("Card:", card_status)

  fort_scenarios |>
    filter(
      scenario == scen,
      if (!is.null(card_status)) any_card == card_status else TRUE
    ) |>
    prep_inadequacy() |>
    make_inad_gt() |>
    tab_header(
      title = paste0("Scenario: ", scen, " (", FORT_SPEC, " spec)"),
      subtitle = paste0("Group: ", label)
    )
}


# ── Batch table builder ────────────────────────────────────────────────────────

scenarios <- c("base", "rice", "wheat", "both")

subgroups <- list(
  all = function(df) df,
  card = function(df) filter(df, any_card == "Yes")
)

gt_tables <- tidyr::expand_grid(
  scenario = scenarios,
  subgroup = names(subgroups)
) |>
  mutate(
    name = paste0(scenario, "_", subgroup),
    gt = map2(scenario, subgroup, \(scen, grp) {
      fort_scenarios |>
        filter(scenario == scen) |>
        subgroups[[grp]]() |>
        prep_inadequacy() |>
        make_inad_gt() |>
        tab_header(
          title = paste0("Scenario: ", scen, " (", FORT_SPEC, " spec)"),
          subtitle = paste0("Group: ", grp)
        )
    })
  ) |>
  select(name, gt) |>
  tibble::deframe()

# ---- Example usage ----

gt_tables$both_card


# ---- Save to HTML ---------------------------------------------------------
library(flextable)
library(officer)
library(htmltools)

gt_to_html <- function(gt_tbl) {
  HTML(as_raw_html(gt_tbl))
}

for (name in names(gt_tables)) {
  html_doc <- tagList(
    tags$h1(paste0(name, " — ", FORT_SPEC, " spec")),
    gt_to_html(gt_tables[[name]])
  )

  save_html(
    html_doc,
    file = paste0(
      figure_path,
      "objective_3/",
      name,
      "_",
      FORT_SPEC_SLUG,
      ".html"
    )
  )
}

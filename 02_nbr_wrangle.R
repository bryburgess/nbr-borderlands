# NBR Borderlands Project

# Created 2025-05-20
# Updated 2025-05-20
# Author: Bryan Burgess

# Reading and wrangling all the data. 

# Header ------------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(janitor)

data_dir <- "/Users/bryan/Documents/nbr_borderlands/raw_data/"
datawrapper <- "/Users/bryan/Documents/nbr_borderlands/datawrapper_sets/"

options(scipen = 999)

# Country list ------------------------------------------------------------

country_list <- c("Afghanistan",
                  "Bhutan",
                  "Brunei", 
                  "Brunei Darussalam",
                  "Democratic People's Republic of Korea",
                  "India", 
                  "Indonesia", 
                  "Japan", 
                  "Kazakhstan", 
                  "Kyrgyzstan", 
                  "Kyrgyz Republic",
                  "Laos", 
                  "Lao People's Democratic Republic",
                  "Malaysia",
                  "Mongolia",
                  "Myanmar",
                  "Nepal",
                  "North Korea", 
                  "Pakistan", 
                  "Philippines",
                  "Republic of Korea", 
                  "Russia", 
                  "South Korea",
                  "Tajikistan",
                  "Vietnam",
                  "Viet Nam"
)



# Read raw data ---------------------------------------------------------

setwd(data_dir)

centroids <- readRDS("borderlands_centroids.rds")

# Read TUFF and deflate to 2024
setwd("./AidDatas_Global_Chinese_Development_Finance_Dataset_Version_3_0")
tuff <- readxl::read_excel("AidDatasGlobalChineseDevelopmentFinanceDataset_v3.0.xlsx", 
                           sheet = "GCDF_3.0") |>
  janitor::clean_names() |>
  mutate(adjusted_amount_constant_usd_2024 = (adjusted_amount_constant_usd_2021 *100)/ 89.38908) |>
  select(-c("adjusted_amount_constant_usd_2021"))|> 
  filter(recipient %in% country_list)

# Public Diplomacy
setwd(data_dir)
setwd("./ChinesePublicDiplomacyExport")
pub_dip <- read_csv("ChinesePublicDiplomacy.csv") |>
  janitor::clean_names() |> 
  filter(receiving_country %in% country_list)
cis_ccs <- read_csv("Confucius-Institutes.csv") |>
  janitor::clean_names() |> 
  filter(country %in% country_list)

setwd(data_dir)
# Sipri
sipri <- read_csv("sipri-trade-register.csv", skip = 11) |>
  janitor::clean_names() |> 
  filter(recipient %in% country_list)

# Military Engagement 
mil_dip <- openxlsx::read.xlsx("China Military Diplomacy DB 2002-2024 v498 02-5-2025 LSE.xlsx",
                               sheet = 2) |>
  janitor::clean_names() |> 
  filter(partner_country %in% country_list)

# Currency Swap
# NEED BASE DATA COLLECTION

# FDI
fdi <- read_csv("CDIS_04-09-2025 12-29-05-21_timeSeries.csv") |>
  filter(`Country Name` %in% c("China, P.R.: Hong Kong", 
                             "China, P.R.: Macao",
                             "China, P.R.: Mainland"))

# Special Economic Zones
# Projects located within SEZs
sez <- openxlsx::read.xlsx("Bdls_PRCSEZ_Clean.xlsx", sheet = 2) |>
  janitor::clean_names()

# United Front Presence
# NEED BASE DATA COLLECTION 

# Judicial Engagement
judicial <- openxlsx::read.xlsx("Bdls_SPCPRCJudicialDiplomacy.xlsx", 
                                sheet = 2) |>
  janitor::clean_names() 


# Security Indicators -----------------------------------------------------
## sec_01_arms_transfers -----------------------------------------------------
# From SIPRI Documentation:
# Transfers of major conventional arms from All countries  to All countries. Deals with deliveries made for the year range 'Not specified' to 'Not specified' 
# A '?' in a column indicates uncertain data. The 'Number delivered' and the 'Year(s) of deliveries' refer only to deliveries in the selected year(s).
# An empty field for 'Number ordered' indicates that data is not yet available.
# SIPRI trend-indicator values (TIVs) are in millions.
# An empty field for 'SIPRI TIV for total order' indicates that data (on the number ordered and/or the TIV per unit) is not available.
# A '0' for 'SIPRI TIV of delivered weapons' indicates that the volume of deliveries is between 0 and 0.5 million SIPRI TIV; and an empty field indicates that no deliveries have been identified.
# Figures may not add up to stated totals due to the conventions of rounding.
# For the method used for the SIPRI TIV and explanations of the conventions; abbreviations and acronyms see <https://www.sipri.org/databases/armstransfers/sources-and-methods>.

sec_01_arms_transfers <- sipri %>%
  filter(supplier == "China",
        year_of_order > 1999,
        recipient %in% country_list) %>%
  group_by(recipient, supplier, year_of_order) %>%
  summarise(sec_01_arms_transfer_tiv = sum(sipri_tiv_for_total_order, na.rm = T),
            sec_01_arms_transfer_tiv_delivered = sum(sipri_tiv_of_delivered_weapons, na.rm = T),
            sec_01_arms_transfer_orders_ct = n()) %>%
  mutate(sec_01_missing_transfer_tiv = sec_01_arms_transfer_tiv - sec_01_arms_transfer_tiv_delivered) %>%
  select(year = year_of_order,
         everything()) %>%
  arrange(recipient, year)

## sec_02_surveillance -----------------------------------------------------

surveillance <- tuff %>%
  filter(grepl("surveill*|safe city|scanner|police", tolower(title)) | 
           grepl("surveill*|safe city|scanner|police", tolower(description)))

table(surveillance$sector_name)
# 22 projects are health projects,       
surveillance <- surveillance %>%
  filter(recommended_for_aggregates == "Yes") %>%
  filter(((((sector_name == "GOVERNMENT AND CIVIL SOCIETY" | sector_name == "OTHER MULTISECTOR") | 
           sector_name == "TRADE POLICIES AND REGULATIONS") | sector_name == "TRANSPORT AND STORAGE") |
           aid_data_record_id == 96256) | aid_data_record_id == 63248)

surveillance <- surveillance %>%
  group_by(commitment_year, recipient) %>%
  summarise(sec_02_surveillance_usd = sum(adjusted_amount_constant_usd_2024, na.rm = T),
            sec_02_surveillance_ct = n())

## sec_03_military_engagement -----------------------------------------------------
## sec_04_joint_exercises -----------------------------------------------------



# Development Indicators --------------------------------------------------
## dev_01_currency_swap -----------------------------------------------------
## dev_02_infrastructure -----------------------------------------------------
## dev_03_fdi -----------------------------------------------------
## dev_04_sez -----------------------------------------------------
# NNeed to do some crative calculating of unique SEZs here. 
# Confirm against IISS data: https://chinaconnects.iiss.org

# Civilization Indicators -------------------------------------------------
## civ_01_united_front -----------------------------------------------------
## civ_02_healthcare -----------------------------------------------------
## civ_03_hlpv -----------------------------------------------------
## civ_04_csps -----------------------------------------------------
## civ_05_judicial_engagement -----------------------------------------------------
## civ_06_ciccs -----------------------------------------------------







# Join together -----------------------------------------------------------


dash_data <- left_join(centroids, pub_dip, by = c("country" = "receiving_country")) 





# Write data --------------------------------------------------------------
setwd(datawrapper)

# v0.1 2024-04-23 
# write_csv(dash_data, "borderlands_datawrapper_v0.1.csv")

library(tidyverse)
library(openxlsx)
library(odeqIRtools)
library(odeqtmdl)
library(duckdb)


# 
# filepath <- 'C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Code Outputs/Draft Outputs/'
# 
# # Filenames -------------------------------------------------------------------------------------------------------
# 
# bact_coast <- 'Done-bacteria coast contact- 2024-01-25.xlsx'
# bact_fresh <- 'LM-bacteria_freshwater_contact-2024-01-24.xlsx'
# 
# chl <- 'LM-chl-a-2024-01-25.xlsx'
# 
# DO <- 'TP-DO-2024-01-25.xlsx'
# 
# pH <- 'LM-pH-2024-01-25.xlsx'
# 
# temp <- 'KSE-temperature-2024-01-26.xlsx'
# 
# tox_al <- 'DB-Tox_AL.xlsx'
# 
# tox_hh <- 'DB-Tox_HH-2024-01-26.xlsx'
# 
# turb <- 'LM-turbidity-2024-01-26.xlsx'
# 
# biocriteria <- 'LM-Biocriteria-2024-01-25.xlsx'
# 
# non_R <- 'Done-non_R-2024-01-25.xlsx'
# 
# 
# 
# # Pull data in ----------------------------------------------------------------------------------------------------
# 
# 
# 
# ## Bacteria --------------------------------------------------------------------------------------------------------
# 
# 
# 
# au_bact_coast <- read.xlsx(paste0(filepath, bact_coast),
#                            sheet = 'AU_Decisions') |> 
#   mutate(across(1:21, .fns = as.character))
# 
# # gnis_bact_coast <- read.xlsx(paste0(filepath, bact_coast),
# #                              sheet = 'WS GNIS categorization')
# 
# au_bact_fresh <- read.xlsx(paste0(filepath, bact_fresh),
#                            sheet = 'AU_Decisions') |> 
#   mutate(across(1:21, .fns = as.character))
# 
# gnis_bact_fresh <- read.xlsx(paste0(filepath, bact_fresh),
#                              sheet = 'WS GNIS categorization') |> 
#   mutate(across(1:24, .fns = as.character))
# 
# 
# ## Chl -------------------------------------------------------------------------------------------------------------
# 
# 
# au_chl <- read.xlsx(paste0(filepath, chl),
#                            sheet = 'AU_Decisions')|> 
#   mutate(across(1:21, .fns = as.character))
# 
# gnis_chl <- read.xlsx(paste0(filepath, chl),
#                              sheet = 'WS GNIS categorization')|> 
#   mutate(across(1:24, .fns = as.character))
# 
# 
# ## DO --------------------------------------------------------------------------------------------------------------
# 
# au_do <- read.xlsx(paste0(filepath, DO),
#                     sheet = 'AU_Decisions') |> 
#   mutate(across(1:21, .fns = as.character))
# 
# gnis_do <- read.xlsx(paste0(filepath, DO),
#                       sheet = 'WS GNIS categorization')|> 
#   mutate(across(1:24, .fns = as.character))
# 
# 
# ## pH --------------------------------------------------------------------------------------------------------------
# 
# au_pH <- read.xlsx(paste0(filepath, pH),
#                    sheet = 'AU_Decisions')|> 
#   mutate(across(1:21, .fns = as.character))
# 
# gnis_pH <- read.xlsx(paste0(filepath, pH),
#                      sheet = 'WS GNIS categorization') |> 
#   mutate(across(1:24, .fns = as.character))
# 
# 
# ## temperature -----------------------------------------------------------------------------------------------------
# 
# 
# au_temp <- read.xlsx(paste0(filepath, temp),
#                    sheet = 'AU_Decisions')|> 
#   mutate(Char_Name = 'Temperature, water') |> 
#   mutate(across(1:21, .fns = as.character))
# 
# gnis_temp <- read.xlsx(paste0(filepath, temp),
#                      sheet = 'WS_GNIS_categorization')  |> 
#   mutate(Char_Name = 'Temperature, water') |> 
#   mutate(across(1:24, .fns = as.character))
# 
# ## tox AL -----------------------------------------------------------------------------------------------------
# 
# 
# au_tox_al <- read.xlsx(paste0(filepath, tox_al),
#                      sheet = 'AU_Decisions') |> 
#   mutate(across(1:21, .fns = as.character))
# 
# gnis_tox_al <- read.xlsx(paste0(filepath, tox_al),
#                        sheet = 'GNIS_cat')  |> 
#   mutate(across(1:24, .fns = as.character))
# 
# ## tox HH -----------------------------------------------------------------------------------------------------
# 
# 
# au_tox_hh <- read.xlsx(paste0(filepath, tox_hh),
#                        sheet = 'AU_Decisions')|> 
#   mutate(across(1:21, .fns = as.character))
# 
# gnis_tox_hh <- read.xlsx(paste0(filepath, tox_hh),
#                          sheet = 'WS GNIS categorization') |> 
#   mutate(across(1:24, .fns = as.character))
# 
# 
# 
# ## turbidity -----------------------------------------------------------------------------------------------------
# 
# 
# au_turb <- read.xlsx(paste0(filepath, turb),
#                        sheet = 'AU_Decisions') |> 
#   mutate(across(1:21, .fns = as.character))
# 
# gnis_turb <- read.xlsx(paste0(filepath, turb),
#                          sheet = 'WS GNIS categorization') |> 
#   mutate(across(1:16, .fns = as.character))
# 
# 
# 
# 
# # Biocriteria -----------------------------------------------------------------------------------------------------
# 
# au_biocriteria <- read.xlsx(paste0(filepath, biocriteria),
#                        sheet = 'AU_Decisions')|> 
#   mutate(across(1:21, .fns = as.character))
# 
# gnis_biocriteria <- read.xlsx(paste0(filepath, biocriteria),
#                          sheet = 'WS GNIS categorization') |> 
#   mutate(across(1:24, .fns = as.character))
# 
# 
# # non_R -----------------------------------------------------------------------------------------------------------
# au_nonR <- read.xlsx(paste0(filepath, non_R),
#                             sheet = 'AU_Decisions')|> 
#   mutate(across(1:21, .fns = as.character))
# 
# gnis_nonR <- read.xlsx(paste0(filepath, non_R),
#                               sheet = 'WS GNIS categorization') |> 
#   mutate(across(1:24, .fns = as.character))
# 
# 
# 
# # Put all together ------------------------------------------------------------------------------------------------
# 
# 
# AU_decisions <- bind_rows(au_bact_coast, au_bact_fresh, au_chl, au_do, au_pH, au_temp, au_tox_al, 
#                           au_tox_hh, au_turb,au_biocriteria, au_nonR)
# 
# 
# 
# # Get unassessed pollutants to move forward -----------------------------------------------------------------------
# 
# 
# 
# assessed_polluids <- AU_decisions$Pollu_ID 
# 
# 
# 
# unassessed_params <- odeqIRtools::prev_list_AU |> 
#   filter(!Pollu_ID %in% assessed_polluids) |>
#   rename(Char_Name = Pollutant) |> 
#   mutate(final_AU_cat = prev_category,
#          Rationale = prev_rationale,
#          status_change = "No change in status- No new assessment") |> 
#   join_TMDL(type = 'AU')
# 
# AU_decisions_joined <- AU_decisions |> 
#   bind_rows(unassessed_params)
# 
# 
# 
# # Missing GNIS ----------------------------------------------------------------------------------------------------
# 
# antijoin <- odeqIRtools::prev_list_AU |> 
#   anti_join(AU_decisions_joined, by = join_by(AU_ID, Pollu_ID, wqstd_code, period)) |> 
#   rename(Char_Name = Pollutant) |> 
#   mutate(final_AU_cat = prev_category,
#          Rationale = prev_rationale,
#          status_change = "No change in status- No new assessment") |> 
#   join_TMDL(type = 'AU')
# 
# 
# AU_decisions_joined <- AU_decisions_joined |> 
#   bind_rows(antijoin)
# 
# 
# 
# 
# # pollutant rename ------------------------------------------------------------------------------------------------
# #open connection to database
# con <- DBI::dbConnect(odbc::odbc(), 'IR_Dev')
# 
# 
# db_qry <- glue::glue_sql( "SELECT distinct [Pollu_ID]
#       ,[Pollutant_DEQ WQS] as Char_Name
#   FROM [IntegratedReport].[dbo].[LU_Pollutant]", .con = con)
# 
# # Send query to database and return with the data
# Char_rename <-  DBI::dbGetQuery(con, db_qry)
# 
# Char_rename <- Char_rename |> 
#   mutate(Pollu_ID = as.character(Pollu_ID))
# 
# 
# 
# AU_decisions <- AU_decisions_joined |> 
#   select(-Char_Name) |> 
#   left_join(Char_rename) |> 
#   relocate(Char_Name, .after = AU_ID)|> 
#   select(-AU_Name, -AU_UseCode, -HUC12) |> 
#   join_AU_info() |> 
#   join_hucs() |> 
#   arrange(AU_ID, Char_Name)
# 
# 
# 
# # Get assessment labels -------------------------------------------------------------------------------------------
# con <- DBI::dbConnect(odbc::odbc(), 'IR_Dev')
# 
# 
# wqstd_info <- tbl(con, "LU_Wqstd_Code") |> 
#   mutate(wqstd_code = as.character(wqstd_code) ) |> 
#   rename('Assessment' = 'wqstd') |> 
#   collect()
# 
# DBI::dbDisconnect(con)
# 
# AU_decisions <- AU_decisions |> 
#   left_join(wqstd_info) |> 
#   relocate(Assessment, .after = Char_Name)


load('C:/Users/tpritch/OneDrive - Oregon/R Projects/IR_2024/draft_list/draft_list.Rdata')

AU_decisions <- print_list$AU_decisions
GNIS_decisions <- print_list$GNIS_decisions

save(AU_decisions, file = 'data/AU_decisions.Rdata')


# Write duckdb database -------------------------------------------------------------------------------------------


con <- dbConnect(duckdb(), dbdir = "data/decisions.duckdb")
dbWriteTable(con, "AU_decisions", AU_decisions, overwrite = TRUE)
duckdb::dbDisconnect(con, shutdown=TRUE)





# GNIS ------------------------------------------------------------------------------------------------------------
# 
# GNIS_Decisions <- bind_rows(gnis_bact_fresh, gnis_biocriteria, gnis_chl, gnis_do, gnis_nonR,
#                             gnis_pH, gnis_temp, gnis_tox_al, gnis_tox_hh, gnis_turb)
# 
# 
# 
# ## Get unassessed pollutants to move forward -----------------------------------------------------------------------
# 
# 
# 
# assessed_polluids <- GNIS_Decisions$Pollu_ID 
# 
# 
# 
# unassessed_params <- odeqIRtools::prev_list_GNIS |> 
#   filter(!Pollu_ID %in% assessed_polluids) |>
#   rename(Char_Name = Pollutant) |> 
#   mutate(final_GNIS_cat = prev_GNIS_category,
#          Rationale_GNIS = prev_GNIS_rationale,
#          status_change = "No change in status- No new assessment") |> 
#   join_TMDL(type = 'GNIS')
# 
# GNIS_decisions <- GNIS_Decisions |> 
#   bind_rows(unassessed_params)
# 
# 
# antijoin2 <- odeqIRtools::prev_list_GNIS |> 
#   anti_join(GNIS_decisions, by = join_by(AU_ID, Pollu_ID, wqstd_code, period)) |> 
#   rename(Char_Name = Pollutant) |> 
#   mutate(final_GNIS_cat = prev_GNIS_category,
#          Rationale_GNIS = prev_GNIS_rationale,
#          status_change = "No change in status- No new assessment") |> 
#   join_TMDL(type = 'GNIS')
# 
# 
# GNIS_decisions <- GNIS_Decisions |> 
#   bind_rows(antijoin2)



con <- dbConnect(duckdb(), dbdir = "data/decisions.duckdb")
dbWriteTable(con, "GNIS_decisions", GNIS_decisions, overwrite = TRUE)
dbDisconnect(con, shutdown=TRUE)




#test 
# 
# con <- dbConnect(duckdb(), dbdir = "data/decisions.duckdb")
# 
# test <- tbl(con, "AU_decisions") |> 
#   collect()
# 
# duckdb::dbDisconnect(con, shutdown=TRUE)






# Prep_nesting ----------------------------------------------------------------------------------------------------
source('functions/nesting_functions.R')

con <- dbConnect(duckdb(), dbdir = "data/decisions.duckdb")

AU_decisions <- tbl(con, 'AU_decisions') |> 
  collect()

GNIS_decisions <- tbl(con, 'GNIS_decisions') |> 
  collect()
dbDisconnect(con, shutdown=TRUE)


nested <-  create_nested_exam_table(AU_decisions,GNIS_decisions, joinby = c('AU_ID', 'Pollu_ID', 'wqstd_code', 'period'))

nest_data <- nested |> 
  select(AU_ID, Pollu_ID, wqstd_code, period, `_details`)


save(nest_data, file = 'data/nest_data.Rdata')
load('data/nest_data.Rdata')

library(tidyverse)
library(DBI)
library(openxlsx)
library(duckdb)


# Permit Data -----------------------------------------------------------------------------------------------------




# Get Org/param combos --------------------------------------------------------------------------------------------

con <- DBI::dbConnect(odbc::odbc(), "IR_Dev")


org_param <- tbl(con, "InputRaw") |> 
  select(AU_ID, OrganizationID, Pollu_ID, wqstd_code) |> 
  distinct() |> 
  collect()

DBI::dbDisconnect(con)


# Get permit orgs -------------------------------------------------------------------------------------------------


permit_orgs_import <- read.xlsx("C:/Users/tpritch/Oregon/DEQ - Integrated Report - IR 2024/Code Outputs/InputRaw Organizations.xlsx")

permit_orgs <- permit_orgs_import |> 
  filter(`permit?` == TRUE) |> 
  select(OrganizationID) |> 
  mutate(permitee = TRUE) |> 
  filter(OrganizationID != 'OREGONDEQ')


# combine ---------------------------------------------------------------------------------------------------------

permitting_data <- org_param |> 
  left_join(permit_orgs) |> 
  mutate(permitee = case_when(permitee ~ TRUE,
                              TRUE ~ FALSE)) |> 
  mutate(Pollu_ID = as.character(Pollu_ID),
         wqstd_code = as.character(wqstd_code)) |> 
  group_by(AU_ID, Pollu_ID, wqstd_code) |> 
  summarise(orgs = stringr::str_c(unique(OrganizationID), collapse = "; "),
            permitee = case_when(any(permitee == TRUE) ~ TRUE,
                                 TRUE ~ FALSE))


save(permitting_data, file = 'data/permitting_data.Rdata')



# join ------------------------------------------------------------------------------------------------------------


con <- dbConnect(duckdb(), dbdir = "data/decisions.duckdb")

dec <- tbl(con, 'AU_decisions') |> 
  collect() |> 
  left_join(permitting_data) |> 
  mutate(permitee = case_when(is.na(permitee) ~ FALSE,
                              TRUE ~ permitee ))

dbWriteTable(con, "AU_decisions", dec, overwrite = TRUE)
duckdb::dbDisconnect(con, shutdown=TRUE)


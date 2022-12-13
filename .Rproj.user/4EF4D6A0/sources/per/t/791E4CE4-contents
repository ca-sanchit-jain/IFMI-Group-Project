library(readr)
library(data.table)

top_shareholders <- read_delim("Prowess Data/76729_1_20_20221212_125039_dat.txt",
delim = "|", escape_double = FALSE, col_types = cols(hpc_date = col_date(format = "%d-%m-%Y")), trim_ws = TRUE)

dividend_data <- read_delim("Prowess Data/76729_1_125_20221212_125039_dat.txt",
delim = "|", escape_double = FALSE, col_types = cols(div_announcement_date = col_date(format = "%d-%m-%Y")),
trim_ws = TRUE)

############## Variable IDs ############
fin_data_qtrly <- read_delim("Prowess Data/76729_1_100_20221212_125039_dat.txt",
                             delim = "|", escape_double = FALSE, col_types = cols(ciq_ntrm_date = col_date(format = "%d-%m-%Y"),
                                                                                  ciq_ntrm_share_appln_suspense = col_double(),
                                                                                  ciq_ntrm_misc_exp_not_written_off = col_double()),
                             trim_ws = TRUE)
fin_data_qtrly_dt= data.table::data.table(fin_data_qtrly)
rm(fin_data_qtrly)

ownrship_data <- read_delim("Prowess Data/76729_1_7_20221212_125039_dat.txt",
                            delim = "|", escape_double = FALSE, col_types = cols(coprd_date = col_date(format = "%d-%m-%Y")),
                            trim_ws = TRUE)
ownrship_data_dt = data.table::data.table(ownrship_data)
rm(ownrship_data)
fin_data_qtrly_dt <- merge(fin_data_qtrly_dt, ownrship_data_dt, by.x = c("ciq_interim_cocode","ciq_ntrm_date"), by.y = c("coprd_company_code","coprd_date"), all.x = TRUE)
rm(ownrship_data_dt)

employee_count <- read_delim("Prowess Data/76729_1_275_20221212_125039_dat.txt",
                             delim = "|", escape_double = FALSE, col_types = cols(indasca_finance1_year = col_date(format = "%d-%m-%Y")),
                             trim_ws = TRUE)
employee_count_dt = data.table::data.table(employee_count)
rm(employee_count)
employee_count_dt = na.omit(employee_count_dt)
fin_data_qtrly_dt <- merge(fin_data_qtrly_dt, employee_count_dt[,c(1,3,5)], by.x = c("ciq_interim_cocode","ciq_ntrm_date"), by.y = c("indasca_finance1_cocode","indasca_finance1_year"), all.x = TRUE)
rm(employee_count_dt)

## Return Data ####
return_data <- read_delim("Prowess Data/76729_1_120_20221212_125039_dat.txt",
                          delim = "|", escape_double = FALSE, col_types = cols(co_stkdate = col_date(format = "%d-%m-%Y")),
                          trim_ws = TRUE)
return_data_dt = data.table::data.table(return_data)
rm(return_data)


## Adding market cap and book value data 
fin_data_qtrly_dt <- merge(fin_data_qtrly_dt, return_data_dt[,c(1,3,7,10)], by.x = c("ciq_interim_cocode","ciq_ntrm_date"), by.y = c("co_code","co_stkdate"))
fin_data_qtrly_dt[order(fin_data_qtrly_dt[,1],DT[,2])]
fin_data_qtrly_dt[,date_diff := ciq_ntrm_date - shift(ciq_ntrm_date), by = ciq_interim_cocode] ## for compounding quarterly changes

fin_data_qtrly_dt[, book_market := equity_bv_on_stkdate/nse_market_cap]

for (i in 1:nrow(fin_data_qtrly_dt)) {
  print (i)
  fin_data_qtrly_dt$total_assets[i] = sum(fin_data_qtrly_dt$ciq_ntrm_net_fixed_assets[i] , fin_data_qtrly_dt$ciq_ntrm_cap_work_in_progress[i] , fin_data_qtrly_dt$ciq_ntrm_investments[i] , fin_data_qtrly_dt$ciq_ntrm_other_non_current_assets[i] , fin_data_qtrly_dt$ciq_ntrm_curr_assets_loans_n_advns[i] , fin_data_qtrly_dt$ciq_ntrm_other_assets[i] , fin_data_qtrly_dt$ciq_ntrm_deferred_tax_asst[i] , fin_data_qtrly_dt$ciq_ntrm_misc_exp_not_written_off[i], na.rm = TRUE)
  }
rm(i)
fin_data_qtrly_dt[total_assets > 0, cash_holding := ciq_ntrm_cash_and_bank_balances/total_assets]
fin_data_qtrly_dt[!is.na(total_assets), cashproductvity := (nse_market_cap + ciq_ntrm_long_term_borrowings - total_assets)/ciq_ntrm_cash_and_bank_balances]
fin_data_qtrly_dt[!is.na(ciq_ntrm_net_sales) & !is.na(shift(ciq_ntrm_net_sales) & date_diff < 95), change_in_sales := ciq_ntrm_net_sales - shift(ciq_ntrm_net_sales), by= ciq_interim_cocode]

saveRDS(fin_data_qtrly_dt, "fin_data_qtrly_dt.rds")

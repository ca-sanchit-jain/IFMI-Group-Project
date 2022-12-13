library(readr)
library(data.table)

return_data <- read_delim("Prowess Data/76729_1_120_20221212_125039_dat.txt",
delim = "|", escape_double = FALSE, col_types = cols(co_stkdate = col_date(format = "%d-%m-%Y")),
trim_ws = TRUE)

top_shareholders <- read_delim("Prowess Data/76729_1_20_20221212_125039_dat.txt",
delim = "|", escape_double = FALSE, col_types = cols(hpc_date = col_date(format = "%d-%m-%Y")), trim_ws = TRUE)


ownrship_data <- read_delim("Prowess Data/76729_1_7_20221212_125039_dat.txt",
delim = "|", escape_double = FALSE, col_types = cols(coprd_date = col_date(format = "%d-%m-%Y")),
trim_ws = TRUE)

fin_data_qtrly <- read_delim("Prowess Data/76729_1_100_20221212_125039_dat.txt",
delim = "|", escape_double = FALSE, col_types = cols(ciq_ntrm_date = col_date(format = "%d-%m-%Y"),
ciq_ntrm_share_appln_suspense = col_double(),
ciq_ntrm_misc_exp_not_written_off = col_double()),
trim_ws = TRUE)


fin_data_ann_igaap <- read_delim("Prowess Data/76729_1_75_20221212_125039_dat.txt",
delim = "|", escape_double = FALSE, col_types = cols(ca_finance1_year = col_date(format = "%d-%m-%Y")),
trim_ws = TRUE)


dividend_data <- read_delim("Prowess Data/76729_1_125_20221212_125039_dat.txt",
delim = "|", escape_double = FALSE, col_types = cols(div_announcement_date = col_date(format = "%d-%m-%Y")),
trim_ws = TRUE)

# beta <- read_delim("Prowess Data/76729_1_150_20221212_125039_dat.txt",
# delim = "|", escape_double = FALSE, col_types = cols(cobeta_from_date = col_date(format = "%d-%m-%Y"),
# cobeta_to_date = col_date(format = "%d-%m-%Y")),
# trim_ws = TRUE)

employee_count <- read_delim("Prowess Data/76729_1_275_20221212_125039_dat.txt",
delim = "|", escape_double = FALSE, col_types = cols(indasca_finance1_year = col_date(format = "%d-%m-%Y")),
trim_ws = TRUE)

industry_data <- read_delim("Prowess Data/76729_1_5_20221212_125039_dat.txt",
delim = "|", escape_double = FALSE, trim_ws = TRUE)




############## Variable IDs ############

fin_data_qtrly <- read_delim("Prowess Data/76729_1_100_20221212_125039_dat.txt",
                             delim = "|", escape_double = FALSE, col_types = cols(ciq_ntrm_date = col_date(format = "%d-%m-%Y"),
                                                                                  ciq_ntrm_share_appln_suspense = col_double(),
                                                                                  ciq_ntrm_misc_exp_not_written_off = col_double()),
                             trim_ws = TRUE)
fin_data_qtrly_dt= data.table::data.table(fin_data_qtrly)
rm(fin_data_qtrly)
fin_data_qtrly_dt[,total_assets:= sum(ciq_ntrm_net_fixed_assets , ciq_ntrm_cap_work_in_progress , ciq_ntrm_investments , ciq_ntrm_other_non_current_assets , ciq_ntrm_curr_assets_loans_n_advns , ciq_ntrm_other_assets , ciq_ntrm_deferred_tax_asst , ciq_ntrm_misc_exp_not_written_off, na.rm=TRUE)]
fin_data_qtrly_dt[, cash_holding := ciq_ntrm_cash_and_bank_balances/total_assets]

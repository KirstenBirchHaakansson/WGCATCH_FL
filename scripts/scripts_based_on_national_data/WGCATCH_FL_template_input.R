

#Script for making intput to the WGCATCH FL template 2017

library(dplyr)
library(readxl)

dir_data <- "Q:/dfad/users/kibi/data/WGCATCH/"
dir_data_out <- "Q:/mynd/WG/WGCATCH/2018/WGCATCH_FL/output/"
dir_template <- "Q:/mynd/WG/WGCATCH/2018/WGCATCH_FL/templates/" 

options(scipen = 9999, digit = 4)


#Read in data 

dfad <- readRDS(paste(dir_data, "WGCATCH_FL_DFAD_input.rds", sep = ""))
afr <- readRDS(paste(dir_data, "WGCATCH_FL_afr_input.rds", sep = ""))

#Making the summary sheet
#Reporting Country	Flag country	Landing country	Year	Source	Total Landings (kg)	Total number vessels

#Keep only relevant
summ <- filter(afr, fnat == "DNK" | nationl == "DNK")

summ_1 <- mutate(summ, rep_ctry = "DNK", flg_ctry = fnat, land_ctry = nationl, 
                 source = ifelse(flg_ctry == "DNK", "Logbooks + sale slips", "Sale slips"))

summ_2 <- summarise(group_by(summ_1, rep_ctry, flg_ctry, land_ctry, year, source), kg = round(sum(hel, na.rm = T), digits = 0), no_vessel = length(unique(fid)), 
                        no_landing_event = length(unique(paste(fid, ldato))))

summ_3 <- mutate(summ_2, no_vessels = ifelse(no_vessel < 5, "<5", as.character(no_vessel)), 
                     no_landing_events = ifelse(no_landing_event < 5, "<5", as.character(no_landing_event)))

summ_final <- select(summ_3, -no_vessel, -no_landing_event)

write.csv(summ_final, paste(dir_data_out, "WGCATCH_FL_template_input_summary.csv", sep = ""), row.names = F)

#National vessels
#Reporting Country	Year	Quarter	Flag Vessel Country	Landing Country 	Species	Area	Metier04	Volume landed (kg)	Do you have timely access to logbook data of the vessels?	Do you have timely access to sales data from the vessels?	Is this component sampled?	Sampling programme	Sampling mode	Sampled components	Sampled variables	If sampled, are data shared with the landing country?	If sampled, are the samples used in estimation?	If stock sampled, do you upload the samples to the RDB?	If stock sampled, do you upload the estimates to Intercatch?	Your expert opinion on this component

nat <- filter(dfad, fnat == "DNK")

nat_1 <- mutate(nat, rep_ctry = "DNK", qtr = paste("Q", quarter, sep = ""), flg_ctry = fnat, land_ctry = nationl, spp = wormsLatin, 
                metier04 = ifelse(level_6 == "No_logbook6", "No_logbook", substr(merged_metier_level6, 1, 3)))

nat_2 <- summarise(group_by(nat_1, rep_ctry, year, qtr, flg_ctry, land_ctry, spp, area, stock, metier04), kg = round(sum(hel, na.rm = T)))

#Delete lines where kg = 0 and no.stock - except pandalus and sandeel

nat_3 <- filter(nat_2, kg != 0 & stock %in% c("her.27.3a47d", "mac.27.nea", "boc.27.6-8", "cod.27.24-32", "pra.27.4a", "pra.27.3a4a", "pra.27.1-2"))

#Access to logbook and sale slips
nat_4 <- mutate(nat_3, logbook_access = ifelse(metier04 == "No_logbook", "no logbook", "yes"),
                  sales_access = "yes")        


#Sampling "her.27.3a47d", "mac.27.nea", "boc.27.6-8"
nat_5 <- mutate(nat_4, 
                sampled = ifelse(land_ctry != "DNK" & stock %in% c("her.27.3a47d", "mac.27.nea", "boc.27.6-8"), "no",
                                 ifelse(stock == "pra.27.1-2", "no", "yes")), 
                samp_program = ifelse(sampled == "no", "not sampled", "national programme"),
                samp_mode = ifelse(sampled == "no", "not sampled",
                                   ifelse(stock == "cod.27.24-32" & land_ctry == "DNK", "at-sea & at-market",
                                          ifelse(stock == "cod.27.24-32" & land_ctry != "DNK", "at-sea",
                                                 ifelse(stock %in% c("her.27.3a47d", "mac.27.nea", "boc.27.6-8") & land_ctry == "DNK", "at-market",
                                                                      ifelse(stock %in% c("pra.27.4a", "pra.27.3a4a"), "at-sea", NA))))),
                samp_comp = ifelse(samp_mode == "not sampled", "not sampled",
                                   ifelse(samp_mode == "at-market", "landings only", 
                                          ifelse(samp_mode == "at-sea" & stock %in% c("pra.27.4a", "pra.27.3a4a"), "landings and discards",
                                                 ifelse(samp_mode == "at-sea" & stock %in% c("cod.27.24-32"), "discard only",
                                                        ifelse(samp_mode == "at-sea & at-market", "landings and discards", NA))))),
                samp_var = ifelse(samp_mode == "not sampled", "not sampled", "volume and length and age structure"),
                samp_shared = ifelse(samp_mode == "not sampled", "not sampled", "no"),
                estimation = ifelse(samp_mode == "not sampled", "not sampled", "yes, flag country do the estimation"),
                rdb = ifelse(samp_mode == "not sampled", "not sampled", "yes"))

test_nat <- distinct(data.frame(nat_5), rep_ctry, flg_ctry, land_ctry, spp, stock, logbook_access, sales_access, sampled, samp_program, samp_mode, samp_comp,
                     samp_var, samp_shared, estimation, rdb)

write.csv(nat_5, paste(dir_data_out, "WGCATCH_FL_template_input_nat_vessels.csv", sep = ""), row.names = F)

#Foreign vessels
fore <- filter(afr, fnat != "DNK")

fore_1 <- mutate(fore, rep_ctry = "DNK", qtr = paste("Q", quarter, sep = ""), flg_ctry = fnat, land_ctry = nationl, spp = wormsLatin, 
                metier04 = "UNK")

fore_2 <- summarise(group_by(fore_1, rep_ctry, year, qtr, flg_ctry, land_ctry, spp, area, stock, metier04), kg = round(sum(hel, na.rm = T)))

#Delete lines where kg = 0 and no.stock - except pandalus and sandeel

fore_3 <- filter(fore_2, kg != 0 & stock %in% c("her.27.3a47d", "whb.27.1-91214", "mac.27.nea", "san.sa.6", "boc.27.6-8", "her.27.20-24", 
                                              "her.27.25-2932", "cod.27.24-32", "spr.27.3a", "spr.27.2232", "spr.27.4", 
                                              "san.sa.2r", "san.sa.3r", "san.sa.1r", "san.sa.4", "spr.27.7de", "pra.27.4a", "pra.27.3a4a", "pra.27.1-2") 
                | kg != 0 &  spp %in% c("Ammodytes"))

#Access to logbook and sale slips
fore_4 <- mutate(fore_3, logbook_access = ifelse(flg_ctry == "SWE", "probably", "no"), sales_access = "yes")        

write.csv(fore_4, paste(dir_data_out, "WGCATCH_FL_template_input_foreign_vessels.csv", sep = ""), row.names = F)




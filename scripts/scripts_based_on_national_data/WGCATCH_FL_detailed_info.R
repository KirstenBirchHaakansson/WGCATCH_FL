

#Script for stock specific info about Danish landings

library(dplyr)
library(lubridate)
library(haven)
library(tidyr)

dir_data <- "Q:/dfad/users/kibi/data/WGCATCH/"
dir_data_out <- "Q:/mynd/WG/WGCATCH/2018/WGCATCH_FL/output/"

options(scipen = 9999, digit = 4)


#Read in data 

dfad <- readRDS(paste(dir_data, "WGCATCH_FL_DFAD_input.rds", sep = ""))
dfad <- mutate(dfad, id = as.numeric(rownames(dfad)))

buyer_all <- read_sas("Q:/mynd/SAS Library/FD_Format/opknvn.sas7bdat")

buyer <- distinct(read_sas("Q:/mynd/SAS Library/FD_Format/opknvn.sas7bdat"), opknr, opknvn, opknat, opstartd, opslutd)
buyer <- mutate(buyer, opslutd_new = ifelse(is.na(opslutd), Sys.Date(), opslutd))
buyer$opslutd_new <- as.Date(buyer$opslutd_new, origin = "1970-01-01")

buyer <- arrange(distinct(buyer, opknr, opknvn, opknat, opstartd, opslutd_new), opknr, opstartd, opslutd_new)

buyer_selected <- slice(group_by(buyer, opknr, opstartd, opslutd_new), 1) #Some opknr has more than one row with same dates, but with differnct opknr - this just select one of them

dfad_1 <- left_join(dfad, buyer_selected)

dfad_2 <- arrange(filter(mutate(dfad_1, mark = ifelse(is.na(opknvn), 1, 
                                                      ifelse(ldato >= opstartd & ldato < opslutd_new, 1, 0))), mark == 1), id)

nrow(dfad)-nrow(dfad_2)
sum(dfad$hel)-sum(dfad_2$hel) #missing a couple of observations, this is due to the fact, that the buyer wasn't found in list 

dfad_2 <- mutate(dfad_2, id_new = as.numeric(rownames(dfad_2)))

test_2 <- filter(dfad_2, id != id_new)
test_3 <- filter(dfad_2, between(id, 554499, 554503))
test_4 <- filter(dfad_1, id == 554500)


#compare sampCtry with landCtry

dfad_samp_land <- filter(dfad_2, !(is.na(sampCtry)) | !(is.na(landCtry)))
dfad_samp_land$year <- as.character(dfad_samp_land$year)

samp_land <- summarise(group_by(dfad_samp_land, year, landCtry, sampCtry, stock), ton = round(sum(hel/1000), digits = 0))

dfad_samp_land_mean <- mutate(dfad_samp_land, year = "2014-2017")
samp_land_mean <- summarise(group_by(dfad_samp_land_mean, year, landCtry, sampCtry, stock), ton = round((sum(hel/1000))/4, digits = 0))

samp_land_1 <- bind_rows(samp_land, samp_land_mean)

samp_land_2 <- spread(samp_land_1, sampCtry, value = ton, fill = 0)

samp_land_2$total <- samp_land_2$DEU+samp_land_2$DNK+samp_land_2$FRA+samp_land_2$FRO+samp_land_2$GBR+samp_land_2$GRL+samp_land_2$IRL+samp_land_2$ISL+samp_land_2$NLD+samp_land_2$NOR+samp_land_2$POL+samp_land_2$SWE

write.csv2(samp_land_2, paste(dir_data_out, "sampCtry_vs_landCtry.csv", sep = ""), row.names = F)

#Landin harbours
dfad_land <- filter(dfad_2, !(is.na(sampCtry)) | !(is.na(landCtry)))
dfad_land_deu <- filter(dfad_land, stock == "mac.27.nea")

dfad_land_deu_1 <- summarise(group_by(dfad_land_deu, year, sampLoc, landLoc, opknvn, metier_level6_ret, stock), ton = round(sum(hel/1000), digits = 0), no_trips = length(unique(match)))

#vessels ladning in DEU
dfad_deu_fid <- filter(dfad_2, fid %in% unique(dfad_land_deu$fid) & stock == "her.27.3a47d")

unique(dfad_deu_fid$fid)

dfad_deu_fid_sum <- summarise(group_by(dfad_deu_fid, year, quarter, sampCtry, stock), ton = round(sum(hel/1000), digits = 0), no_trips = length(unique(match)))

#Landings per mertier

sum_metier <- summarise(group_by(dfad_2, year, sampCtry, stock, metier_level6_ret, area), ton = round(sum(hel/1000), digits = 0))

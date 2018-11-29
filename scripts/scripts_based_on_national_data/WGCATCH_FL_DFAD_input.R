

#WGCATCH - script for making data for national vessels


library(dplyr)
library(haven)
library(fishPiCodes)
library(lubridate)

options(scipen = 999)

yearF <- c(2014:2017)

dir_out <- "Q:/dfad/users/kibi/data/WGCATCH/"

#Read in dfad

dfad <- c()

for (i in yearF) {
  
  dfad_0 <- read_sas(paste("Q:/dfad/data/Data/udvidet_data/dfad_udvidet", i, ".sas7bdat", sep = ""))
  dfad_0 <- select(dfad_0, fid, match, fngdato, fvd, afrfvd, square, redskb, maske, art, sort, ltilst, forarb, anvend, ldato, afshavn, lplads, koebdag,
                     koebsted, afstid, anktid, rejsetid, havtime, havdag_tur, havdag_fvd, opknr, zone, btype, rectype, fangst, hel, vgt, vrd, match_alle, GU,
                     BMS, oal, btbrt, kw, max, square_ret, DFADfvd_ret, level_6, metier_level6_ret, metier_ret_mrk, ihovedart, FDFmark)
  names(dfad_0) <- tolower(names(dfad_0))
  dfad_0$hel[is.na(dfad_0$hel)] <- 0
  
  dfad_0 <- mutate(dfad_0, art = ifelse(ihovedart != "", ihovedart, art))
  
  ### add stock to dfad
  
  species <- select(read_sas("Q:/mynd/SAS Library/Arter/art.sas7bdat"), start, X_A_CODE, wormsLatin)
  species <- mutate(species, X_A_CODE = ifelse(start == "HAG", "BOC", X_A_CODE))
  areas <- select(read_sas("Q:/mynd/SAS Library/Farvand/farvand.sas7bdat"), start, faoArea)
  
  dfad_0 <- left_join(dfad_0, species, by = c("art" = "start"))
  dfad_0 <- left_join(dfad_0, areas, by = c("dfadfvd_ret" = "start"))
  
  ##Area corrections to DFAD
  dfad_0 <- mutate(dfad_0, faoArea = ifelse(substr(faoArea, 1, 9) == "27.3.d.28" & X_A_CODE != "HER", "27.3.d.28", 
                                            ifelse(substr(faoArea, 1, 9) == "27.3.d.28" & X_A_CODE == "HER", "27.3.d.28.2",
                                                   ifelse(substr(faoArea, 1, 6) == "27.2.b", "27.2.b", 
                                                          ifelse(substr(faoArea, 1, 6) == "27.6.a", "27.6.a.n",
                                                                 ifelse(substr(faoArea, 1, 8) == "27.5.b.1", "27.5.b.1", 
                                                                        ifelse(substr(faoArea, 1, 5) == "27.1." & X_A_CODE == "PRA", "27.1", faoArea)))))))

  dfad_0 <- rename(dfad_0, sppFAO = X_A_CODE, area = faoArea, rect = square_ret)
  
  source("Q:/mynd/RCM/2018/small_pelagic_baltic/fishPi2_functions/GetStock_v1_1.R")
  dfad_0 <- GetStock(dfad_0)
  
  #Add landing nation and eucode to data
  lplads <- select(read_sas("Q:/mynd/SAS Library/lplads/lplads.sas7bdat"), start, nationl, harbourEU)
  dfad_0 <- left_join(dfad_0, lplads, by = c("lplads" = "start"))
  
  dfad_0 <- rename(dfad_0, landCtry = nationl, landLoc = harbourEU)
  
  
  #Add sampling nation and eucode to data
  dfad_0 <- mutate(dfad_0, koebsted = ifelse(koebsted == "", lplads, koebsted))
  dfad_0 <- left_join(dfad_0, lplads, by = c("koebsted" = "start"))
  
  dfad_0 <- rename(dfad_0, sampCtry = nationl, sampLoc = harbourEU)
  
  #add time variables
  dfad_0 <- mutate(dfad_0, year = year(ldato), quarter = quarter(ldato), month = month(ldato))
  
  dfad <- bind_rows(dfad, dfad_0)
  
}

rm(dfad_0)

#Test stock

test_stock <- summarise(group_by(filter(dfad, art %in% c("TOR","SIL","MAK","BLH") | sppFAO %in% c("BOC","SAN","SPR","PRA")), art, sppFAO, dfadfvd_ret, area, stock), 
                               ton = sum(hel/1000, na.rm = T))
                      
#Select data and store

dfad_1 <- filter(dfad, art %in% c("TOR","SIL","MAK","BLH") | sppFAO %in% c("BOC","SAN","SPR","PRA"))

saveRDS(dfad_1, paste(dir_out, "WGCATCH_FL_DFAD_input.rds", sep = ""))



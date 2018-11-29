

#WGCATCH - FL - sale slips extraction


library(dplyr)
library(haven)
library(fishPiCodes)
library(lubridate)

options(scipen = 999)

yearF <- c(14:17)

dir_out <- "Q:/dfad/users/kibi/data/WGCATCH/"

#Read in dfad

afr <- c()

for (i in yearF) {
  
  afr_0 <- read_sas(paste("Q:/dfad/data/Data/afrdata/afr", i, ".sas7bdat", sep = ""))
  names(afr_0) <- tolower(names(afr_0))
  afr_0$hel[is.na(afr_0$hel)] <- 0
  
  ### add stock to afr
  
  species <- select(read_sas("Q:/mynd/SAS Library/Arter/art.sas7bdat"), start, X_A_CODE, wormsLatin)
  species <- mutate(species, X_A_CODE = ifelse(start == "HAG", "BOC", X_A_CODE))
  areas <- select(read_sas("Q:/mynd/SAS Library/Farvand/farvand.sas7bdat"), start, faoArea)
  
  afr_0 <- left_join(afr_0, species, by = c("art" = "start"))
  afr_0 <- left_join(afr_0, areas, by = c("fvd" = "start"))
  
  ##Area corrections to afr
  afr_0 <- mutate(afr_0, faoArea = ifelse(substr(faoArea, 1, 9) == "27.3.d.28" & X_A_CODE != "HER", "27.3.d.28", 
                                          ifelse(substr(faoArea, 1, 9) == "27.3.d.28" & X_A_CODE == "HER", "27.3.d.28.2",
                                                 ifelse(substr(faoArea, 1, 6) == "27.2.b", "27.2.b", 
                                                        ifelse(substr(faoArea, 1, 6) == "27.6.a", "27.6.a.n",
                                                               ifelse(substr(faoArea, 1, 8) == "27.5.b.1", "27.5.b.1", 
                                                                      ifelse(substr(faoArea, 1, 5) == "27.1." & X_A_CODE == "PRA", "27.1", faoArea)))))),
                  X_A_CODE = ifelse(is.na(X_A_CODE), "XXXX", X_A_CODE))

  afr_0 <- rename(afr_0, sppFAO = X_A_CODE, area = faoArea)
  afr_0 <- mutate(afr_0, rect = " ")
  
  source("Q:/mynd/RCM/2018/small_pelagic_baltic/fishPi2_functions/GetStock_v1_1.R")
  afr_0 <- GetStock(afr_0)
  
  #Add lplads nation to data
  lplads <- select(read_sas("Q:/mynd/SAS Library/lplads/lplads.sas7bdat"), start, nationl)
  afr_0 <- left_join(afr_0, lplads, by = c("lplads" = "start"))
  
  #add time variables
  afr_0 <- mutate(afr_0, year = year(ldato), quarter = quarter(ldato), month = month(ldato))
  
  afr <- bind_rows(afr, afr_0)
  
}

rm(afr_0)

#Test stock

test_stock <- summarise(group_by(filter(afr, art %in% c("TOR","SIL","MAK","BLH") | sppFAO %in% c("BOC","SAN","SPR","PRA")), art, sppFAO, fvd, area, stock), 
                               ton = sum(hel/1000, na.rm = T))
                      
#Select data and store

afr_1 <- filter(afr, art %in% c("TOR","SIL","MAK","BLH") | sppFAO %in% c("BOC","SAN","SPR","PRA"))

saveRDS(afr_1, paste(dir_out, "WGCATCH_FL_afr_input.rds", sep = ""))

saveRDS(afr, paste(dir_out, "WGCATCH_FL_afr_input_all_spp.rds", sep = ""))
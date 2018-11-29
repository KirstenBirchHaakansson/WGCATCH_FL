

#WGCATCH - script for making data for national vessels


library(dplyr)
library(haven)
library(fishPiCodes)
library(lubridate)

options(scipen = 999)

yearF <- c(14:17)

dir_out <- "Q:/dfad/users/kibi/data/WGCATCH/"

#Read in dfad

log <- c()

for (i in yearF) {
  
  log_0 <- read_sas(paste("Q:/dfad/data/Data/logdata/nylog", i, ".sas7bdat", sep = ""))
  names(log_0) <- tolower(names(log_0))
  log_0$fangst[is.na(log_0$fangst)] <- 0
  
  ### add stock to log
  
  species <- select(read_sas("Q:/mynd/SAS Library/Arter/art.sas7bdat"), start, X_A_CODE, wormsLatin)
  species <- mutate(species, X_A_CODE = ifelse(start == "HAG", "BOC", X_A_CODE))
  areas <- select(read_sas("Q:/mynd/SAS Library/Farvand/farvand.sas7bdat"), start, faoArea)
  
  log_0 <- left_join(log_0, species, by = c("art" = "start"))
  log_0 <- left_join(log_0, areas, by = c("fvd" = "start"))
  
  ##Area corrections to log
  log_0 <- mutate(log_0, faoArea = ifelse(substr(faoArea, 1, 9) == "27.3.d.28" & X_A_CODE != "HER", "27.3.d.28", 
                                            ifelse(substr(faoArea, 1, 9) == "27.3.d.28" & X_A_CODE == "HER", "27.3.d.28.2",
                                                   ifelse(substr(faoArea, 1, 6) == "27.2.b", "27.2.b", 
                                                          ifelse(substr(faoArea, 1, 6) == "27.6.a", "27.6.a.n",
                                                                 ifelse(substr(faoArea, 1, 8) == "27.5.b.1", "27.5.b.1", 
                                                                        ifelse(substr(faoArea, 1, 5) == "27.1." & X_A_CODE == "PRA", "27.1", faoArea)))))))

  log_0 <- rename(log_0, sppFAO = X_A_CODE, area = faoArea)
  log_0 <- mutate(log_0, rect = paste(isb, isl, sep = ""))
  
  source("Q:/mynd/RCM/2018/small_pelagic_baltic/fishPi2_functions/GetStock_v1_1.R")
  log_0 <- GetStock(log_0)
  
  #Add lplads nation to data
  lplads <- select(read_sas("Q:/mynd/SAS Library/lplads/lplads.sas7bdat"), start, nationl)
  log_0 <- left_join(log_0, lplads, by = c("lplads" = "start"))
  
  #add time variables
  log_0 <- mutate(log_0, year = year(ldato), quarter = quarter(ldato), month = month(ldato))
  
  #log_0 <- filter(log_0, rectype == "AS")
  
  log <- bind_rows(log, log_0)
  
}

rm(log_0)

#Test stock

test_stock <- summarise(group_by(filter(log, art %in% c("TOR","SIL","MAK","BLH") | sppFAO %in% c("BOC","SAN","SPR","PRA")), art, sppFAO, fvd, area, stock), 
                               ton = sum(fangst/1000, na.rm = T))
                      
#Select data and store

log_1 <- filter(log, art %in% c("TOR","SIL","MAK","BLH") | sppFAO %in% c("BOC","SAN","SPR","PRA"))

saveRDS(log_1, paste(dir_out, "WGCATCH_FL_log_input.rds", sep = ""))


saveRDS(log, paste(dir_out, "WGCATCH_FL_log_input_all_spp.rds", sep = ""))






#Samples in FiskeLine

library(dplyr)
library(RODBC)
options(scipen = 9999, digits = 8, max.print = 100000)

dir_out <- "Q:/mynd/WG/WGCATCH/2018/WGCATCH_FL/output/"


channel <- odbcConnect("FishLineDW")
dat <- sqlQuery(channel, paste("SELECT Trip.tripId, Sample.dfuArea, Trip.nationalityPlatform1, Trip.harbourSample, Trip.harbourLanding, Trip.year, Sample.targetSpecies1, Trip.cruise, Trip.trip, SpeciesList.speciesCode
FROM     Trip INNER JOIN
                               Sample ON Trip.tripId = Sample.tripId INNER JOIN
                               SpeciesList ON Sample.sampleId = SpeciesList.sampleId
                               WHERE  (Trip.nationalityPlatform1 <> 'DNK') AND (Trip.year BETWEEN 2010 AND 2018) AND (Trip.tripType = 'HVN')
                               ORDER BY Trip.year, Trip.trip"))
close(channel)

channel <- odbcConnect("FishLine")
area <- sqlQuery(channel, paste("SELECT DFUArea as dfuArea, areaICES FROM L_dfuArea"))
harbour <- sqlQuery(channel, paste("SELECT harbour as harbourSample, harbourEU FROM L_harbour"))
close(channel)

dat$harbourLanding <- as.character(dat$harbourLanding)
dat$harbourSample <- as.character(dat$harbourSample)

dat <- mutate(dat, harbourSample = ifelse(is.na(harbourSample), harbourLanding, harbourSample))

dat <- left_join(left_join(dat, area), harbour)

#Swedish samples of her.3a47d
swe_her_4 <- filter(dat, nationalityPlatform1 == "SWE" & speciesCode == "SIL" & dfuArea %in% c("4A", "4B", "4C") & is.na(targetSpecies1))

swe_her_4_sum <- summarise(group_by(swe_her_4, nationalityPlatform1, year, speciesCode, areaICES,  harbourEU), no_samp_trip = length(unique(tripId)))

write.csv2(swe_her_4_sum, paste(dir_out, "swe_her_4_samples.csv", sep = ""), row.names = F)


#Swedish samples of her.3a47d
swe_her_3 <- filter(dat, nationalityPlatform1 == "SWE" & speciesCode == "SIL" & dfuArea %in% c("20", "21", "22", "23", "24") & is.na(targetSpecies1))

swe_her_3_sum <- summarise(group_by(swe_her_3, nationalityPlatform1, year, speciesCode, areaICES,  harbourEU), no_samp_trip = length(unique(tripId)))

write.csv2(swe_her_3_sum, paste(dir_out, "swe_her_3_samples.csv", sep = ""), row.names = F)


#Swedish samples of spr.2232
swe_spr <- filter(dat, nationalityPlatform1 == "SWE" & speciesCode == "BRS" & dfuArea %in% c("22", "23", "24", "25", "26", "27", "28", "29", 
                                                                                             "30", "31", "32"))

swe_spr_sum <- summarise(group_by(swe_spr, nationalityPlatform1, year, speciesCode, areaICES,  harbourEU), no_samp_trip = length(unique(tripId)))

write.csv2(swe_spr, paste(dir_out, "swe_spr_samples.csv", sep = ""), row.names = F)

#Swedish samples of spr.2232
swe_spr <- filter(dat, nationalityPlatform1 == "POL" & speciesCode == "BRS" & dfuArea %in% c("22", "23", "24", "25", "26", "27", "28", "29", 
                                                                                             "30", "31", "32"))

swe_spr_sum <- summarise(group_by(swe_spr, nationalityPlatform1, year, speciesCode, areaICES,  harbourEU), no_samp_trip = length(unique(tripId)))

write.csv2(swe_spr_sum, paste(dir_out, "pol_spr_samples.csv", sep = ""), row.names = F)


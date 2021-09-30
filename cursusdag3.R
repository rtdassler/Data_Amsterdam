library("tidyverse")
#library("tidymodels")

projdir <- getwd()
setwd("C:/Users/Dell/Downloads")
raw <- read.csv2("bbga_latest_and_greatest.csv")
gebied <- read.csv2("gebied.csv")

setwd(projdir)
dataAms <- pivot_wider(raw,names_from = variabele,values_from = waarde)

dataAms[3:ncol(dataAms)] <- sapply(dataAms[3:ncol(dataAms)],as.numeric)
#names(dataAms) <- names(janitor::clean_names(dataAms))
gc()
test <- merge(gebied,dataAms,by="gebiedcode15")
# bestand is te groot, dus nemen een sample

?sample


sampleAms <- dataAms[sample(1:nrow(dataAms), 5000), ]

gc()
test <- merge(gebied,sampleAms,by="gebiedcode15")

dataAms <- select(test,gebiednaam,gebiedcode15:SKACTI_1000INW)
names(dataAms) <- names(janitor::clean_names(dataAms))

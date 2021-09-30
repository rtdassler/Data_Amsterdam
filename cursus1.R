install.packages("janitor")

library ("tidyverse")

library ("tidymodels")

projdir <- getwd()

setwd("C:/Users/Dell/Downloads")

raw <- read.csv2("bbga_latest_and_greatest(1).csv")



gebied <- read.csv2("gebied.csv")

gc()



setwd (projdir)

dataAms <- pivot_wider(raw, names_from = variabele, values_from = waarde)

gc()

test <- merge(gebied,dataAms, by="gebiedcode15")





dataAms[3:ncol(dataAms)]<-sapply(dataAms[3:ncol(dataAms)], as.numeric)

names(dataAms) <-names(janitor::clean_names(dataAms))





test <- subset(dataAms, nchar(as.character(gebiedcode15)) == 3)



data1 <- test

data1       

View(data1)


test <- subset(dataAms, nchar(as.character(gebiedcode15)) == 3)

postcode <- 
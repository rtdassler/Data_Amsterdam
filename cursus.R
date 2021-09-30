install.packages("janitor")
library("tidyverse")
library("tidymodels")


setwd("C:/Users/Dell/Downloads")
list.files()
raw <- read.csv2("bbga_latest_and_greatest.csv")
setwd(projdir)
dataAms <- pivot_wider(raw, names_from = variabele, values_from = waarde)

dataAms[3:ncol(dataAms)] <- sapply(dataAms[3:ncol(dataAms)], as.numeric)
names(dataAms) <- names(janitor::clean_names(dataAms))

unique(dataAms$gebiedcode15)

test <- dataAms %>%
  filter(str_detect(gebiedcode15, "^^1"))
unique(test$gebiedcode15)

dataAms <- test
rm(test)
na.omit(dataAms)

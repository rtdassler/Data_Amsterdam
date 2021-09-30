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

test <- merge(dataAms,gebied,by="gebiedcode15")

dataAms <- select(test,gebiednaam,gebiedcode15:skacti_1000inw)

unique(dataAms$gebiedcode15) # welke gebieden willen we uitkiezen?

unique(dataAms$gebiednaam) # welke gebieden willen we uitkiezen?

unique(select(dataAms, gebiedcode15, gebiednaam))





test<- dataAms %>%
  
  filter(str_detect(gebiedcode15, "^1")) #4-cijferig postcodegebied?



unique(test$gebiedcode15) # check of de filter gewerkt heeft



dataAms <- test # overschrijf data

rm(test) # delete test databestand



# er staan veel NAs in, wat moten we weggooien?



na.omit(dataAms) # 0 rijen over, die kunnen we niet gebruiken.



test <- dataAms[, colSums(is.na(dataAms)) != nrow(dataAms)] # If the count of NAs in a

#column is equal to the number of rows, it must be entirely NA.



dataAms <- test



# er staat altijd een waarde in de eerste twee kolommen (jaar en gebied).

# Maar kunnen we ook rijen excluderen die voor de rest alleen NAs hebben?

dataAms[rowSums(is.na(dataAms)) != ncol(dataAms)-2, ] # Apply is.na function



unique(dataAms$bevgeb) # klopt er staan genoeg waardes in bijvoorbeeld deze



colSums(is.na(dataAms))



unique(dataAms$jaar)



names(dataAms)



# dat is leuk maar dat blijft er geen interessante uitkomstmaat over...



dataAms <- pivot_wider(raw,names_from = variabele,values_from = waarde)

dataAms[3:ncol(dataAms)] <- sapply(dataAms[3:ncol(dataAms)],as.numeric)

names(dataAms) <- names(janitor::clean_names(dataAms))

# uitkomstvariabele dataAms$beveenouderhh_p

dataAms$bevturk_p,

dataAms$bevmarok_p

dataAms$bevsur_p

dataAms$bevantil_p

dataAms$bevovnw_p

dataAms$bevman_p

dataAms$bevvrouw_p

dataAms$bevpotbbv15_64_p

dataAms$bevopllaag_p

dataAms$bevoplmid_p

dataAms$bevoplhoog_p

dataAms$wcorhuur_p

dataAms$wparthuur_p

dataAms$wkoop_p







iww <- select(dataAms, gebiedcode15, jaar, iww, bevtotaal) %>% arrange(gebiedcode15) %>% na.omit



unique(iww$gebiedcode15)



test <- filter(iww, gebiedcode15 != "STAD", gebiedcode15 != "A", gebiedcode15 != "E",
               
               gebiedcode15 != "T", gebiedcode15 != "K", gebiedcode15 != "M", gebiedcode15 != "N",
               
               gebiedcode15 != "Z", gebiedcode15 != "F", gebiedcode15 != "D") %>%
  
  arrange(gebiedcode15)





dataAms <- subset(dataAms, nchar(as.character(gebiedcode15)) == 3)

hist(dataAms$beveenouderhh_p)
log(dataAms$beveenouderhh_p)
dataAms$logbeveenoderhh <- log(dataAms$beveenouderhh_p)
hist(dataAms$logbeveenoderhh)

ggplot(dataAms, aes( x=jaar, y=beveenouderhh_p))+
  geom_point(aes(color=gebiedcode15))+
  geom_smooth()

ggplot(dataAms, aes( x=jaar, y=beveenouderhh_p))+
  geom_point()+
  geom_smooth(aes(color=gebiedcode15))

install.packages("corrplot")
library("corrplot")
correlaties <- cor(na.omit(select(dataAms, beveenouderhh_p, bevturk_p, 
        bevmarok_p, bevsur_p,
        bevantil_p, bevovnw_p,
        bevman_p, bevvrouw_p,
        bevpotbbv15_64_p, bevopllaag_p,
        bevoplmid_p, bevoplhoog_p,
        wcorhuur_p, wparthuur_p, wkoop_p )))

corrplot(correlaties, method = 'number')
corrplot.mixed(correlaties, order= 'AOE')
corrplot(correlaties, order = 'hclust', addrect = 2)
install.packages("VennDiagram")
library("VennDiagram")

mean1 <- mean(dataAms$beveenouderhh_p, na.rm = TRUE)
dataAms$beveenouderhh_bovengemiddeld <- ifelse(dataAms$beveenouderhh_p>mean1, 1, 0)

set1 <- filter (dataAms, beveenouderhh_bovengemiddeld>0)$gebiedcode15

mean2 <- mean(dataAms$bevsur_p, na.rm = TRUE)
dataAms$bevsur_bovengemiddeld <- ifelse(dataAms$bevsur_p>mean2, 1, 0)

set2 <- filter(dataAms, bevsur_bovengemiddeld>0)$gebiedcode15

mean3 <- mean(dataAms$bevopllaag_p, na.rm = TRUE)

dataAms$bevopllaag_bovengemiddeld <- ifelse(dataAms$bevopllaag_p>mean3, 1, 0)
set3 <- filter(dataAms, bevopllaag_bovengemiddeld>0)$gebiedcode15
venn.diagram(
  x= list(set1, set2, set3),
  category.names = c("set1", "set2", "set3"),
  filename = '#14_venndiagramm.png',
  output = TRUE
)


dataStad <-pivot_wider(raw,names_from = variabele,values_from = waarde) %>%
  filter(gebiedcode15 == "STAD")%>%
  filter(!is.na(jaar)) %>%
  filter(!is.na(BEVEENOUDERHH))%>%
filter(jaar<2021)


ggplot(data=dataStad, aes(x=as.factor(jaar), y=as.numeric(BEVEENOUDERHH)))+
  geom_bar(colour= "black", stat = "identity")+
  guides(fill=FALSE)
  
fit=lm(log(beveenouderhh_p +1)~ bevturk_p+ + bevsur_p+
       bevantil_p+ bevovnw_p+
       bevman_p+ bevvrouw_p+
       bevpotbbv15_64_p+ bevopllaag_p+
       bevoplmid_p+ bevoplhoog_p+
       wcorhuur_p+ wparthuur_p+ wkoop_p, data= dataAms)
summary(fit)

fit2<-lm(log(beveenouderhh_p +1)~ bevsur_p + +
         bevantil_p + bevovnw_p +
         bevpotbbv15_64_p, data = dataAms)
summary(fit2)

fit$transterug <- exp(fit$coefficients)-1
fit$transterug
fit$transterug[[1]] + 51*fit$transterug[[2]]+ 49*fit$transterug[[3]] + 
  1*fit$transterug[[4]] + 1*fit$transterug[[5]]+
  1*fit$transterug[[6]] + 1* fit$transterug[[7]]+ 1* fit$transterug[[8]] +
 1* fit$transterug[[9]]+ 1* fit$transterug[[10]]+ 1* fit$transterug[[11]] +
  1* fit$transterug[[12]]+ 1* fit$transterug[[13]]+ 1* fit$transterug[[14]]


ggplot(dataAms, aes(y= beveenouderhh_p, x=bevsur_p)) +
  geom_point(aes(color=gebiedcode15)) +
  geom_smooth(method= "lm")

ggplot(dataAms, aes(y= beveenouderhh_p, x=bevoplhoog_p)) +
  geom_point(aes(color=gebiedcode15)) +
  geom_smooth(method= "lm")

ggplot(dataAms, aes(y= beveenouderhh_p, x=wkoop_p)) +
  geom_point(aes(color=gebiedcode15)) +
  geom_smooth(method= "lm")

ggplot(dataAms, aes(y= beveenouderhh_p, x=wparthuur_p)) +
  geom_point(aes(color=gebiedcode15)) +
  geom_smooth(method= "lm")

ggplot(dataAms, aes(y= beveenouderhh_p, x=wcorhuur_p)) +
  geom_point(aes(color=gebiedcode15)) +
  geom_smooth(method= "lm")

oneway <- aov(beveenouderhh_p ~ bevsur_p, data = dataAms)
summary(oneway)

dataAms <- pivot_wider(raw,names_from = variabele,values_from = waarde)
dataAms$gebiedcode15 <- as.numeric(dataAms$gebiedcode15)
dataMinZO <-filter(dataAms, !is.na(gebiedcode15<1100))
dataMinZO <- janitor::clean_names(dataMinZO)

dataMinZO[3:ncol(dataMinZO)] <-sapply(dataMinZO[3:ncol(dataMinZO)], as.numeric)
dataMinZO$trans <- log(dataMinZO$beveenouderhh_p+1)

fitMinZO <-lm(trans ~
                bevturk_p+ bevsur_p+
                bevantil_p+ bevovnw_p+
                bevman_p+ bevvrouw_p+
                bevpotbbv15_64_p+
                wcorhuur_p+ wparthuur_p+ wkoop_p, data= dataMinZO)
summary(fitMinZO)

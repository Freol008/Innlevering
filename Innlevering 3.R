options(encoding="UTF-8")
library(httr)
library(rjstat)
library(tidyverse)
library(dplyr)

#Laste inn data for fylker
dffylke <- (fromJSONstat(content(GET("http://data.ssb.no/api/v0/dataset/95274.json?lang=no"),"text")))[[1]]

#Laste inn data for hele landet
dflandet <- (fromJSONstat(content(GET("http://data.ssb.no/api/v0/dataset/95276.json?lang=no"),"text")))[[1]]

#Kombinerer de to datasettene
dfcomb <- left_join(dffylke, dflandet, c("måned", "statistikkvariabel"))

#Separerer kolonnen måned til år og måned
dfcomb <- separate(dfcomb, måned, into = c("år", "måned"), sep = "M")

#Filterer ut verdier på 0
dfcomb <- dfcomb %>%
  filter(!(value.x <= 0))


#Filtrering for testing
Rompris <- dfcomb %>%
  filter(statistikkvariabel == "Pris per rom (kr)")

#Filterer slik at vi bare ser fra 1992, fordi det ikke er noe statistikk for pris per rom før 1992
Kap_seng <- dfcomb %>%
  filter(statistikkvariabel == "Kapasitetsutnytting av senger (prosent)") %>%
  filter(år >= 1992)

Kap_rom <- dfcomb %>%
  filter(statistikkvariabel == "Kapasitetsutnytting av rom (prosent)") %>%
  filter(år >= 1992)

#Lager datasett som viser gjennomsnittlig pris i hvert fylke
avg_fylke <- Rompris %>%
  group_by(region.x) %>%
  summarise(avgvalue = mean(value.x))

#Legger til om det er over eller under gjennomsnittet i hele landet
Overgjsnitt <- avg_fylke$avgvalue >= mean(Rompris$value.y)
cbind(avg_fylke, Overgjsnitt)

#Testing av korrelasjon mellom kapasitet og pris
cor.test(Kap_seng$value.x, Rompris$value.x)
cor.test(Kap_rom$value.x, Rompris$value.x)

#Korrelasjonen mellom pris og kapasitet er svak, er litt sterkere med rom
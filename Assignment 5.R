library(readr)
library(plyr)
library(tidyverse)



Nat2017 <- read_fwf("BED-2056/Nat2017PublicUS.c20180516.r20180808.txt",
                    fwf_positions(start = c(13, 475, 504),
                                  end = c(14, 475, 507),
                                  col_names = c("birth_month", "sex", "birth_weight")
                                  )
                                  )


#Teller hvor mange gutter og jenter
sex_freq <- plyr::count(Nat2017$sex)

#Proposjon av gutter til jenter
sex_prop <- sex_freq$freq[2]/sex_freq$freq[1]
#Proposjonen er ca. 1,048

#Setter birth_weight as.numeric
Nat2017$birth_weight <- as.numeric(Nat2017$birth_weight)

#Filtrerer ut "outliers"
Nat2017 <- filter(Nat2017, birth_weight > 1000 & birth_weight < 7000)

#Gjennomsnittlig fødselsvekt
mean_weight <- mean(Nat2017$birth_weight)
#Gjennomsnittsvekten er 3279,41 gram

#Oppdatere sex_freq etter filtrering
sex_freq <- plyr::count(Nat2017$sex)

#Lager denisty plot for gutter og jenter
ggplot(Nat2017, aes(x = birth_weight, color = sex)) +
  geom_density()+
  labs(title = "Density plot over fødselsvekt mellom gutter og jenter",
       x = "Fødselsvekt",
       y = "Density")


#Gjennomsnittlig vekt per måned
avg_weight_per_month <- aggregate(birth_weight ~ birth_month, Nat2017, mean)
#GJennomsnittlige vekten variere i svært liten grad mellom månedene
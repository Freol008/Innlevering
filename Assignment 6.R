library(tidyverse)
library(plyr)

#Task 1
DS = tribble(
  ~fk_account_code, ~Ansvar, ~fk_function_code, ~fk_project_code, ~amount,
  101030,40220,"Det",3432,1493.00,
  101030,40220,"Met",3586,2827.00,
  101030,40320,"Det",3456,49440.00,
  101030,40330,"Sal",NA,870716.00,
  101030,40350,"Met",NA,559928.00,
  101030,40360,"Sal",NA,125534.00,
  101030,40280,"Pol",NA,251611.00)

#1.Fjerner fk_project_code
DS <- DS[-4]

#Gjør sånn at vi bare har de tre første siffrene under "Ansvar"
DS$Ansvar <- substr(DS$Ansvar, 1, 3)

#I koden over ble verdiene omgjort til chr, så vi må endre tilbake til num
DS$Ansvar <- as.numeric(DS$Ansvar)

#2. Sum av Ansvar
sum(DS$Ansvar)


#3. Lager nye "labels" for "fk_function_code"
DS$fk_function_code <- ifelse(DS$fk_function_code == "Det", "supplies",
                              ifelse(DS$fk_function_code == "Sal", "supplies",
                                     ifelse(DS$fk_function_code == "Met", "inventories",
                                            ifelse(DS$fk_function_code == "Pol", "other expenses", NA))))


#Task 2
df <- data.frame(Product=gl(3,10,labels=c("A","B", "C")), 
                 Year=factor(rep(2002:2011,3)), 

                                  Sales=1:30)
#Lager en funksjon slik at summen av de observasjonen i samme å blir 100
fn <- function(x) (x*100)/sum(x)

#Endrer på dette i datasette
df <- plyr :: ddply(df, "Year", transform, Share=fn(Sales))

#Lager en plot som viser salg per år per selskap
ggplot() +
  geom_line(data = df, aes (x = Year, y = Sales, group = Product, color = Product)) +
  labs(title = "Sales per year per company",
       x = "Year",
       y = "Sales")

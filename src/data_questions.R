library(git2rdata)
library(tidyverse)

######################################################################

tellers_vuursalamander <- read_vc(file = "meetnetten_users", root = "raw") %>%
  filter(meetnet == "Vuursalamander") %>%
  filter(role == 10) %>%
  select(meetnet, voornaam = first_name, achternaam = last_name, adres, postcode, gemeente, email)

write.csv2(tellers_vuursalamander, "processed/vuursalamander/vrijwilligers_vuursalamander.csv")


###############################################

bezoeken_abv <- read_vc(file = "bezoeken", root = "raw") %>%
  filter(meetnet == "Algemene Broedvogelmonitoring (ABV)") %>%
  filter(jaar == 2020)

locaties_abv <- bezoeken_abv %>%
  group_by(meetnet, jaar, locatie) %>%
  summarise(aantal_bezoeken = n_distinct(visit_id)) %>%
  ungroup()

write.csv2(locaties_abv, "processed/locaties_abv_2020.csv", row.names = FALSE)

#####################################################


tellers_orig <- read_vc(file = "tellers", root = "raw")

tellers_n_dagen <- tellers_orig %>%
  filter(jaar >= 2018) %>%
  mutate(inbo_contactpersoon = ifelse(meetnet == "Algemene Vlindermonitoring", "Dirk Maes",
                                      ifelse(meetnet == "Algemene Broedvogelmonitoring (ABV)", "Glenn Vermeersch", "Toon Westra"))) %>%
  group_by(naam_teller) %>%
  mutate(contactpersoon  = str_c(unique(inbo_contactpersoon), collapse = "; "),
         meetnetten = str_c(unique(meetnet), collapse = "; ")) %>%
  ungroup() %>%
  group_by(jaar, naam_teller, contactpersoon, meetnetten) %>%
  summarise(dagen = n_distinct(datum)) %>%
  ungroup() %>%
  group_by(naam_teller, contactpersoon, meetnetten) %>%
  summarise(dagen = max(dagen)) %>%
    ungroup()
  
write.csv2(tellers_n_dagen, "processed/vrijwilligers_dagen.csv", row.names = FALSE)

tellers_n_dagen_totaal <- tellers_orig %>%
  filter(jaar >= 2016) %>%
  group_by(jaar) %>%
  summarise(vrijwilligersdagen = n_distinct(datum, naam_teller)) %>%
  ungroup() 

write.csv2(tellers_n_dagen_totaal, "processed/vrijwilligers_dagen_totaal.csv", row.names = FALSE)

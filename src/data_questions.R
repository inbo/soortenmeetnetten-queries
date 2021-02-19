library(git2rdata)
library(tidyverse)
library(sf)

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

##############################################################

militaire_domeinen <- read_sf(dsn = "gis/militaire_domeinen", layer = "MilitaireDomeinen_BeheerANB_DissolveWGS84") %>%
  select(domein_id =DomeinID)

locaties <- read_sf(dsn = "raw/meetnetten_locaties.gpkg", "locaties")

tellers_info <- read_vc(root = "raw", "meetnetten_users")   

locaties_mil_domeinen <- locaties %>%
  filter(is_active == TRUE) %>%
  filter(locatie_type == "locatie") %>%
  st_join(militaire_domeinen) %>%
  filter(!is.na(domein_id)) %>%
  select(meetnet, locatie, domein_id, geclaimd_door) %>%
  st_drop_geometry()


locatie_users_claim <- read_vc(root = "raw", "locatie_users") %>%
  mutate(type_teller = "geclaimd")

locatie_users_reserve <- read_vc(root ="raw", "locatie_users_reserve") %>%
  mutate(type_teller = "reserve")

locatie_users <- bind_rows(locatie_users_claim,
                           locatie_users_reserve) %>%
  left_join(tellers_info, by = c("meetnet", "first_name", "last_name", "email"))
  
locatie_users_mil_domeinen <- locatie_users %>%
  inner_join(locaties_mil_domeinen, by = c("meetnet", "locatie"))

users_mil_domeinen <- locatie_users_mil_domeinen %>%
  group_by(first_name, last_name, email, meetnet) %>%
  mutate(locaties = str_c(locatie, " (", type_teller,")", collapse = ", ")) %>%
  ungroup() %>%
  mutate(locaties_meetnet = str_c(meetnet, ": ", locaties)) %>%
  group_by(first_name, last_name, email, postcode, gemeente, adres) %>%
  summarise(meetnetten = str_c(unique(meetnet), collapse = ", "),
            locaties = str_c(unique(locaties_meetnet), collapse = "; ")) %>%
  ungroup() %>%
  rename(voornaam = first_name, achternaam = last_name) %>%
  arrange(achternaam)

write.csv2(users_mil_domeinen, "processed/tellers_militaire_domeinen/tellers_militaire_domeinen.csv", row.names = FALSE)
      


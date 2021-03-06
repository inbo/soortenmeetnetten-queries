library(git2rdata)
library(tidyverse)
library(sf)
library(lubridate)

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
 


##############################################################


locatie_users <- read_vc("raw/locatie_users")
locatie_users_reserve_orig <- read_vc("raw/locatie_users_reserve")
users <- read_vc("raw/meetnetten_users")

locatie_users_gereserveerd <- locatie_users %>%
  filter(is_active) %>%
  filter(meetnet !="Algemene Vlindermonitoring") %>%
  mutate(tekst = str_c(meetnet, ": ", locatie)) %>%
  group_by(first_name, last_name, email) %>%
  summarise(locaties = str_c(unique(tekst), collapse = "; ")) %>%
  ungroup() %>%
  mutate(type = "gereserveerd")

locatie_users_reserve <- locatie_users_reserve_orig %>%
  filter(is_active) %>%
  filter(interested) %>%
  filter(meetnet !="Algemene Vlindermonitoring") %>%
  mutate(tekst = str_c(meetnet, ": ", locatie)) %>%
  group_by(first_name, last_name, email) %>%
  summarise(locaties = str_c(unique(tekst), collapse = "; ")) %>%
  ungroup() %>%
  mutate(type = "reserve")

users_distinct <- users %>%
  distinct(first_name, last_name, adres, postcode, gemeente, email)


locatie_users_all <- locatie_users_gereserveerd %>%
  bind_rows(locatie_users_reserve) %>%
  spread(key = type, value = locaties) %>%
  left_join(users_distinct, by = c("first_name", "last_name", "email")) %>%
  select(Voornaam = first_name, Achternaam = last_name, Adres = adres, Postcode = postcode, Gemeente = gemeente, "Locatie gereserveerd" = gereserveerd, "Locatie reserve" = reserve, Email = email)

write.csv2(locatie_users_all, "processed/overzicht_tellers_versie2021-03-31.csv", row.names = FALSE, na = "")     

#############################################################################################################

bezoeken_per_jaar <- read_vc(file = "bezoeken", root = "raw") %>%
  filter(meetnet != "Algemene Vlindermonitoring") %>%
  group_by(jaar) %>%
  summarise(n_tellingen_totaal = n_distinct(visit_id)) %>%
  ungroup()

##############################################

das_overzicht <- read_vc(file = "data_burchten", root = "processed") %>%
  filter(meetnet == "Das") %>%
  mutate(jaar = year(datum)) %>%
  group_by(meetnet, jaar) %>%
  summarise(n_tellingen_totaal = n_distinct(visit_id)) %>%
  ungroup()

write.csv2(das_overzicht, "processed/overzicht_das.csv", row.names = FALSE)


#####################################################

tracks_libellen <- read_sf("raw/tracks.gpkg") %>%
  filter(soortgroep == "libellen")

st_write(tracks_libellen, "processed/tracks_libellen.gpkg", "tracks")

transecten_libellen <- read_sf("raw/meetnetten_locaties.gpkg", "locaties") %>%
  filter(soortgroep == "libellen")

users_libellen <- read_vc("raw/meetnetten_users") %>%
  filter(soortgroep == "libellen") %>%
  mutate(hoofdteller = str_c(first_name, " ", last_name)) %>%
  distinct(hoofdteller, meetnet, email, reference_obs)

visits_libellen <- read_vc("raw/bezoeken") %>%
  filter(soortgroep == "libellen") %>%
  anti_join(tracks_libellen, by = "visit_id") %>%
  left_join(users_libellen, by = c("meetnet", "hoofdteller")) %>%
  filter(meetnet!= "Rivierrombout") %>%
  filter(!is.na(reference_obs) & reference_obs != "") %>%
  select(meetnet, locatie, datum, start_time, end_time, naam_teller = hoofdteller, email, id_wnm_account = reference_obs)

write.csv2(visits_libellen, "output/bezoeken_libellen_id_wnm_account.csv", row.names = FALSE)


########################################################
# Rapport afwijking soortenbesluit

aantallen <- read_vc("raw/aantallen")

aantallen_planten_orig <- read_vc("raw/aantallen_planten") 
unique(aantallen_planten_orig$beschrijving_floroncode)

aantallen_planten <- aantallen_planten_orig %>%
  filter(validatie != -1) %>%
  mutate(aantal_min = ifelse(beschrijving_floroncode == "<1 m²", 0,
                             ifelse(beschrijving_floroncode == "1-5 m²", 1,
                                    ifelse(beschrijving_floroncode == ">5-25 m²", 5,
                                           ifelse(beschrijving_floroncode == ">25-50 m²", 25,
                                                  ifelse(beschrijving_floroncode == ">50-500 m²", 50,
                                                         ifelse(beschrijving_floroncode == ">500-5000 m²", 500, aantal_min)))))),
         aantal_max = ifelse(beschrijving_floroncode == "<1 m²", 1,
                             ifelse(beschrijving_floroncode == "1-5 m²", 5,
                                    ifelse(beschrijving_floroncode == ">5-25 m²", 25,
                                           ifelse(beschrijving_floroncode == ">25-50 m²", 50,
                                                  ifelse(beschrijving_floroncode == ">50-500 m²", 500,
                                                         ifelse(beschrijving_floroncode == ">500-5000 m²", 5000, 
                                                                ifelse(beschrijving_floroncode == "> 5000 exx.", 10000, aantal_max))))))),
         aantal = (aantal_max - aantal_min)/2,
         levensstadium = ifelse(protocol == "Vaatplanten - Oppervlakte", "vegetatievlek (m²)",
                                ifelse(protocol == "Vaatplanten - Aantal individuen", "individu", NA)),
         jaar = year(datum))

aantallen_rapportageANB <- aantallen %>%
  bind_rows(aantallen_planten) %>%
  filter(levensstadium != "") %>%
  mutate(levensstadium = ifelse(meetnet == "Hazelmuis", "nest", levensstadium),
         meetnet = ifelse(meetnet == "Hazelmuis - Nestbuizen", "Hazelmuis", meetnet)) %>%
  filter(jaar == 2020) %>%
  group_by( meetnet, levensstadium, locatie, visit_id) %>%
  summarise(aantal_bezoek = sum(aantal,na.rm=TRUE)) %>%
  group_by(meetnet, levensstadium, locatie) %>%
  summarise(aantal_locatie = sum(aantal_bezoek),
            maxTelling_locatie = max(aantal_bezoek),
            n_bezoeken = n()) %>%
  group_by(meetnet, levensstadium) %>%
  summarise(aantalGeteldTotaal = round(sum(aantal_locatie,na.rm = TRUE), 0),
            somMaxGeteldPerLocatie = round(sum(maxTelling_locatie, na.rm=TRUE), 0),
            nBezoeken = sum(n_bezoeken) ,
            nLocaties = n())%>%
  filter(aantalGeteldTotaal > 0) %>%
  rename('Nederlandse naam' = meetnet, specimens = levensstadium) %>%
  mutate(datum = 2020,
         locatie = "Vlaanderen",
         impact = "geen",
         tijdstip = "Onbekend") %>%
  select('Nederlandse naam', specimens, aantalGeteldTotaal, datum, impact, locatie, tijdstip, everything())

write.csv2(aantallen_rapportageANB, "output/150601_Afwijkingsvergunning_ANB_BL_FF_V16_00034_VB_rapportage_2020_bijlage.csv", row.names = FALSE)


########################################################
# datavraag Heivlinder en Rugstreeppad

bezoeken <- read_vc("raw/bezoeken")

status_bezoek <- bezoeken %>%
  filter(voor_analyse) %>%
  filter(validatie != -1) %>%
  select(visit_id, bezoek_status) %>%
  unique()

aantallen <- read_vc("raw/aantallen")

locaties <- st_read("raw/meetnetten_locaties.gpkg", "locaties")

transecten <- st_read("raw/meetnetten_locaties.gpkg", "transecten")

heivlinder_aantallen <- aantallen %>%
  inner_join(status_bezoek, by = "visit_id") %>%
  filter(meetnet == "Heivlinder") %>%
  group_by(meetnet, protocol, locatie, sublocatie, visit_id, datum, bezoek_status) %>%
  summarise(aantal = sum(aantal)) %>%
  ungroup()

write_excel_csv2(heivlinder_aantallen, "output/datavraag_heivlinder_rugstreeppad/heivlinder_aantallen_sectie_versie2021-04-12.csv")

heivlinder_aantallen_bezoek <- heivlinder_aantallen %>%
  group_by(meetnet, protocol, locatie, visit_id, datum, bezoek_status) %>%
  summarise(aantal_transect = sum(aantal, na.rm = TRUE)) %>%
  ungroup()

write_excel_csv2(heivlinder_aantallen_bezoek, "output/datavraag_heivlinder_rugstreeppad/heivlinder_aantallen_transect_versie2021-04-12.csv")

locaties_heivlinder <- locaties %>%
  filter(meetnet == "Heivlinder") %>%
  filter(is_active) %>%
  filter(locatie_type == "locatie") %>%
  select(meetnet, locatie, is_sample) 

locaties_heivlinder %>%
  st_write("output/datavraag_heivlinder_rugstreeppad/heivlinder_locaties_versie2021-04-12.shp", driver = "ESRI Shapefile")

transecten_heivlinder <- transecten %>%
  filter(meetnet == "Heivlinder") %>%
  filter(locatie %in% locaties_heivlinder$locatie) %>%
  select(meetnet, locatie, sublocatie, is_sample, is_active) 

transecten_heivlinder %>%
  st_write("output/datavraag_heivlinder_rugstreeppad/heivlinder_transecten_versie2021-04-12.shp", driver = "ESRI Shapefile")

rugstreeppad_aantallen <-  aantallen %>%
  inner_join(status_bezoek, by = "visit_id") %>%
  filter(meetnet == "Rugstreeppad") %>%
  select(meetnet, protocol, locatie, visit_id, datum, bezoek_status, sample_id, x, y, levensstadium, activiteit, aantal)

write_excel_csv2(rugstreeppad_aantallen, "output/datavraag_heivlinder_rugstreeppad/rugstreeppad_aantallen_puntlocatie_versie2021-04-12.csv")

rugstreeppad_aantallen_bezoek <-  rugstreeppad_aantallen %>%
  group_by(meetnet, protocol, locatie, visit_id, datum, bezoek_status, levensstadium, activiteit) %>%
  summarise(aantal = sum(aantal)) %>%
  ungroup()

write_excel_csv2(rugstreeppad_aantallen, "output/datavraag_heivlinder_rugstreeppad/rugstreeppad_aantallen_versie2021-04-12.csv")

locaties_rugstreeppad <- locaties %>%
  filter(meetnet == "Rugstreeppad") %>%
  filter(is_active) %>%
  filter(locatie_type == "locatie") %>%
  select(meetnet, locatie, is_sample) 

locaties_rugstreeppad %>%
  st_write("output/datavraag_heivlinder_rugstreeppad/rugstreeppad_locaties_versie2021-04-12.shp", driver = "ESRI Shapefile")

########################################

locaties <- read_sf("raw/meetnetten_locaties.gpkg", "locaties")

transecten <- read_sf("raw/meetnetten_locaties.gpkg", "transecten")

transecten_vlinders <- transecten %>%
  st_drop_geometry() %>%
  filter(soortgroep == "dagvlinders") %>%
  group_by(meetnet, locatie) %>%
  summarise(transect = TRUE,
            n_secties = n()) %>%
  ungroup()

locaties_vlinders <- locaties %>%
  st_drop_geometry() %>%
  filter(soortgroep == "dagvlinders") %>%
  filter(locatie_type == "locatie") %>%
  filter(meetnet != "Gentiaanblauwtje") %>%
  filter(is_active) %>%
  mutate(type_locatie = ifelse(is_sample, "meetnetlocatie", "extra locatie")) %>%
  left_join(transecten_vlinders, by = c("meetnet", "locatie")) %>%
  mutate(transect = ifelse(is.na(transect), FALSE, transect)) %>%
  select(soortgroep, meetnet, locatie, type_locatie,  transect, n_secties) %>%
  arrange(meetnet, locatie)

write.csv2(locaties_vlinders, "output/vlindermeetnetten_transecten.csv", row.names = FALSE)

######################################






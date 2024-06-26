library(git2rdata)
library(tidyverse)
library(sf)
library(lubridate)
library(n2khab)
library(leaflet)
library(readxl)
library(units)
library(lwgeom)

# remotes::install_github("inbo/n2khab",
#                         build_vignettes = TRUE,
#                         upgrade = TRUE)
source("../soortenmeetnetten-analysis/source/functions_smp.r")

######################################################################

tellers_vuursalamander <- read_vc(file = "meetnetten_users", root = "raw") %>%
  filter(meetnet == "Vuursalamander") %>%
  filter(role == 10) %>%
  select(meetnet, voornaam = first_name, achternaam = last_name, adres, postcode, gemeente, email)

write.csv2(tellers_vuursalamander, "processed/vuursalamander/vrijwilligers_vuursalamander.csv")


###############################################

bezoeken_abv <- read_vc(file = "bezoeken", root = "raw") %>%
  filter(meetnet == "Algemene Broedvogelmonitoring (ABV)") 

locaties_abv <- bezoeken_abv %>%
  group_by(meetnet, jaar, locatie) %>%
  summarise(aantal_bezoeken = n_distinct(visit_id)) %>%
  ungroup()

overzich_bezoeken <- locaties_abv %>%
  mutate(n_bezoeken = ifelse(aantal_bezoeken < 3, aantal_bezoeken, ">= 3")) %>%
  group_by(meetnet, jaar, n_bezoeken) %>%
  summarise(n_locaties = n_distinct(locatie)) %>%
  ungroup()

aantallen_abv_2023 <- aantallen_abv %>%
  filter(year(datum) >= 2023)

overzicht_abv_2023 <- aantallen_abv_2023 %>%
  mutate(jaar = year(datum)) %>%
  group_by(jaar) %>%
  mutate(tot_locaties = n_distinct(locatie),
         tot_sublocaties = n_distinct(sublocatie)) %>%
  ungroup() %>%
  group_by(meetnet, jaar, tot_locaties, tot_sublocaties, soort_nl, soort_wet) %>%
  summarise(aantal = sum(aantal),
            n_locaties = n_distinct(locatie),
            n_sublocaties = n_distinct(sublocatie)) %>%
  ungroup() %>%
  mutate(prop_locaties =  round(n_locaties/tot_locaties * 100, 1) ,
         prop_sublocaties = round(n_sublocaties/tot_sublocaties * 100, 1)) %>%
  filter(!is.na(soort_wet))

write.csv2(locaties_abv, "processed/bezoeken_per_locaties_abv.csv", row.names = FALSE)
write.csv2(overzich_bezoeken, "processed/overzicht_abv.csv", row.names = FALSE)

write.csv2(overzich_bezoeken, "processed/aantallen_abv_2023.csv", row.names = FALSE)

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

militaire_domeinen <- read_sf(dsn = "gis/militaire_domeinen/vleermuizen", layer = "mil_domeinen_vleermuizen") %>%
  select(naam = NAAM)

militaire_domeinen_2 <- militaire_domeinen %>%
  st_transform(31370)

locaties <- read_sf(dsn = "raw/meetnetten_locaties.gpkg", "locaties")

tellers_info <- read_vc(root = "raw", "meetnetten_users")   

locaties_mil_domeinen <- locaties %>%
  st_transform(31370) %>%
  filter(is_active == TRUE) %>%
  filter(locatie_type == "locatie") %>%
  st_join(militaire_domeinen_2) %>%
  filter(!is.na(domein_id)) %>%
  select(meetnet, locatie, domein_id, geclaimd_door) %>%
  st_drop_geometry()

locatie_users_claim <- read_vc(root = "raw", "locatie_users_20240115") %>%
  mutate(type_teller = "hoofdteller")

locatie_users_reserve <- read_vc(root = "raw", "locatie_users_reserve_20240115") %>%
  mutate(type_teller = "reserveteller") %>%
  filter(interested = TRUE)

locatie_users <- bind_rows(locatie_users_claim,
                           locatie_users_reserve) %>%
  left_join(tellers_info, by = c("soortgroep","meetnet", "first_name", "last_name", "email"))
  
locatie_users_mil_domeinen <- locatie_users %>%
  inner_join(locaties_mil_domeinen, by = c("meetnet", "locatie"))

users_mil_domeinen <- locatie_users_mil_domeinen %>%
  group_by(first_name, last_name, email, meetnet, domein_id) %>%
  mutate(locaties = str_c(locatie, " (", type_teller,")", collapse = ", ")) %>%
  ungroup() %>%
  mutate(locaties_meetnet = str_c(meetnet, ": ", locaties)) %>%
  group_by(first_name, last_name, email, postcode, gemeente, adres, domein_id) %>%
  summarise(meetnetten = str_c(unique(meetnet), collapse = ", "),
            locaties = str_c(unique(locaties_meetnet), collapse = "; ")) %>%
  ungroup() %>%
  rename(voornaam = first_name, achternaam = last_name) %>%
  select(domein_id, everything()) %>%
  arrange(domein_id, achternaam)

write.csv2(users_mil_domeinen, "processed/tellers_militaire_domeinen/tellers_militaire_domeinen.csv", row.names = FALSE)
 
locaties_md_vleermuizen <- locaties %>%
  #filter(meetnet == "Vleermuizen - Wintertellingen") %>%
  st_transform(31370) %>%
  filter(is_active) %>%
  filter(locatie_type == "locatie") %>%
  st_join(militaire_domeinen_2) %>%
  filter(!is.na(naam)) %>%
  select(naam_md = naam, meetnet, locatie, eurobats = is_sample) %>%
  st_drop_geometry() %>%
  left_join(select(locatie_users, meetnet, locatie, type_teller, first_name, last_name, email), by = c("meetnet", "locatie"))
  
locaties_md_vleermuizen_wide <- locaties_md_vleermuizen %>%
  mutate(naam = str_c(first_name, last_name, sep = " ")) %>%
  group_by(meetnet, locatie, naam_md, eurobats, type_teller) %>%
  summarise(naam = str_c(naam, collapse = "; "),
            email = str_c(email, collapse = "; ")) %>%
  ungroup() %>%
  pivot_wider(names_from = "type_teller", values_from = c("naam", "email")) %>%
  select(meetnet, naam_md, locatie, eurobats, naam_hoofdteller, email_hoofdteller, naam_reserveteller, email_reserveteller)

write_csv2(locaties_md_vleermuizen, "processed/tellers_militaire_domeinen/tellers_militaire_domeinen.csv", na = "")

##############################################################


locatie_users <- read_vc("raw/locatie_users")
locatie_users_reserve_orig <- read_vc("raw/locatie_users_reserve")
users <- read_vc("raw/meetnetten_users")
medetellers <- read_vc("raw/medetellers")

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

write.csv2(locatie_users_all, "processed/overzicht_tellers_versie2024-03-29.csv", row.names = FALSE, na = "")

bezoeken_per_hoofdteller <- read_vc(file = "bezoeken", root = "raw") %>%
  mutate(teller = hoofdteller,
         teller_type = "hoofdteller") %>%
  group_by(meetnet, teller, teller_type) %>%
  summarise(n_tellingen_meetnet = n_distinct(visit_id),
            jaar_min_telling = min(jaar),
            jaar_max_telling = max(jaar)) %>%
  ungroup()

bezoeken_per_medeteller <- read_vc(file = "bezoeken", root = "raw") %>%
  select(-hoofdteller) %>%
  left_join(medetellers, by = "visit_id") %>%
  mutate(teller = str_c(first_name, " ", last_name),
         teller_type = "medeteller") %>%
  filter(!is.na(teller)) %>%
  group_by(meetnet, teller, teller_type) %>%
  summarise(n_tellingen_meetnet = n_distinct(visit_id),
            jaar_min_telling = min(jaar),
            jaar_max_telling = max(jaar)) %>%
  ungroup()

bezoeken_per_teller <- bezoeken_per_hoofdteller %>%
  bind_rows(bezoeken_per_medeteller)

overzicht_bezoeken_users <- users %>%
  mutate(teller = str_c(first_name, " ", last_name)) %>%
  left_join(bezoeken_per_teller, by = c("meetnet", "teller")) %>%
  select(teller, email, role, soortgroep, meetnet, teller_type, n_tellingen_meetnet, jaar_min_telling, jaar_max_telling)

write_csv2(overzicht_bezoeken_users, "output/overzicht_tellingen_users.csv", na = "")
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

year_rapportage <- 2023

aantallen <- read_vc("raw/aantallen")

aantallen_planten_orig <- read_vc("raw/aantallen_planten") 

beschr_floron_code <- aantallen_planten_orig %>%
  distinct(protocol, code, beschrijving_floroncode)

aantallen_migratie <- read_csv2("output/controle_plantendata/plantenmeetnetten_migratie_2023.csv") %>%
  left_join(beschr_floron_code, by = c("code", "protocol"))


aantallen_planten <- aantallen_planten_orig %>%
  filter(validatie != -1) %>%
  bind_rows(aantallen_migratie) %>%
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
  filter(type_aantal != "max aantal") %>%
  filter(aantal != 999) %>%
  mutate(levensstadium = ifelse(meetnet == "Hazelmuis", "nest", levensstadium)) %>%
  filter(jaar == year_rapportage) %>%
  group_by( meetnet, levensstadium, locatie, visit_id, soort_nl, soort_wet, primaire_soort) %>%
  summarise(aantal_bezoek = sum(aantal,na.rm=TRUE)) %>%
  group_by(meetnet, soort_nl, soort_wet, primaire_soort, levensstadium, locatie) %>%
  summarise(aantal_locatie = sum(aantal_bezoek),
            maxTelling_locatie = max(aantal_bezoek),
            n_bezoeken = n()) %>%
  group_by(meetnet, levensstadium, primaire_soort, soort_nl, soort_wet) %>%
  summarise(aantalGeteldTotaal = round(sum(aantal_locatie,na.rm = TRUE), 0),
            somMaxGeteldPerLocatie = round(sum(maxTelling_locatie, na.rm=TRUE), 0),
            nBezoeken = sum(n_bezoeken) ,
            nLocaties = n())%>%
  ungroup() %>%
  filter(aantalGeteldTotaal > 0) %>%
  rename( 'Nederlandse naam' = soort_nl, 'wetenschappelijke naam' = soort_wet, specimens = levensstadium) %>%
  mutate(datum = year_rapportage,
         locatie = "Vlaanderen",
         impact = "geen",
         tijdstip = "Onbekend") %>%
  filter(primaire_soort) %>%
  select(meetnet, 'Nederlandse naam', 'wetenschappelijke naam', specimens, aantalGeteldTotaal, datum, impact, locatie, tijdstip, everything()) %>%
  select(-primaire_soort)

write_excel_csv2(aantallen_rapportageANB, "Output/Afwijkingsvergunning_21-202358_rapportage_2024_resultaten_2023_bijlage.csv", na = "")


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

data_hamster <- read_vc("processed/data_burchten") %>%
  filter(meetnet == "Hamster") %>%
  select(meetnet, protocol, locatie, datum, soort_wet, soort_nl, aantal, zeker, opmerking, x, y) %>%
  arrange(datum)

write_csv2(data_hamster, "output/meetnetten_burchten_hamster_2016_2023.csv")

data_hamster_sf <- data_hamster %>%
  st_as_sf(coords = c("x", "y"), crs = 31370)

st_write(data_hamster_sf, "output/meetnetten_burchten_hamster_2016_2023.shp")

########################################

locaties_boomkikker <- get_locations_smp() %>%
  filter(meetnet == "Boomkikker") %>%
  select(meetnet, locatie, is_active, is_sample) %>% 
  st_transform(31370)

locaties_boomkikker <- locaties_boomkikker %>%
  mutate(x_coord = st_coordinates(locaties_boomkikker)[,1],
         y_coord = st_coordinates(locaties_boomkikker)[,2]) %>%
  st_drop_geometry() 


bezoeken_boomkikker <- get_visits_smp() %>%
  filter(meetnet == "Boomkikker") %>%
  filter(validatie != -1) %>%
  filter(voor_analyse) %>%  
  select(visit_id, bezoek_status)

boomkikker_data <-get_counts_smp() %>%
  filter(meetnet == "Boomkikker") %>%
  filter(primaire_soort) %>%
  inner_join(bezoeken_boomkikker) %>%
  select(meetnet, protocol, locatie, jaar, datum, visit_id, bezoek_status, opmerkingen, soort_nl, geslacht, activiteit, levensstadium, aantal) %>%
  left_join(locaties_boomkikker, by = c("meetnet", "locatie"))



write_csv2(boomkikker_data, "output/meetnetten_boomkikker_2021-10-04.csv")

locaties_boomkikker_active <- locaties_boomkikker %>%
  filter(is_active) %>%
  select(meetnet, locatie)

st_write(locaties_boomkikker_active, "processed/locaties_meetnet_boomkikker", driver = "ESRI Shapefile")

########################################################################

locaties_rugstreeppad <- get_locations_smp() %>%
  filter(meetnet == "Rugstreeppad") %>%
  filter(is_active) %>%
  select(meetnet, locatie)

locaties_rugstreeppad %>%
  st_write("output/locaties_rugstreeppad", "locaties_rugstreeppad", driver = "ESRI Shapefile")

############################################################################

visits <- get_visits_smp() %>%
  filter(meetnet == "Vuursalamander") %>%
  filter(validatie != -1) %>%
  filter(voor_analyse) %>%
  select(visit_id, bezoek_status, start_time, end_time, notes)

aantallen_vuursalamander_buggenhout  <- get_counts_smp() %>%
  filter(meetnet == "Vuursalamander") %>%
  filter(str_detect(locatie, "Buggenhout")) %>%
  inner_join(visits, by = "visit_id") %>%
  select(meetnet, protocol, locatie, jaar, datum, visit_id, startuur_bezoek = start_time, einduur_bezoek = end_time, bezoek_status, x_lambert72 = x, y_lambert72 = y, aantal, levensstadium, opmerking = notes)

unique(aantallen_vuursalamander_buggenhout$locatie)  
  
aantallen_vuursalamander_buggenhout %>%
  write_csv2("output/meetnetten_vuursalamander_buggenhoutbos_2021-12-03.csv")

############################################################################

visits_wintertellingen <- read_vc("raw/bezoeken") %>%
  filter(meetnet == "Vleermuizen - Wintertellingen")

objecten_winter_2021_2022 <- visits_wintertellingen %>%
  filter(datum >= "2021-06-15", datum < "2022-06-15") %>%
  group_by(locatie) %>%
  summarise(n_tellingen = n_distinct(visit_id),
            datum_bezoek = str_c(datum, collapse = "; ")) %>%
  ungroup() %>%
  mutate(geteld_winter_2021_2022 = "ja")

locaties_winter_2021_2022 <- st_read("raw/meetnetten_locaties.gpkg", "locaties") %>%
  filter(meetnet == "Vleermuizen - Wintertellingen") %>%
  filter(locatie_type == "locatie") %>%
  left_join(objecten_winter_2021_2022, by = "locatie") %>%
  mutate(geteld_winter_2021_2022 = ifelse(is.na(geteld_winter_2021_2022), "nee", geteld_winter_2021_2022)) %>%
  select(meetnet, locatie, geteld_winter_2021_2022, datum_bezoek) %>%
  st_transform(31370)

st_write(locaties_winter_2021_2022, "processed/objecten_winter_2021_2022.gpkg")

##########################################

militair_domein_beverlo  <- read_sf("gis/militaire_domeinen/Gewestplan_MilitairDomein.shp", crs = 31370) %>%
  filter(OIDN %in% c(7653, 38325)) %>%
  st_transform(4326) %>%
  select(hoofdcode = HOOFDCODE)

locaties_beverlo <- st_read("raw/meetnetten_locaties.gpkg", "locaties") %>%
  filter(locatie_type == "locatie",
         meetnet_type == "meetnet",
         meetnet != "Algemene Vlindermonitoring") %>%
  filter(is_active) %>%
  mutate(check = sf::st_is_valid(geom),
         geom_type = ifelse(str_detect(geom_text, "POINT"), "point", "polygon")) %>%
  filter(check) %>%
  st_join(militair_domein_beverlo) %>%
  filter(!is.na(hoofdcode)) %>%
  arrange(soortgroep)

militair_domein_beverlo %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons() %>%
  addPolygons(data = filter(locaties_beverlo, geom_type == "polygon"),  color = "yellow", popup = ~str_c(meetnet, " - ", locatie)) %>%
  addPolygons(data = filter(locaties_beverlo, geom_type == "polygon"),  color = "yellow", popup = ~str_c(meetnet, " - ", locatie)) %>%
  addCircleMarkers(data = filter(locaties_beverlo, geom_type == "point"),  color = "red", popup = ~str_c(meetnet, " - ", locatie))

buiten_md <- c("Kiefhoek/Veewei")

locaties_beverlo <- locaties_beverlo %>%
  filter(! locatie %in% buiten_md ) %>%
  select(meetnet, locatie) %>%
  unique() %>%
  arrange(meetnet, locatie)

aantallen_beverlo <- read_vc("raw/aantallen") %>%
  mutate(locatie = str_to_lower(locatie)) %>%
  filter(str_detect(locatie, "beverlo") | 
           (locatie %in% str_to_lower(locaties_beverlo$locatie))) %>%
  group_by(meetnet, protocol, locatie, datum, visit_id, soort_nl, soort_wet, levensstadium) %>%
  summarize(aantal = sum(aantal)) %>%
  ungroup() %>%
  filter(!is.na(soort_nl))

aantallen_beverlo_planten <- read_vc("raw/aantallen_planten") %>%
  mutate(locatie = str_to_lower(locatie)) %>%
  filter(str_detect(locatie, "beverlo") | 
           (locatie %in% str_to_lower(locaties_beverlo$locatie))) %>%
  select(meetnet, protocol, locatie, datum, visit_id, soort_nl, soort_wet, code, beschrijving_floroncode) %>%
  filter(!is.na(soort_nl))

aantallen_beverlo_abv <- read_vc("raw/aantallen_abv") %>%
  mutate(locatie = str_to_lower(locatie)) %>%
  filter(locatie %in% str_to_lower(locaties_beverlo$locatie)) %>%
  group_by(meetnet, protocol, locatie, datum, visit_id, soort_nl, soort_wet) %>%
  summarize(aantal = sum(aantal)) %>%
  ungroup() %>%
  filter(!is.na(soort_nl))

tellingen_beverlo <- aantallen_beverlo %>%
  bind_rows(aantallen_beverlo_abv) %>%
  bind_rows(aantallen_beverlo_planten) %>%
  arrange(meetnet, locatie, datum, soort_nl)

write_csv2(tellingen_beverlo, "processed/tellingen_beverlo.csv") 

st_write(locaties_beverlo, "processed/locaties_beverlo.gpkg")


####

tellers_libellen <- meetnetten_users %>%
  filter(soortgroep == "libellen") %>%
  filter(role == 10) %>%
  group_by(first_name, last_name, email) %>%
  summarise(meetnetten = str_c(unique(meetnet), collapse = "; ")) %>%
  ungroup() %>%
  filter(first_name != "")

write_csv2(tellers_libellen, "output/tellers_libellenmeetnetten.csv")

aantallen_rombout <- get_counts_smp() %>%
  filter(meetnet %in% c("Rivierrombout", "Beekrombout"))

visits_rombout <- get_visits_smp() %>%
  filter(meetnet %in% c("Rivierrombout", "Beekrombout")) %>%
  select(visit_id, bezoek_status, voor_analyse, opmerking)

aantallen_rombout_loc <- aantallen_rombout %>%
  filter(primaire_soort) %>%
  group_by(visit_id) %>%
  mutate(type_telling = ifelse(any(!is.na(x)), "punttelling", "locatietelling")) %>%
  ungroup() %>%
  filter(!(type_telling == "punttelling" & is.na(x))) %>% 
  group_by(meetnet, locatie, datum, visit_id, levensstadium) %>%
  summarise(aantal = sum(aantal),
            n = (n())) %>%
  ungroup() %>%
  left_join(visits_rombout, by = c("visit_id")) %>%
  select(meetnet, locatie, datum, bezoek_status, voor_analyse, levensstadium, aantal, opmerking) %>%
  filter(voor_analyse) 

locaties_rombout <- get_locations_smp() %>%
  filter(meetnet %in% c("Rivierrombout", "Beekrombout")) %>%
  select(meetnet, locatie)

start_transect <- locaties_rombout %>%
  st_startpoint() %>%
  st_as_sf()

end_transect <- locaties_rombout %>%
  st_endpoint() %>%
  st_as_sf()

leaflet(locaties_rombout) %>%
  addTiles() %>%
  addPolylines() %>%
  addCircleMarkers(data = start_transect, color = "yellow") %>%
  addCircleMarkers(data = end_transect, color = "red")

metadata_transect <- locaties_rombout %>%
  st_transform(31370) %>%
  mutate(lengte_m = round(drop_units(st_length(geom)), 1)) %>%
  st_drop_geometry() %>%
  mutate(start_transect_x = st_coordinates(start_transect)[, 1],
         start_transect_y = st_coordinates(start_transect)[, 2],
         end_transect_x = st_coordinates(end_transect)[, 1],
         end_transect_y = st_coordinates(end_transect)[, 2])

aantallen_rombout_loc %>%
  filter(meetnet == "Beekrombout") %>%
  write_csv2("output/aantallen_beekrombout.csv")

aantallen_rombout_loc %>%
  filter(meetnet == "Rivierrombout") %>%
  write_csv2("output/aantallen_rivierrombout.csv")

metadata_transect %>%
  write_csv2("output/metadata_locaties_rombout.csv")

#########################################

aantallen_vleermuizen <- get_counts_smp() %>%
  filter(meetnet %in% "Vleermuizen - Wintertellingen") %>%
  filter(jaar >= 2020) %>%
  filter(primaire_soort) %>%
  group_by(soort_wet) %>%
  summarise(aantal_tot = sum(aantal)) %>%
  ungroup()

######################################

bezoeken <- get_visits_smp() %>%
  filter(jaar >= 2012) %>%
  select(visit_id, bezoek_status, voor_analyse, notes)

check <- bezoeken %>%
  distinct(soortgroep,meetnet, protocol)

aantallen <- get_counts_smp() 
  
aantallen_select <- aantallen %>%
  filter(primaire_soort) %>%
  semi_join(bezoeken, by = "visit_id") %>%
  select(soortgroep, meetnet, locatie, protocol,  datum, visit_id, sublocatie, sample_id, soort_nl, soort_wet, activiteit, geslacht, levensstadium, aantal, x, y)

data_meetneten <- aantallen_select %>%
  arrange(soortgroep, meetnet, datum) %>%
  inner_join(bezoeken, by = "visit_id")

write_csv2(data_meetneten, "output/datavraag_luis/counts_meetnetten.csv")

aantallen_abv <- read_vc("raw/aantallen_abv")

data_meetneten_abv <- aantallen_abv %>%
  arrange(meetnet, datum) %>%
  select(soortgroep, meetnet, locatie, protocol,  datum, visit_id, sublocatie, sample_id, soort_nl, soort_wet,  aantal) %>%
  inner_join(bezoeken, by = "visit_id")

write_csv2(data_meetneten_abv, "output/datavraag_luis/counts_meetnetten_commonbirds.csv")

data_meetneten_wintertellingen <- read_vc("raw/aantallen_wintertellingen") %>%
  filter(primaire_soort) %>%
  arrange(meetnet, datum) %>%
  select(soortgroep, meetnet, locatie, protocol,  datum, visit_id, sublocatie, sample_id, soort_nl, soort_wet,  aantal) %>%
  inner_join(bezoeken, by = "visit_id")

write_csv2(data_meetneten_wintertellingen, "output/datavraag_luis/counts_meetnetten_bats_winterobjects.csv")

data_planten <- read_sf(dsn = "raw/planten_puntlocaties.gpkg") %>%
  st_transform(31370) 

data_planten <- data_planten %>%
  mutate(x = st_coordinates(data_planten)[,1],
         y = st_coordinates(data_planten)[,1],
         soortgroep = "planten",
         soort_nl = meetnet) %>%
  st_drop_geometry() %>%
  arrange(meetnet, datum) %>%
  select(soortgroep, meetnet, locatie, protocol,  datum, visit_id,  soort_nl, soort_wet = soort_w,  schaal, code, beschrijving_floroncode, x, y, notes = opm) 

write_csv2(data_planten, "output/datavraag_luis/counts_meetnetten_plants.csv")

data_gentiaanblauwtje <- read_vc("raw/aantallen_gentiaanblauwtje") %>%
  filter(primaire_soort) %>%
  mutate(plant = as.numeric(plant),
         stengel = as.numeric(stengel),
         knop = as.numeric(knop)) %>%
  arrange(meetnet, datum, locatie, plant, stengel, knop) %>%
  select(soortgroep, meetnet, locatie, protocol,  datum, visit_id, sample_id, soort_nl, soort_wet, plant, stengel, knop, levensstadium,  aantal) %>%
  inner_join(bezoeken, by = "visit_id")

write_csv2(data_gentiaanblauwtje, "output/datavraag_luis/counts_meetnetten_alconblue.csv")

locaties <- read_sf(dsn = "raw/meetnetten_locaties.gpkg", layer = "locaties") %>%
  filter(meetnet != "Algemene Vlindermonitoring") %>%
  select(soortgroep, meetnet, locatie, is_sample, is_active)

sublocaties <- read_sf(dsn = "raw/meetnetten_locaties.gpkg", layer = "transecten") %>%
  filter(meetnet != "Algemene Vlindermonitoring") %>%
  select(soortgroep, meetnet, locatie, sublocatie)

st_write(locaties, "output/datavraag_luis/meetnetten_locations.gpkg", "main_locations", driver = "GPKG")
st_write(sublocaties, "output/datavraag_luis/meetnetten_locations.gpkg", "sublocations", driver = "GPKG")

##################################################

locaties_libellen <- get_locations_smp(species_group = "libellen") %>%
  filter(meetnet %in% c("Speerwaterjuffer", "Maanwaterjuffer","Kempense heidelibel", "Sierlijke witsnuitlibel", "Gevlekte witsnuitlibel")) %>%
  filter(is_active) %>%
  arrange(meetnet, locatie) %>%
  select(meetnet, locatie)

locaties_libellen %>%
  st_transform(31370) %>%
  st_write("output/meetnetlocaties_libellen", "meetnetlocaties_libellen", driver = "ESRI Shapefile")

locaties_bosbeek <- get_locations_smp(species_group = "libellen") %>%
  filter(meetnet == "Bosbeekjuffer") %>%
  arrange(meetnet, locatie) %>%
  select(meetnet, locatie)

locaties_bosbeek %>%
  st_transform(31370) %>%
  st_write("output/meetnetlocaties_bosbeekjuffer", "meetnetlocaties_bosbeekjuffer", driver = "ESRI Shapefile")

##################################################

kempense_heidelibel <- get_counts_smp() %>%
  filter(meetnet == "Kempense heidelibel")

aantallen_kempense_heidelibel <- kempense_heidelibel %>%
  filter(primaire_soort) %>%
  select(meetnet, protocol, locatie, datum, soort_nl, soort_wet, levensstadium, activiteit, geslacht, aantal, x, y)

aantallen_kempense_heidelibel %>%
  write_csv2("output/aantallen_kempense_heidelibel.csv", na = "")

##################################################

vroedmeesterpad <- get_counts_smp() %>%
  filter(meetnet == "Vroedmeesterpad")

aantallen_vroedmeesterpad <- vroedmeesterpad %>%
  filter(primaire_soort) %>%
  select(meetnet, protocol, locatie, datum, soort_nl, soort_wet, levensstadium, activiteit, geslacht, aantal, x, y)

aantallen_vroedmeesterpad %>%
  write_csv2("output/aantallen_vroedmeesterpad.csv", na = "")

##########################################
# liiiiiiiiiiiimburg

limburg <- read_admin_areas(dsn = "provinces") %>%
  filter(name == "Limburg") %>%
  select(name) %>%
  st_transform(crs = 4326) 

locaties_limburg <- get_locations_smp() %>%
  filter(meetnet != "Algemene Vlindermonitoring") %>%
  filter(locatie_type != "sublocatie") %>%
  filter(! meetnet %in% c("Hoogveenglanslibel", "Bataafse stroommossel", "Platte schijfhoren", "Vliegend hert - inhaalslag (afgerond)")) %>%
  filter(st_is_valid(geom)) %>%
  filter(type != "optionele locatie") %>%
  st_join(limburg) 

overzicht_limburg <- locaties_limburg %>%
  st_drop_geometry() %>%
  group_by(meetnet) %>%
  summarise(deels_in_limburg = any(name == "Limburg"),
            volledig_in_limburg = all(name == "Limburg")) %>%
  ungroup()

# das, gladde slang, watervogeltellingen, bijzonder broedvodels
nt_in_meetnetten_tot <- 4

nt_in_meetnetten_limburg <- 4

totaal <- n_distinct(overzicht_limburg$meetnet) + nt_in_meetnetten_tot

totaal_limburg <- sum(!is.na(overzicht_limburg$deels_in_limburg)) + nt_in_meetnetten_limburg

totaal_limburg_volledig <- sum(!is.na(overzicht_limburg$volledig_in_limburg)) + 1 

#################################

year_planning <- 2024
year_evaluation <- c(2022, 2023)
meetnet_planning <- "Algemene Broedvogelmonitoring (ABV)"

locaties_regio <- get_locations_smp() %>%
  filter(meetnet == meetnet_planning) %>%
  filter(locatie_type == "locatie") %>%
  st_drop_geometry() %>%
  distinct(meetnet, locatie, regio)

locatie_users <- read_vc("raw/locatie_users") %>%
  filter(meetnet == meetnet_planning) %>%
  mutate(naam = str_c(first_name, last_name, sep = " "),
         type_teller = "hoofdteller") %>%
  select(meetnet, locatie, type_teller, naam, email)

locatie_users_reserve <- read_vc("raw/locatie_users_reserve") %>%
  filter(meetnet == meetnet_planning) %>%
  mutate(naam = str_c(first_name, last_name, sep = " "),
         type_teller = "reserveteller") %>%
  select(meetnet, locatie, type_teller, naam, email)

users <- read_vc("raw/meetnetten_users") %>%
  mutate(naam = str_c(first_name, last_name, sep = " ")) %>%
  select(naam, adres, gemeente, postcode) %>%
  unique()

locatie_users_all <- locatie_users %>%
  bind_rows(locatie_users_reserve) %>%
  left_join(users, by = "naam")

werkpakketten_locaties <- read_vc("raw/werkpakketten_locaties")

bezoeken_evaluatie <- read_vc("raw/bezoeken") %>%
  filter(meetnet == meetnet_planning) %>%
  filter(jaar %in% year_evaluation) %>%
  filter(jaardoel) %>%
  group_by(meetnet, locatie, jaar) %>%
  summarise(n_bezoeken = n_distinct(visit_id)) %>%
  ungroup() %>%
  mutate(voldoende_geteld = n_bezoeken >= 3) %>%
  group_by(meetnet, locatie) %>%
  summarise(voldoende_geteld = any(voldoende_geteld),
            n_bezoeken = max(n_bezoeken),
            jaar_geteld = str_c(jaar, collapse = ";")) %>%
  ungroup()

werkpakket_planning <- werkpakketten_locaties %>%
  filter(meetnet == meetnet_planning) %>%
  filter(str_detect(werkpakket, "2022|2023|2024")) %>%
  group_by(meetnet, locatie) %>%
  summarise(werkpakket = str_c(werkpakket, collapse = "; ")) %>%
  ungroup() %>%
  left_join(bezoeken_evaluatie, by = c("meetnet", "locatie")) %>%
  mutate(voldoende_geteld = ifelse(is.na(voldoende_geteld), FALSE, voldoende_geteld),
         n_bezoeken = ifelse(is.na(n_bezoeken), 0, n_bezoeken)) %>%
  filter(!voldoende_geteld) %>%
  left_join(locatie_users_all, by = c("meetnet", "locatie")) %>%
  left_join(locaties_regio, by = c("meetnet", "locatie")) %>%
  select(meetnet, regio, everything()) %>%
  arrange(regio, locatie) %>%
  filter(!is.na(naam))

write_csv2(werkpakket_planning, "output/abv_locaties_tellers_2024.csv", na = "")

##################################################################
### data roi

filename_roi <- "zwartebeek/Vallei van de Zwarte Beek.shp"

roi <- read_sf(str_c("gis/roi/", filename_roi), crs = 31370) %>%
  st_transform(4326) %>%
  mutate(in_roi = TRUE) %>%
  select(in_roi)

locaties_roi <- st_read("raw/meetnetten_locaties.gpkg", "locaties") %>%
  filter(locatie_type == "locatie",
         meetnet_type == "meetnet",
         meetnet != "Algemene Vlindermonitoring") %>%
  filter(is_active) %>%
  mutate(check = sf::st_is_valid(geom),
         geom_type = ifelse(str_detect(geom_text, "POINT"), "point", "polygon")) %>%
  filter(check) %>%
  st_join(roi) %>%
  filter(!is.na(in_roi)) %>%
  arrange(soortgroep)

roi %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons() %>%
  addPolygons(data = filter(locaties_roi, geom_type == "polygon"),  color = "yellow", popup = ~str_c(meetnet, " - ", locatie)) %>%
  addCircleMarkers(data = filter(locaties_roi, geom_type == "point"),  color = "red", popup = ~str_c(meetnet, " - ", locatie))

aantallen_roi <- read_vc("raw/aantallen") %>%
  semi_join(locaties_roi, c("meetnet", "locatie")) %>%
  group_by(meetnet, protocol, locatie, datum, visit_id, soort_nl, soort_wet, levensstadium, x, y) %>%
  summarize(aantal = sum(aantal)) %>%
  ungroup() %>%
  filter(!is.na(soort_nl))

aantallen_roi_planten <- read_vc("raw/aantallen_planten") %>%
  semi_join(locaties_roi, c("meetnet", "locatie")) %>%
  select(meetnet, protocol, locatie, datum, visit_id, soort_nl, soort_wet, code, beschrijving_floroncode) %>%
  filter(!is.na(soort_nl))

aantallen_roi_abv <- read_vc("raw/aantallen_abv") %>%
  semi_join(locaties_roi, c("meetnet", "locatie")) %>%
  group_by(meetnet, protocol, locatie, datum, visit_id, soort_nl, soort_wet) %>%
  summarize(aantal = sum(aantal)) %>%
  ungroup() %>%
  filter(!is.na(soort_nl))

tellingen_roi <- aantallen_roi %>%
  bind_rows(aantallen_roi_abv) %>%
  bind_rows(aantallen_roi_planten) %>%
  arrange(meetnet, locatie, datum, soort_nl)

write_csv2(tellingen_roi, "processed/tellingen_roi.csv") 

st_write(locaties_roi, "processed/locaties_roi.gpkg")

locaties_roi %>%
  st_drop_geometry() %>%
  select(meetnet, locatie) %>%
  filter(meetnet != "Heivlinder") %>%
  write_csv2("processed/meetnetlocaties_zwarte_beek.csv")

tellingen_roi %>%
  filter(meetnet != "Heivlinder") %>%
  write_csv2("processed/tellingen_zwarte_beek.csv")
  


####
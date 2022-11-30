library(git2rdata)
library(tidyverse)
library(sf)
library(lubridate)
library(n2khab)
library(leaflet)

# remotes::install_github("inbo/n2khab",
#                         build_vignettes = TRUE,
#                         upgrade = TRUE)
source("../soortenmeetnetten-analysis/src/functions_smp.r")

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

data_hamster <- read_vc("processed/data_burchten") %>%
  filter(meetnet == "Hamster") %>%
  select(meetnet, protocol, locatie, datum, soort_wet, soort_nl, aantal, x, y) %>%
  arrange(datum)

write_csv2(data_hamster, "output/meetnetten_burchten_hamster_2016_2020.csv")

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

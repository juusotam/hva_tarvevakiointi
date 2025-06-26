library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# HUOM! THL:n sivuilta ladattavassa xlsx-tiedostossa on virhe yhden välilehden nimessä.
# Vuoden 2021 välilehden nimessä on lopussa välilyöntimerkki, jonka seurauksena funktio ei lue tietoja oikein.
# Tässä julkaisussa mukana olevassa tiedostossa tämä virhe on korjattu, mutta jos käytät THL:n sivuilta saatavaa tiedostoa korjaa virhe itse.

# Skripti tuottaa xlsx-tiedoston jonka sisältö on:
#
#  Muuttuja = tarvevakioinnissa käytettävä ryhmittelijä, esim. diagnoosiryhmä
#  Alue = hyvinvointialueet, Helsingin kaupunki, koko maa
#  Osuus = THL:n taulukosta kyseiselle muuttujalle, alueelle ja vuodelle ilmoitettu väestöosuus
#  Väkiluku = THL:n taulukosta kyseiselle alueelle ja vuodelle ilmoitettu väestömäärä. Huom! Vanhustenhuollon muuttujissa sisältää ilmoitetun yli 64-vuotiaiden määrän
#  Arvioitu määrä = Osuuden ja väkiluvun tulo
#  Vuosi = tarkasteltava vuosi
#  Tyyppi = terveydenhuolto ja sosiaalihuolto (käytetään koko väestöä kertoimena) tai vanhustenhuolto (käytetään vanhusten määrää kertoimena)

vuodet <- 2018:2023

laske_todelliset_vakiluvut <- function(vuosi) {
  tiedosto <- "data/Laskentataulukko tarvekertoimille 2017-2023 lopullinen 2025-06-25.xlsx"
  sheet_nimi <- paste0("Vuosi_", vuosi)
  
  # 1. Lue väestöosuudet (F4:AB214), sarakkeessa A on muuttujat, rivillä 3 alueiden nimet
  osuudet_raw <- read_xlsx(tiedosto, sheet = sheet_nimi, range = "F4:AB214", col_names = FALSE)
  muuttujat <- read_xlsx(tiedosto, sheet = sheet_nimi, range = "B4:B214", col_names = FALSE)[[1]]
  alueet <- read_xlsx(tiedosto, sheet = sheet_nimi, range = "F3:AB3", col_names = FALSE) %>% as.character()
  
  colnames(osuudet_raw) <- alueet
  osuudet <- osuudet_raw %>%
    mutate(Muuttuja = muuttujat) %>%
    relocate(Muuttuja)
  
  # 2. Lue väkiluvut
  vakiluku_raw <- read_xlsx(tiedosto, sheet = sheet_nimi, range = "B3:AB280")
  colnames(vakiluku_raw) <- c("Muuttuja", colnames(vakiluku_raw)[-1])
  
  vakiluvut <- vakiluku_raw %>%
    filter(Muuttuja == "Väkiluku") %>%
    select(-c(Muuttuja, Terveydenhuolto, Sosiaalihuolto, Vanhustenhuolto)) %>%
    pivot_longer(everything(), names_to = "alue", values_to = "vakiluku")
  
  # 3. Muunna osuudet pitkäksi muodoksi ja kerro väkiluvuilla
  osuudet_long <- osuudet %>%
    pivot_longer(-Muuttuja, names_to = "alue", values_to = "osuus") %>%
    left_join(vakiluvut, by = "alue") %>%
    mutate(arvioitu_maara = osuus * vakiluku,
           vuosi = vuosi)
  
  return(osuudet_long)
}

terveydenhuolto_ja_sosiaalihuolto <- purrr::map_dfr(vuodet, laske_todelliset_vakiluvut)
terveydenhuolto_ja_sosiaalihuolto$tyyppi <- c("terveydenhuolto ja sosiaalihuolto")

laske_vanhusjakaumat_vuodelta <- function(vuosi) {
  tiedosto <- "data/Laskentataulukko tarvekertoimille 2017-2023 lopullinen 2025-06-25.xlsx"
  sheet_nimi <- paste0("Vuosi_", vuosi)
  
  # 1. Lue osuudet (F215:AB276)
  osuudet_raw <- readxl::read_xlsx(tiedosto, sheet = sheet_nimi, range = "F215:AB276", col_names = FALSE)
  muuttujat <- readxl::read_xlsx(tiedosto, sheet = sheet_nimi, range = "B215:B276", col_names = FALSE)[[1]]
  alueet <- readxl::read_xlsx(tiedosto, sheet = sheet_nimi, range = "F3:AB3", col_names = FALSE) %>% as.character()
  
  colnames(osuudet_raw) <- alueet
  osuudet <- osuudet_raw %>%
    mutate(Muuttuja = muuttujat) %>%
    relocate(Muuttuja)
  
  # 2. Lue vanhusmäärät (Muuttuja == "Yli 64-vuotias")
  muuttujadata <- readxl::read_xlsx(tiedosto, sheet = sheet_nimi, range = "B3:AB280")
  colnames(muuttujadata) <- c("Muuttuja", colnames(muuttujadata)[-1])
  
  vanhusluvut <- muuttujadata %>%
    filter(Muuttuja == "Yli 64-vuotias") %>%
    select(-c(Muuttuja, Terveydenhuolto, Sosiaalihuolto, Vanhustenhuolto)) %>%
    pivot_longer(cols = everything(), names_to = "alue", values_to = "vanhuksia")
  
  # 3. Pivot ja laske arvioidut määrät
  osuudet_long <- osuudet %>%
    pivot_longer(cols = -Muuttuja, names_to = "alue", values_to = "osuus") %>%
    left_join(vanhusluvut, by = "alue") %>%
    mutate(arvioitu_maara = osuus * vanhuksia,
           vuosi = vuosi)
  
  return(osuudet_long)
}

vanhustenhuolto <- purrr::map_dfr(vuodet, laske_vanhusjakaumat_vuodelta)
vanhustenhuolto$tyyppi <- "vanhustenhuolto"

colnames(vanhustenhuolto) <- colnames(terveydenhuolto_ja_sosiaalihuolto)

kertoimet_lopullinen <- rbind(terveydenhuolto_ja_sosiaalihuolto, vanhustenhuolto)

laske_todelliset_vakiluvut <- function(vuosi) {
  tiedosto <- "data/Laskentataulukko tarvekertoimille 2017-2023 ENNAKKO.xlsx"
  sheet_nimi <- paste0("Vuosi_", vuosi)
  
  # 1. Lue väestöosuudet (F4:AB214), sarakkeessa A on muuttujat, rivillä 3 alueiden nimet
  osuudet_raw <- read_xlsx(tiedosto, sheet = sheet_nimi, range = "F4:AB214", col_names = FALSE)
  muuttujat <- read_xlsx(tiedosto, sheet = sheet_nimi, range = "B4:B214", col_names = FALSE)[[1]]
  alueet <- read_xlsx(tiedosto, sheet = sheet_nimi, range = "F3:AB3", col_names = FALSE) %>% as.character()
  
  colnames(osuudet_raw) <- alueet
  osuudet <- osuudet_raw %>%
    mutate(Muuttuja = muuttujat) %>%
    relocate(Muuttuja)
  
  # 2. Lue väkiluvut
  vakiluku_raw <- read_xlsx(tiedosto, sheet = sheet_nimi, range = "B3:AB280")
  colnames(vakiluku_raw) <- c("Muuttuja", colnames(vakiluku_raw)[-1])
  
  vakiluvut <- vakiluku_raw %>%
    filter(Muuttuja == "Väkiluku") %>%
    select(-c(Muuttuja, Terveydenhuolto, Sosiaalihuolto, Vanhustenhuolto)) %>%
    pivot_longer(everything(), names_to = "alue", values_to = "vakiluku")
  
  # 3. Muunna osuudet pitkäksi muodoksi ja kerro väkiluvuilla
  osuudet_long <- osuudet %>%
    pivot_longer(-Muuttuja, names_to = "alue", values_to = "osuus") %>%
    left_join(vakiluvut, by = "alue") %>%
    mutate(arvioitu_maara = osuus * vakiluku,
           vuosi = vuosi)
  
  return(osuudet_long)
}

terveydenhuolto_ja_sosiaalihuolto <- purrr::map_dfr(vuodet, laske_todelliset_vakiluvut)
terveydenhuolto_ja_sosiaalihuolto$tyyppi <- c("terveydenhuolto ja sosiaalihuolto")

laske_vanhusjakaumat_vuodelta <- function(vuosi) {
  tiedosto <- "data/Laskentataulukko tarvekertoimille 2017-2023 ENNAKKO.xlsx"
  sheet_nimi <- paste0("Vuosi_", vuosi)
  
  # 1. Lue osuudet (F215:AB276)
  osuudet_raw <- readxl::read_xlsx(tiedosto, sheet = sheet_nimi, range = "F215:AB276", col_names = FALSE)
  muuttujat <- readxl::read_xlsx(tiedosto, sheet = sheet_nimi, range = "B215:B276", col_names = FALSE)[[1]]
  alueet <- readxl::read_xlsx(tiedosto, sheet = sheet_nimi, range = "F3:AB3", col_names = FALSE) %>% as.character()
  
  colnames(osuudet_raw) <- alueet
  osuudet <- osuudet_raw %>%
    mutate(Muuttuja = muuttujat) %>%
    relocate(Muuttuja)
  
  # 2. Lue vanhusmäärät (Muuttuja == "Yli 64-vuotias")
  muuttujadata <- readxl::read_xlsx(tiedosto, sheet = sheet_nimi, range = "B3:AB280")
  colnames(muuttujadata) <- c("Muuttuja", colnames(muuttujadata)[-1])
  
  vanhusluvut <- muuttujadata %>%
    filter(Muuttuja == "Yli 64-vuotias") %>%
    select(-c(Muuttuja, Terveydenhuolto, Sosiaalihuolto, Vanhustenhuolto)) %>%
    pivot_longer(cols = everything(), names_to = "alue", values_to = "vanhuksia")
  
  # 3. Pivot ja laske arvioidut määrät
  osuudet_long <- osuudet %>%
    pivot_longer(cols = -Muuttuja, names_to = "alue", values_to = "osuus") %>%
    left_join(vanhusluvut, by = "alue") %>%
    mutate(arvioitu_maara = osuus * vanhuksia,
           vuosi = vuosi)
  
  return(osuudet_long)
}

vanhustenhuolto <- purrr::map_dfr(vuodet, laske_vanhusjakaumat_vuodelta)
vanhustenhuolto$tyyppi <- "vanhustenhuolto"

colnames(vanhustenhuolto) <- colnames(terveydenhuolto_ja_sosiaalihuolto)

kertoimet_ennakko <- rbind(terveydenhuolto_ja_sosiaalihuolto, vanhustenhuolto)

kertoimet_ennakko$versio <- "ennakko"
kertoimet_lopullinen$versio <- "lopullinen"

kertoimet <- rbind(kertoimet_ennakko, kertoimet_lopullinen)

write_xlsx(kertoimet, "data/Kertoimet.xlsx")


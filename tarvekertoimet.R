# Tarvevakiointikertomien laskenta

# Tämä tiedosto tekee tarvevakiointikertoimen laskutoimitukset.

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(writexl)

# Poimitaan vanhusten osuudet alueilta (tarvitaan vanhustenhuollon tarvevakiointikertoimen laskemista varten)

# Määritellään vuosivälilehdet
vuodet <- 2018:2023
tiedosto <- "data/Laskentataulukko tarvekertoimille 2017-2023 lopullinen 2025-06-25.xlsx"

# Funktio poimimaan riviltä 279 sarakkeet F–AB (alueet)
hae_vanhusosuudet <- function(vuosi) {
  sheet_nimi <- paste0("Vuosi_", vuosi)
  
  # Poimi alueiden nimet riviltä 3
  alueet <- read_xlsx(tiedosto, sheet = sheet_nimi, range = "F3:AB3", col_names = FALSE) %>%
    as.character()
  
  # Poimi riviltä 279 osuudet sarakkeista F–AB
  arvot <- read_xlsx(tiedosto, sheet = sheet_nimi, range = "F279:AB279", col_names = FALSE) %>%
    as.numeric()
  
  # Rakenna dataframe
  tibble(
    alue = alueet,
    vanhusosuus = arvot,
    vuosi = vuosi
  )
}

# Käy läpi kaikki vuodet
vanhusosuudet <- map_dfr(vuodet, hae_vanhusosuudet)

# Lisätään vakiotermit tietoihin. Nämä ovat joka vuosi samat.

# Vakiotermien lisäys
kertoimet <- readxl::read_xlsx("data/Kertoimet.xlsx")
vakiotermit <- kertoimet %>% 
  distinct(alue, vuosi) %>% 
  mutate(
    Muuttuja = "Vakiotermi", 
    osuus = 1.0,
    vakiluku = NA_real_,
    arvioitu_maara = NA_real_,
    tyyppi = "vakiotermi",
    versio = "lopullinen"
  )

kertoimet <- bind_rows(kertoimet, vakiotermit)

# Poimitaan regressiokertoimet omaan taulukkoon.

# Tiedoston ja välilehden nimi
tiedosto <- "data/Laskentataulukko tarvekertoimille 2017-2023 lopullinen 2025-06-25.xlsx"
sheet_nimi <- "Vuosi_2023"

# Poimi muuttujien nimet (B4:B277) ja regressiokertoimet (C4:E277)
muuttujat <- read_xlsx(tiedosto, sheet = sheet_nimi, range = "B4:B277", col_names = FALSE)[[1]]
kertoimet_raw <- read_xlsx(tiedosto, sheet = sheet_nimi, range = "C4:E277", col_names = FALSE)

# Yhdistä muuttujien nimet ja kertoimet yhdeksi dataframiksi
regressiokertoimet <- kertoimet_raw %>%
  mutate(Muuttuja = muuttujat) %>%
  relocate(Muuttuja) %>%
  rename(
    Terveydenhuolto = ...1,
    Sosiaalihuolto = ...2,
    Vanhustenhuolto = ...3
  ) %>% 
  mutate(tyyppi = ifelse(Muuttuja == "Vakiotermi", "vakiotermi", ifelse(is.na(Vanhustenhuolto), "terveydenhuolto ja sosiaalihuolto", "vanhustenhuolto")))

# Luodaan taulukko sektoripainoille. Nämä ovat joka vuosi samat (kunnes lainsäädäntöä muutetaan)

sektoripainot <- data.frame(
  sektori = c("Terveydenhuolto", "Sosiaalihuolto", "Vanhustenhuolto"),
  paino = c(0.5886, 0.2139, 0.1975)
)

# Lasketaan tarvevakiointi

# 1. Liitä regressiokertoimet
kertoimet_laskenta <- kertoimet %>%
  filter(versio == "lopullinen") %>%
  left_join(regressiokertoimet, by = c("Muuttuja", "tyyppi"))

# 2. Laske kolmen eri tarpeen arvo
tarpeet <- kertoimet_laskenta %>%
  mutate(
    th_tarve = osuus * Terveydenhuolto,
    sh_tarve = osuus * Sosiaalihuolto,
    vh_tarve = osuus * Vanhustenhuolto
  ) %>%
  group_by(alue, vuosi) %>%
  summarize(
    th_kokonaistarve = sum(th_tarve, na.rm = TRUE),
    sh_kokonaistarve = sum(sh_tarve, na.rm = TRUE),
    vh_kokonaistarve = sum(vh_tarve, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Erotellaan koko maan tarpeet
koko_maa <- tarpeet %>%
  filter(alue == "Koko maa") %>%
  select(
    vuosi,
    th_koko_maa = th_kokonaistarve,
    sh_koko_maa = sh_kokonaistarve,
    vh_koko_maa = vh_kokonaistarve
  )

# 4. Laske aluekohtaiset tarvekertoimet
tarvekertoimet <- tarpeet %>%
  #filter(alue != "Koko maa") %>%
  left_join(koko_maa, by = "vuosi") %>%
  mutate(
    th_tarvekerroin = th_kokonaistarve / th_koko_maa,
    sh_tarvekerroin = sh_kokonaistarve / sh_koko_maa
  )

# 5. Yhdistä vanhusosuudet mukaan
vh_tarve <- tarvekertoimet %>%
  select(alue, vuosi, vh_kokonaistarve) %>%
  left_join(vanhusosuudet, by = c("alue", "vuosi"))

# 6. Erottele koko maan tarve ja vanhusosuus
vh_koko_maa <- vh_tarve %>%
  filter(alue == "Koko maa") %>%
  select(vuosi, vh_kokonaistarve_koko_maa = vh_kokonaistarve, vanhusosuus_koko_maa = vanhusosuus)

# 7. Liitä koko maan luvut alueisiin ja laske oikaistu kerroin
vh_tarvekerroin_oikaistu <- vh_tarve %>%
  left_join(vh_koko_maa, by = "vuosi") %>%
  mutate(
    vh_tarvekerroin = (vh_kokonaistarve / vh_kokonaistarve_koko_maa) *
      (vanhusosuus / vanhusosuus_koko_maa)
  ) %>% 
  select(alue, vuosi, vanhusosuus, vh_tarvekerroin)

# 8. Liitä vanhustenhuollon tarvekertoimet terveydenhuollon ja sosiaalihuollon tarvekertoimien kanssa samaan tauluun.

tarvekertoimet <- tarvekertoimet %>% 
  left_join(vh_tarvekerroin_oikaistu, by = c("alue", "vuosi"))

# Lisää kokonaistarvekerroin dataan
tarvekertoimet <- tarvekertoimet %>%
  mutate(
    kokonaistarvekerroin =
      th_tarvekerroin * sektoripainot$paino[sektoripainot$sektori == "Terveydenhuolto"] +
      sh_tarvekerroin * sektoripainot$paino[sektoripainot$sektori == "Sosiaalihuolto"] +
      vh_tarvekerroin * sektoripainot$paino[sektoripainot$sektori == "Vanhustenhuolto"]
  )

# Tallennetaan alkuperäiset tarvekertoimet tarvekertoimet_original tauluun

tarvekertoimet_original <- tarvekertoimet

# Muuta vuoden 2023 tarvekerroin vuosien 2022 ja 2023 keskiarvoksi. Tämä tarvitaan vuoden 2026 rahoituksen laskemista varten.

tarvekertoimet <- tarvekertoimet %>% 
  group_by(alue) %>% 
  mutate(th_tarvekerroin = ifelse(
    vuosi == 2023,
    (th_tarvekerroin[vuosi == 2022] + th_tarvekerroin[vuosi == 2023]) / 2,
    th_tarvekerroin
  ),
  sh_tarvekerroin = ifelse(
    vuosi == 2023,
    (sh_tarvekerroin[vuosi == 2022] + sh_tarvekerroin[vuosi == 2023]) / 2,
    sh_tarvekerroin
  ),
  vh_tarvekerroin = ifelse(
    vuosi == 2023,
    (vh_tarvekerroin[vuosi == 2022] + vh_tarvekerroin[vuosi == 2023]) / 2,
    vh_tarvekerroin
  )
  ) %>% 
  ungroup() 

# Kirjoitetaa tarvekertoimet xlsx-tiedostoon

#write_xlsx(tarvekertoimet, "data/Tarvekertoimet.xlsx")
#write_xlsx(tarvekertoimet_original, "data/Tarvekertoimet_original.xlsx")
#write_xlsx(kertoimet_laskenta, "data/Kertoimet_laskenta.xlsx")

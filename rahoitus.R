# Rahoituksen laskenta

# Tämän tiedoston luvut ovat VM:n Hyvinvointialueiden rahoituslaskelma 2026 (26.6.2025) tiedostosta.
# Tiedosto löytyy sijainnista: https://vm.fi/rahoituslaskelmat

# Välilehti "SOTE laskennallinen rahoitus" solu B7

vakiluku_koko_maa <- 5605317

# Välilehti "SOTE laskennallinen rahoitus" solut D21-F21

perushinnat <- tibble(vuosi = 2023,
       th_perushinta = 11801754282 / vakiluku_koko_maa,
       vh_perushinta = 4491221936 / vakiluku_koko_maa,
       sh_perushinta = 5151682647 / vakiluku_koko_maa)

# Haetaan alueiden väkiluvut kertoimet taulukosta. Tässä väkiluvut vuosille 2018-2023.

asukasluvut <- kertoimet %>%
  filter(tyyppi == "terveydenhuolto ja sosiaalihuolto" & versio == "lopullinen") %>% 
  select(alue, vuosi, vakiluku) %>%
  distinct()

# Rahoituslaskelmassa vuodelle 2026 käytetään kertoimena vuoden 2024 väkilukuja. Näitä ei ole THL:n aineistossa.
# Uudet väkiluvut taulu on koottu VM:n xlsx-tiedoston pohjalta (SOTE laskennallinen rahoitus, solut C27:C48)

uudet_vakiluvut <- tibble(alue = c("Helsingin kaupunki", "Vantaan ja Keravan hyvinvointialue", "Länsi-Uudenmaan hyvinvointialue", "Itä-Uudenmaan hyvinvointialue", 
                "Keski-Uudenmaan hyvinvointialue", "Varsinais-Suomen hyvinvointialue", "Satakunnan hyvinvointialue", "Kanta-Hämeen hyvinvointialue", 
                "Pirkanmaan hyvinvointialue", "Päijät-Hämeen hyvinvointialue", "Kymenlaakson hyvinvointialue", "Etelä-Karjalan hyvinvointialue", 
                "Etelä-Savon hyvinvointialue", "Pohjois-Savon hyvinvointialue", "Pohjois-Karjalan hyvinvointialue", "Keski-Suomen hyvinvointialue", 
                "Etelä-Pohjanmaan hyvinvointialue", "Pohjanmaan hyvinvointialue", "Keski-Pohjanmaan hyvinvointialue", "Pohjois-Pohjanmaan hyvinvointialue", 
                "Kainuun hyvinvointialue", "Lapin hyvinvointialue"),
  vakiluku = c(684018, 289730, 502067, 99415, 
               207070, 494819, 211261, 169455, 
               545406, 204635, 157442, 125083, 
               129376, 248815, 162091, 274112, 
               189929, 178749, 67723, 418331, 
               69639, 176151),
  vuosi = 2023)

# Lasketaan taervittavat jakajat rahoituslaskennassa käytettävien tarvekerrointen määrittämiseen
# VM:n Excel, välilehti Tarvekertoimet solut G30:I30

jakajat <- tarvekertoimet %>% 
  filter(alue != "Koko maa") %>% 
  left_join(perushinnat, by = "vuosi") %>% 
  left_join(asukasluvut, by = c("alue", "vuosi"))  %>%
  left_join(uudet_vakiluvut, by = c("alue", "vuosi"), suffix = c("", "_uusi")) %>%
  mutate(
    vakiluku = if_else(!is.na(vakiluku_uusi), vakiluku_uusi, vakiluku)
  ) %>%
  select(-vakiluku_uusi) %>% 
  filter(vuosi == 2023) %>% 
  mutate(th_painotettu = th_tarvekerroin * (vakiluku / vakiluku_koko_maa),
         sh_painotettu = sh_tarvekerroin * (vakiluku / vakiluku_koko_maa),
         vh_painotettu = vh_tarvekerroin * (vakiluku / vakiluku_koko_maa)) %>% 
  summarize(th_summa = sum(th_painotettu),
            sh_summa = sum(sh_painotettu),
            vh_summa = sum(vh_painotettu))

# Lasketaan rahoituksessa käytettävät tarvekertoimet
# VM:n Excel välilehti Tarvekertoimet solut J8:L29

rahoitus <- tarvekertoimet %>% 
  filter(alue != "Koko maa") %>% 
  left_join(perushinnat, by = "vuosi") %>% 
  left_join(asukasluvut, by = c("alue", "vuosi"))  %>%
  left_join(uudet_vakiluvut, by = c("alue", "vuosi"), suffix = c("", "_uusi")) %>%
  mutate(
    vakiluku = if_else(!is.na(vakiluku_uusi), vakiluku_uusi, vakiluku)
  ) %>%
  select(-vakiluku_uusi) %>% 
  filter(vuosi == 2023) %>% 
  mutate(th_rahoitus_kerroin = th_tarvekerroin / jakajat$th_summa,
         sh_rahoitus_kerroin = sh_tarvekerroin / jakajat$sh_summa,
         vh_rahoitus_kerroin = vh_tarvekerroin / jakajat$vh_summa) %>% 
  mutate(th_rahoitus = th_rahoitus_kerroin * th_perushinta * vakiluku,
         sh_rahoitus = sh_rahoitus_kerroin * sh_perushinta * vakiluku,
         vh_rahoitus = vh_rahoitus_kerroin * vh_perushinta * vakiluku,
         sote_rahoitus = th_rahoitus + sh_rahoitus + vh_rahoitus) 

# Lasketaan euroa / asukas arvot jokaiselle alueelle ja sektorille.

rahoitus %>% 
  mutate(th_euroa_asukas = th_rahoitus / vakiluku,
         sh_euroa_asukas = sh_rahoitus / vakiluku,
         vh_euroa_asukas = vh_rahoitus / vakiluku) %>% 
  select(alue, th_euroa_asukas, sh_euroa_asukas, vh_euroa_asukas, th_kokonaistarve, sh_kokonaistarve, vh_kokonaistarve) %>% 
  mutate(th = th_euroa_asukas / th_kokonaistarve,
         sh = sh_euroa_asukas / sh_kokonaistarve,
         vh = vh_euroa_asukas / vh_kokonaistarve,
         euroa_asukas = th_euroa_asukas + sh_euroa_asukas + vh_euroa_asukas) %>% 
  View()

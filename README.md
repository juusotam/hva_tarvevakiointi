# Väestöosuuksien laskenta lukumääriksi

## Vastuuvapautuslauseke

Tiedot ovat THL:n ilmoittamia tietoja ja skripti laskee osuuksista takaisin päin arvioituja väkilukuja. Tarkista tietojen oikeellisuus aina ennen käyttöä. En ota vastuuta mahdollisista virheellisyyksistä koodissa, eli käytä tietoja omalla vastuullasi.

## Ohjeet

**HUOM!** THL:n sivuilta ladattavassa xlsx-tiedostossa on virhe yhden välilehden nimessä. **Vuoden 2021 välilehden nimessä on lopussa välilyöntimerkki, jonka seurauksena funktio ei lue tietoja oikein.** Tässä julkaisussa mukana olevassa tiedostossa tämä virhe on korjattu, mutta jos käytät THL:n sivuilta saatavaa tiedostoa korjaa virhe itse.

Skripti tuottaa xlsx-tiedoston jonka sisältö on:

-   **Muuttuja** = tarvevakioinnissa käytettävä ryhmittelijä, esim. diagnoosiryhmä
-   **Alue** = hyvinvointialueet, Helsingin kaupunki, koko maa
-   **Osuus** = THL:n taulukosta kyseiselle muuttujalle, alueelle ja vuodelle ilmoitettu väestöosuus
-   **Väkiluku** = THL:n taulukosta kyseiselle alueelle ja vuodelle ilmoitettu väestömäärä. Huom! Vanhustenhuollon muuttujissa sisältää ilmoitetun yli 64-vuotiaiden määrän \_ Arvioitu määrä = Osuuden ja väkiluvun tulo
-   **Vuosi** = tarkasteltava vuosi
-   **Tyyppi** = terveydenhuolto ja sosiaalihuolto (käytetään koko väestöä kertoimena) tai vanhustenhuolto(käytetään vanhusten määrää kertoimena)

Skriptin käyttämä tiedoto löytyy THL:n sivuilta osoitteesta: <https://thl.fi/aiheet/sote-palvelujen-johtaminen/rahoitus-ja-kustannukset/hyvinvointialueiden-sote-palvelujen-tarveperustainen-rahoitus>

(kts. kohta "Tarvekertoimien laskenta" ja "Laskentataulukko tarvekertoimille")

## Shiny.app

Julkaisussa on matkassa Shiny.app tiedosto (app.R) johon on tehty kaksi yksinkertaista visualisointia.

-   Mahdollisuus tarkastella yhden muuttujan kehittymistä yhdella alueella vuosien aikana

-   Mahdollisuus vertailla yksittäisen muuttujan eroja yhden vuoden aikana eri alueiden välillä (absoluuttiset lukumäärät)

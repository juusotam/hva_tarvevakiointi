# Tarvekertoimien trendimuutos (lineraarinen regressio)

library(dplyr)
library(broom)  # tidy-lm-tuloksia varten
library(highcharter)

trendit <- tarvekertoimet %>%
  filter(alue != "Koko maa") %>%
  group_by(alue) %>%
  do(tidy(lm(kokonaistarvekerroin ~ vuosi, data = .))) %>%
  filter(term == "vuosi") %>%
  select(alue, trendi = estimate) %>%
  arrange(trendi)

# Kuvaaja näyttää lineaarisen regression vuosittaisen muutoksen kulmakertoimen.
# Negatiivinen kulmakerroin kuvaa laskevaa tarvevakioinnin trendiä.

trendit %>%
  arrange(trendi) %>%
  hchart("column", hcaes(x = alue, y = trendi, color = trendi)) %>%
  hc_title(text = "Kokonaistarvekertoimen trendi alueittain") %>%
  hc_subtitle(text = "Kulmakerroin vuosittaisesta muutoksesta (2018–2023)") %>%
  hc_yAxis(title = list(text = "Trendin suuruus (muutos per vuosi)")) %>%
  hc_xAxis(labels = list(rotation = -45)) %>%
  hc_tooltip(pointFormat = "Trendimuutos: <b>{point.y:.4f}</b>") %>%
  hc_colors(c("firebrick", "forestgreen"))


# Kokonaistarvekertoimen kehitys alueittain

library(highcharter)
library(dplyr)

# Suodataan pois mahdolliset Koko maa -rivit, jos niitä on
tarvekertoimet_plot <- tarvekertoimet %>%
  filter(alue != "Koko maa") %>%
  arrange(alue, vuosi)

# Luo linjakaavio
highchart() %>%
  hc_chart(type = "line") %>%
  hc_title(text = "Kokonaistarvekertoimen kehitys alueittain") %>%
  hc_xAxis(
    categories = sort(unique(tarvekertoimet_plot$vuosi)),
    title = list(text = "Vuosi")
  ) %>%
  hc_yAxis(title = list(text = "Kokonaistarvekerroin")) %>%
  hc_tooltip(shared = TRUE, valueDecimals = 3) %>%
  hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
  hc_add_series_list(
    tarvekertoimet_plot %>%
      group_by(alue) %>%
      group_split() %>%
      lapply(function(df) {
        list(
          name = df$alue[1],
          data = round(df$kokonaistarvekerroin, 3)
        )
      })
  )

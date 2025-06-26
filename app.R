library(shiny)
library(dplyr)
library(highcharter)
library(readxl)

# Lue data
kertoimet <- readxl::read_xlsx("data/Kertoimet.xlsx")

ui <- fluidPage(
  titlePanel("Tarvevakiointitekijöiden tarkastelu"),
  
  tabsetPanel(
    tabPanel("Kehitys alueella",
             sidebarLayout(
               sidebarPanel(
                 selectInput("tyyppi1", "Valitse palvelutyyppi:",
                             choices = unique(kertoimet$tyyppi)),
                 
                 selectInput("muuttuja1", "Valitse muuttuja:", choices = NULL),
                 
                 selectInput("alue1", "Valitse alue:", choices = NULL)
               ),
               mainPanel(
                 highchartOutput("kaavio1", height = "500px")
               )
             )
    ),
    
    tabPanel("Vertailu alueittain",
             sidebarLayout(
               sidebarPanel(
                 selectInput("vuosi2", "Valitse vuosi:",
                             choices = sort(unique(kertoimet$vuosi)),
                             selected = max(kertoimet$vuosi)),
                 
                 selectInput("muuttuja2", "Valitse muuttuja:",
                             choices = sort(unique(kertoimet$Muuttuja)))
               ),
               mainPanel(
                 highchartOutput("kaavio2", height = "500px")
               )
             )
    ),
    tabPanel("Absoluuttiset erot: Ennakko vs. lopullinen (2023)",
             sidebarLayout(
               sidebarPanel(
                 selectInput("valittu_alue", "Valitse alue:", choices = sort(unique(kertoimet$alue)))
               ),
               mainPanel(
                 DTOutput("taulukko")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  ## Välilehti 1: Kehitys alueella ----
  
  observeEvent(input$tyyppi1, {
    muuttujat <- kertoimet %>%
      filter(versio == "lopullinen") %>% 
      filter(tyyppi == input$tyyppi1) %>%
      pull(Muuttuja) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "muuttuja1", choices = muuttujat, selected = muuttujat[1])
  })
  
  observeEvent(input$muuttuja1, {
    alueet <- kertoimet %>%
      filter(versio == "lopullinen") %>% 
      filter(Muuttuja == input$muuttuja1, tyyppi == input$tyyppi1) %>%
      pull(alue) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "alue1", choices = alueet, selected = alueet[1])
  })
  
  output$kaavio1 <- renderHighchart({
    df <- kertoimet %>%
      filter(
        Muuttuja == input$muuttuja1,
        alue == input$alue1,
        tyyppi == input$tyyppi1,
        versio == "lopullinen"
      )
    
    hchart(df, "line", hcaes(x = vuosi, y = arvioitu_maara)) %>%
      hc_title(text = paste0(input$muuttuja1, " – ", input$alue1)) %>%
      hc_subtitle(text = paste("Tyyppi:", input$tyyppi1)) %>%
      hc_xAxis(title = list(text = "Vuosi")) %>%
      hc_yAxis(title = list(text = "Arvioitu määrä")) %>%
      hc_tooltip(pointFormat = "{point.y:,.0f} henkilöä")
  })
  
  ## Välilehti 2: Vertailu alueittain ----
  
  output$kaavio2 <- renderHighchart({
    df <- kertoimet %>%
      filter(
        vuosi == input$vuosi2,
        Muuttuja == input$muuttuja2,
        !alue %in% c("Koko maa", "Koko maa yhteensä", "Suomi")
      ) %>%
      arrange(alue)  # <-- pitää x-akselin aakkosjärjestyksessä
    
    hchart(df, "column", hcaes(x = alue, y = arvioitu_maara, group = tyyppi)) %>%
      hc_title(text = paste0(input$muuttuja2, " – ", input$vuosi2)) %>%
      hc_subtitle(text = "Arvioitu määrä alueittain (ilman koko maata)") %>%
      hc_xAxis(title = list(text = "Alue"), labels = list(rotation = -45)) %>%
      hc_yAxis(title = list(text = "Arvioitu määrä")) %>%
      hc_tooltip(pointFormat = "<b>{point.y:,.0f}</b> henkilöä<br/>Tyyppi: {point.tyyppi}") %>%
      hc_legend(enabled = TRUE)
  })
  
  ## Välilehti 3: Vertailu ennakko vs. lopullinen
  
  erot_data <- reactive({
    kertoimet %>%
      filter(vuosi == 2023, alue == input$valittu_alue) %>%
      group_by(Muuttuja, versio) %>%
      summarise(arvioitu_maara = sum(arvioitu_maara), .groups = "drop") %>%
      pivot_wider(names_from = versio, values_from = arvioitu_maara) %>%
      mutate(
        absoluuttinen_muutos = lopullinen - ennakko
      ) %>%
      arrange(desc(abs(absoluuttinen_muutos)))
  })
  
  output$taulukko <- renderDT({
    erot_data() %>%
      select(Muuttuja, ennakko, lopullinen, absoluuttinen_muutos) %>%
      datatable(options = list(pageLength = 25), rownames = FALSE) %>%
      formatRound(columns = c("ennakko", "lopullinen", "absoluuttinen_muutos"), digits = 0)
  })
  
}

shinyApp(ui, server)

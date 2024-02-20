library(shinyjs)
library(shiny)
library(tidyverse)
# Aggiungi queste librerie se non sono già presenti
library(shiny)
library(DT)  # Questa libreria è necessaria per le tabelle DataTable

# Aggiungi questa funzione per creare la tabella
createDataTable <- function() {
  data <- data.frame(
    Route = c("Milano-Parigi", "Milano-Pechino", "Earth's Circumference"),
    Distance = c("853 km", "8074 km", "40075 km")
  )
  
  datatable(data, options = list(paging = FALSE, searching = FALSE))
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML(
        "
        #notAvailableText1, #notAvailableText2, #notAvailableText3 {
          font-size: 34px;
          text-align: center;
        }
      "
      )
    )
  ),
  
  titlePanel("CO2 consumption conditioned on a specific Hyper Volume level achieved through HPO of MLP"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("hv", "HV", min = 0.72, max = 0.858, value = 0.728),
      sliderInput("kw_hr", "Kwh (hardware specific)", min = 0.1, max = 10, value = 0.5),
      sliderInput("kg", "KgCo2/Kwh", min = 0.01, max = 1, value = 0.53),
      sliderInput("mix_energetico", "Energy Mix", min = 1, max = 100, value = 50),
     # HTML("<div id='legend10' style='text-align:center;'>0.53 KgCo2/Kwh. Constant Parameter
      #     <br>Kg.Co2 = 0.53 x Kwh x Energy Mix</div>"),
     HTML("<div id='legend10' style='text-align:center;'>
           <br>Kg.Co2 = Kg.Co2/Kwh x Kwh x h x Energy Mix</div>"),
     
      div(class = "row text-center",
          div(class = "col-xs-12",
              div(class = "legend-table", createDataTable())
          )
      )
    ),
    mainPanel(
      fluidRow(
        column(
          width = 4,
          align = "center",
          h3("autogluon-FairBO"),  
          img(id = "img1", src = "fuelauto.jpg", width = "100%"),
          br(),
          textOutput("notAvailableText1"),
          textOutput("resultNumber1"),
          shinyjs::extendShinyjs(text = "shinyjs.hideImg = function() { $('#img1').hide(); }",
                                 functions = c("hideImg")),
          shinyjs::extendShinyjs(text = "shinyjs.showImg = function() { $('#img1').show(); }",
                                 functions = c("showImg"))
        ),
        column(
          width = 4,
          align = "center",
          h3("FanG-HPO"),  
          img(id = "img2", src = "fuelfang.jpg", width = "100%"),
          br(),
          textOutput("notAvailableText2"),
          textOutput("resultNumber2"),
          shinyjs::extendShinyjs(text = "shinyjs.hideImg = function() { $('#img2').hide(); }",
                                 functions = c("hideImg")),
          shinyjs::extendShinyjs(text = "shinyjs.showImg = function() { $('#img2').show(); }",
                                 functions = c("showImg"))
        ),
        column(
          width = 4,
          align = "center",
          h3("BoTorch-MOMF"),
          img(id = "img3", src = "fuelboto.jpg", width = "100%"),
          br(),
          textOutput("notAvailableText3"),
          textOutput("resultNumber3"),
          shinyjs::extendShinyjs(text = "shinyjs.hideImg = function() { $('#img3').hide(); }",
                                 functions = c("hideImg")),
          shinyjs::extendShinyjs(text = "shinyjs.showImg = function() { $('#img3').show(); }",
                                 functions = c("showImg"))
        )
      )
    )
  )
)



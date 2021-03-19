library(shiny)
library(shinyWidgets) #fuer select input picker
library(DT) # for big tables (search, filter and so on)
library(rhandsontable)
library(formattable) #percent
source("ofc.R")

ui <- pageWithSidebar(

  # App title ----
  headerPanel("Flavelloni's Pineapple-Rechner"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    shinyWidgets::checkboxGroupButtons(
      inputId = "numOfPlayers",
      label = "Anzahl Spieler",
      choiceNames = c("1 Spieler", "2 Spieler", "3 Spieler"),
      choiceValues = 1:3,
      justified = FALSE,
      individual = TRUE,
      selected = 2,
      status = "info",
      checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
    )
    
    , shinyWidgets::pickerInput(inputId="round",
                              label="Runde",
                              choices=1:4,
                              #options = list(`actions-box` = TRUE),
                              multiple=FALSE,
                              selected=1)
    
    , shinyWidgets::pickerInput(inputId="runners",
                                label="Wie viele Karten fehlen dir noch (z.B. zum Flush)?",
                                choices=1:3,
                                #options = list(`actions-box` = TRUE),
                                multiple=FALSE,
                                selected=1)
    
    
    , shinyWidgets::knobInput(
      inputId = "outs",
      label = "Outs",
      value = 1,
      min = 0,
      max = 15,
      displayPrevious = TRUE,
      lineCap = "round",
      fgColor = "#428BCA",
      inputColor = "#428BCA"
    )
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    rHandsontableOutput("probs"),
    123
  )
)

################################################################################

server <- function(input, output) {
  
  probs <- reactive({
    
    n_players <- as.numeric(input$numOfPlayers)
    round <- as.numeric(input$round)
    outs <- as.numeric(input$outs)
    runners <- as.numeric(input$runners)

    res <- data.frame(
      Position=1:3
      , Probs=overview(round, outs, runners)[, n_players] %>% 
        formattable::percent() %>% 
        as.character()
    )
    
   
    
    return(res)
  })

  output$probs <- renderRHandsontable(
    rhandsontable(probs())
  )
  
}


################################################################################
shinyApp(ui, server)
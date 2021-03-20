probs_calculator_input <- function(){
  shinydashboard::box(
    
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
  ) #box
}
  
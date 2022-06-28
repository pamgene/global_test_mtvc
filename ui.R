library(shiny)
library(shinyjs)

ui = shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  tags$script(HTML('setInterval(function(){ $("#hiddenButton").click(); }, 1000*30);')),
  tags$footer(shinyjs::hidden(actionButton(inputId = "hiddenButton", label = "hidden"))),
  
  titlePanel("Global Test MTvC"),
  
  sidebarPanel(
      selectInput("gv", "Set the reference value",choices = ""),
      selectInput("sf", "Set a stratification factor", choices = ""),
      checkboxInput("standardize", "Standardize", value = FALSE),
      checkboxInput("directional", "Directional", value = FALSE),
      actionButton("runBtn", "Run test", disabled=TRUE),
      h3(textOutput("mode")),
      h5(textOutput("msg"))
   ),
  
  mainPanel(
    tableOutput("design")
  )
  
))
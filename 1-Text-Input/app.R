####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "My first app",
                  tabPanel("Navbar 1",
                          sidebarPanel(
                            tags$h3("Input:"),
                            textInput("txt1", "Given Name:", "John"), # txt1 will be sent to the server
                            textInput("txt2", "Surname:", "Doe"), # txt2 will be sent to the server
                            
                          ), # sidebarPanel
                          mainPanel(
                          h1("Header 1"),
                            
                            h4("Output 1"),
                            verbatimTextOutput("txtout"), # txtout is generated from the server
                            
                          ) # mainPanel
                          
                  ), # Navbar 1, tabPanel
                  tabPanel("Navbar 2", "This panel is intentionally left blank"),
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                ) # navbarPage
) # fluidPage


# Define server function
# Handles the processing of the data to produce output results that are displayed
server <- function(input, output) {
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
} # server


# Create Shiny object
# Fuses the UI and server components
shinyApp(ui = ui, server = server)
require(shiny)
require(ggplot2)

ui <- fluidPage(
  titlePanel("Car Weight"),
  br(),
  uiOutput(outputId = "cylinders"),
  sidebarLayout(
    mainPanel(
      tableOutput("table"),
      uiOutput(outputId = "dataFilter"),
      actionButton(inputId = "update1", label = "Apply Filters"),
      width = 9
    ),
    sidebarPanel(
      actionButton(inputId = "update2", label = "Apply Filters"),
      uiOutput(outputId = "modelFilter"),
      actionButton(inputId = "update3", label = "Apply Filters"),
      width = 3
    )
  )
)

server <- function(input, output) {
  # Read data.  Real code will pull from database.
  df <- mtcars
  df$model <- row.names(df)
  df <- df[order(df$model), c(12,1,2,3,4,5,6,7,8,9,10,11)]
  print(str(df))
  # Get cylinders
  output$cylinders <- renderUI({
    selectInput(
      inputId = "cyl",
      label = "Select Cylinders",
      choices = c("", as.character(unique(df$cyl)))
    )})
  
  # print(str(output))
  # Check if data frame has been updated.
  values <- reactiveValues(update = 0)
  
  # Subset data by cyl.
  df2 <-
    reactive({
      values$update <- 0
      df2 <- droplevels(df[df$cyl == input$cyl,])})
  
  # Filter data.
  df3 <-
    eventReactive({
      input$update1
      input$update2
      input$update3
      df2()
    },
    {
      if (values$update > 0) {
        req(input$modelFilter)
        modelFilterDf <-
          data.frame(model = input$modelFilter)
        df3a <-
          merge(df2(), modelFilterDf, by = "model")
        df3a <- df3a[df3a$wt >= input$dataFilter[1] &
                       df3a$wt <= input$dataFilter[2], ]
      } else {
        df3a <- df2()
      }
      
      values$update <- values$update + 1
      df3a
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE)
  
  # Plot table.
  output$table <- renderTable(df3())
  
  # Filter by data value.
  output$dataFilter <-
    renderUI({
      req(df2()$wt[1])
      sliderInput(
        inputId = "dataFilter",
        label = "Filter by Weight (1000 lbs)",
        min = floor(min(df2()$wt, na.rm = TRUE)),
        max = ceiling(max(df2()$wt, na.rm = TRUE)),
        value = c(floor(min(df2()$wt, na.rm = TRUE)),
                  ceiling(max(df2()$wt, na.rm = TRUE))),
        step = round(max(df2()$wt, na.rm = TRUE) - min(df2()$wt, na.rm = TRUE)) / 100,
        round = round(log((
          max(df2()$wt, na.rm = TRUE) - min(df2()$wt, na.rm = TRUE)
        ) / 100))
      )
    })
  
  # Filter by lot / wafer.
  output$modelFilter <- renderUI({
    req(input$cyl)
    checkboxGroupInput(
      inputId = "modelFilter",
      label = "Filter by Model",
      choices = as.character(unique(df2()$model)),
      selected = as.character(unique(df2()$model))
    )
  })
}

# Run shiny.
shinyApp(ui = ui, server = server)
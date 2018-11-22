library(shiny)
library(tidyverse)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)
# Define UI for application that draws a histogram
ui <- fluidPage(
  img(src = "bcl.png", size=0.5),
  titlePanel("BC Liquor price app", 
             windowTitle = "BCL app"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Select your desired price range.",
                  min = 0, max = 100, value = c(15, 30), pre="$"),
      checkboxInput("sortInput", "Sort by price"),
      colourpicker::colourInput("colorInput", "Input color", "red"),
      checkboxGroupInput("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      conditionalPanel(condition = "input.typeInput == 'WINE'", 
                       checkboxGroupInput("typeInputConditional", "Sweetness level",
                                          choices = 0:10,
                                          selected = 0)),
      downloadButton("downloadData", "Download")
    ),
    mainPanel(
      plotOutput("price_hist"),
      DT::dataTableOutput("bcl_data")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  bcl_filtered <- reactive({
    bcl %>% 
      { if(nrow(.)==0) NULL else filter(., Price < input$priceInput[2], Price > input$priceInput[1], Type == input$typeInput) } %>%
      { if(nrow(.)==0) NULL else filter(., Sweetness %in% input$typeInputConditional, Type == input$typeInput) } %>% 
      { if(input$sortInput == TRUE) arrange(., Price) else .}
  })
  output$price_hist <- renderPlot({
    bcl_filtered() %>% 
      ggplot(aes(Price))+
      geom_histogram(fill=input$colorInput)
  })
  output$bcl_data <- DT::renderDataTable({
    bcl_filtered()
  })
  output$downloadData <- downloadHandler("bcl_filtered.csv", function(file) { write.csv(bcl_filtered(), file) })
}

# Run the application 
shinyApp(ui = ui, server = server)


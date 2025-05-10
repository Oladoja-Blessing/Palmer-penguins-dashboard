# app.R

library(shiny)
library(ggplot2)
library(dplyr)
library(palmerpenguins)
library(bslib)
library(DT)

ui <- fluidPage(
  theme = bs_theme(bg = "white", fg = "black", version = 4),
  titlePanel("Palmer Penguins • Colored Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      img(src = "penguins.png", height = "100px"),  # add cartoon image to www/
      br(),
      sliderInput("billLength", "Bill length (mm):", min = 30, max = 60, value = c(30, 60)),
      checkboxGroupInput("islands", "Island:", 
                         choices = unique(penguins$island),
                         selected = unique(penguins$island)),
      helpText("A simple example based on Allison Horst’s Palmer Penguins dataset."),
      helpText("Filter and view how penguin body mass varies across islands and species.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 h4("Bill Length Distribution by Species"),
                 plotOutput("facetHist")
        ),
        tabPanel("Data",
                 h4("Filtered Data Table"),
                 DTOutput("dataTable")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    penguins %>%
      filter(
        !is.na(bill_length_mm),
        bill_length_mm >= input$billLength[1],
        bill_length_mm <= input$billLength[2],
        island %in% input$islands
      )
  })
  
  output$facetHist <- renderPlot({
    ggplot(filtered_data(), aes(x = bill_length_mm, fill = species)) +
      geom_histogram(bins = 15, alpha = 0.7, position = "identity") +
      facet_wrap(~ species) +
      scale_fill_manual(values = c("Adelie" = "#1f77b4", "Chinstrap" = "#ff7f0e", "Gentoo" = "#2ca02c")) +
      theme_minimal() +
      labs(x = "Bill Length (mm)", y = "Frequency") +
      theme(legend.position = "none")
  })
  
  output$dataTable <- renderDT({
    datatable(filtered_data())
  })
}

shinyApp(ui, server)

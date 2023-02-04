#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(stringr)
library(ggplot2)
library(DT)
library(dplyr)

# Import January 2023 Pittsburgh building permits dataset
jan.2023.permits <- read_csv(
  "pli-permit-summary-january-2023.csv", 
  col_types = cols(
    ISSUEDATE = col_date(format="%Y/%m/%d")
  )
) %>%
  filter(TOTALPROJECTVALUE < 1000000)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("City of Pittsburgh Building Permit Data, January 2023"),

  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      
      # Slider input for project value
      sliderInput(
        "cost", "Project Value ($)",
        min = min(jan.2023.permits$TOTALPROJECTVALUE),
        max = max(jan.2023.permits$TOTALPROJECTVALUE),
        value = c(
          min(jan.2023.permits$TOTALPROJECTVALUE),
          max(jan.2023.permits$TOTALPROJECTVALUE)
        )
      ),
      
      # Checkbox input for different neighborhoods
      checkboxGroupInput(
        "nbrhd", "Neighborhoods",
        choices = jan.2023.permits$SNP_NEIGHBORHOOD %>%
          unique() %>% sort(),
        selected = c("North Oakland","Shadyside","Bloomfield")
      )
    ),

    # Output
    mainPanel(
      
      # Show a stacked bar plot of the number of projects in each nbrhd
      plotOutput("stacked.bar.plot"),
      br()  # a little bit of visual separation
      
      # # Split two histograms across the page
      # fluidRow(
      # 
      #   # Show a histogram of residential project value
      #   column(4, plotOutput("res.hist")),
      # 
      #   # Show a histogram of commercial project value
      #   column(4, plotOutput("com.hist"))
      # ),
      
      # Show data table (DT) of selected data
      #::dataTableOutput(outputId = "permit.table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Create a subset of data filtered by selected data
  nbrhd.subset <- reactive({
    req(input$nbrhd)
    filter(jan.2023.permits,
      SNP_NEIGHBORHOOD %in% input$nbrhd &
      TOTALPROJECTVALUE >= input$cost[1] &
      TOTALPROJECTVALUE <= input$cost[2]
    )
  })
  
  # Create a permit count by neighborhood of reactive
  nbrhd.counts <- reactive({
    req(input$nbrhd)
    filter(jan.2023.permits,
      SNP_NEIGHBORHOOD %in% input$nbrhd &
      TOTALPROJECTVALUE >= input$cost[1] &
      TOTALPROJECTVALUE <= input$cost[2]
    ) %>%
      group_by(SNP_NEIGHBORHOOD) %>%
      summarize(permits = length(PERMITNUMBER))
  })
  
  # Plot the bar plot
  output$stacked.bar.plot <- renderPlot({
    ggplot(
      nbrhd.counts(),
      aes(
        x = input$nbrhd,
        y = permits
      )
    ) +
      geom_bar(stat="identity") +
      labs(
        x="Neighborhood", y="count",
        title="Number of Permits Issued in Each Neighborhood (Under $1M)"
      ) +
      theme_minimal()
  })
  
  # Plot residential histogram
  #output$
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(shinyWidgets)
library(modeldata)
data(ames, package = "modeldata")


ui <- fluidPage(

    # Application title
    titlePanel("Analysis of Ames Dataset"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            pickerInput("house_style", "House Style",
                        choices = unique(ames$House_Style),
                        selected = unique(ames$House_Style),
                        options = list(`actions-box` = TRUE),
                        multiple = T)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("mapPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


  filtered_data <- reactive({

    filtered_ames <- ames %>%
      filter(House_Style %in% input$house_style)

    return(filtered_ames)


  })


    output$mapPlot <- renderPlotly({

        plot_ly(
          data = filtered_data(),
          lat = ~Latitude,
          lon = ~Longitude,
          marker = list(alpha = 0.5),
          mode = 'markers',
          type = 'scattermapbox',
          color = ~Sale_Price,
          text = ~Sale_Price,
          hoverinfo = text

        ) %>%
        layout(
          mapbox = list(
            style = 'open-street-map',
            zoom = 11,
            center = list(lon = -93.62, lat = 42.02)),
          showlegend = FALSE)
    })
}

# Run the application
shinyApp(ui = ui, server = server)

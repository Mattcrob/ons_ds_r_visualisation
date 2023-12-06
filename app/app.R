library(shiny)
library(shinyWidgets)
library(modeldata)
library(plotly)
library(ggplot2)
library(viridis)
library(RColorBrewer)
library(readxl)
library(tidyverse)

data(ames, package = "modeldata")

Ames_Data <- read_csv("raw_ames.csv")
Ratings <- tibble(Scale = c(1,2,3,4,5,6,7,8,9,10),
                  Rating = c('Very Poor','Poor','Fair','Below Average','Average','Above Average','Good','Very Good','Excellent','Very Excellent')) %>%
  mutate(Rating = factor(Rating,levels = c('Very Poor','Poor','Fair','Below Average','Average','Above Average','Good','Very Good','Excellent','Very Excellent')))

Ames_Data <- Ames_Data %>%
  left_join(Ratings, by = c('Overall Cond'='Scale'))%>%
  rename('Overall Condition'=Rating) %>%
  left_join(Ratings, by = c('Overall Qual'='Scale'))%>%
  rename('Overall Quality'=Rating)

HouseStyles <- Ames_Data$`House Style`%>% unique()

HouseRef <- tibble(HS = HouseStyles, 'House Type' = c('One story','Two story','One and one-half story: 2nd level finished','Split Foyer','Split Level','Two and one-half story: 2nd level unfinished','One and one-half story: 2nd level unfinished','Two and one-half story: 2nd level finished'))

Ames_Data <- Ames_Data %>%
  left_join(HouseRef , by = c('House Style'='HS'))%>%
  select(-`House Style`)%>%
  rename('House Style'=`House Type`)

ui <- fluidPage(

  # Application title
  titlePanel("Ames Iowa Housing Dataset"),

  navlistPanel(widths = c(2,8),
               tabPanel("Map", fluid = TRUE,

                        # Dropdown menus

                            pickerInput("house_style", "House Style",
                                        choices = unique(ames$House_Style),
                                        selected = unique(ames$House_Style),
                                        options = list(`actions-box` = TRUE),
                                        multiple = T),
                            pickerInput("year_built", "Year Built",
                                        choices = sort(unique(ames$Year_Built)),
                                        selected = unique(ames$Year_Built),
                                        options = list(`actions-box` = TRUE),
                                        multiple = T),
                            pickerInput("garage_condition", "Garage Condition",
                                        choices = unique(ames$Garage_Cond),
                                        selected = unique(ames$Garage_Cond),
                                        options = list(`actions-box` = TRUE),
                                        multiple = T),


                          # Map

                            plotlyOutput("mapPlot", height = 700)


               ),
               tabPanel("Plots", fluid = TRUE,
                        pickerInput("housestyle", "Filter by House Style",
                                    choices = sort(unique(Ames_Data$`House Style`)),
                                    selected = sort(unique(Ames_Data$`House Style`)),
                                    options = list(`actions-box` = TRUE),
                                    multiple = T),
                        pickerInput("neighbourhood", "Filter by Neighbourhood",
                                    choices = sort(unique(Ames_Data$Neighborhood)),
                                    selected = sort(unique(Ames_Data$Neighborhood)),
                                    options = list(`actions-box` = TRUE),
                                    multiple = T),
                        pickerInput("ggcolour", "Colour by Variable",
                                    choices = c('Overall Quality','Overall Condition')),
                        plotlyOutput("plot1", height = 500)
               )
  ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {


  filtered_data_map <- reactive({

    filtered_ames <- ames %>%
      filter(House_Style %in% input$house_style,
             Year_Built %in% input$year_built,
             Garage_Cond %in% input$garage_condition)

    return(filtered_ames)


  })

  filtered_data_plot <- reactive({
    ames_filtered <- Ames_Data %>%
      filter(`House Style` %in% input$housestyle,
             Neighborhood %in% input$neighbourhood)


    return(ames_filtered)
  })


  output$mapPlot <- renderPlotly({

    plot_ly(
      data = filtered_data_map(),
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

  ranges <- reactiveValues(x = NULL, y = NULL)

  output$plot1 <- renderPlotly({
    ggplotly(
    ggplot(filtered_data_plot() , aes(`Lot Area`, SalePrice)) +
      geom_point(border='black',shape=21, size=2.5, aes(fill=as.factor(get(input$ggcolour))))+
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
      ggtitle('Ames Iowa Houses: Sales Price by Lot Area',subtitle = paste0('Neighbourhood: ', input$neighbourhood, ' | House Style: ',input$housestyle,' | Coloured by ',input$ggcolour))+
      scale_fill_brewer(palette='Spectral',name=input$ggcolour)+
      theme_bw()+
      ylab('Sales Price (Dollars)')+
      xlab('Lot Area (Square Feet)')
    )
  })


}

# Run the application
shinyApp(ui = ui, server = server)

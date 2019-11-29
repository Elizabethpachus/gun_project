library(readr)
library(markdown)
library(shiny)
library(ggthemes)
library(maps)
library(openintro)
library(tidyverse)
library(ggplot2)
library(maps)
library(ggthemes)
library(janitor)
library(openintro)
library(tidyverse)
library(tidyverse)
library(sf)
library(fs)
library(ggplot2)
library(maps)
library(ggthemes)
library(janitor)
library(openintro)
library(scales)
library(infer)
library(readr)
library(broom)
library(ggthemes)
library(mapdata)
library(mapproj)
library(tidyverse)


map_data_app <- read_rds("map_data.rds")

suicide_data_app <- read_rds("final_data.rds")

# Define UI for application that displays my graph
ui <- fluidPage(
    
    # Application title
    titlePanel("Guns in America"),
    
    navbarPage("Menu",
               
               # First panel on website
               
               tabPanel("Firearm Death Rate",
                        plotOutput("firearm_plot"),
                        selectInput(inputId = "year", 
                                    label = "Select Year", 
                                    choices = c("2017" = 2017,
                                                "2016" = 2016,
                                                "2015" = 2015,
                                                "2014" = 2014,
                                                "2005" = 2005), selected = "a"),
                        plotOutput("suicide_map"),
                        
               ),
               
               # Second panel on website
               
               tabPanel("Suicide Rate and Deaths by Guns",
                        selectInput(inputId = "year2",  # Give the input a name "genotype"
                                    label = "Select Year",  # Give the input a label to be displayed in the app
                                    choices = c("2017" = 2017,
                                                "2016" = 2016,
                                                "2015" = 2015,
                                                "2014" = 2014,
                                                "2005" = 2005), selected = "a"),
                        plotOutput("suicide_plot")
               ),
               
               # Including the About page info here
               
               tabPanel("About",
                        includeMarkdown("about.md")
               )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    # Creating the map graphic on the home page
    
    output$firearm_plot <- renderPlot(ggplot(data = map_data_app[map_data_app$year == input$year,],
                                             mapping = aes(x = long, y = lat, group = group, fill = rate_per_1000)) + 
                                                 
                                          geom_polygon(color = "gray90", size = 0.1) +
                                          coord_map(projection = "albers", 
                                                    lat0 = 39, lat1 = 45) + 
                                         
                                          labs(title = "Firearm Mortality by State") + 
                                          theme_map() + 
                                          labs(fill = "Death Rate per 100,000") + 
                                          
                                          # Adding the color scale 
                                          
                                          scale_fill_gradient(low = "#FCB4B4",
                                                              high = "#FB0000",
                                                              limits = c(0,25)) +
                                          labs(title = "Firearm Mortality by State",
                                               subtitle = "Firearm Death Rate Per 1000 People",
                                               caption = "Data from CDC") +
                                          theme(legend.position = "right",
                                                plot.title = element_text(hjust = 0.5))
    )
    
    # Creating map of suicide rates in America per year
    
    output$suicide_map <- renderPlot(ggplot(data = map_data_app[map_data_app$year == input$year,],
                                             mapping = aes(x = long, y = lat, group = group, fill = suicide_rate)) + 
                                         
                                         # Adding the neccesary aspects for the plot to look like a map 
                                         
                                          geom_polygon(color = "gray90", size = 0.1) +
                                          coord_map(projection = "albers", 
                                                    lat0 = 39, lat1 = 45) + 
                                          
                                          labs(title = "Suicide Rate by State") + 
                                          theme_map() + 
                                         
                                         # Setting the coloring of the states with the max as the max for that category
                                         
                                          scale_fill_gradient(low = "#C4C5F1",
                                                              high = "#080BBD",
                                                              limits = c(0,30)) +
                                          labs(title = "Suicide Rate by State",
                                               subtitle = "Suicide Death Rate Per 1000 People",
                                               caption = "Data from CDC",
                                               fill = "Rate per 100,000") +
                                          theme(legend.position = "right",
                                                plot.title = element_text(hjust = 0.5))
    )
    
    # Creating a graph trying to find the correlation between suicides and gun deaths per year
    
    output$suicide_plot <- renderPlot(ggplot(data = suicide_data_app[suicide_data_app$year == input$year2,], aes(x = suicide_rate, y = deaths_year)) +
                                          geom_point() +
                                          geom_smooth(method = "lm") +
                                          labs(
                                              title = "Deaths per Year by Guns by Suicide Rate of State",
                                              x = "Suicide Rate per 1000",
                                              y = "Deaths per Year"
                                          ) +
                                          theme_gdocs(base_size = 12, base_family = "sans")
                                      
                                      
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

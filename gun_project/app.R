# This is the main page behind the webpage
# It renders all my graphics. The first section is mostly prep, then it turns into the shiny app interface

# Installing necessary libraries

library(readr)
library(markdown)
library(shiny)
library(maps)
library(sf)
library(fs)
library(ggplot2)
library(ggthemes)
library(openintro)
library(scales)
library(infer)
library(broom)
library(ggridges)
library(ggthemes)
library(shinythemes)
library(mapdata)
library(mapproj)
library(tidyverse)

# Reading in the RDS files 

map_data_app <- read_rds("map_data.rds")

suicide_data_app <- read_rds("final_data.rds")

data_538 <- read_rds("data_538.rds")

final_bootstrap <- read_rds("final_bootstrap.rds")



# MAIN SHINY APP

# Define UI for application that displays my graph

ui <- fluidPage(theme = shinytheme("flatly"),
    
    # Application title
    
    titlePanel("Guns & Suicide in America"),
    
    # Installing the Nav bar across the top of the application
    
    navbarPage("Menu",
               
               # First panel on website, going to incoroorate my favorite type of graphic (maps)
               # The contrasting color and strong visual appearance make it an easy pick for the first thing
               # people see when they open the website
               
               tabPanel("Maps",
                        
                        # Side bar layout
                        
                        sidebarLayout(
                            
                            # In the panel is the dropdown to select the year. I chose not to have the slider because 
                            # the years are sequential except for 2005
                            
                            sidebarPanel("At What Rates are Americans Dying from Guns and Suicides?",
                                         br(),
                                         selectInput(inputId = "year", 
                                                     label = "Select Year", 
                                                     choices = c("2017" = 2017,
                                                                 "2016" = 2016,
                                                                 "2015" = 2015,
                                                                 "2014" = 2014,
                                                                 "2005" = 2005), selected = "a")),
                            
                            # Display the graphics
                            
                            mainPanel("Where in America?",
                                      plotOutput("firearm_plot"),
                                      plotOutput("suicide_map"))
                        ),

                        
                        
                        
               ),
               
               # This is the mega graphic of the project
               
               tabPanel("Who's Dying?",
                        
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                        
                                    # Including a tab so people can choose how to break down the graphic
                                    
                                    selectInput(inputId = "choice",  
                                                label = "View deaths by:", 
                                                choices = c("Gender" = "gender",
                                                            "Intent" = "intent",
                                                            "Age" = "age"), selected = "a"),
                                    
                                    # This input selected chooses if the data should be displayed in RATE of death or total deaths
                                    # Personally, I believe when dealing with deaht the total number is more powerful than the rate
                                    
                                    selectInput(inputId = "viewpoint",  
                                                label = "Measure", 
                                                choices = c("Total Deaths" = "deaths",
                                                            "Death Rate Per 100,000" = "rate"), selected = "a")),
                                    
                        mainPanel(           
                        plotOutput("death_by_race")))
               ),
               
               # Third panel on website, the statistical modeling portion of the project
               
               tabPanel("Suicide Rate and Firearm Death",
                        
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                    # Setting up the input for the regression modeling
                                    
                                    selectInput(inputId = "year2",  
                                                label = "Select Year", 
                                                choices = c("2017" = 2017,
                                                            "2016" = 2016,
                                                            "2015" = 2015,
                                                            "2014" = 2014,
                                                            "2005" = 2005), selected = "a")),
                            
                            # Adding a panel where the user can choose which graph to view
                        
                            mainPanel(
                                      tabsetPanel(
                                          
                                          # Graph displaying the correlation between firearm death rate averages and 
                                          # suicide rates per state
                                          
                                          tabPanel("Plot", plotOutput("suicide_plot")),
                                          
                                          # This graph shows the coefficents which show the correlation between firearm 
                                          # and suicide rate per state
                                          
                                          # This graph shows the r squared value per state
                                          
                                          tabPanel("Summary", plotOutput("suicide_plot2"))
                                      ))
                        )
               ),
               
               # Including the About page info here
               
               tabPanel("About",
                        includeMarkdown("about.md")
               )
    )
)




# SERVER SIDE OF APP

server <- function(input, output) {
    
    
    # Creating the map graphic on the home page, the first graphic which lists the firearm death rate per state
    
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
    
    # Creating visual statistical
    
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
    
    output$suicide_plot <- renderPlot(ggplot(data = suicide_data_app[suicide_data_app$year == input$year2,],
                                             aes(x = suicide_rate, y = deaths_year)) +
                                          geom_point() +
                                          
                                          # Setting limits for the graphs so that even as the year changes the 
                                          # actual scales will remain the same to make it easier to differentiate 
                                          # between the years
                                          
                                          scale_y_continuous(limits = c(0,3300)) +
                                          scale_x_continuous(limits = c(8,30)) +
                                          geom_smooth(method = "lm") +
                                          labs(
                                              title = "Deaths per Year by Guns by Suicide Rate of State",
                                              x = "Suicide Rate per 1000",
                                              y = "Deaths per Year"
                                          ) +
                                          theme_gdocs(base_size = 12, base_family = "sans")
                                      
                                      
    )
    
    # R value correlation between suicides and firearm deaths
    
    output$suicide_plot2 <- renderPlot(ggplot(data = final_bootstrap[final_bootstrap$year == input$year2,],
                                              mapping = aes(x = mean_rsquared, y = reorder(state_name, mean_rsquared), color = state_name)) + 
                                           geom_jitter(width = 0.05) + 
                                           labs(title = "Uncertainty of R Squared Value by State",
                                                subttile = "Is Firearm Death Rate Associated with Suicide Rate?",
                                                x = 'R Square Value',
                                                y = "State") +
                                           theme_gdocs(base_size = 12, base_family = "sans") +
                                           theme(legend.position = "none")
                                      
                                      
    )
    
    
    # Creating a graphic which incorporates as much as the 385's data as possible
    # This is the graphic I hope people will spend the most amount of time on.
    
    output$death_by_race <- renderPlot(ggplot(data = data_538, aes(x = race, fill = race, y = deaths)) +
                                                  geom_col() +
                                                  labs(title = "Total Gun Deaths By Race",
                                                       x = "Race",
                                                       y = "Deaths",
                                                       caption = "Deaths are an average from 2012-2014"
                                                  ))
                                      
                                      
    
}

# Running  the application 

shinyApp(ui = ui, server = server)

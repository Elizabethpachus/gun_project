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
library(shinyWidgets)
library(mapdata)
library(mapproj)
library(tidyverse)

# Reading in the RDS files 

map_data_app <- read_rds("map_data.rds")

suicide_data_app <- read_rds("final_data.rds")

data_538 <- read_rds("data_538.rds")

final_bootstrap <- read_rds("final_bootstrap.rds")

veteran_data <- read_rds("veterans.rds")

veteran_small <- read_rds("veterans_small.rds")




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
               
               tabPanel("Home",
                        h2("Welcome to my Project!", align = "center"),
                        htmlOutput("gun_pic")),
               
               tabPanel("Maps",
                        
                        # Side bar layout
                        
                        sidebarLayout(
                            
                            # In the panel is the dropdown to select the year. I chose not to have the slider because 
                            # the years are sequential except for 2005
                            
                            sidebarPanel("At What Rates are Americans Dying from Guns and Suicides?",
                                         p(""),
                                         selectInput(inputId = "year", 
                                                     label = "Select Year", 
                                                     choices = c("2017" = 2017,
                                                                 "2016" = 2016,
                                                                 "2015" = 2015,
                                                                 "2014" = 2014,
                                                                 "2005" = 2005), selected = "a"),
                                         p("These maps depict the death rate by firearms and the suicide death rate for each of the states")),
                                    
                            
                            # Output the two maps
                            
                            mainPanel(
                                      tabsetPanel(
                                          tabPanel("Firearm plot",
                                                   h4("Firearm Morality by State", align = "center"),
                                                   h6("Death rate per 1,000 people", align = "center"),
                                                   plotOutput("firearm_map"),
                                                   p("DO A WRITEUP HERE")),
                                          tabPanel("Suicide Map",
                                                   h4("Suicide Rate by State", align = "center"),
                                                   h6("Death rate per 1,000 people", align = "center"),
                                                   plotOutput("suicide_map"),
                                                   p("DO A WRITEUP HERE"))
                                          
                                      ))

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
                                                            "Age" = "age"), selected = "gender"),
                                    p("Choose how to break down the total population deaths")),
                                    
                        mainPanel(           
                        plotOutput("death_by_race")))
               ),
               
               # Third panel on website, the statistical modeling portion of the project
               
               tabPanel("Is there a correlation?",
                        
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
                        
                            mainPanel("What is the relationship between Firearm Deaths and Suicide Rate?",
                                      tabsetPanel(
                                          
                                          # Graph displaying the correlation between firearm death rate averages and 
                                          # suicide rates per state
                                          
                                          tabPanel("Plot", plotOutput("suicide_plot")),
                                          
                                          # This graph shows the coefficents which show the correlation between firearm 
                                          # and suicide rate per state
                                          
                                          tabPanel("Correlation?", plotOutput("correlation_plot", height = "700px")),
                                          
                                          # This graph shows the r squared value per state
                                          
                                          tabPanel("Uncertainty", plotOutput("uncertainty_plot", height = "700px"))
                                      ))
                        )
               ),
               
               # Include a section on veteran data
               
               tabPanel("Veteran Data",
                        h3("Number of Veteran Suicides Per Year", align = "center"),
                        plotOutput("veteran_linegraph", height = "300px"), 
                        h3("Number of Living Veterans Per Year", align = "center"),
                        plotOutput("veteran_linegraph2", height = "200px")
               ),
               
               # Including the About page info here
               
               tabPanel("About",
                        includeMarkdown("about.md")
               )
    )
)




# SERVER SIDE OF APP

server <- function(input, output) {
    
    
    # Image output
    
    output$gun_pic <- renderText({
        c(
            '<img src="',
            "https://static01.nyt.com/images/2015/12/14/opinion/14mon1/14mon1-superJumbo.jpg?quality=90&auto=webp",
            '">')
        
    })
    
    
    
    # Creating the map graphic on the home page, the first graphic which lists the firearm death rate per state
    
    output$firearm_map <- renderPlot(ggplot(data = map_data_app[map_data_app$year == input$year,],
                                             mapping = aes(x = long, y = lat, group = group, fill = rate_per_1000)) + 
                                                 
                                          geom_polygon(color = "gray90", size = 0.1) +
                                          coord_map(projection = "albers", 
                                                    lat0 = 39, lat1 = 45) + 
                                          theme_map() + 
                                          labs(fill = "Rate per 1,000") + 
                                          
                                          # Adding the color scale 
                                          
                                          scale_fill_gradient(low = "#FCB4B4",
                                                              high = "#FB0000",
                                                              limits = c(0,25)) +
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
                                          theme_map() + 
                                         
                                         # Setting the coloring of the states with the max as the max for that category
                                         
                                          scale_fill_gradient(low = "#C4C5F1",
                                                              high = "#080BBD",
                                                              limits = c(0,30)) +
                                         
                                         # Adding titles 

                                          theme(legend.position = "right",
                                                plot.title = element_text(hjust = 0.5))
    )
    
    # Veteran Map
    
    output$veteran_map <- renderPlot(ggplot(data = map_data_app[map_data_app$year == input$year,],
                                            mapping = aes(x = long, y = lat, group = group, fill = as.integer(veteran_suicides))) + 
                                         
                                         # Adding the neccesary aspects for the plot to look like a map 
                                         
                                         geom_polygon(color = "gray90", size = 0.1) +
                                         coord_map(projection = "albers", 
                                                   lat0 = 39, lat1 = 45) + 
                                         theme_map() + 
                                         
                                         # Setting the coloring of the states with the max as the max for that category
                                         
                                         scale_fill_gradient(low = "#96d497",
                                                             high = "#006e02",
                                                             limits = c(0,500)) +
                                         
                                         # Adding titles 
                                              labs(fill = "Legend") +
                                         theme(legend.position = "right",
                                               plot.title = element_text(hjust = 0.5))
    )
    
    
    
    
    
    # Creating a graph trying to find the correlation between suicides and gun deaths per year
    # I am trying to answer the question, does suicide death rate impact firearm death rate?
    
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
                                              x = "Suicide Rate per 1,000",
                                              y = "Deaths per Year"
                                          ) +
                                          theme_minimal()
                                      
    )
    
    # R value correlation between suicides and firearm deaths
    
    output$uncertainty_plot <- renderPlot(ggplot(data = final_bootstrap,
                                              mapping = aes(x = mean_rsquared,
                                                            y = reorder(state_name, mean_rsquared),
                                                            color = state_name)) + 
                                           geom_jitter(width = 0.03,
                                                       height = 0.01) + 
                                           scale_y_discrete(expand = expand_scale(mult = 0, add = 0)) +
                                           labs(title = "Uncertainty of R Squared Value by State",
                                                subttile = "What is the uncertainity with this model?",
                                                x = 'R Square Value',
                                                y = "State") +
                                           theme_minimal() +
                                           theme(legend.position = "none")
                                      
                                      
    )
    
    
    output$correlation_plot <- renderPlot(ggplot(data = final_bootstrap,
                                              mapping = aes(x = mean_coefficient,
                                                            y = reorder(state_name, mean_coefficient),
                                                            color = state_name)) + 
                                           geom_point() + 
                                           labs(title = "What is the Correlation Between Firearm Death Rate and Suicide Rate?",
                                                x = 'Intercept',
                                                y = "State") +
                                           theme_minimal() +
                                           theme(legend.position = "none")
                                       
                                       
    )
    
    
    # Creating a graphic which incorporates as much as the 385's data as possible
    # This is the graphic I hope people will spend the most amount of time on.
   
    
    output$death_by_race <- renderPlot({
        
        if (input$choice == "gender") {
            data_538 %>% 
                
                # filtering data
                
                filter(intent == "X") %>% 
                filter(age == "X") %>% 
                filter(race != "X") %>% 
                filter(gender != "X") %>% 
                
                # ggplot
                
                ggplot(aes(x = race, fill = gender, group = gender, y = deaths)) +
                geom_col(position = position_dodge(width = 0.9)) +
                theme_minimal() +
                labs(title = "Total Gun Deaths By Race",
                     x = "Race",
                     y = "Deaths",
                     caption = "Deaths are an average from 2012-2014")
            
        } else if(input$choice == "intent") {
            
            data_538 %>% 
                
                # filtering data
                
                filter(intent != "X") %>% 
                filter(age == "X") %>% 
                filter(race != "X") %>% 
                filter(gender == "X") %>% 
                
                # ggplot
                
                ggplot(aes(x = race, fill = intent, group = intent, y = deaths)) +
                geom_col(position = position_dodge(width = 0.9)) +
                theme_minimal() +
                labs(title = "Total Gun Deaths By Race",
                     x = "Race",
                     y = "Deaths",
                     caption = "Deaths are an average from 2012-2014")
            
        } else if(input$choice == "age") {
            
            data_538 %>% 
                
                # filtering data
                
                filter(intent == "X") %>% 
                filter(age != "X") %>% 
                filter(race != "X") %>% 
                filter(gender == "X") %>% 
                
                # ggplot
                
                ggplot(aes(x = race, fill = age, group = age, y = deaths)) +
                geom_col(position = position_dodge(width = 0.9)) +
                theme_minimal() +
                labs(title = "Total Gun Deaths By Race",
                     x = "Race",
                     y = "Deaths",
                     caption = "Deaths are an average from 2012-2014")
        }
        
        })
    
 
# Plotting the veteran data
    
    output$veteran_linegraph <- renderPlot({
        veteran_data %>% 
            filter(state_of_death == "Total U.S.") %>% 
            ggplot(aes(x = year, y = as.integer(veteran_suicides), group = sex, color = sex)) +
            geom_point() +
            geom_line() +
            scale_y_log10(labels = comma) +
            scale_x_continuous(breaks = c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) +
            theme_minimal() +
            labs(x = "Year",
                 y = "Number of Suicides",
                 fill = "Gender")
    })
    
    
    output$veteran_linegraph2 <- renderPlot({
        veteran_small %>% 
            ggplot(aes(x = year, y = total, color = gender)) +
            geom_point() +
            geom_line() +
            scale_y_log10(labels = comma) +
            scale_x_continuous(breaks = c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) +
            expand_limits(x = c(2005,2017)) +
            theme_minimal() +
            labs(x = "Year",
                 y = "Number of Living Veterans",
                 fill = "Gender")
    })
    
                                      
                                      
    
}

# Running  the application 

shinyApp(ui = ui, server = server)

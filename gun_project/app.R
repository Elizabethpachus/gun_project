# This is the main page behind the webpage
# It renders all my graphics. The first section is mostly prep, then it turns into the shiny app interface

#### LIBRARIES ####

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

#### READING RDS ####

# Reading in the RDS files 

map_data_app <- read_rds("map_data.rds")

suicide_data_app <- read_rds("final_data.rds")

data_538 <- read_rds("data_538.rds")

final_bootstrap <- read_rds("final_bootstrap.rds")

veteran_data <- read_rds("veterans.rds")

veteran_small <- read_rds("veterans_small.rds")




#### SHINY UI ####

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
                        
                        # Creating a header to my home page with various graphics 
                        
                        # Centering an image on the front page  
                        
                        fluidRow(
                                column(2),
                                column(8, img(src = 'veteran_gun.jpg', align = "center", height = "100%", width = "100%"),
                                       p("Source: New York Times", align = "right")),
                                column(2)
                                ),
                        h2("Welcome", align = "center"),
                        
                        # Adding project overview here:
                        
                        p("This project investigates the correlation between suicide rate and firearm death rates 
                          while also exploring the various groups of people who are being primarily affected. Specifically,
                          I look at veteran suicide rates over the years and any trends which appear.", align = "center")),
               
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
                                         p("These maps depict the death rate by firearms and the suicide death rate for each of the states"),
                                        h3("Overall Conclusions"),
                                        p("These maps showcase the diversity of the American States. No matter the statistic being investigated, there is a 
                                          wide range of values for each of the states.")),
                                    
                            
                            # This is my main panel which contains three seperate tabs to showcase the three diffferent heat maps I created.
                            
                            mainPanel(
                                      tabsetPanel(
                                          tabPanel("Firearm plot",
                                                   h4("Firearm Morality by State", align = "center"),
                                                   h6("Death rate per 1,000 people", align = "center"),
                                                   plotOutput("firearm_map"),
                                                   h3("About the Map"),
                                                   p("Throughout the years the overall trend of of firearm death rate for most of the states is that
                                                     of an increase. This is especially noticible in states in the Southeast of the United states, and
                                                     in Colorado as well."),
                                                   h4("Intent in Firearm Deaths", align = "center"),
                                                   h3("Most Firearm Deaths are Caused by Suicide", align = "center"),
                                                   p(""),
                                                   plotOutput("piechart_firearm")),
                                          tabPanel("Suicide Map",
                                                   h4("Suicide Rate by State", align = "center"),
                                                   h6("Death rate per 1,000 people", align = "center"),
                                                   plotOutput("suicide_map"),
                                                   h3("About the Map"),
                                                   p("There seems to be an increasing number of suicides across the states throughout the years as seen
                                                     in this map and linegraph."),
                                                   p(""),
                                                    h3("Gender Breakdown of Suicides in America", align = "center"),
                                                    h4("Men make up an Overwhelming Percentage of Suicide Deaths"),
                                                    plotOutput("piechart_suicide")),
                                          tabPanel("Veteran Deaths",
                                                   h4("Veteran Suicides by State", align = "center"),
                                                   h6("Number of Death", align = "center"),
                                                   plotOutput("veteran_map"),
                                                   h3("About the Map"),
                                                   p("This map displays the number of veteran suicides by state by the selcted year. It is important
                                                     to note that this graph does not use suicide rate, but number of suicides given the data avaliable.
                                                     It is also important to note that the top three states with the highest number of veteran suicides
                                                     also the states which contain the highest number of living veterans. They are in order, California,
                                                     Texas and Florida."))
                                          
                                      ))

                        ),
                        
                        
               ),
               
               # This is the mega graphic of the project
               
               tabPanel("Who's Dying?",
                        
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                        
                                    # Including a tab so people can choose how to break down the graphic
                                p("Choose how to break down the total population deaths"),
                                    selectInput(inputId = "choice",  
                                                label = "View deaths by:", 
                                                choices = c("Gender" = "gender",
                                                            "Intent" = "intent",
                                                            "Age" = "age"), selected = "gender"),
                                h4("Conclusions"),
                                p("The various breakdowns tell a lot about each of the races. As you can see, the majority of people who die
                                  by firearms in the United States are white males.")),
                                    
                        mainPanel(           
                        plotOutput("death_by_race")))
               ),
               
               # Third panel on website, the statistical modeling portion of the project
               
               tabPanel("Is there a correlation?",
    
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                    # Setting up the input for the regression modeling
                                
                                    p("Please select a year to view the analysis from that given year."),
                                    selectInput(inputId = "year2",  
                                                label = "Select Year", 
                                                choices = c("2017" = 2017,
                                                            "2016" = 2016,
                                                            "2015" = 2015,
                                                            "2014" = 2014,
                                                            "2005" = 2005), selected = "a"),
                            h4("Conclusions from Statistical Analysis"),
                            
                            # Writing the overall analysis for the various graphs, making note of what to look for
                            
                            p("In conclusion, it seems each state has a very different relationship with the data. The model being used
                          explores the relationship between the suicide rate, the year, and the firearm death rate. I hypothesized that there would
                          be a positive correlation between the suicide rate and firearm death rate, as ~70% of suicides use a firearm. However, 
                          I found the relationship to be very different state by state. Furthermore, I found that how well the data fit the model had a 
                          very high level of variance on a state by state breakdown. Each panel on the right will provide more in depth information.")),
                            
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
                        plotOutput("veteran_linegraph2", height = "200px"),
                        br(),
                        br(),
                        h3("Conclusions from the Data", align = "center"),
                        p("The number of living veterans in the United States has begun a slow but steady decline ever since."),
                        includeMarkdown("veteran_info.md")
               ),
               
               # Including the About page info here. It is a markdown file which is simply rendered on the shiny app
               
               tabPanel("About",
                        h3("Walkthrough of Website"),
                        
                        # Adding video to website and centering it using fluid rows.
                        
                        fluidRow(
                            column(2),
                            column(4, tags$video(src = "test.mp4",
                                                 type = "vide/mp4",
                                                 align = "center",
                                                 height = "300px",
                                                 controls = "controls",
                                                 align = "center")),
                            column(2)
                        ),
                        p(''),
                        
                        # I did my explanation and typing seperaley in a MD file because I think its much easier
                        # to type long sections of text there. I linked the file below, and I think it keeps my code clean as well.
                        
                        includeMarkdown("about.md")
                       
               )
    )
)


#### SHINY SERVER ####

server <- function(input, output) {
    

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
                                         labs(fill = "Rate per 1,000") +
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
                                                             limits = c(0,600)) +
                                         
                                         # Adding titles 
                                              labs(fill = "Deaths") +
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
                     caption = "Deaths are an average from 2012-2014",
                     fill = "Gender")
            
        # Data broken down by intent    
            
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
                     caption = "Deaths are an average from 2012-2014",
                     fill = "Intent")
            
        # Data broken down by age
            
        } else if(input$choice == "age") {
            
            data_538 %>% 
                
                # filtering data for age
                
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
                     caption = "Deaths are an average from 2012-2014",
                     fill = "Age")
        }
        
        })
    
    
    # Creating piecharts to acompany the other charts
    
    
    output$piechart_firearm <- renderPlot({
        
        total_firearm_deaths <- 33599
        
        data_538 %>% 
            
            # filtering data to be broken down by intent. This data source had an interesting way of formatting
            # the data which made it trickly to plot the way I wanted to.
            
            filter(intent != "X") %>% 
            filter(age == "X") %>% 
            filter(race == "X") %>% 
            filter(gender == "X") %>% 
            group_by(intent) %>% 
            mutate(count = sum(deaths)) %>% 
            mutate(percent = count/total_firearm_deaths) %>% 
            
            # Piping it into ggplot
            
            ggplot(aes(x = "", fill = intent, y = percent)) +
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y", start = 0) +
            theme_void() +
            geom_text(aes(x = c(1, 1, 1.2, 1.5),label = paste0(round(percent*100), "%")),
                      position = position_stack(vjust = 0.5),
                      size = 7) +
            labs(fill = "Intent")
        
        
    })
    
    
    # Creating suicide pie chart, specifically broken down by gender
    
    output$piechart_suicide <- renderPlot({
        
        total_suicide_deaths <- 21058
        
        data_538 %>% 
            
            # filtering data
            
            filter(intent == "Suicide") %>% 
            filter(age == "X") %>% 
            filter(race == "X") %>% 
            filter(gender != "X") %>% 
            group_by(gender) %>% 
            mutate(count = sum(deaths)) %>% 
            mutate(percent = count/total_suicide_deaths) %>% 
            
            # Piping it into ggplot
            
            ggplot(aes(x = "", fill = gender, y = percent)) +
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y", start = 0) +
            theme_void() +
            geom_text(aes(label = paste0(round(percent*100), "%")),
                      position = position_stack(vjust = 0.5), size = 7) +
            labs(fill = "Gender")
        
        
        
    })
    
 
# This graph depicts the number of veteran suicides, in total, and broken down by gender format
    
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
    
    # Creating a linegraph which comprises the number of living veterans in the United States for the few years data was publically
    # avaliable on the Census's website
    
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

#### RUN APP ####

shinyApp(ui = ui, server = server)

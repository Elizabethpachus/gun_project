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
                               
                                                   br(),
                   
                                                   h3("Intent in Firearm Deaths", align = "center"),
                                                   h4("Most Firearm Deaths are Caused by Suicide", align = "center"),
                                                   p(" "),
                                                   plotOutput("piechart_firearm")),
                                          tabPanel("Suicide Map",
                                                   h4("Suicide Rate by State", align = "center"),
                                                   h6("Death rate per 1,000 people", align = "center"),
                                                   plotOutput("suicide_map"),
                                                   h3("About the Map"),
                                                   p("There seems to be an increasing number of suicides across the states throughout the years as seen
                                                     in this map and linegraph."),
                               
                                                   br(),
                         
                                                    h3("Gender Breakdown of Suicides in America", align = "center"),
                                                    h4("Men make up an Overwhelming Percentage of Suicide Deaths", align = "center"),
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
                                p("Data is an average from 2012-2014"),
                                    selectInput(inputId = "choice",  
                                                label = "View deaths by:", 
                                                choices = c("Gender" = "gender",
                                                            "Intent" = "intent",
                                                            "Age" = "age",
                                                            "Race" = "race"), selected = "gender"),
                                h4("Conclusions"),
                                p("The various breakdowns tell a lot about each of the races. As you can see, the majority of people who die
                                  by firearms in the United States are white males.")),
                                    
                        mainPanel(           
                        plotOutput("death_by_race"),
                        h3("Conclusions"),
                        p("As you can see from the varying graphs above, there is a lot of information to be gained from the
                          various categorical breakdowns above. For example, males account for a majority of deaths, most people
                          dying from a firearm are dying from suicide, the largest age bracket facing deaths is 35-65, and white americans
                          make up most of the deaths."))),
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
                          explores the relationship between of suicide rate and year on firearm death rate. I hypothesized that there would
                          be a positive correlation between the suicide rate and firearm death rate, as ~70% of suicides use a firearm. However, 
                          I found the relationship to be very different state by state, with only some states matching it. Furthermore, I found that how well the data fit the model had a 
                          very high level of variance on a state by state breakdown. Each panel on the right will provide more in depth information on the
                              graphic it displa.")),
                            
                            # Adding a panel where the user can choose which graph to view
                        
                            mainPanel("What is the relationship between Firearm Deaths and Suicide Rate?",
                                      tabsetPanel(
                                          
                                          # Graph displaying the correlation between firearm death rate averages and 
                                          # suicide rates per state
                                          
                                          tabPanel("Plot", 
                                                   h3("Firearm Death Rate by Suicide Rate", align = "center"),
                                                   h4("There seems to be a positive trend", align = "center"),
                                                   plotOutput("suicide_plot"),
                                                   h5("Rate is per 1,000 people, data from C.D.C", align = "right"),
                                                   h4("Conclusions"),
                                                   p("The first graph depicts the scatter plot correlation between the suicide death rate 
                                                     and the firearm death rate for a given state. A linear regression model in the form of
                                                     a line was plotted. The data presents a positive slope, indicating that there is a positive
                                                     correlation between the suicide rate and the firearm death rate. However, the data points are 
                                                     scattered about the plot and each state seems to have a very different correlation with the variables 
                                                     and the year.")),
                                          
                                          # This graph shows the coefficents which show the correlation between firearm 
                                          # and suicide rate per state
                                          
                                          tabPanel("Correlation?",
                                                   h3("What is the Correlation Between Firearm Death Rate, Suicide Rate and Year?", align = "center"),
                                                   h4("A Wide Variety of Correlations Among the States", align = "center"),
                                                   plotOutput("correlation_plot", height = "700px"),
                                                   h4("Conclusions"),
                                                   p("The Linear Regression Model graphic depicts a wide range in intercept values for the intercepts of the model.
                                                     The intercepts are plotting on the x axis, and they depict the relationship between the parallel lines which 
                                                     represent the overall intercept, the slope, and the year. As can be seen in the graph there is a wide variety in 
                                                     the correlation amongst the states. Some states further to the right on the graph have coefficients which are positive
                                                     and closer to two or four. The states with the highest average coefficient were Alabama, Missouri, and Rhode Island.
                                                     This indicates that on average, the suicide rate is correlated with around a 2-3% increase in firearm death rate for
                                                     that state. On the other hand of the spectrum there are the states which had a negative correlation. The states with 
                                                     the top three negative correlations were Arizona, California, and Maryland. This indicates that on average, when the 
                                                     suicide rate increases in that state, the firearm death rate decreases by around 2-3%. All of the other states fall 
                                                     somewhere in the middle, with the bulk of states falling slightly to the right of 0, indicating a slight positive
                                                     correlation for most of the state between suicide death rate and firearm death rate.")),
                                          
                                          # This graph shows the r squared value per state
                                          
                                          tabPanel("Uncertainty",
                                                   h3("How Well Does the Data Match the Model?", align = "center"),
                                                   h4("There is also a Wide Variety in Uncertainty Among the States", align = "center"),
                                                   plotOutput("uncertainty_plot", height = "700px"),
                                                   h4("Conclusions"),
                                                   p("The last graph depicts the R squared value for each of the states. The R squared value indicates how well a model 
                                                     fits the data. An R squared value of one is ideal because it means that the model fits the data with a one to one 
                                                     correlation. As can be seen in this graph, there is a wide range in the R squared values. A bootstrap resampling was 
                                                     performed in order to run 1,oo0 repetitions of the data and get a random sample to test uncertainty. Each state has
                                                     about the same amount of uncertainty, as told by the length of the colored line. However, how well the data fits the 
                                                     data varies widely. States like Virginia, North Carolina, and Massachusetts had a very low R squared value indicating 
                                                     that the model is not very representative of the data from that state. Other states like Washington, Georgia and Alaska 
                                                     had R squared values closer to one indicating that the model fit the data very well."))
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
                        h4("Conclusions from the Data", align = "center"),
                        p("The number of living veterans in the United States has begun a slow but steady decline ever since. You can see in the graphs
                          that a significant portion of veterans are male, and this is reflected in their suicide rates as well. Unfortunately as the number
                          of female veterans increases, so does their suicide rate."),
                        br(),
                        includeMarkdown("veteran_info.md")
               ),
               
               # Including the About page info here. It is a markdown file which is simply rendered on the shiny app
               
               tabPanel("About",
                        h3("Walkthrough of Website"),
                        
                        # Adding video to website and centering it using fluid rows.
                        fluidRow(
                            column(2),
                            
                            # Embeding my video as a Youtube Video here:
                            
                            column(4, HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/3652ciwXtpY" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
                            column(2)
                        ),
                        
                        p(''),
                        
                        # I did my explanation and typing seperatlet in a MD file because I think its much easier
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
                                             aes(x = suicide_rate, y = rate_per_1000)) +
                                          geom_point() +
                                          
                                          # Setting limits for the graphs so that even as the year changes the 
                                          # actual scales will remain the same to make it easier to differentiate 
                                          # between the years
                          
                                          scale_x_continuous(limits = c(8,30)) +
                                          geom_smooth(method = "lm") +
                                          labs(x = "Suicide Rate",
                                              y = "Firearm Death Rate"
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
                                           labs(
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
                                           labs(
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
                
                ggplot(aes(x = gender, fill = gender, group = gender, y = deaths)) +
                geom_col(position = position_dodge(width = 0.9)) +
                theme_minimal() +
                labs(title = "Total Firearm Deaths by Gender",
                     x = "Race",
                     y = "Deaths",
                     fill = "Gender") +
                theme(text = element_text(size = 20),
                      plot.title = element_text(hjust = 0.5))
            
        # Data broken down by intent    
            
        } else if(input$choice == "intent") {
            
            data_538 %>% 
                
                # filtering data
                
                filter(intent != "X") %>% 
                filter(age == "X") %>% 
                filter(race != "X") %>% 
                filter(gender == "X") %>% 
                
                # ggplot
                
                ggplot(aes(x = intent, fill = intent, group = intent, y = deaths)) +
                geom_col(position = position_dodge(width = 0.9)) +
                theme_minimal() +
                labs(title = "Total Firearm Deaths By Intent",
                     x = "Race",
                     y = "Deaths",
                     fill = "Intent") +
                theme(text = element_text(size = 20),
                      plot.title = element_text(hjust = 0.5))
            
        # Data broken down by age
            
        } else if(input$choice == "age") {
            
            data_538 %>% 
                
                # filtering data for age
                
                filter(intent == "X") %>% 
                filter(age != "X") %>% 
                filter(race != "X") %>% 
                filter(gender == "X") %>% 
                
                # ggplot
                
                ggplot(aes(x = age, fill = age, group = age, y = deaths)) +
                geom_col(position = position_dodge(width = 0.9)) +
                theme_minimal() +
                labs(title = "Total Firearm Deaths By Age",
                     x = "Age",
                     y = "Deaths",
                     fill = "Age") +
                theme(text = element_text(size = 20),
                      plot.title = element_text(hjust = 0.5))
        }
        
        else if(input$choice == "race") {
            
            data_538 %>% 
                
                # filtering data for age
                
                filter(intent == "X") %>% 
                filter(age == "X") %>% 
                filter(race != "X") %>% 
                filter(gender == "X") %>% 
                
                # ggplot
                
                ggplot(aes(x = race, fill = race, group = race, y = deaths)) +
                geom_col(position = position_dodge(width = 0.9)) +
                theme_minimal() +
                labs(title = "Total Firearm Deaths By Age",
                     x = "Age",
                     y = "Deaths",
                     fill = "Age") +
                theme(text = element_text(size = 20),
                      plot.title = element_text(hjust = 0.5))
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

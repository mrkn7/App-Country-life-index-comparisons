# O?uzhan Ayd?n 2361111
# Arda Bal 2361137
# Mehmet Ali Erkan 2467991
# Halil Eren Ko?ak 2361376

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(tidyr)
library(leaflet)
library(sqldf)

dataset <- read.csv("lifeindexbycountries.csv")
dataset %<>% rename(Country = ?..Country) 
dataset %<>% arrange(dataset, Country)

lng_lat <- read.table("long_lat_cont.txt", header = T)

cbind(dataset,lng_lat)

dataset <- cbind(dataset,lng_lat)
dataset1 <- dataset %>% select(., -c(latitude, longitude, Continent))


ui <- navbarPage("Rvivor",
                 tabPanel("Main Page",
                          titlePanel("Rvivor's App"),
                          sidebarPanel(
                              p(h3(strong("Created by ", span("Rvivor", style = "color:red")), align = "center")),
                              br(),
                              h4("Mehmet Ali Erkan - 2467991"),
                              h4("Oguzhan Aydin - 2361111"),
                              h4("Arda Bal - 2361137"),
                              h4("Halil Eren Kocak - 2361376")
                          ),
                          mainPanel(
                              h3(strong("Application and Dataset Description:")),
                              br(),
                              h4("This application have four pages and pages contain different apps inside them. The dataset is called 'Quality of life index by countries 2020' and it contains 10 different variables and 80 observations. The first variable is Country. 2nd variable up to 10th variable are representing the countries' specific demographic indexes, such as safety index, purcashing power index, etc. In order to obtain better visualizations, three more columns added; longitude, latitude, and Continent, respectively."),
                              br(),
                              h4("The first app simply compares the two selected demographic indexes and checks the relationship between them with a plot."),
                              br(),
                              h4("The second app compares the selected continents by using the selected demographic index. User may reach general information about the selected continents with a check box. A leaflet map can be seen at the top of the main panel which contains the dataset countries in the selected continent, and with the help of the bar plot, user can see the overall comparison of countries with the selected demographic index."),
                              br(),
                              h4("In the third app, user selects a limit for the quality of life index. Then app shows the countries that satisfies the index in the leaflet map with the pop-ups. User may click the markers to see additional information about the country."),
                              br(),
                              h4("The fourth app offers the user five countries according to the user's preference among the indexes from -5 to 5. With basic calculations, app simply multiplies the index with the selected value (-5 to 5), and then adds or substracts according to the impact of the index.")
                          )
                 ),
                 
                 ### 1st APP ###
                 
                 tabPanel("1st App",
                          h2("Linear Model", align = "center"),
                          p(strong("Created by ", span("Rvivor", style = "color:red")), align = "center"),
                          titlePanel("Quality of life Comparison"),
                          sidebarPanel(
                              sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(dataset),
                                          value=min(1, nrow(dataset)), step=1, round=0),
                              
                              checkboxInput('smooth', 'Smooth', value = TRUE),
                              
                              selectInput('x', 'X', names(dataset[-c(1,11,12,13)])),
                              selectInput('y', 'Y', names(dataset[-c(1,11,12,13)]), names(dataset)[[2]]),
                              selectInput('color', 'Color', c('None',names(dataset[-c(11,12,13)]))),
                              
                          ),
                          mainPanel(
                              plotOutput("plot"),
                              h5("Comments:"),
                              textOutput("Comments"),
                              br(),
                              textOutput("Last"),
                              br(),
                              p(h4(strong("Note about the app from RVIVOR :"))),
                              p(h5("In this R shiny app, as X and Y, we wanted to visualize the comparison of the data in each column of our dataset for each other with ggplot. We made our Sample Size as a range from 0 to our row number 80. We aimed to randomly select the sample size you want from the dataset by scrolling and compare the plots to help the user make comments about their linear relationships. For example, to evaluate all countries, we can easily observe the positive linear relationship from the plot when we select the sample size as 80 and select X as Quality of Life Index, Y as Purchasing Power Index, and Color as Country. When we think of it as real life, if a country has a high level of the Quality of Life Index of a country, it should have a high level of the Purchasing Power Index. We wanted to share this situation as an example that confirms the plot we made."))                       )
                 ),
                 
                 ### 2nd APP ###
                 
                 tabPanel("2nd App",
                          titlePanel("Continent Comparison"),
                          sidebarPanel(
                              h2("Visualization and Analysis by Continent", align = "center"),
                              p(strong("Created by ", span("Rvivor", style = "color:red")), align = "center"),
                              checkboxGroupInput(inputId = "Continent",
                                                 label = "Choose Continents you want to compare and reach general information:",
                                                 choices = unique(dataset$Continent),
                                                 selected = "Europe"),
                              checkboxInput(inputId = "Summary",
                                            label = "Do you want to reach general information about Continent selected?",
                                            value = T),
                              selectInput(inputId = "y_axis",
                                          label = "In which aspect do you want to compare all the continents?",
                                          choices = c("Quality of Life Index",
                                                      "Purchasing Power Index",
                                                      "Safety Index",
                                                      "Health Care Index",
                                                      "Cost of Living Index",
                                                      "Traffic Commute Time Index",
                                                      "Pollution Index",
                                                      "Climate Index"))
                              
                          ),
                          mainPanel(
                              h4(strong("Which countries are on this continent?")),
                              leafletOutput("map"),
                              br(),
                              h5(strong("Average value of each index for selected continents")),
                              tableOutput("Summary_out"),
                              br(),
                              h4(strong("Visual comparison for each continent - Summation of Countries")),
                              plotOutput("plot_out"),
                              br(),
                              h3(strong("Comments for Map and Table")),
                              textOutput("CommentsMapTable"),
                              br(),
                              h3(strong("Comments for Plot")),
                              textOutput("Comments2ndapp"),
                              br(),
                              p(h4(strong("Note about the app from RVIVOR :"))),
                              p(h5("In map, you can find the countries in our data set by selecting the continent from sidebar panel. You can also optionally enable the box to see the average values for each variable for each continent. Finally, you can see a visual comparison of each continent and country based on the variable you want to evaluate.")),
                              br(), 
                              br()
                          )
                 ),
                 
                 ### 3rd APP ###
                 
                 tabPanel("3rd App",
                          sidebarPanel(
                              h2("World Map by Quality of Life Index", align = "center"),        
                              p(strong("Created by ", span("Rvivor", style = "color:red")), align = "center"),
                              sliderInput(inputId = "Score", 
                                          label = "Quality of Life Index score", 
                                          min = 55.65, max = 192.66, value = 132),
                              h6("You can access general information about the country by clicking on the pop-ups.")),
                          mainPanel(
                              leafletOutput("map3"),
                              br("Countries:"),
                              textOutput("remain"),
                              br(),
                              br(),
                              p(h4(strong("Note about the app from RVIVOR :"))),
                              p(h5("As you can see in our application, the limits that determine our range in our dataset are 55.65 at the lowest and 192.66 at the top. Therefore, by scrolling through these value ranges with your mouse, you can easily see the countries with the quality of life index score above your selected value. In addition, you can have detailed information about those countries by clicking on the countries you will see on the world map as a marker. In these boxes of markers, we have prepared a summary that will allow you to see the countries' names, their value of life quality index, purchasing power index, safety index, health care index, and pollution values, respectively. Finally, you can see the country names easily at the bottom of the map.")),
                              br(),
                              br()
                          )
                 ),
                 
                 
                 ### 4th APP ###
                 
                 tabPanel("4th App",
                          sidebarPanel(
                              h2("Which country is best for you?"),
                              p(strong("Created by ", span("Rvivor", style = "color:red")), align = "center"),
                              p("Choose variables from -5 to 5 according to your priority."),
                              p("5 - It must be/I want."),
                              p("-5 - It should not be / I do not want."),
                              p("0 - It does not matter."),
                              br(),
                              selectizeInput(inputId = "Money",
                                             label = "Money",
                                             choices = -5:5, selected = 0),
                              selectizeInput(inputId = "Safety", 
                                             label = "Safety Index",
                                             choices = -5:5, selected = 0),
                              selectizeInput(inputId = "HealthCare",
                                             label = "Health Care Index",
                                             choices = -5:5, selected = 0),
                              selectizeInput(inputId = "Traffic",
                                             label = "Traffic Commute Time Index",
                                             choices = -5:5, selected = 0),
                              selectizeInput(inputId = "Pollution",
                                             label = "Pollution Index",
                                             choices = -5:5, selected = 0),
                              selectizeInput(inputId = "Cost",
                                             label = "Cost of living Index",
                                             choices = -5:5, selected = 0),
                              selectizeInput(inputId = "Climate",
                                             label = "Climate Index",
                                             choices = -5:5, selected = 0),
                              actionButton(inputId = "Calculate", label = "Calculate")
                          ),
                          mainPanel(
                              uiOutput("img"),
                              br(),
                              p(h3("Your choices:")),
                              textOutput("Money_out"),
                              textOutput("Safety_out"),
                              textOutput("HealthCare_out"),
                              textOutput("Traffic_out"),
                              textOutput("Pollution_out"),
                              textOutput("Cost_out"),
                              textOutput("Climate_out"),
                              br(),
                              p(h2("The country that you should live in:")),
                              br(),
                              tableOutput("Country4"),
                              br(),
                              br(),
                              p(h4(strong("Note about the app from RVIVOR :"))),
                              p(h5("In this R shiny app, we tried to develop a formula in which we tried to get the score of the country people would like to live in by taking four positives and three negative samples. These are money, safety, health care, climate, traffic jam, pollution, and cost of living index, respectively, from our dataset. Besides, we asked the user to enter values between -5 and 5 for these samples according to the users' wishes, and thus, we multiplied samples of our dataset and got an overall score. As a result, we wanted to tabulate the evaluation of the most suitable countries with the scores obtained and demonstrate the five countries that met the scores correctly to the user and list the five most suitable countries for users."))
                          )
                 )
                 
)


server <- function(input, output) {
    ######## 1. APP ################
    
    data <- reactive({
        dataset1[sample(nrow(dataset1), input$sampleSize),]
    })
    output$plot <- renderPlot({
        p <- ggplot(data(), aes_string(x=input$x, y=input$y)) + geom_point()
        if (input$color != 'None'){
            p <- p + aes_string(color=input$color)
            print(p)}
        
        if (input$smooth){
            p <- p + geom_smooth(method = "lm", formula = "y~x")
            print(p)} else{
                print(p)
            }
        
        
    })
    
    output$Comments <- renderText ({
        if(input$x == "Quality.of.Life.Index"){
            if(input$y == "Quality.of.Life.Index"){
                paste("X and Y must be different for the correct results.")
            }else if(input$y == "Purchasing.Power.Index"){
                paste("Their covariance:", cov(dataset$Quality.of.Life.Index,dataset$Purchasing.Power.Index),
                      "Their correlation coefficient:", cor(dataset$Quality.of.Life.Index,dataset$Purchasing.Power.Index),
                      "Their linear equation:" ,lm(Quality.of.Life.Index ~ Purchasing.Power.Index,dataset)[1],
                      "There is a strong correlation between them")}
            
            else if(input$y == "Safety.Index"){
                paste("Their covariance:", cov(dataset$Quality.of.Life.Index,dataset$Safety.Index),
                    "Their correlation coefficient:", cor(dataset$Quality.of.Life.Index,dataset$Safety.Index),
                     "Their linear equation:", lm(Quality.of.Life.Index ~ Safety.Index,dataset)[1],
                     "There is a moderate correlation between them")
            }else if(input$y == "Health.Care.Index"){
                paste("Their covariance:", cov(dataset$Quality.of.Life.Index,dataset$Health.Care.Index),
                      "Their correlation coefficient:", cor(dataset$Quality.of.Life.Index,dataset$Health.Care.Index),
                    "Their linear equation:", lm(Quality.of.Life.Index ~ Health.Care.Index,dataset)[1],
                    "There is a moderate correlation between them")
            }else if(input$y == "Cost.of.Living.Index"){
                paste("Their covariance:", cov(dataset$Quality.of.Life.Index,dataset$Cost.of.Living.Index),
                    "Their correlation coefficient:", cor(dataset$Quality.of.Life.Index,dataset$Cost.of.Living.Index),
                    "Their linear equation:", lm(Quality.of.Life.Index ~ Cost.of.Living.Index,dataset)[1],
                    "There is a strong correlation between them"
                )
            }else if(input$y == "Property.Price.to.Income.Ratio"){
                paste("Their covariance:", cov(dataset$Quality.of.Life.Index,dataset$Property.Price.to.Income.Ratio),
                    "Their correlation coefficient:", cor(dataset$Quality.of.Life.Index,dataset$Property.Price.to.Income.Ratio),
                    "Their linear equation:", lm(Quality.of.Life.Index ~ Property.Price.to.Income.Ratio,dataset)[1],
                    "There is a moderate correlation between them"
                )
            }else if(input$y == "Traffic.Commute.Time.Index"){
                paste("Their covariance:", cov(dataset$Quality.of.Life.Index,dataset$Traffic.Commute.Time.Index),
                    "Their correlation coefficient:", cor(dataset$Quality.of.Life.Index,dataset$Traffic.Commute.Time.Index),
                    "Their linear equation:", lm(Quality.of.Life.Index ~ Traffic.Commute.Time.Index,dataset)[1],
                    "There is a strong correlation between them"
                )
            }else if(input$y == "Pollution.Index"){
                paste("Their covariance:", cov(dataset$Quality.of.Life.Index,dataset$Pollution.Index),
                    "Their correlation coefficient:", cor(dataset$Quality.of.Life.Index,dataset$Pollution.Index),
                    "Their linear equation:", lm(Quality.of.Life.Index ~ Pollution.Index,dataset)[1],
                    "There is a strong correlation between them"
                )
            }else if(input$y == "Climate.Index"){
                paste("Their covariance:", cov(dataset$Quality.of.Life.Index,dataset$Climate.Index),
                    "Their correlation coefficient:", cor(dataset$Quality.of.Life.Index,dataset$Climate.Index),
                    "Their linear equation:", lm(Quality.of.Life.Index ~ Climate.Index,dataset)[1],
                    "There is a weak correlation between them")
            }}
        
        else if(input$x == "Purchasing.Power.Index"){
            if(input$y == "Quality.of.Life.Index"){
                paste("Their covariance:", cov(dataset$Purchasing.Power.Index,dataset$Quality.of.Life.Index),
                      "Their correlation coefficient:", cor(dataset$Purchasing.Power.Index,dataset$Quality.of.Life.Index),
                      "Their linear equation:", lm(Purchasing.Power.Index ~ Quality.of.Life.Index,dataset)[1],
                      "There is a strong correlation between them"
                )
            }else if(input$y == "Purchasing.Power.Index"){
                paste("X and Y must be different for the correct results.")
            }else if(input$y == "Safety.Index"){
                paste("Their covariance:", cov(dataset$Purchasing.Power.Index,dataset$Safety.Index),
                      "Their correlation coefficient:", cor(dataset$Purchasing.Power.Index,dataset$Safety.Index),
                      "Their linear equation:", lm(Purchasing.Power.Index ~ Safety.Index,dataset)[1],
                      "There is a moderate correlation between them"
                )
            }else if(input$y == "Health.Care.Index"){
                paste("Their covariance:", cov(dataset$Purchasing.Power.Index,dataset$Health.Care.Index),
                      "Their correlation coefficient:", cor(dataset$Purchasing.Power.Index,dataset$Health.Care.Index),
                      "Their linear equation:", lm(Purchasing.Power.Index ~ Health.Care.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Cost.of.Living.Index"){
                paste("Their covariance:", cov(dataset$Purchasing.Power.Index,dataset$Cost.of.Living.Index),
                      "Their correlation coefficient:", cor(dataset$Purchasing.Power.Index,dataset$Cost.of.Living.Index),
                      "Their linear equation:", lm(Purchasing.Power.Index ~ Cost.of.Living.Index,dataset)[1],
                      "There is a strong correlation between them")
            }else if(input$y == "Property.Price.to.Income.Ratio"){
                
                paste("Their covariance:", cov(dataset$Purchasing.Power.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their correlation coefficient:", cor(dataset$Purchasing.Power.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their linear equation:", lm(Purchasing.Power.Index ~ Property.Price.to.Income.Ratio,dataset)[1],
                      "There is a moderate correlation between them")
                
            }else if(input$y == "Traffic.Commute.Time.Index"){
                paste("Their covariance:", cov(dataset$Purchasing.Power.Index,dataset$Traffic.Commute.Time.Index),
                      "Their correlation coefficient:", cor(dataset$Purchasing.Power.Index,dataset$Traffic.Commute.Time.Index),
                      "Their linear equation:", lm(Purchasing.Power.Index ~ Traffic.Commute.Time.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Pollution.Index"){
                paste("Their covariance:", cov(dataset$Purchasing.Power.Index,dataset$Pollution.Index),
                      "Their correlation coefficient:", cor(dataset$Purchasing.Power.Index,dataset$Pollution.Index),
                      "Their linear equation:", lm(Purchasing.Power.Index ~ Pollution.Index,dataset)[1],
                      "There is a strong correlation between them")
            }else if(input$y == "Climate.Index"){
                paste("Their covariance:", cov(dataset$Purchasing.Power.Index,dataset$Climate.Index),
                      "Their correlation coefficient:", cor(dataset$Purchasing.Power.Index,dataset$Climate.Index),
                      "Their linear equation:", lm(Purchasing.Power.Index ~ Climate.Index,dataset)[1],
                      "There is a moderate weak between them")
            }}
        
        else if(input$x == "Safety.Index"){
            if(input$y == "Quality.of.Life.Index"){
                paste("Their covariance:", cov(dataset$Safety.Index,dataset$Quality.of.Life.Index),
                      "Their correlation coefficient:", cor(dataset$Safety.Index,dataset$Quality.of.Life.Index),
                      "Their linear equation:", lm(Safety.Index ~ Quality.of.Life.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Purchasing.Power.Index"){
                paste("Their covariance:", cov(dataset$Safety.Index,dataset$Purchasing.Power.Index),
                      "Their correlation coefficient:", cor(dataset$Safety.Index,dataset$Purchasing.Power.Index),
                      "Their linear equation:", lm(Safety.Index ~ Purchasing.Power.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Safety.Index"){
                paste("X and Y must be different for the correct results.")
            }else if(input$y == "Health.Care.Index"){
                paste("Their covariance:", cov(dataset$Safety.Index,dataset$Health.Care.Index),
                      "Their correlation coefficient:", cor(dataset$Safety.Index,dataset$Health.Care.Index),
                      "Their linear equation:", lm(Safety.Index ~ Health.Care.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Cost.of.Living.Index"){
                paste("Their covariance:", cov(dataset$Safety.Index,dataset$Cost.of.Living.Index),
                      "Their correlation coefficient:", cor(dataset$Safety.Index,dataset$Cost.of.Living.Index),
                      "Their linear equation:", lm(Safety.Index ~ Cost.of.Living.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Property.Price.to.Income.Ratio"){
                paste("Their covariance:", cov(dataset$Safety.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their correlation coefficient:", cor(dataset$Safety.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their linear equation:", lm(Safety.Index ~ Property.Price.to.Income.Ratio,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Traffic.Commute.Time.Index"){
                paste("Their covariance:", cov(dataset$Safety.Index,dataset$Traffic.Commute.Time.Index),
                      "Their correlation coefficient:", cor(dataset$Safety.Index,dataset$Traffic.Commute.Time.Index),
                      "Their linear equation:", lm(Safety.Index ~ Traffic.Commute.Time.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Pollution.Index"){
                paste("Their covariance:", cov(dataset$Safety.Index,dataset$Pollution.Index),
                      "Their correlation coefficient:", cor(dataset$Safety.Index,dataset$Pollution.Index),
                      "Their linear equation:", lm(Safety.Index ~ Pollution.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Climate.Index"){
                paste("Their covariance:", cov(dataset$Safety.Index,dataset$Climate.Index),
                      "Their correlation coefficient:", cor(dataset$Safety.Index,dataset$Climate.Index),
                      "Their linear equation:", lm(Safety.Index ~ Climate.Index,dataset)[1],
                      "There is a weak correlation between them")
            }}
        else if(input$x == "Health.Care.Index"){
            if(input$y == "Quality.of.Life.Index"){
                paste("Their covariance:", cov(dataset$Health.Care.Index,dataset$Quality.of.Life.Index),
                      "Their correlation coefficient:", cor(dataset$Health.Care.Index,dataset$Quality.of.Life.Index),
                      "Their linear equation:", lm(Health.Care.Index ~ Quality.of.Life.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Purchasing.Power.Index"){
                paste("Their covariance:", cov(dataset$Health.Care.Index,dataset$Purchasing.Power.Index),
                      "Their correlation coefficient:", cor(dataset$Health.Care.Index,dataset$Purchasing.Power.Index),
                      "Their linear equation:", lm(Health.Care.Index ~ Purchasing.Power.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Safety.Index"){
                paste("Their covariance:", cov(dataset$Health.Care.Index,dataset$Safety.Index),
                      "Their correlation coefficient:", cor(dataset$Health.Care.Index,dataset$Safety.Index),
                      "Their linear equation:", lm(Health.Care.Index ~ Safety.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Health.Care.Index"){
                paste("X and Y must be different for the correct results.")
            }else if(input$y == "Cost.of.Living.Index"){
                paste("Their covariance:", cov(dataset$Health.Care.Index,dataset$Cost.of.Living.Index),
                      "Their correlation coefficient:", cor(dataset$Health.Care.Index,dataset$Cost.of.Living.Index),
                      "Their linear equation:", lm(Health.Care.Index ~ Cost.of.Living.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Property.Price.to.Income.Ratio"){
                paste("Their covariance:", cov(dataset$Health.Care.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their correlation coefficient:", cor(dataset$Health.Care.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their linear equation:", lm(Health.Care.Index ~ Property.Price.to.Income.Ratio,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Traffic.Commute.Time.Index"){
                paste("Their covariance:", cov(dataset$Health.Care.Index,dataset$Traffic.Commute.Time.Index),
                      "Their correlation coefficient:", cor(dataset$Health.Care.Index,dataset$Traffic.Commute.Time.Index),
                      "Their linear equation:", lm(Health.Care.Index ~ Traffic.Commute.Time.Index,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Pollution.Index"){
                paste("Their covariance:", cov(dataset$Health.Care.Index,dataset$Pollution.Index),
                      "Their correlation coefficient:", cor(dataset$Health.Care.Index,dataset$Pollution.Index),
                      "Their linear equation:", lm(Health.Care.Index ~ Pollution.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Climate.Index"){
                paste("Their covariance:", cov(dataset$Health.Care.Index,dataset$Climate.Index),
                      "Their correlation coefficient:", cor(dataset$Health.Care.Index,dataset$Climate.Index),
                      "Their linear equation:", lm(Health.Care.Index ~ Climate.Index,dataset)[1],
                      "There is a weak correlation between them")
            }
        }
        else if(input$x == "Cost.of.Living.Index"){
            if(input$y == "Quality.of.Life.Index"){
                paste("Their covariance:", cov(dataset$Cost.of.Living.Index,dataset$Quality.of.Life.Index),
                      "Their correlation coefficient:", cor(dataset$Cost.of.Living.Index,dataset$Quality.of.Life.Index),
                      "Their linear equation:", lm(Cost.of.Living.Index ~ Quality.of.Life.Index,dataset)[1],
                      "There is a strong correlation between them")
            }else if(input$y == "Purchasing.Power.Index"){
                paste("Their covariance:", cov(dataset$Cost.of.Living.Index,dataset$Purchasing.Power.Index),
                      "Their correlation coefficient:", cor(dataset$Cost.of.Living.Index,dataset$Purchasing.Power.Index),
                      "Their linear equation:", lm(Cost.of.Living.Index ~ Purchasing.Power.Index,dataset)[1],
                      "There is a strong correlation between them")
            }else if(input$y == "Safety.Index"){
                paste("Their covariance:", cov(dataset$Cost.of.Living.Index,dataset$Safety.Index),
                      "Their correlation coefficient:", cor(dataset$Cost.of.Living.Index,dataset$Safety.Index),
                      "Their linear equation:", lm(Cost.of.Living.Index ~ Safety.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Health.Care.Index"){
                paste("Their covariance:", cov(dataset$Cost.of.Living.Index,dataset$Health.Care.Index),
                      "Their correlation coefficient:", cor(dataset$Cost.of.Living.Index,dataset$Health.Care.Index),
                      "Their linear equation:", lm(Cost.of.Living.Index ~ Health.Care.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Cost.of.Living.Index"){
                paste("X and Y must be different for the correct results.")
            }else if(input$y == "Property.Price.to.Income.Ratio"){
                paste("Their covariance:", cov(dataset$Cost.of.Living.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their correlation coefficient:", cor(dataset$Cost.of.Living.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their linear equation:", lm(Cost.of.Living.Index ~ Property.Price.to.Income.Ratio,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Traffic.Commute.Time.Index"){
                paste("Their covariance:", cov(dataset$Cost.of.Living.Index,dataset$Traffic.Commute.Time.Index),
                      "Their correlation coefficient:", cor(dataset$Cost.of.Living.Index,dataset$Traffic.Commute.Time.Index),
                      "Their linear equation:", lm(Cost.of.Living.Index ~ Traffic.Commute.Time.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Pollution.Index"){
                paste("Their covariance:", cov(dataset$Cost.of.Living.Index,dataset$Pollution.Index),
                      "Their correlation coefficient:", cor(dataset$Cost.of.Living.Index,dataset$Pollution.Index),
                      "Their linear equation:", lm(Cost.of.Living.Index ~ Pollution.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Climate.Index"){
                paste("Their covariance:", cov(dataset$Cost.of.Living.Index,dataset$Climate.Index),
                      "Their correlation coefficient:", cor(dataset$Cost.of.Living.Index,dataset$Climate.Index),
                      "Their linear equation:", lm(Cost.of.Living.Index ~ Climate.Index,dataset)[1],
                      "There is a weak correlation between them")
            }
        }
        else if(input$x == "Property.Price.to.Income.Ratio"){
            if(input$y == "Quality.of.Life.Index"){
                paste("Their covariance:", cov(dataset$Property.Price.to.Income.Ratio,dataset$Quality.of.Life.Index),
                      "Their correlation coefficient:", cor(dataset$Property.Price.to.Income.Ratio,dataset$Quality.of.Life.Index),
                      "Their linear equation:", lm(Property.Price.to.Income.Ratio ~ Quality.of.Life.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Purchasing.Power.Index"){
                paste("Their covariance:", cov(dataset$Property.Price.to.Income.Ratio,dataset$Purchasing.Power.Index),
                      "Their correlation coefficient:", cor(dataset$Property.Price.to.Income.Ratio,dataset$Purchasing.Power.Index),
                      "Their linear equation:", lm(Property.Price.to.Income.Ratio ~ Purchasing.Power.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Safety.Index"){
                paste("Their covariance:", cov(dataset$Property.Price.to.Income.Ratio,dataset$Safety.Index),
                      "Their correlation coefficient:", cor(dataset$Property.Price.to.Income.Ratio,dataset$Safety.Index),
                      "Their linear equation:", lm(Property.Price.to.Income.Ratio ~ Safety.Index,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Health.Care.Index"){
                paste("Their covariance:", cov(dataset$Property.Price.to.Income.Ratio,dataset$Health.Care.Index),
                      "Their correlation coefficient:", cor(dataset$Property.Price.to.Income.Ratio,dataset$Health.Care.Index),
                      "Their linear equation:", lm(Property.Price.to.Income.Ratio ~ Health.Care.Index,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Cost.of.Living.Index"){
                paste("Their covariance:", cov(dataset$Property.Price.to.Income.Ratio,dataset$Cost.of.Living.Index),
                      "Their correlation coefficient:", cor(dataset$Property.Price.to.Income.Ratio,dataset$Cost.of.Living.Index),
                      "Their linear equation:", lm(Property.Price.to.Income.Ratio ~ Cost.of.Living.Index,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Property.Price.to.Income.Ratio"){
                paste("X and Y must be different for the correct results.")
            }else if(input$y == "Traffic.Commute.Time.Index"){
                paste("Their covariance:", cov(dataset$Property.Price.to.Income.Ratio,dataset$Traffic.Commute.Time.Index),
                      "Their correlation coefficient:", cor(dataset$Property.Price.to.Income.Ratio,dataset$Traffic.Commute.Time.Index),
                      "Their linear equation:", lm(Property.Price.to.Income.Ratio ~ Traffic.Commute.Time.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Pollution.Index"){
                paste("Their covariance:", cov(dataset$Property.Price.to.Income.Ratio,dataset$Pollution.Index),
                      "Their correlation coefficient:", cor(dataset$Property.Price.to.Income.Ratio,dataset$Pollution.Index),
                      "Their linear equation:", lm(Property.Price.to.Income.Ratio ~ Pollution.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Climate.Index"){
                paste("Their covariance:", cov(dataset$Property.Price.to.Income.Ratio,dataset$Climate.Index),
                      "Their correlation coefficient:", cor(dataset$Property.Price.to.Income.Ratio,dataset$Climate.Index),
                      "Their linear equation:", lm(Property.Price.to.Income.Ratio ~ Climate.Index,dataset)[1],
                      "There is a weak correlation between them")
            }
        }
        else if(input$x == "Traffic.Commute.Time.Index"){
            if(input$y == "Quality.of.Life.Index"){
                paste("Their covariance:", cov(dataset$Traffic.Commute.Time.Index,dataset$Quality.of.Life.Index),
                      "Their correlation coefficient:", cor(dataset$Traffic.Commute.Time.Index,dataset$Quality.of.Life.Index),
                      "Their linear equation:", lm(Traffic.Commute.Time.Index ~ Quality.of.Life.Index,dataset)[1],
                      "There is a strong correlation between them")
            }else if(input$y == "Purchasing.Power.Index"){
                paste("Their covariance:", cov(dataset$Traffic.Commute.Time.Index,dataset$Purchasing.Power.Index),
                      "Their correlation coefficient:", cor(dataset$Traffic.Commute.Time.Index,dataset$Purchasing.Power.Index),
                      "Their linear equation:", lm(Traffic.Commute.Time.Index ~ Purchasing.Power.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Safety.Index"){
                paste("Their covariance:", cov(dataset$Traffic.Commute.Time.Index,dataset$Safety.Index),
                      "Their correlation coefficient:", cor(dataset$Traffic.Commute.Time.Index,dataset$Safety.Index),
                      "Their linear equation:", lm(Traffic.Commute.Time.Index ~ Safety.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Health.Care.Index"){
                paste("Their covariance:", cov(dataset$Traffic.Commute.Time.Index,dataset$Health.Care.Index),
                      "Their correlation coefficient:", cor(dataset$Traffic.Commute.Time.Index,dataset$Health.Care.Index),
                      "Their linear equation:", lm(Traffic.Commute.Time.Index ~ Health.Care.Index,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Cost.of.Living.Index"){
                paste("Their covariance:", cov(dataset$Traffic.Commute.Time.Index,dataset$Cost.of.Living.Index),
                      "Their correlation coefficient:", cor(dataset$Traffic.Commute.Time.Index,dataset$Cost.of.Living.Index),
                      "Their linear equation:", lm(Traffic.Commute.Time.Index ~ Cost.of.Living.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Property.Price.to.Income.Ratio"){
                paste("Their covariance:", cov(dataset$Traffic.Commute.Time.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their correlation coefficient:", cor(dataset$Traffic.Commute.Time.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their linear equation:", lm(Traffic.Commute.Time.Index ~ Property.Price.to.Income.Ratio,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Traffic.Commute.Time.Index"){
                paste("X and Y must be different for the correct results.")
            }else if(input$y == "Pollution.Index"){
                paste("Their covariance:", cov(dataset$Traffic.Commute.Time.Index,dataset$Pollution.Index),
                      "Their correlation coefficient:", cor(dataset$Traffic.Commute.Time.Index,dataset$Pollution.Index),
                      "Their linear equation:", lm(Traffic.Commute.Time.Index ~ Pollution.Index,dataset)[1],
                      "There is a strong correlation between them")
            }else if(input$y == "Climate.Index"){
                paste("Their covariance:", cov(dataset$Traffic.Commute.Time.Index,dataset$Climate.Index),
                      "Their correlation coefficient:", cor(dataset$Traffic.Commute.Time.Index,dataset$Climate.Index),
                      "Their linear equation:", lm(Traffic.Commute.Time.Index ~ Climate.Index,dataset)[1],
                      "There is a weak correlation between them")}
        }
        else if(input$x == "Pollution.Index"){
            if(input$y == "Quality.of.Life.Index"){
                paste("Their covariance:", cov(dataset$Pollution.Index,dataset$Quality.of.Life.Index),
                      "Their correlation coefficient:", cor(dataset$Pollution.Index,dataset$Quality.of.Life.Index),
                      "Their linear equation:", lm(Pollution.Index ~ Quality.of.Life.Index,dataset)[1],
                      "There is a strong correlation between them")
            }else if(input$y == "Purchasing.Power.Index"){
                paste("Their covariance:", cov(dataset$Pollution.Index,dataset$Purchasing.Power.Index),
                      "Their correlation coefficient:", cor(dataset$Pollution.Index,dataset$Purchasing.Power.Index),
                      "Their linear equation:", lm(Pollution.Index ~ Purchasing.Power.Index,dataset)[1],
                      "There is a strong correlation between them")
            }else if(input$y == "Safety.Index"){
                paste("Their covariance:", cov(dataset$Pollution.Index,dataset$Safety.Index),
                      "Their correlation coefficient:", cor(dataset$Pollution.Index,dataset$Safety.Index),
                      "Their linear equation:", lm(Pollution.Index ~ Safety.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Health.Care.Index"){
                paste("Their covariance:", cov(dataset$Pollution.Index,dataset$Health.Care.Index),
                      "Their correlation coefficient:",cor(dataset$Pollution.Index,dataset$Health.Care.Index),
                      "Their linear equation:", lm(Pollution.Index ~ Health.Care.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Cost.of.Living.Index"){
                paste("Their covariance:", cov(dataset$Pollution.Index,dataset$Cost.of.Living.Index),
                      "Their correlation coefficient:", cor(dataset$Pollution.Index,dataset$Cost.of.Living.Index),
                      "Their linear equation:", lm(Pollution.Index ~ Cost.of.Living.Index,dataset)[1],
                      "There is a strong correlation between them")
            }else if(input$y == "Property.Price.to.Income.Ratio"){
                paste("Their covariance:", cov(dataset$Pollution.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their correlation coefficient:", cor(dataset$Pollution.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their linear equation:", lm(Pollution.Index ~ Property.Price.to.Income.Ratio,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Traffic.Commute.Time.Index"){
                paste("Their covariance:", cov(dataset$Pollution.Index,dataset$Traffic.Commute.Time.Index),
                      "Their correlation coefficient:", cor(dataset$Pollution.Index,dataset$Traffic.Commute.Time.Index),
                      "Their linear equation:", lm(Pollution.Index ~ Traffic.Commute.Time.Index,dataset)[1],
                      "There is a strong correlation between them")
            }else if(input$y == "Pollution.Index"){
                paste("X and Y must be different for the correct results.")
            }else if(input$y == "Climate.Index"){
                paste("Their covariance:", cov(dataset$Pollution.Index,dataset$Climate.Index),
                      "Their correlation coefficient:", cor(dataset$Pollution.Index,dataset$Climate.Index),
                      "Their linear equation:", lm(Pollution.Index ~ Climate.Index,dataset)[1],
                      "There is a strong correlation between them")}
        }else if(input$x == "Climate.Index"){
            if(input$y == "Quality.of.Life.Index"){
                paste("Their covariance:", cov(dataset$Climate.Index,dataset$Quality.of.Life.Index),
                      "Their correlation coefficient:", cor(dataset$Climate.Index,dataset$Quality.of.Life.Index),
                      "Their linear equation:", lm(Climate.Index ~ Quality.of.Life.Index,dataset)[1],
                      "There is a moderate correlation between them")
            }else if(input$y == "Purchasing.Power.Index"){
                paste("Their covariance:", cov(dataset$Climate.Index,dataset$Purchasing.Power.Index),
                      "Their correlation coefficient:", cor(dataset$Climate.Index,dataset$Purchasing.Power.Index),
                      "Their linear equation:", lm(Climate.Index ~ Purchasing.Power.Index,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Safety.Index"){
                paste("Their covariance:", cov(dataset$Climate.Index,dataset$Safety.Index),
                      "Their correlation coefficient:", cor(dataset$Climate.Index,dataset$Safety.Index),
                      "Their linear equation:", lm(Climate.Index ~ Safety.Index,dataset)[1],
                      "There is a weakcorrelation between them")
            }else if(input$y == "Health.Care.Index"){
                paste("Their covariance:", cov(dataset$Climate.Index,dataset$Health.Care.Index),
                      "Their correlation coefficient:", cor(dataset$Climate.Index,dataset$Health.Care.Index),
                      "Their linear equation:", lm(Climate.Index ~ Health.Care.Index,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Cost.of.Living.Index"){
                paste("Their covariance:", cov(dataset$Climate.Index,dataset$Cost.of.Living.Index),
                      "Their correlation coefficient:", cor(dataset$Climate.Index,dataset$Cost.of.Living.Index),
                      "Their linear equation:", lm(Climate.Index ~ Cost.of.Living.Index,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Property.Price.to.Income.Ratio"){
                paste("Their covariance:", cov(dataset$Climate.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their correlation coefficient:", cor(dataset$Climate.Index,dataset$Property.Price.to.Income.Ratio),
                      "Their linear equation:", lm(Climate.Index ~ Property.Price.to.Income.Ratio,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Traffic.Commute.Time.Index"){
                paste("Their covariance:", cov(dataset$Climate.Index,dataset$Traffic.Commute.Time.Index),
                      "Their correlation coefficient:", cor(dataset$Climate.Index,dataset$Traffic.Commute.Time.Index),
                      "Their linear equation:", lm(Climate.Index ~ Traffic.Commute.Time.Index,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Pollution.Index"){
                paste("Their covariance:", cov(dataset$Climate.Index,dataset$Pollution.Index),
                      "Their correlation coefficient:", cor(dataset$Climate.Index,dataset$Pollution.Index),
                      "Their linear equation:", lm(Climate.Index ~ Pollution.Index,dataset)[1],
                      "There is a weak correlation between them")
            }else if(input$y == "Climate.Index"){
                paste("X and Y must be different for the correct results.")}
        }})
    
    
    ######## 2. APP ################
    
    output$map <- renderLeaflet({
        dataset <- subset(dataset, select = c(Country, latitude, longitude) , subset = (dataset$Continent %in% c(input$Continent)))
        dataset %>%
            leaflet() %>%
            addTiles() %>%
            addMarkers(lng = dataset$longitude, lat = dataset$latitude,
                       label = dataset$Country) %>%
            setView(lng = median(dataset$longitude),
                    lat = median(dataset$latitude),
                    zoom = 2) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas)
    })
    
    summary_condition <- reactive({ input$Summary })
    
    output$Summary_out <- renderTable({
        if(summary_condition()){
            dataset <- dataset[-c(1,11,12)]
            dataset <- subset(dataset, (Continent %in% c(input$Continent)))
            names(dataset) <- c("Quality","Purchasing","Safety","HealthCare","CostOfLivingIndex","PropertyPrice","Traffic","Pollution","Climate","Continent")
            sqldf("SELECT Continent,AVG(Quality),AVG(Purchasing),AVG(Safety),AVG(HealthCare),AVG(CostOfLivingIndex),AVG(PropertyPrice),AVG(Traffic),AVG(Pollution),AVG(Climate) FROM dataset GROUP BY Continent")
        }
        
    })
    
    output$plot_out <- renderPlot(
        if(input$y_axis == "Quality of Life Index"){
            ggplot(data=dataset,aes(x=Continent, y=mean(Quality.of.Life.Index), fill=Country)) +
                geom_bar(stat="identity")+
                theme_minimal()+ labs(title = "Quality life index for Continent and Countries",
                                      x = "Continents", y = "Total Quality of Life Index")
            
        }
        else if(input$y_axis == "Purchasing Power Index"){
            ggplot(data=dataset,aes(x=Continent, y=(Purchasing.Power.Index), fill=Country)) +
                geom_bar(stat="identity")+
                theme_minimal()+ labs(title = "Purchasing Power Index for Continent and Countries",
                                      x = "Continents", y = "Total Purchasing Power Index")
        }
        else if(input$y_axis == "Safety Index"){
            ggplot(data=dataset,aes(x=Continent, y=Safety.Index, fill=Country)) +
                geom_bar(stat="identity")+
                theme_minimal() + labs(title = "Safety index for Continent and Countries",
                                       x = "Continents", y = "Total Safety Index")
        }
        else if(input$y_axis == "Health Care Index"){
            ggplot(data=dataset,aes(x=Continent, y=Health.Care.Index, fill=Country)) +
                geom_bar(stat="identity")+
                theme_minimal() + labs(title = "Health Care index for Continent and Countries",
                                       x = "Continents", y = "Total Health Care Index")
        }
        else if(input$y_axis == "Cost of Living Index"){
            ggplot(data=dataset,aes(x=Continent, y=Cost.of.Living.Index, fill=Country)) +
                geom_bar(stat="identity")+
                theme_minimal() + labs(title = "Cost of Living Index for Continent and Countries",
                                       x = "Continents", y = "Total Cost of Living Index")
        }
        else if(input$y_axis == "Traffic Commute Time Index"){
            ggplot(data=dataset,aes(x=Continent, y=Traffic.Commute.Time.Index, fill=Country)) +
                geom_bar(stat="identity")+
                theme_minimal() + labs(title = "Traffic Commute Index for Continent and Countries",
                                       x = "Continents", y = "Total Traffic Commute Index")
        }
        else if(input$y_axis == "Pollution Index"){
            ggplot(data=dataset,aes(x=Continent, y=Pollution.Index, fill=Country)) +
                geom_bar(stat="identity")+
                theme_minimal() + labs(title = "Pollution index for Continent and Countries",
                                       x = "Continents", y = "Total Pollution Index")
        }
        else if(input$y_axis == "Climate Index"){
            ggplot(data=dataset,aes(x=Continent, y=Climate.Index, fill=Country)) +
                geom_bar(stat="identity")+
                theme_minimal() + labs(title = "Climate index for Continent and Countries",
                                       x = "Continents", y = "Total Climate Index")
        }
    )
    
    output$CommentsMapTable <- renderText({
        paste("When we look at the map, it is obvious that the majority is from European Continent, while there are only 2 countries for Ocenia and 5 countries for Africa. Also, when we look at the table, we can see that there is a huge difference between Africa and Ocenia in purchasing power index, safety, health care, of living and pollution index. As a result of this, life quality index for Ocenia is double compared to the Africa. On the other hand, when we investigate Europe, Asia and America, there is no huge difference between the variables except pollution, safety and purchasing indexes. In these 3 variables, Asia has the highest value for population, America has the lowest value for safety and Europe has the desirable values for these variables. As a result, Europe has more life quality index than Asia and America.")
    })
    
    output$Comments2ndapp <- renderText({
        if(input$y_axis == "Quality of Life Index"){
            paste("When we look at the plot, we can see that Denmark, Switzerland, Finland and Australia has the highest for quality of life index while Nigeria, Bangladesh and Kenya has the lowest. Although 3 of the top 4 countries are from the European continent, Europe has not the highest life quality index. We can conclude that standard deviation is high for European continent in this index.")
            
        }
        else if(input$y_axis == "Purchasing Power Index"){
            paste("We can see the same order with the Life quality index except Asia and America. Asia has more purchasing power index than America. Also, since the result is similar to the life quality index, we may conclude that Purchasing Power Index has a positive contribution to Life Quality Index.
            According to the analysis, 
            the continents with the most suitable purchasing power are Europe and Oceania, while the lowest continent is Africa.")
            
        }
        else if(input$y_axis == "Safety Index"){
            paste("In this plot, we see that there is a decreasing for Ocean, and we can confirm that from the table. Ocean is the 3rd continent that has the highest safety index. Also, we can say the same for America. Europe and Asia have the highest index. Since there is no huge difference in order, we can say that Safety has a positive contribution to Life Quality Index.
            ")
        }
        else if(input$y_axis == "Health Care Index"){
            paste("We can see that health care is at the same level in Asia, Europe and America. 
                  Still, Oceania continent leads, Africa takes the last place.")
        }
        else if(input$y_axis == "Cost of Living Index"){
            paste("Since the order are same with Quality index, we may say that these are associated with each other.
                  Moreover, when we look at it on a country basis, 
                  Europe has an advantage over other countries in cost of living.")
        }
        else if(input$y_axis == "Traffic Commute Time Index"){
            paste("Africa takes the lead in Traffic commute time index and we can see that the order is completely changed. Therefore, we may say that Traffic commute time affects life quality index negatively.")
            
        }
        else if(input$y_axis == "Pollution Index"){
            paste("Africa takes the lead in Traffic commute time index and we can see that the order is completely changed. Therefore, we may say that Traffic commute time affects life quality index negatively.")
            
        }
        else if(input$y_axis == "Climate Index"){
            paste("When we look at the country basis, we can conclude that the climate is more favorable in less populated countries.")
            
        }
    })
    
    
    ######## 3. APP ################
    
    output$map3 <- renderLeaflet({
        dataset <- subset(dataset, input$Score < dataset$Quality.of.Life.Index)
        dataset %>%
            leaflet() %>%
            addProviderTiles("Stamen.Watercolor") %>%
            setView(lng = median(dataset$longitude),
                    lat = median(dataset$latitude),
                    zoom = 1) %>%
            addMarkers(lng = (dataset$longitude),
                       lat = (dataset$latitude),
                       label = dataset$Country,
                       popup = paste0( "<b> Name: </b>",dataset$Country,
                                       "<br>",
                                       "<b> Life Quality Index: </b>",dataset$Quality.of.Life.Index,
                                       "<br>",
                                       "<b> Purchasing Power Index: </b>", dataset$Purchasing.Power.Index,
                                       "<br>",
                                       "<b> Safety Index: </b>", dataset$Safety.Index,
                                       "<br>",
                                       "<b> Health Care Index: </b>", dataset$Health.Care.Index,
                                       "<br>",
                                       "<b> Pollution: </b>", dataset$Pollution.Index))
    })
    output$remain <- renderText({
        dataset <- subset(dataset, input$Score < dataset$Quality.of.Life.Index)
        isolate(paste0(dataset$Country,","))
    })
    
    ######## 4. APP ################
    
    output$img <- renderUI({
        tags$img(src = "https://edumag.net/wp-content/uploads/2015/11/studyabroad.jpg", width = 700, height = 350)
    })
    output$Money_out <- renderText({
        input$Calculate
        isolate(paste("Money:", input$Money))
    })
    
    output$Safety_out <- renderText({
        input$Calculate
        isolate(paste("Safety:", input$Safety))
    })
    output$HealthCare_out <- renderText({
        input$Calculate
        isolate(paste("Health Care:", input$HealthCare))
    })
    
    output$Traffic_out <- renderText({
        input$Calculate
        isolate(paste("Traffic:", input$Traffic))
    })
    
    output$Pollution_out <- renderText({
        input$Calculate
        isolate(paste("Pollution:", input$Pollution))
    })
    
    output$Cost_out <- renderText({
        input$Calculate
        isolate(paste("Cost:", input$Cost))
    })
    
    output$Climate_out <- renderText({
        input$Calculate
        isolate(paste("Climate:", input$Climate))
    })
    
    Score <- reactive({
        input$Calculate
        dataset$Score <- abs((dataset$Purchasing.Power.Index * as.numeric(input$Money) +  dataset$Safety.Index * as.numeric(input$Safety) +   dataset$Health.Care.Index * as.numeric(input$HealthCare) + dataset$Traffic.Commute.Time.Index * as.numeric(input$Traffic) +  dataset$Pollution.Index * as.numeric(input$Pollution) + dataset$Cost.of.Living.Index * as.numeric(input$Cost) + dataset$Climate.Index * as.numeric(input$Climate)))/10
        sqldf("SELECT Country,Score FROM dataset ORDER BY Score DESC LIMIT 5")
    })
    
    output$Country4 <- renderTable({
        input$Calculate
        Score()
    })
    
}



shinyApp(ui = ui, server = server)




# References

### https://www.kaggle.com/dumbgeek/countries-dataset-2020?select=Quality+of+life+index+by+countries+2020.csv
### https://developers.google.com/public-data/docs/canonical/countries_csv

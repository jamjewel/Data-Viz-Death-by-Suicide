#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(rgeos)
library(maptools)
library(ggmap)
library(broom)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(htmlwidgets)
library(htmltools)

# Define UI for application that draws a histogram
ui <- fluidPage( theme = shinytheme("slate"),
                 chooseSliderSkin("Flat"),
                 
                 
                 
                 tags$head(
                   tags$style(HTML("
                                   @import url('//fonts.googleapis.com/css?family=Black Ops One|Cabin:400,700');
                                   
                                   h1 {
                                   font-family: 'Black Ops One', cursive;
                                   font-weight: 500;
                                   line-height: 1.1;
                                   color: #48ca3b;
                                   }
                                   
                                   "))
                   ),
                 
                 # Application title
                 headerPanel("Death by Suicide - Odds of Men or Women more? "),
                 
                 # Sidebar with a slider input for year
                 sidebarLayout(position = "left",
                               sidebarPanel( 
                                 sliderInput("year", label = "Year", min = 1997, sep="", max = 2015, value = 2004, 
                                             animate = animationOptions(interval = 500, loop = FALSE)),
                                 
                                 
                                 tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 30pt !important; color: yellow}")),
                                 
                                 radioButtons("Gender", "Gender:", c("male"= "male","female"="female")),
                                 
                                 
                                 
                                 helpText("\"When people kill themselves, they think they're ending the pain, but all they're doing is passing it on to those they leave behind\" -Jeannette Walls .
Suicide is highly complex with lots of underlying and precipitating causes.While there are few human problems as complex or multi-determinate as suicide, one factor
                                          remains constant: men are considerably more likely to end their lives by suicide than women.Men tend to choose more violent and immediately lethal means, which increases their likelihood of dying. Women tend to choose less lethal means, which increases their chance of survival. The visualization constructed emphasise and prove the gender disparity in suicide.")
                                 
                                 ),
                               
                               # Population Pyramid
                               
                               mainPanel(
                                 tabsetPanel(id = 'tabs1',
                                             tabPanel("Yearly Statistics",
                                                      fluidRow(
                                                        column(8,
                                                               leafletOutput("mymap",height = 600, width =1000))),
                                                      
                                                      fluidRow(
                                                        p("Age wise ratio"),
                                                        column(12,
                                                               plotOutput("pyramid", height = 250, width =1000)))
                                             ),
                                             
                                             
                                             tabPanel("Across The Years",
                                                      p('Trend in Male and Female Suicides Across the years'),
                                                      column(12,
                                                             plotlyOutput("barplot", height = 400, width =1000))
                                                      
                                             )
                                 ))
                               
                               
                               
                               
                               
                               
                               
                               )
                               )




vic.lga.shp <- readShapeSpatial("TM_WORLD_BORDERS_SIMPL-0.3.shp") 


class (vic.lga.shp)
names(vic.lga.shp)

p2 <- leaflet(vic.lga.shp) %>% setView(lng = 145.5, lat = 36.5, zoom = 5)
p2 %>% addPolygons()

#Import Data and Pre-processing the data
data <- read.csv("who_suicide_statistics.csv")
suicide_stat_sum <- suicide_stat %>% group_by(year, country, sex) %>%  summarise(suicide = sum(as.numeric(suicides_no),na.rm = TRUE), population = sum(as.numeric(population),na.rm = TRUE))
suicide_stat_sum$NAME <- suicide_stat_sum$country
suicide_stat_sum$NAME <- as.character(suicide_stat_sum$NAME)
suicide_stat_sum$NAME[suicide_stat_sum$NAME == "Russian Federation"] <- "Russia"
suicide_stat_sum$NAME[suicide_stat_sum$NAME == "Hong Kong SAR"] <- "Hong Kong"
suicide_stat_sum$NAME[suicide_stat_sum$NAME == "Republic of Korea"] <- "Korea, Democratic People's Republic of"
suicide_stat_sum$NAME[suicide_stat_sum$NAME == "United States of America"] <- "United States"
suicide_stat_sum$NAME[suicide_stat_sum$NAME == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"

#Import Data
Suicide_Data <- read.csv("who_suicide_statistics.csv")
Suicide_Data2 <-  Suicide_Data %>% select(year, age, sex, suicides_no)
Suicide_Data2 <- Suicide_Data2 %>% group_by(year, age, sex) 
Suicide_Data2$suicides_no[is.na(Suicide_Data2$suicides_no)] <- 0
Suicide_Data2$age <- factor(Suicide_Data2$age, levels = c("5-14 years","15-24 years","25-34 years","35-54 years", "55-74 years","75+ years"), ordered= TRUE)


# importing data
suicide_stat_sum8 <- Suicide_Data %>% group_by(year, sex) %>%  summarise(suicide = sum(as.numeric(suicides_no),na.rm = TRUE))


# Define server logic required to draw the world map and pyramid

server <- function(input, output) 
{
  bins <- quantile(
    suicide_stat_sum$suicide,
    probs = seq(0,1,.25), names = FALSE, na.rm = TRUE)
  
  
  pal <- colorBin(
    "YlOrRd",
    domain = suicide_stat_sum$suicide, 
    bins = bins
  )
  
  title <- tags$div(
    HTML('<h3>Suicide Count Over the Years</h3>')
  )
  #creating the pyramid
  
  output$pyramid <- renderPlot({
    p1 <- ggplot(Suicide_Data2, aes(x = age, y = suicides_no, fill = sex))
    p1 + geom_bar(data = filter(Suicide_Data2,sex == "female" & year == input$year), 
                  stat = "identity") +
      geom_bar(data = filter(Suicide_Data2,sex == "male" & year == input$year),
               aes(y=suicides_no *(-1)), stat = "identity") +
      scale_y_continuous(breaks = seq(-70000, 70000, 20000), 
                         limits = c(-70000, 70000),
                         labels = (seq(-70000, 70000, 20000))) +
      labs(x = "Age Category", y = "Suicide Count")+
      coord_flip()
    
  })
  
  #creating the map
  output$mymap <- renderLeaflet({
    
    validate(
      need(input$year != "", "No year selected")
    )
    
    suicide_stat_sum <- suicide_stat_sum %>% filter(year == input$year & sex == input$Gender)
    
    merge.lga.profiles3 <- sp::merge(vic.lga.shp, suicide_stat_sum, by="NAME", duplicateGeoms = TRUE)
    
    p3 <- leaflet(merge.lga.profiles3) %>% setView(lng = 47, lat = 47.5, zoom = 2)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g suicides",
      merge.lga.profiles3$NAME, 
      merge.lga.profiles3$suicide
    ) %>% lapply(htmltools::HTML)
    
    
    p3 %>% addPolygons(
      fillColor = ~pal(suicide),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>% 
      addLegend(pal = pal, 
                values = ~suicide, 
                opacity = 0.7, title = "Suicide count",
                position = "bottomleft") %>% 
      addControl(title, position = "topright")
    
  })
  
  # Creating the bar graph
  
  output$barplot <-renderPlotly({
    
    
    plot_ly(data = suicide_stat_sum8, x = ~year, y = ~suicide, type = "scatter", color = ~sex,
            colors = c("#ef8a62", "#67a9cf")) %>% 
      
      layout(yaxis = list(zeroline = FALSE, title = "Suicide Count"),
             xaxis = list(zeroline = FALSE, title = "Year"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


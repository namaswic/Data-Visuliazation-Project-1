library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)

sidebar <-
  dashboardSidebar(
    sidebarMenu(style = "position: fixed; overflow: visible;",
    sidebarMenuOutput("menu"))
  )  

body <-
  
  dashboardBody(
    tabItems(
      #Tab elements displaying the table and line graph for UK cholera
    tabItem(tabName = "avsd",
    fluidPage(
      
      box( title = "Atacks,Deaths and Total of Deaths and Attacks", solidHeader = TRUE, status = "primary",width=12, skin = "purple",
           plotOutput("linegraph",height = "800px"),style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"
           
      ),
      box( title = "Table of Atacks,Deaths and Total of Deaths and Attacks", solidHeader = TRUE ,width=12,status = "primary", skin = "purple",
           dataTableOutput("mytable",height = "600px"),style = "font-family:Arial, Helvetica, sans-serif;font-size:30px"),
  height="950px")),
  #Tab elements displaying the bar and pie plot for naples cholera
    tabItem(tabName = "set1",
    fluidPage(
      
      box( title = "Table of age categories for male fatalities and female fatalities ", solidHeader = TRUE, status = "primary",width=12,skin = "purple", 
           dataTableOutput("table2"),style = "font-family:Arial, Helvetica, sans-serif;font-size:30px"),
      box( title = "Age categories for male fatalities", solidHeader = TRUE, status = "primary",width=6, skin = "purple",
                plotOutput('plotmale',height="600px") ,style = "font-family:Arial, Helvetica, sans-serif;font-size:25px"   
           ),
      box( title = "Age categories for female fatalities", solidHeader = TRUE, status = "primary",width=6, skin = "purple",
           plotOutput('plotfemale',height="600px")  ,style = "font-family:Arial, Helvetica, sans-serif;font-size:25px"  
      ),height="1000px"
      
      
    )),
    #All plots showing the data for the UK Census in 1851
    tabItem(tabName = "charts",
    fluidPage(
      box( title = "Table of the census age data for Men and Women", solidHeader = TRUE, status = "primary",width=8, skin = "purple",
           dataTableOutput("table3"),style = "font-family:Arial, Helvetica, sans-serif;font-size:30px"
      ),
      box( title = "Pie chart for the overall number of Men vs Women", solidHeader = TRUE, status = "primary",width=4, skin = "purple",
           plotOutput('pieMenWomen',height = "550px") ,style = "font-family:Arial, Helvetica, sans-serif;font-size:20px" 
      ),
      box( title = "Pie Chart for Men", solidHeader = TRUE, status = "primary",width=6, skin = "purple",
           plotOutput('pieMale',height="700px"),style = "font-family:Arial, Helvetica, sans-serif;font-size:20px") ,
      box( title = "Pie Chart for Female", solidHeader = TRUE, status = "primary",width=6, skin = "purple",
                plotOutput('pieFemale',height="700px") ,style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"   
           ),
      box( title = "Bar chart of the census age data for Men", solidHeader = TRUE, status = "primary",width=6, skin = "purple",
           plotOutput('barplotmale3',height="500px") ,style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"   
      ),
      box( title = "Bar chart of the census age data for Women", solidHeader = TRUE, status = "primary",width=6, skin = "purple",
           plotOutput('barplotfemale3',height="500px") ,style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"   
      )
      
      
      
    )),
   #Tab for showing the leaflet map
    tabItem(tabName = "lmap",
    fluidPage(
      
      box( title = "Today's Map", solidHeader = TRUE, status = "primary",width=12, skin = "purple",
           leafletOutput("leaf",height = "1900px") ,style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"
      )
      
    )),
  #Tab for showing the original map
    tabItem(tabName = "omap",
    fluidPage(
      fluidRow(
        tags$style(type='text/css', '#hover_info { color: black;font-size:30px;}'), 
        tags$style(type='text/css', '#hover_info1 { color: black;font-size:30px;}'), 
        tags$style(type='text/css', '#hover_info2 { color: black;font-size:30px;}'), 
        box( title = "Orginal Map", solidHeader = TRUE, status = "primary",skin = "purple",width = 8,
             plotOutput("myImage",hover = hoverOpts(id ="plot_hover"),height="1750px")),  
       
             column(width = 4,verbatimTextOutput("hover_info"),tags$head(tags$style(HTML("
                            #printout {
                              font-size: 30px;
                            }
                            ")))),
        column(width = 4,verbatimTextOutput("hover_info1"),style = "font-family:Arial, Helvetica, sans-serif;font-size:25px"),
        column(width = 4,verbatimTextOutput("hover_info2"),style = "font-family:Arial, Helvetica, sans-serif;font-size:25px"),
        htmlOutput("caption1")
             
        
      )
      
      )
     
      
    ),
  #Tab for showing the information about the project
  tabItem(tabName = "info",
          fluidPage(
            fluidRow(
              htmlOutput("caption")
              )  
             
            )
            
          )
          
          
  
  )
    
    
 )   

dashboardPage(
  dashboardHeader(title="Project 1"),
  sidebar,
  body
)

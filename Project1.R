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
    tabItem(tabName = "avsd",
    fluidPage(
      
      box( title = "Atacks,Deaths and Total of Deaths and Attacks", solidHeader = TRUE, status = "primary",width=12, skin = "purple",
           plotOutput("linegraph",height = "500px"),style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"
           
      ),
      box( title = "Table of Atacks,Deaths and Total of Deaths and Attacks", solidHeader = TRUE ,width=12,status = "primary", skin = "purple",
           dataTableOutput("mytable",height = "400px"),style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"),
  height="950px")),
    tabItem(tabName = "set1",
    fluidPage(
      
      box( title = "Table of age categories for male fatalities and female fatalities ", solidHeader = TRUE, status = "primary",width=12,skin = "purple", 
           dataTableOutput("table2"),style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"),
      box( title = "Age categories for male fatalities", solidHeader = TRUE, status = "primary",width=6, skin = "purple",
                plotOutput('plotmale',height="300px") ,style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"   
           ),
      box( title = "Age categories for female fatalities", solidHeader = TRUE, status = "primary",width=6, skin = "purple",
           plotOutput('plotfemale',height="300px")  ,style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"  
      ),height="1000px"
      
      
    )),
    
    tabItem(tabName = "charts",
    fluidPage(
      box( title = "Table of the census age data for men and women", solidHeader = TRUE, status = "primary",width=8, skin = "purple",
           dataTableOutput("table3"),style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"
      ),
      box( title = "Pie chart for the overall number of men vs women", solidHeader = TRUE, status = "primary",width=4, skin = "purple",
           plotOutput('pieMenWomen',height = "450px") ,style = "font-family:Arial, Helvetica, sans-serif;font-size:20px" 
      ),
      box( title = "Pie Chart for Men", solidHeader = TRUE, status = "primary",width=6, skin = "purple",
           plotOutput('pieMale',height="400px"),style = "font-family:Arial, Helvetica, sans-serif;font-size:20px") ,
      box( title = "Pie Chart for Female", solidHeader = TRUE, status = "primary",width=6, skin = "purple",
                plotOutput('pieFemale',height="400px") ,style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"   
           ),
      box( title = "Bar chart of the census age data for Men", solidHeader = TRUE, status = "primary",width=6, skin = "purple",
           plotOutput('barplotmale3',height="355px") ,style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"   
      ),
      box( title = "Bar chart of the census age data for Women", solidHeader = TRUE, status = "primary",width=6, skin = "purple",
           plotOutput('barplotfemale3',height="355px") ,style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"   
      )
      
      
      
    )),
   
    tabItem(tabName = "lmap",
    fluidPage(
      
      box( title = "Today's Map", solidHeader = TRUE, status = "primary",width=12, skin = "purple",
           leafletOutput("leaf",height = "950px") ,style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"
      )
      
    )),
    tabItem(tabName = "omap",
    fluidPage(
      fluidRow(
        box( title = "Orginal Map", solidHeader = TRUE, status = "primary",skin = "purple",width = 10,
             plotOutput("myImage",hover = hoverOpts(id ="plot_hover"),height="1000px")),  
       
             column(width = 2,verbatimTextOutput("hover_info"),style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"),
        column(width = 2,verbatimTextOutput("hover_info1"),style = "font-family:Arial, Helvetica, sans-serif;font-size:20px"),
        column(width = 2,verbatimTextOutput("hover_info2"),style = "font-family:Arial, Helvetica, sans-serif;font-size:20px")
             
        
      )
      
      )
     
      
    ))
    
    
 )   

dashboardPage(
  dashboardHeader(title="Project 1"),
  sidebar,
  body
)

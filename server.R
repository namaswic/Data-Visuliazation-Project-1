#libraries to be used
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(htmltools)
library(plyr)
library(RColorBrewer)
library(rgdal)
library(sp)

#shiny server 
shinyServer(function(input, output) {
  #code to import and preprocess the data for visualization
  #choleraDeaths.tsv
  dataset <-read.table("choleraDeaths.tsv",sep="\t",header=TRUE)
  #preprocessing
  dataset$cumSumA<-cumsum(dataset$Attack)
  dataset$cumSumD<-cumsum(dataset$Death)
  
 
  dataset$Date1 <- as.Date(dataset$Date, "%d-%b-%Y")
  dataset$Date <- factor(dataset$Date, levels = dataset$Date[order(dataset$Date1)])
  subset <- subset(dataset, select= c(Date,Attack,Death,cumSumA,cumSumD))
  
  #naplesCholeraAgeSexData.tsv
  dataset2<-read.table("naplesCholeraAgeSexData.tsv",sep="\t",header=TRUE)
  #preprocessing
  temp<-c("0-1","2-5","6-10","11-15","16-20","21-40","41-60","61-80","over 80")
  dataset2$Age <- factor(dataset2$Age,levels=temp)
  sort(dataset2$Age)
  
  #UKcensus1851.csv
  dataset3<-read.csv("UKcensus1851.csv",header=TRUE)
  
  ##preprocessing
  dataset3$Total<-dataset3$Male+dataset3$Female
  maleTotal<-sum(dataset3$Male)
  femaleTotal<-sum(dataset3$Female)
  dataset3$Male1<-format(dataset3$Male,big.mark=",",scientific=FALSE)
  dataset3$Female1<-format(dataset3$Female,big.mark=",",scientific=FALSE)
  dataset3$Male2<-round(dataset3$Male/1000,0)
  dataset3$Female2<-round(dataset3$Female/1000,0)
  
  tableForD3 <- matrix(c("Male",maleTotal,"Female",femaleTotal),ncol=2,byrow=TRUE)
  colnames(tableForD3) <- c("Gender","Total")
  tableForD3<-as.table(tableForD3)
  tableForD3<-as.data.frame.matrix(tableForD3)
  c4 = c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", "70 to 79", "80+")
  df = cbind(df, c4)
  dataset3$Age<-gsub(" to ", "-", dataset3$Age)
  
  
  datasetDeath<-read.csv("choleraDeathLocations.csv",header=FALSE)
  datasetPump<-read.csv("choleraPumpLocations.csv",header=FALSE)
  Icons <- iconList(
    pump = makeIcon("C:/Users/namas/Desktop/Visual Analytics/Project-1/Project-1/pump.png", "C:/Users/namas/Desktop/Visual Analytics/Project-1/Project-1/pump.png", 32)
  )
  datasetDeath$Icon<-ifelse(datasetDeath$V1<2,"death","death")
  datasetPump$Icon<-ifelse(datasetPump$V1<2,"pump","pump")
  coloursRegion <- colorFactor(topo.colors(5), datasetDeath$V1)
  pal <- colorNumeric(palette = "Reds", domain = datasetDeath$V1)
  #import john snows image
  jp <- jpeg::readJPEG('snowMapRobinWilson.jpg')
  #preprocessing
  df1 <- data.frame(x = -0.143755:-0.131110, y = 51.509440:51.516522)
  bottom_left = c(0, -0.143755, 51.509440)
  top_right = c(0, -0.131110, 51.516522)
  cord = rbind(bottom_left, top_right, subset(datasetDeath,select=c(V1,V2,V3)))
  ################Rendering outputs
  #line graph for UK CHOLERA
  output$linegraph <- renderPlot({
    
    ggplot(dataset, aes(x = Date) ) + 
     
      geom_line(aes(y=dataset$Attack,group=1,colour="Attack"),size=2)+#geom_point(aes(y=dataset$Attack,group=1))+
     geom_line(aes(y=dataset$Death,group=1,colour="Death"),size=2)+#geom_point(aes(y=dataset$Death,group=1))+
      geom_line(aes(y=dataset$cumSumA,group=1,colour="CummalativeA"),size=2)+#geom_point(aes(y=dataset$cumSumA,group=1))+
      geom_line(aes(y=dataset$cumSumD,group=1,colour="CummalativeD"),size=2)+#geom_point(aes(y=dataset$cumSumD,group=1))+
      
      xlab("\nDate")+
      ylab("\nNumber of Event occured")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(text = element_text(size=28),legend.position = "right")+ theme(legend.text=element_text(size=25))+
      scale_colour_manual("Events",labels = c("Attack", "Cumalative of Attacks","Cumalative of Deaths","Death" ),values = c("Attack"="#008b8b","CummalativeA"="blue","Death"="red","CummalativeD"="#CC0000"))
  
  })
#Rendering the menubar
      output$menu <- renderMenu({
        sidebarMenu(
          menuItem("Census Data UK(1851)",tabName = "charts"),
          menuItem("UK Cholera(1854)",tabName = "avsd"),
          menuSubItem("Attack V/S Deaths",tabName = "avsd"),
          menuSubItem("Original map",tabName = "omap"),
          menuSubItem("Today's map",tabName = "lmap"),
          menuItem("Naples Cholera(1884-1911)",tabName = "set1"),
          menuItem("Information about the Project",tabName = "info")
        )        
      })
      #rendering the table for UK CHOLERA
      output$mytable = renderDataTable({
        subset
      },options = list(orderClasses = TRUE,
                      lengthMenu = list(c(14,28,-1), c('14','28','All')),
                      pageLength = 14,
                      columnDefs = list(list(width = '200px', targets = "_all",class="dt-right")),
                      #aoColumnDefs = list(list(sClass="alignLeft",aTargets=c(list(1),list(2),list(3),list(4),list(5)))),
                      #list(width = "50px", targets = "_all"),
                      rownames=FALSE,
                      
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': 'Navy', 'color': '#fff'});",
                        "}"),
                   
                   
                      columns = list(
                        NULL,
                        NULL,
                        list(title = 'Attacks'), 
                        list(title = 'Deaths'),
                        list(title = 'Cumalative of Attacks'),
                        list(title = 'Cumalative of Deaths')
                      ))#, striped = TRUE,align = 'crrrr',digits = 0,size=20, 
      )
      #Rendering table for the Naples dataset

      output$table2 = DT::renderDataTable({
        dataset2
      },options = list(orderClasses = TRUE,
                       #lengthMenu = list(c(14,28,-1), c('14','28','All')),
                       pageLength = 14,
                       columnDefs = list(list(width = '200px', targets = "_all",class="dt-right")),
                       #aoColumnDefs = list(list(sClass="alignLeft",aTargets=c(list(1),list(2),list(3),list(4),list(5)))),
                       #list(width = "50px", targets = "_all"),
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': 'Navy', 'color': '#fff'});",
                         "}"),
                       dom='t',
                       columns = list(
                         NULL,
                         list(title = 'Age Categories'),
                         list(title = 'Male'), 
                         list(title = 'Female')
                       )))
      #Rendering bar plot for the Naples Male age categories
      output$plotmale <- renderPlot({
        ggplot(dataset2,aes(y=dataset2$Male,x=dataset2$Age))+ geom_bar(stat = "identity",fill="#02A8B5") +
          xlab("\nMale Age Categories")+ ylab("\nAverage Fatalities")+theme_bw()+geom_text(aes(label = dataset2$Male, y = dataset2$Male/2), size = 9)+theme(text = element_text(size=28))+
        #scale_fill_manual("Legend",breaks = c("0 to 9", "10 to 19", "20 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", "70 to 79", "80+"),values = c("blue","orange","#008b8b","red","blue","orange","#008b8b","red","cyan"))
          scale_fill_brewer(palette="Dark2")      
      })
      #Rendering bar plot for the Naples Male age categories
      output$plotfemale <- renderPlot({
        ggplot(dataset2,aes(y=dataset2$Female,x=dataset2$Age))+ geom_bar(stat = "identity",fill = "#ff3d53")+
          xlab("\nFemale Age Categories")+ ylab("\nAverage Fatalities")+theme_bw()+geom_text(aes(label = dataset2$Female, y = dataset2$Female/2), size = 9)+theme(text = element_text(size=28))
        
      })
      
      #Rendering table for the UK Census Data
      output$table3 = DT::renderDataTable({
        subset(dataset3,select=c(Age,Male1,Female1))
      }
      ,options = list(orderClasses = TRUE,
                     # lengthMenu = list(c(14,28,-1), c('14','28','All')),
                      pageLength = 14,
                      columnDefs = list(list(width = '200px', targets = "_all",class="dt-right")),
                      #aoColumnDefs = list(list(sClass="alignLeft",aTargets=c(list(1),list(2),list(3),list(4),list(5)))),
                      #list(width = "50px", targets = "_all"),
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': 'Navy', 'color': '#fff'});",
                        "}"),
                      dom='t',
                      columns = list(
                        NULL,
                        list(title = 'Age Categories'),
                        list(title = 'Male'), # skip column 2
                        list(title = 'Female')
                        
                      )
      ))
      #Rendering pie chart for the male age categories in UK Census Data
      output$pieMale <- renderPlot({
        dataset3$percentageM<-round(dataset3$Male/sum(dataset3$Male)*100,1)
        coul <- brewer.pal(9, "Blues")
        par(xpd=TRUE)
        pie(dataset3$Male, labels = paste0(dataset3$percentageM,"%"),col=coul,cex=2)
        legend(-1.4,1.3,legend=dataset3$Age,bty="n",
               fill=coul,cex=2)
        
        #df <- subset(dataset3, select= c(Age,Male))
          # factor levels need to be the opposite order of the cumulative sum of the values
         #df<- mutate(dataset3,Age = factor(Age, levels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")),
          #       cumulative = cumsum(Male),
           #      midpoint = cumulative - Male / 2,
            #     label = paste0(round(Male / sum(Male) * 100, 1), "%"))
        
      #  ggplot(df, aes(x = 1, weight = Male, fill = Age)) +
       #   geom_bar(width = 1, position = position_stack(reverse = TRUE)) +
        #  coord_polar(theta = "y") +theme(text = element_text(size=25),legend.text=element_text(size=25))+
         # geom_text(aes(x = c(1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.3,1.7), y = midpoint, label = label),size=10)+scale_fill_brewer(palette="Blues")+ labs(x="", y="")+theme(axis.text = element_blank(),axis.ticks = element_blank(), panel.grid  = element_blank())
      })
      #Rendering pie chart for the female age categories in UK Census Data
      output$pieFemale <- renderPlot({
        dataset3$percentageF<-round(dataset3$Female/sum(dataset3$Female)*100,1)
        coul <- brewer.pal(9, "Reds") 
        par(xpd=TRUE)
        pie(dataset3$Female, labels = paste0(dataset3$percentageF,"%"),col=coul,cex=2)
        legend(-1.4,1.3,legend=dataset3$Age,bty="n",
               fill=coul,cex=2)
        
        
       # df <- subset(dataset3, select= c(Age,Female))
        # factor levels need to be the opposite order of the cumulative sum of the values
        #df<- mutate(dataset3,Age = factor(Age, levels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")),
         #           cumulative = cumsum(Female),
          #          midpoint = cumulative - Female / 2,
           #         label = paste0(round(Female / sum(Female) * 100, 1), "%"))
        
        #ggplot(df, aes(x = 1, weight = Female, fill = Age)) +
         # geom_bar(width = 1, position = position_stack(reverse = TRUE)) +
          #coord_polar(theta = "y") +theme(text = element_text(size=25),legend.text=element_text(size=25))+theme(axis.text = element_blank(),axis.ticks = element_blank(), panel.grid  = element_blank())+
          #geom_text(aes(x = c(1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.3,1.7), y = midpoint, label = label),size=10)+scale_fill_brewer(palette="Reds")+ labs(x="", y="")
      })
      
      #Rendering bar chart for the male age categories in UK Census Data
      output$barplotmale3 <- renderPlot({
        ggplot(dataset3,aes(y=dataset3$Male2,x=dataset3$Age,fill = Age))+ 
          geom_bar(stat = "identity") +
          scale_fill_brewer(palette="Blues")+ xlab("\nMale Age Categories")+ 
          
        ylab("\nPopulation (In Thousands)")+theme_bw()+geom_text(aes(label = dataset3$Male2, y = ifelse(dataset3$Male2>300,dataset3$Male2/2,(dataset3$Male2+100))), size = 9)+
          theme(text = element_text(size=28),legend.text=element_text(size=25))+theme(panel.background = element_rect(fill = '#E8E8E8', colour = 'red'))
          
        
      })
      #Rendering bar chart for the female age categories in UK Census Data
      output$barplotfemale3 <- renderPlot({
        ggplot(dataset3,aes(y=dataset3$Female2,x=dataset3$Age,fill = Age))+ geom_bar(stat = "identity") +
          xlab("\nFemale Age Categories")+ ylab("\nPopulation (In Thousands)")+theme_bw()+
          geom_text(aes(label = dataset3$Female2, y = ifelse(dataset3$Female2>300,dataset3$Female2/2,(dataset3$Female2+100))), size = 9)+
          scale_fill_brewer(palette="Reds")+theme(text = element_text(size=28),legend.text=element_text(size=25))+theme(panel.background = element_rect(fill = '#E8E8E8', colour = 'red'))
        
      })
      #Rendering pie chart for the total of male and female population in UK Census Data
      output$pieMenWomen <- renderPlot({
       #ggplot(tableForD3, aes(x=factor(1),y=Total,fill=factor(Gender)))+
        #  geom_bar(width = 1, stat = "identity")+coord_polar(theta="y",start = 0)+xlab("")+ylab("")+theme_bw()+theme(text = element_text(size=20))+
         # scale_fill_manual(values=c("#ff3d53", "#02A8B5"))+ guides(fill=guide_legend(title="Gender"))
        tableForD3$Total<-as.numeric(levels(tableForD3$Total))[tableForD3$Total]
        # factor levels need to be the opposite order of the cumulative sum of the values
        df<- mutate(tableForD3,Gender = factor(Gender),
                    cumulative = cumsum(Total),
                    midpoint = cumulative - Total / 2,
                    label = paste0(round(Total / sum(Total) * 100, 0), "%"))
        
        ggplot(df, aes(x = 1, weight = Total, fill = factor(Gender,levels = c("Male","Female")))) +
          geom_bar(width = 1, position = position_stack(reverse = TRUE)) +
          coord_polar(theta = "y") +theme(text = element_text(size=25))+
          geom_text(aes(x = c(1, 1), y = midpoint, label = label),size=12)+ labs(x="", y="")+scale_fill_manual(values=c("#3B5998", "#bf0000"))+
           guides(fill=guide_legend(title="Gender"))+theme(axis.text = element_blank(),axis.ticks = element_blank(), panel.grid  = element_blank(),legend.text=element_text(size=25))
        
      })
      
      #Rendering leaflet map for the UK CHOLERA outbreak
      output$leaf <- renderLeaflet({
        Icons <- iconList(
          pump = makeIcon("pump.png", "pumpp.png", 32,32)
        )
        map <- leaflet()
        map <- addTiles(map)
        map <- setView(map, lng = mean(datasetDeath$V2), lat = mean(datasetDeath$V3)-0.001+0.0006, zoom = 18)
      map<-addProviderTiles(map,providers$CartoDB.Positron)
        for(i in 1:250)
     
        map<-addCircleMarkers(map, lng = datasetDeath$V2[i], lat = datasetDeath$V3[i],color = "red",fillOpacity = 0.5,stroke = FALSE,radius=datasetDeath$V1[i]+8,label = htmlEscape(paste("No of Deaths: ", datasetDeath$V1[i])),
                              
                              labelOptions = labelOptions(noHide = FALSE,riseOnHover=TRUE,riseOffset=10,direction = "auto",showCoverageOnHover=TRUE, style = list(
                                "color" = "Black",
                                "font-family" = "helvetica",
                                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                "font-size" = "20px",
                                "border-color" = "rgba(0,0,0,0.5)")))
        map <- addCircleMarkers(map, lng = datasetPump$V1, lat = datasetPump$V2,radius = 20,label="Pump",labelOptions = labelOptions(noHide = FALSE,riseOnHover=TRUE,riseOffset=10,direction = "auto",showCoverageOnHover=TRUE, style = list(
          "color" = "Black",
          "font-family" = "helvetica",
          "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
          "font-size" = "20px",
          "border-color" = "rgba(0,0,0,0.5)")))#, icon = Icons[datasetPump$Icon]
 

        map
      })
      
      #Rendering original map(John Snow's map) for the UK CHOLERA outbreak
      output$myImage <- renderPlot({
        
        bottom_left = c(0, -0.143755, 51.509440)
        top_right = c(0, -0.131110, 51.516522)
        cordtemp = rbind(bottom_left, top_right, subset(datasetDeath,select=c(V1,V2,V3)))
        cord = rbind(bottom_left, top_right, subset(datasetDeath,select=c(V1,V2,V3)))
        #cord = rbind(subset(datasetDeath,select=c(V1,V2,V3)))
        cord.dec = SpatialPoints(cbind(cord$V2, cord$V3), proj4string=CRS("+proj=longlat"))
        cord.UTM = spTransform(cord.dec, CRS("+init=epsg:27700"))
 cord<-as.data.frame(cord)
cord$V1<- as.factor(cord$V1)
 
          g <- ggplot(df1, aes(x,y)) + geom_blank() + labs(x="", y = "") +
            annotation_custom(rasterGrob(jp, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
            theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
        
          
          g<- g+ geom_point(data=cord,aes(x=V2-0.00025,y=V3-0.000045,colour=cord$V1),size=9)+guides(fill=guide_legend(title="Number of Deaths"))+
             #scale_colour_brewer(palette = "RdGy")
            scale_colour_manual("Number of Deaths",values=c("#FFFFFF", "#FA8072","#CD5C5C","#C21807","#B22222","#960018","#8d021f","#7E191B","#5e1914"))#+xlim(-0.143755,-0.131110)+ylim(51.509440,51.516522)
          #"Number of Deaths",breaks=c("0","1","2","3","4","5","7","8","15"), breaks=c("0","1","2","3","4","5","7","8","15"), g<- g+ geom_point(aes(x=-0.143755,y=51.509440),color="red",size=5)#+xlim(-0.143755,0.131110)+ylim(51.509440,51.516522)
          #g<- g+ geom_point(data=datasetDeath,aes(x=datasetDeath$x,y=datasetDeath$y),color="red",size=5)
          bottom_left = c(-0.143755, 51.509440)
          top_right = c(-0.131110, 51.516522)
          g<-g+ geom_point(aes(x=(bottom_left$V1),y=(bottom_left$V2),color="blue",size=9)+theme(text = element_text(size=25)))
          g<-g+ geom_point(aes(x=(top_right$V1),y=(top_right$V2),color="blue",size=9)+theme(text = element_text(size=25)))
          cord1 = rbind(subset(datasetPump,select=c(V1,V2)))
          #cord1 = rbind(subset(datasetPump,select=c(V1,V2)))
          cord1.dec = SpatialPoints(cbind(cord1$V1, cord1$V2), proj4string=CRS("+proj=longlat"))
          cord1.UTM = spTransform(cord1.dec, CRS("+init=epsg:27700"))
          cord1<-as.data.frame(cord1)
          g<-g+ geom_point(data=cord1,aes(x=(V1-0.00025),y=V2-0.00004),color="blue",size=10)+theme(text = element_text(size=25),legend.text=element_text(size=25) )
          
          
          
          
     
        #for (i in 1:250)
       #g <- g + annotate("text",x =datasetDeath$x[i]+0.2,y=datasetDeath$y[i]+0.2, label = ".",color="red",size=40)
    
        #for (i in 1:8)
         # g <- g + annotate("text",x =datasetPump$x[i]+0.3,y=datasetPump$y[i]+0.9, label = ".",color="blue",size=45)
     g
   
      })
      
      #Rendering hover to display the Number of deaths and latitude and longitudes of the deaths in the original map
      output$hover_info <- renderPrint({
        if(!is.null(input$plot_hover)){
          hover=input$plot_hover
          dist=sqrt((hover$x-(cord$V2-0.00025))^2+(hover$y-(cord$V3-0.000045))^2)
         cat("No of Deaths:\n")
          if(min(dist) < 1.25){
            cord$V1[which.min(dist)]
            
            }
        }})
      output$hover_info1 <- renderPrint({
        if(!is.null(input$plot_hover)){
          hover=input$plot_hover
          dist=sqrt((hover$x-(cord$V2-0.00025))^2+(hover$y-(cord$V3-0.000045))^2)
          cat("Longitude:\n")
          if(min(dist) < 1.25){
            cord$V2[which.min(dist)]
            
          }
        }})
      output$hover_info2 <- renderPrint({
        if(!is.null(input$plot_hover)){
          hover=input$plot_hover
          dist=sqrt((hover$x-(cord$V2-0.00025))^2+(hover$y-(cord$V3-0.000045))^2)
          cat("Latitude:\n")
          if(min(dist) < 1.25){
            cord$V3[which.min(dist)]
            
          }
        }})
      #Rendering the text output to display the information about the project
      
      output$caption <- renderText({
        
        HTML(paste0("<h2>","Information about the Project","<h3><br>","This project has been made by Namaswi Chandarana, a student at the University of Illinois At Chicago for a course, Visualization and Visual analytics(CS 424) under professor Andy Johnson.","<br>","The project has been made using R with Shiny. The libraries used for the project are: DT, jpeg, grid, leaflet,lubridate, htmltools, plyr, RColorBrewer, rgdal and sp.<br><br>","The data was given by the professor. The data to work on was provided by my Professor Andy Johnson from UIC. There were in total 5 datasets and 1 image provided to be visualized. One dataset was related to the UK census data of 1851(Male and Female) which included attributes namely: age, male,female.
            There was one dataset on the Naples Cholera Outbreak having attributes: age, male,	female. There were 3 datasets on the UK Cholera Outbreak, out of which 2 datasets listed the location of pumps and deaths as well as the number of deaths in a particular location. The other dataset had the attributes: Date , Attacks, Deaths.
            The image of the John Snow's map was provided to visualize the deaths and pumps in the original map.<br><br>
            The project has been scaled so that it clearly represent the visualization on a screen size of 4080 by 2304.<br><br>
            Before the datasets could be visualized, they were preprocessed and analyzed to make the visulaization more insightful. Some preprocessing operations were were calculating the cummalative of deaths and attacks from the UK Cholera outbreak dataset and transforming the Latitude and Longitude values provided for maping deaths and attacks locations in the original map.
                    <br> <br>The source code along with data files for the project can be found <a href=","http://nchand22.people.uic.edu/p1.html",">here</a>"))
                    
        
        
      })
      output$caption1 <- renderText({
        
        HTML(paste0("converted by robin@rtwilson.com, www.rtwilson.com/academic"))
        
        
        
      })
      
        
})

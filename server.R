#The setting directary varies with different file locations
options(useFancyQuotes = FALSE) # this is to solve gibberish issue

library(maptools)
library(rgdal)
library(RColorBrewer)
library(leaflet)
library(geosphere)
library(classInt)
library(plyr)
library(dplyr)
library(DT)
library(raster)
#--------------------#
#library(lubridate)
#library(plotly)
#library(readxl)
#library(MCI)
#library(magrittr)
#--------------------#

huffNewDistances<-NULL
error <- FALSE
clusteringData <- NULL
mergclusterPASubzone <- NULL
paPALib <- NULL
newLibData <- NULL   #add/remove new location
huffMapLat <- NULL
huffMapLng <- NULL
huffMapName <- NULL
huffMapCode <- NULL
huffMapSize <- NULL
huffMapType <- NULL
huffMapArea <- NULL
totalLng <- NULL
totalLat <- NULL
totalCollectionSize <- NULL
totalName <- NULL
totalCode <- NULL
totalBranchType <- NULL
totalFloorArea <- NULL
totalMRTno <- NULL
totalMallno <- NULL
totalTCno <- NULL
huffMapLatChange <- NULL
huffMapLngChange <- NULL
libLocList <- NULL  #list of lib changed location. change existing lib location
ijmatrix_Full_Txn_Data <- NULL
libMatrix <- NULL
subzone_map <- NULL
huffMap <- NULL
actualMap <- NULL
diffMap <- NULL


shinyServer(function(input, output, session) {
  
  # subzoneCentroids <- read.csv("C:/nlb/data/subzone_centroid.csv")
  # subzonePA <- read.csv("C:/nlb/data/Subzone and PA.csv")
  # lib <- read.csv("C:/nlb/data/Library.csv")
  # mrt <- read.csv("C:/nlb/data/MRT Stations Coordinates.csv")
  # mall <- read.csv("C:/nlb/data/mall wgs.csv")
  # tuition <- read.csv("C:/nlb/data/TuitionWGS84.csv")
  # patronLib <- read.csv("C:/nlb/data/Patron to Lib.csv")
  # libData <- read.csv("C:/nlb/data/libData.csv")
  # 
  subzoneCentroids <- read.csv("data/subzone_centroid.csv")
  subzonePA <- read.csv("data/Subzone and PA.csv")
  lib <- read.csv("data/Library.csv")
  mrt <- read.csv("data/MRT Stations Coordinates.csv")
  mall <- read.csv("data/mall wgs.csv")
  tuition <- read.csv("data/TuitionWGS84.csv")
  patronLib <- read.csv("data/Patron to Lib.csv")
  libData <- read.csv("data/libData.csv")
  
  cluster <- read.csv("data/ClusteringData.csv")
  # cluster <- read.csv("C:/nlb/data/ClusteringData.csv")
  colVariables <- c("Total TXN" = 4, "Books Borrowed" = 5, "Avg Books Borrowed" = 6,"Day from Last TXN" = 7)
  mergclusterPASubzone <- merge(cluster, subzonePA, by.x="Locale.Planning.ADZID", by.y="Locale.Planning.ADZID",all.x=TRUE)
  
  #read in a kml file for choropleth
  data.list<-list(mrt, mall, tuition)
  name.list<-list("MRT", "Mall", "Tuition Centre")
  
  lib$X[1]
  
  libName <- paste(lib$NAME)
  
  libLng <- paste(lib$X)
  libLat <- paste(lib$Y)
  
  #lib loc
  libLocList <<- lib
  libLocLibList <- lib$NAME
  
  
  #----------------------------------------------------------------------------------------------------------------------------#
  
  # {PART 0: Read and clean data for MCI Model}
  
  # Load distance data - between each subzone and each library
  
  library(MCI)
  library(rgdal)
  
  Subzone_Distance <- read.csv("data/Subzone_Distance.csv")
  subzone_map <- readOGR(dsn="data/Subzone.shp", layer="Subzone", encoding="UTF-8")
  # Subzone_Distance <- read.csv("C:/nlb/data/Subzone_Distance.csv")
  # subzone_map <- readOGR(dsn="C:/nlb/data/Subzone.shp", layer="Subzone", encoding="UTF-8")
  
  # Load library information
  Lib_Info <- read.csv("data/Lib_Info_13.csv")
  # Lib_Info <- read.csv("C:/nlb/data/Lib_Info_13.csv")
  # transform banch type to dummy variable
  Lib_Info <- transform(Lib_Info, type_if_mall = ifelse(Branch.Type == "Mall",1,0))
  Lib_Info <- transform(Lib_Info, type_if_standalone = ifelse(Branch.Type == "Stand-Alone",1,0))
  
  # Load patron data
  Patron_Dataset_FY13_Subzone <- read.csv("data/Patron_Dataset_FY13_Subzone.csv")
  # Patron_Dataset_FY13_Subzone <- read.csv("C:/nlb/data/Patron_Dataset_FY13_Subzone.csv")
  
  # remove "Bad Value" & "Missing Value" under Locale Planning ADZID
  # remove invalid subzone "CKSZ07" and invalid patrons with birth year = 1900
  Patron_Dataset_FY13_Subzone <- Patron_Dataset_FY13_Subzone[(Patron_Dataset_FY13_Subzone$Locale.Planning.ADZID != "Bad Value")&
                                                               (Patron_Dataset_FY13_Subzone$Locale.Planning.ADZID != "Missing Value")&
                                                               (Patron_Dataset_FY13_Subzone$Subzone != "CKSZ07")&
                                                               (Patron_Dataset_FY13_Subzone$Patron.Birthyear != 1900) ,]
  
  # read population by subzone data
  Subzone_Pop_simple <- read.csv("data/Subzone_Pop_2013.csv")[ ,c('Subzone', 'Population')]
  # Subzone_Pop_simple <- read.csv("C:/nlb/data/Subzone_Pop_2013.csv")[ ,c('Subzone', 'Population')]
  # To exclude the "total" row in population data, to make sure the row number matches 323
  Subzone_Pop_simple <- Subzone_Pop_simple[-324, ] 
  
  # Delete patrons from those subzones where population is 0 - invalid patrons
  Patron_Dataset_FY13_Subzone <- merge(Patron_Dataset_FY13_Subzone, Subzone_Pop_simple, 
                                       by.x="Subzone", by.y="Subzone", stringsAsFactors = FALSE)
  Patron_Dataset_FY13_Subzone <- Patron_Dataset_FY13_Subzone[(Patron_Dataset_FY13_Subzone$Population != 0),]
  
  
  # Import transaction data
  TXN_FY13 <- read.csv("data/TXN_FY13_cleaned.csv")
  # TXN_FY13 <- read.csv("C:/nlb/data/TXN_FY13_cleaned.csv")
  
  # Merge transaction data and patron data to get the "Subzone" of each transaction
  # Use inner join to make sure invalid data from transaction records are not passed
  TXN_FY13_wPatron <- merge(TXN_FY13, Patron_Dataset_FY13_Subzone, by.x="Patron.UID", by.y="Patron.UID", stringsAsFactors = FALSE)
  
  TXN_FY13_wPatron$interaction <- paste(TXN_FY13_wPatron$Subzone, TXN_FY13_wPatron$Branch.Code, sep="-")
  
  TXN_FY13_wPatron_dist <- merge(Subzone_Distance, TXN_FY13_wPatron, by.x="route", by.y="interaction", 
                                 all.x = TRUE, stringsAsFactors = FALSE)
  
  # Convert all columns from factor to character
  # TXN_FY13_wPatron_dist[] <- lapply(TXN_FY13_wPatron_dist, as.character)
  
  # Replace all NA with string "NA" to generate 8075 rows
  # TXN_FY13_wPatron_dist[is.na(TXN_FY13_wPatron_dist)] <- "NA"
  
  # Create matrix of 8075 rows
  ijmatrix_TXN_FY13 <- ijmatrix.create(TXN_FY13_wPatron_dist, "from_Subzone","to_Branch.Code",
                                       correctVar = TRUE, correctVar.val = 1)

  ijmatrix_Full_Txn_Data <- merge(ijmatrix_TXN_FY13, Lib_Info, by.x="to_Branch.Code", by.y="Branch.Code", all.x = TRUE)
  ijmatrix_Full_Txn_Data <- merge(ijmatrix_Full_Txn_Data, Subzone_Distance, by.x="interaction", by.y="route", all.x = TRUE)

  # Rename columns for convenience
  names(ijmatrix_Full_Txn_Data)[2] <- "Branch.Code"
  names(ijmatrix_Full_Txn_Data)[3] <- "Subzone"

  ijmatrix_Full_Txn_Data$to_Branch.Code.y <- NULL
  ijmatrix_Full_Txn_Data$from_Subzone.y <- NULL

  # create data frame to put calibrated weightages

  # mci_fit <- data.frame(0,0,0,0,0)
  # names(mci_fit) <- c("Distance_km", "Branch.Gross.Floor.Area", "Collection.Size", "No..of.Tuition.Centres.Within.1KM")
  # mci_share <- NULL

  #----------------------------------------------------------------------------------------------------------------------------# 
  
  pal <- colorFactor(c("navy", "red"), domain = c("mrt", "mall"))
  
  nlbIcon <- makeIcon("img/icon.png", 25, 34)
  mrtIcon <- makeIcon("img/mall2.png", 23, 23)
  mallIcon <- makeIcon("img/mall2.png", 23, 23)
  
  # nlbIcon <- makeIcon("C:/nlb/img/icon.png", 25, 34)
  # mrtIcon <- makeIcon("C:/nlb/img/mall2.png", 23, 23)
  # mallIcon <- makeIcon("C:/nlb/img/mall2.png", 23, 23)
  
  
  output$selectedLib <- renderText({ 
    paste("You have selected", input$lib)
  })
  
  
  DF <- data.frame(lat = c(libLat), lon = c(libLng), outcome = c(libName))
  
  #---------------------------------------------------------------------------------------------------------------------------------# 
  
  # Tab 1.1 -{Catchment Area Analysis - Tab 1: Surrounding Facilities}
  
  pointsInMultipleLayer<-function(circleX,circleY, amenitiesList, nameList,bufferDistance){
    toReturn<-""
    for (i in 1:NROW(amenitiesList)){
      
      amenitiesLayer<-as.data.frame(data.list[i])
      count<-0
      pointOrigin<-c(circleX, circleY)
      
      for(j in 1:nrow(amenitiesLayer)){
        pointTarget<-c((amenitiesLayer[j,"X"]), (amenitiesLayer[j,"Y"]))
        distance<-distm (pointOrigin, pointTarget, fun = distHaversine) 
        if(distance<=bufferDistance){
          count<-count+1
        }
      }
      temp<-paste(nameList[i],":",count, sep = " ")
      toReturn<-paste(toReturn, "</br>" ,temp, sep=" ")
    }
    return(toReturn)
  }
  
  
  facilitiesCalculator <- function(circleX, circleY, facilitiesList, nameList, bufferDistance){
    
    toReturn <- c()
    
    for (i in 1:NROW(facilitiesList)){
      facilitiesLayer <- as.data.frame(data.list[i])
      count <- 0
      pointOrigin <- c(circleX, circleY)
      
      for(j in 1:nrow(facilitiesLayer)){
        pointTarget <- c((facilitiesLayer[j,"X"]), (facilitiesLayer[j,"Y"]))
        distance <- distm(pointOrigin, pointTarget, fun = distHaversine) 
        if(distance <= bufferDistance){
          count<-count+1
        }
      }
      toReturn <- append(toReturn, count ,length(toReturn)+1)
    }
    return(toReturn)
  }
  
  
  # Points in buffer method
  pointsInBuffer<-function(circleX,circleY, amenitiesLayer, bufferDistance){
    count<-0
    pointOrigin<-c(circleX, circleY)
    for(i in 1:nrow(amenitiesLayer)){
      pointTarget<-c((amenitiesLayer[i,"X"]), (amenitiesLayer[i,"Y"]))
      distance<-distm (pointOrigin, pointTarget, fun = distHaversine) 
      if(distance<=bufferDistance){
        count<-count+1
      }
    }
    return(paste("Malls:",count, sep = " "))
  }
  
  # Observe change in buffer slide
  observeEvent(input$buffer, {
    barVector <- NULL
    leafletProxy("generalMap")%>% removeShape(c("Buffer"))%>%
      clearGroup(c("Buffer"))
    for(i in 1:nrow(lib)){
      if((lib[i,"NAME"]==input$lib)|(input$lib=="All")){
        if(is.null(barVector)){
          barVector<-facilitiesCalculator(lib[i,"X"], lib[i,"Y"], data.list, name.list, input$buffer * 1000)
        }else{
          tempVector<-facilitiesCalculator(lib[i,"X"], lib[i,"Y"], data.list, name.list, input$buffer * 1000)
          barVector<-tempVector+barVector
        }
        leafletProxy("generalMap")%>%
          addCircles(lng=lib[i,"X"],lat=lib[i,"Y"],popup = toString(pointsInMultipleLayer(lib[i,"X"], lib[i,"Y"], data.list, name.list, input$buffer * 1000)), weight = 1,radius = input$buffer * 1000, group = "Buffer")
      }
    }
    if(input$lib=="All"){
      amenTable <- matrix(barVector,ncol=3,byrow=FALSE)
      colnames(amenTable) <- c("MRT","Mall","Tuition Centre")
      amenDf <- data.frame(amenTable)
      output$amenitiesTable <- renderTable(amenDf,striped = TRUE, bordered = TRUE, width = "100", digits = 0)
      
    }else{
      amenTable <- matrix(barVector,ncol=3,byrow=FALSE)
      colnames(amenTable) <- c("MRT","Mall","Tuition Centre")
      amenDf <- data.frame(amenTable)

      output$amenitiesTable <- renderTable(amenDf,striped = TRUE, bordered = TRUE, width = "100", digits = 0)
      
      if(!is.null(mergclusterPASubzone$ClusterNo)){

        output$plotCluster <- renderPlot({
          cluster <- matrix(barVector,ncol=max(mergclusterPASubzone$ClusterNo),byrow=FALSE)
          colnames(cluster) <- unique(mergclusterPASubzone$ClusterNo)
          cluster <- as.table(cluster)
          bar<-barplot(cluster, ylab="Number of Patrons", xlab="Cluster", main="Cluster Profile", ylim=c(0,1.25*max(cluster)))
          text(x = bar, y = cluster, label = cluster, pos = 3, cex = 0.8, col = "red")
        })
      }
    }
  })
  
  
  observeEvent(input$lib, {
      #default = "Ang Mo Kio Public Library"
      default = input$lib
      barVector<-NULL
      
      leafletProxy('generalMap') %>% 
        removeMarker(layerId = "selected")%>%
        clearGroup(c("selected"))
      for(i in 1:nrow(lib)){
        if((lib[i,"NAME"]==default)|(default=="All")){
          leafletProxy("generalMap")%>%
            addMarkers(icon = nlbIcon, lng = as.numeric(subset(DF, outcome == default)$lon), lat = as.numeric(subset(DF, outcome == default)$lat), popup = subset(DF, outcome == default)$outcome, layerId = "selected") 
        }
      }
      leafletProxy("generalMap")%>% 
        removeShape(c("Buffer"))%>%
        clearGroup(c("Buffer"))
      for(i in 1:nrow(lib)){
        if((lib[i,"NAME"]==default)|(default=="All")){
          if(is.null(barVector)){
            barVector<-facilitiesCalculator(lib[i,"X"], lib[i,"Y"], data.list, name.list, input$buffer * 1000)
          }else{
            tempVector<-facilitiesCalculator(lib[i,"X"], lib[i,"Y"], data.list, name.list, input$buffer * 1000)
            barVector<-tempVector+barVector
          }
          leafletProxy("generalMap")%>%
            addCircles(lng=lib[i,"X"],lat=lib[i,"Y"],popup = toString(pointsInMultipleLayer(lib[i,"X"], lib[i,"Y"], data.list, name.list, input$buffer * 1000)), weight = 1,radius = input$buffer * 1000, layerId = "Buffer")
        }
      }
      
      amenTable <- matrix(barVector,ncol=3,byrow=FALSE)
      colnames(amenTable) <- c("MRT","Mall","Tuition Centre")
      amenDf <- data.frame(amenTable)

      output$amenitiesTable <- renderTable(amenDf,striped = TRUE, bordered = TRUE, width = "100", digits = 0)
      
      if(!is.null(mergclusterPASubzone$ClusterNo)){

        output$plotCluster <- renderPlot({
          cluster <- matrix(barVector,ncol=max(mergclusterPASubzone$ClusterNo),byrow=FALSE)
          colnames(cluster) <- unique(mergclusterPASubzone$ClusterNo)
          cluster <- as.table(cluster)
          bar<-barplot(cluster, ylab="Number of Patrons", xlab="Cluster", main="Cluster Profile", ylim=c(0,1.25*max(cluster)))
          text(x = bar, y = cluster, label = cluster, pos = 3, cex = 0.8, col = "red")
        })
      }
      
  })
  
  # Plot General lMap
  output$generalMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
      addCircleMarkers(data = mall, lng = ~X, lat = ~Y, popup = ~Mall, group = "Mall", radius = 5, color = 'black') %>%
      addMarkers(data = tuition, lng = ~X, lat = ~Y, popup = ~Name, group = "Tuition Centre", clusterOptions = markerClusterOptions()) %>%
      addCircleMarkers(data = mrt, lng = ~X, lat = ~Y, popup = ~MRT.Stations, group = "MRT Station", radius = 5, color = 'red') %>%
      addCircles(lng=lib[1,"X"],lat=lib[1,"Y"],popup = toString(pointsInMultipleLayer(lib[1,"X"], lib[1,"Y"], data.list, name.list, 1000)), weight = 1,radius = 1000, layerId = "Buffer") %>%
      
      addLayersControl(
        #overlayGroups = c("Mall", "Tuition Centre", "MRT Station", "Patron Flow by Subzone"),
        overlayGroups = c("Mall", "Tuition Centre", "MRT Station"),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      )
  })
  #---------------------------------------------------------------------------------------------------------------------------------# 
  
  # Tab 1.2 -{Catchment Area Analysis - Tab 2: Overview of All Libraries}
  
  #1 Get the analysis type for OVERVIEW TAB
  anaSelected1 = "General Statistics Across Libraries"
  # Pass the selected analysis type
  observeEvent(input$overAnaType, {
    anaSelected1 <<- input$overAnaType

  })
  
  # 2. Plot the Graph
  observeEvent(input$btnOverPlot,{
    
    if(anaSelected1 == "Collection Size vs No. of Books Borrowed"){

      output$ovPlot <- renderPlot({
        
        plot(libData$`Collection Size`,libData$`Number of Books Borrowed`, main="Collection Size vs Number of books Borrowed", xlab="Collection Size",ylab="No.Books Borrowed", pch=17,
             col=ifelse(libData$`Library Type`=="Mall", "red", ifelse(libData$`Library Type`=="Stand-Alone", "blue", "green"))
        )
        
        abline(h=mean(libData$`Number of Books Borrowed`),col="purple",lty=2)
        abline(v=mean(libData$`Collection Size`),col="purple",lty=2)
        legend(16268590	, 150274, pch=c(17,17,17), col=c("red", "blue","green"), c("Mall", "Stand-Alone", "Regional"), bty="o",  box.col="black", cex=.8)
      })
      
      output$plotui <- renderUI({
        plotOutput("ovPlot", height=600,
                   brush = brushOpts(id = "plot_brush")
        )
      })
      
      # for part 2 and 3
      output$plot_brushed_points <- renderDataTable({
        df <- libData
        # this function gets the data for you
        res <- brushedPoints(df, input$plot_brush, "Collection Size","Number of Books Borrowed")
        #  x variable, y variable
        
        # puts the results in a datatable format
        datatable(res)
      })
    }
    
    if(anaSelected1 == "Floor Size vs No. of Books Borrowed"){
      output$ovPlot <- renderPlot({
        
        plot(libData$`Floor Size` ,libData$`Number of Books Borrowed`, main="Floor Size vs Number of books Borrowed", xlab="Floor Size",ylab="No.Books Borrowed", pch=15,
             col=ifelse(libData$`Library Type`=="Mall", "red", ifelse(libData$`Library Type`=="Stand-Alone", "blue", "green"))
        )
        #abline(lm(libData$`Number of Books Borrowed`~libData$`Collection Size`), col="red") # regression line (y~x)
        abline(h=mean(libData$`Number of Books Borrowed`),col="purple",lty=2)
        abline(v=mean(libData$`Floor Size`),col="purple",lty=2)
        legend(10990	, 150274, pch=c(15,15,15), col=c("red", "blue","green"), c("Mall", "Stand-Alone", "Regional"), bty="o",  box.col="black", cex=.8)
      })
      
      output$plotui <- renderUI({
        plotOutput("ovPlot", height=600,
                   brush = brushOpts(id = "plot_brush")
        )
      })
      # for part 2 and 3
      output$plot_brushed_points <- renderDataTable({
        df <- libData
        # this function gets the data for you
        res <- brushedPoints(df, input$plot_brush, "Floor Size","Number of Books Borrowed")
        #  x variable, y variable
        
        # puts the results in a datatable format
        datatable(res)
      })
      
    }
    
    
    if(anaSelected1 == "Identification of Seasonal Months"){
      
      output$plotui <- renderUI({
        plotlyOutput("ovPlot1", height = 600)
        
      })
      
      table <- table((TXN_FY13_wPatron$month), (TXN_FY13_wPatron$toCount))
      df <- as.data.frame.matrix(table)
      timeUnit_ordered <- factor(rownames(df), levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                        "Aug", "Sep", "Oct", "Nov", "Dec"))
      
      output$ovPlot1 <- renderPlotly({
        plot_ly(df, x = timeUnit_ordered, y = ~toCount, name = 'Total Transactions',
                type = 'scatter',mode = 'lines+markers') %>%
          layout(yaxis = list(title = 'No. of Books Borrowed'), title = "No. of Books Borrowed over Months")
        #type = 'bar', barmode = 'stack'
        
      })
      
    }
    
  })
  
  #---------------------------------------------------------------------------------------------------------------------------------# 
  
  # Tab 1.3 - {Catchment Area Analysis - Tab 3: Individual Library Analysis}
  
  # 0. read and merge data for visualization
  
  # TXN_FY13 <- read.csv("C:/Users/WANG Tiantong/Desktop/SMU/Y4T2/AP/SMU-NLB/Group3/nlbOld/nlb/data/TXN_FY13_cleaned.csv")
  # Patron_Dataset_FY13 <- read.csv("C:/Users/WANG Tiantong/Desktop/SMU/Y4T2/AP/SMU-NLB/raw_data/Patron_Dataset_FY13.csv")
  # TXN_FY13_ready <- TXN_FY13[(TXN_FY13$Patron.UID != 0),]
  # TXN_FY13_wPatron <- merge(TXN_FY13_ready, Patron_Dataset_FY13, by.x="Patron.UID", by.y="Patron.UID",
  #                           all.x = TRUE)
  
  # Format the Transaction Date Time as the required value (by hour/week/month)
  # TXN_FY13_wPatron$datetime <- strptime(x = as.character(TXN_FY13_wPatron$Txn.Date.Time),
  #                                 format = "%m/%d/%Y %H:%M")
  TXN_FY13_wPatron$datetime <- as.POSIXct.POSIXlt(strptime(x = as.character(TXN_FY13_wPatron$Txn.Date.Time),
                                                           format = "%m/%d/%Y %H:%M"))
  TXN_FY13_wPatron$hour <- as.numeric(format(TXN_FY13_wPatron$datetime, "%H"))
  TXN_FY13_wPatron$month <- month(TXN_FY13_wPatron$datetime, label=TRUE)
  TXN_FY13_wPatron$weekName <- wday(TXN_FY13_wPatron$datetime, label=TRUE)
  TXN_FY13_wPatron$year <- as.numeric(format(TXN_FY13_wPatron$datetime, "%Y"))
  TXN_FY13_wPatron$Patron.Age <- (TXN_FY13_wPatron$year)-(TXN_FY13_wPatron$Patron.Birthyear)
  TXN_FY13_wPatron$ageBins <- cut(TXN_FY13_wPatron$Patron.Age, breaks=c(0,9,19,29,39,49,59,69,200), 
                                  labels=c("b0t9","b10t19","b20t29","b30t39","b40t49","b50t59","b60t69", "over69"))
  TXN_FY13_wPatron$toCount <- "toCount"
  
  
  
  # 1. Get Selected Library
  
  #Set Default Lib
  TXN_FY13_wPatron_lib <- TXN_FY13_wPatron[(TXN_FY13_wPatron$Branch.Code == "AMKPL"),]
  
  # Pass the selected individual lib
  observeEvent(input$invLib, {

    if(input$invLib != "All"){
      for(i in 1:nrow(lib)){
        if(lib[i,"NAME"]==input$invLib){
          invLibCode <- lib[i,"DESCRIPTION"]
        }
      }
      TXN_FY13_wPatron_lib <<- TXN_FY13_wPatron[(TXN_FY13_wPatron$Branch.Code == invLibCode),]
    }else{
      TXN_FY13_wPatron_lib <<- TXN_FY13_wPatron
    }

  })
  
  
  # 2. Get Selected Analysis Type
  
  # Set default value
  anaSelected = "Race Segmentation"
  # Pass the selected analysis type
  observeEvent(input$invAnaType, {
    anaSelected <<- input$invAnaType

  })
  
  
  # 3. Get Selected Time Unit
  
  # Set default value
  timeUnitSelected = "by Hour (0-24)"
  # Pass the selected time unit
  observeEvent(input$timeUnit, {
    timeUnitSelected <<- input$timeUnit
  })
  
  
  # 4. Plot the Graph
  observeEvent(input$btnInvPlot,{
    
    if(timeUnitSelected == "by Hour (0-24)"){
      
      if(anaSelected == "Race Segmentation"){
        TXN_FY13_wPatron_lib <- TXN_FY13_wPatron_lib[!is.na(TXN_FY13_wPatron_lib$Patron.Race),]
        table <- table((TXN_FY13_wPatron_lib$hour), (TXN_FY13_wPatron_lib$Patron.Race))
        df <- as.data.frame.matrix(table)
        timeUnit_ordered <- factor(rownames(df), levels=seq(from = 0, to = 24, by = 1))
        
        output$invPlot <- renderPlotly({
          plot_ly(df, x = timeUnit_ordered, y = ~Chinese, name = 'Chiense',
                  type = 'scatter',mode = 'lines+markers') %>%
            add_trace(y = ~Indian, name = 'Indian') %>%
            add_trace(y = ~Malay, name = 'Malay') %>%
            add_trace(y = ~Others, name = 'Others') %>%
            layout(yaxis = list(title = 'No. of Transactions'), title = "Race Segmentation by Hour(0-24)")
          #type = 'bar', barmode = 'stack'
          
        })
      }
      
      if(anaSelected == "Age Distribution"){
        TXN_FY13_wPatron_lib <- TXN_FY13_wPatron_lib[!is.na(TXN_FY13_wPatron_lib$ageBins),]
        table <- table((TXN_FY13_wPatron_lib$hour), (TXN_FY13_wPatron_lib$ageBins))
        df <- as.data.frame.matrix(table)
        timeUnit_ordered <- factor(rownames(df), levels=seq(from = 0, to = 24, by = 1))
        
        output$invPlot <- renderPlotly({
          plot_ly(df, x = timeUnit_ordered, y = ~b0t9, name = '0-9',
                  type = 'scatter',mode = 'lines+markers') %>%
            add_trace(y = ~b10t19, name = '10-19') %>%
            add_trace(y = ~b20t29, name = '20-29') %>%
            add_trace(y = ~b30t39, name = '30-39') %>%
            add_trace(y = ~b40t49, name = '40-49') %>%
            add_trace(y = ~b50t59, name = '50-59') %>%
            add_trace(y = ~b60t69, name = '60-69') %>%
            add_trace(y = ~over69, name = 'over 69') %>%
            layout(yaxis = list(title = 'No. of Transactions'), title = "Age Distribution by Hour(0-24)")
          #type = 'bar', barmode = 'stack'
          
        })
        
      }
      
      if(anaSelected == "Gender Distribution"){
        TXN_FY13_wPatron_lib <- TXN_FY13_wPatron_lib[!is.na(TXN_FY13_wPatron_lib$Patron.Gender),]
        table <- table((TXN_FY13_wPatron_lib$hour), (TXN_FY13_wPatron_lib$Patron.Gender))
        df <- as.data.frame.matrix(table)
        timeUnit_ordered <- factor(rownames(df), levels=seq(from = 0, to = 24, by = 1))
        
        output$invPlot <- renderPlotly({
          plot_ly(df, x = timeUnit_ordered, y = ~Male, name = 'Male',
                  type = 'scatter',mode = 'lines+markers') %>%
            add_trace(y = ~Female, name = 'Female') %>%
            layout(yaxis = list(title = 'No. of Transactions'), title = "Gender Distribution by Hour(0-24)")
          #type = 'bar', barmode = 'stack'
          
        })
        
      }
      
      if(anaSelected == "Total Transaction Analysis"){
        table <- table((TXN_FY13_wPatron_lib$hour), (TXN_FY13_wPatron_lib$toCount))
        df <- as.data.frame.matrix(table)
        timeUnit_ordered <- factor(rownames(df), levels=seq(from = 0, to = 24, by = 1))
        
        output$invPlot <- renderPlotly({
          plot_ly(df, x = timeUnit_ordered, y = ~toCount, name = 'Total Transactions',
                  type = 'scatter',mode = 'lines+markers') %>%
            layout(yaxis = list(title = 'No. of Total Transactions'), title = "Total Transactions Analysis by Hour(0-24)")
          #type = 'bar', barmode = 'stack'
          
        })
        
      }
    }
    
    if(timeUnitSelected == "by Week Day (Sun-Sat)"){
      
      if(anaSelected == "Race Segmentation"){
        TXN_FY13_wPatron_lib <- TXN_FY13_wPatron_lib[!is.na(TXN_FY13_wPatron_lib$Patron.Race),]
        table <- table((TXN_FY13_wPatron_lib$weekName), (TXN_FY13_wPatron_lib$Patron.Race))
        df <- as.data.frame.matrix(table)
        timeUnit_ordered <- factor(rownames(df), levels=c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
        
        output$invPlot <- renderPlotly({
          plot_ly(df, x = timeUnit_ordered, y = ~Chinese, name = 'Chiense',
                  type = 'scatter',mode = 'lines+markers') %>%
            add_trace(y = ~Indian, name = 'Indian') %>%
            add_trace(y = ~Malay, name = 'Malay') %>%
            add_trace(y = ~Others, name = 'Others') %>%
            layout(yaxis = list(title = 'No. of Transactions'), title = "Race Segmentation by Week Day (San-Sat)")
          #type = 'bar', barmode = 'stack'
          
        })
      }
      
      if(anaSelected == "Age Distribution"){
        TXN_FY13_wPatron_lib <- TXN_FY13_wPatron_lib[!is.na(TXN_FY13_wPatron_lib$ageBins),]
        table <- table((TXN_FY13_wPatron_lib$weekName), (TXN_FY13_wPatron_lib$ageBins))
        df <- as.data.frame.matrix(table)
        timeUnit_ordered <- factor(rownames(df), levels=c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
        
        output$invPlot <- renderPlotly({
          plot_ly(df, x = timeUnit_ordered, y = ~b0t9, name = '0-9',
                  type = 'scatter',mode = 'lines+markers') %>%
            add_trace(y = ~b10t19, name = '10-19') %>%
            add_trace(y = ~b20t29, name = '20-29') %>%
            add_trace(y = ~b30t39, name = '30-39') %>%
            add_trace(y = ~b40t49, name = '40-49') %>%
            add_trace(y = ~b50t59, name = '50-59') %>%
            add_trace(y = ~b60t69, name = '60-69') %>%
            add_trace(y = ~over69, name = 'over 69') %>%
            layout(yaxis = list(title = 'No. of Transactions'),title = "Age Distribution by Week Day (San-Sat)")
          #type = 'bar', barmode = 'stack'
          
        })
        
      }
      
      if(anaSelected == "Gender Distribution"){
        TXN_FY13_wPatron_lib <- TXN_FY13_wPatron_lib[!is.na(TXN_FY13_wPatron_lib$Patron.Gender),]
        table <- table((TXN_FY13_wPatron_lib$weekName), (TXN_FY13_wPatron_lib$Patron.Gender))
        df <- as.data.frame.matrix(table)
        timeUnit_ordered <- factor(rownames(df), levels=c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
        
        output$invPlot <- renderPlotly({
          plot_ly(df, x = timeUnit_ordered, y = ~Male, name = 'Male',
                  type = 'scatter',mode = 'lines+markers') %>%
            add_trace(y = ~Female, name = 'Female') %>%
            layout(yaxis = list(title = 'No. of Transactions'),title = "Gender Distribution by Week Day (San-Sat)")
          #type = 'bar', barmode = 'stack'
          
        })
      }
      
      if(anaSelected == "Total Transaction Analysis"){
        table <- table((TXN_FY13_wPatron_lib$weekName), (TXN_FY13_wPatron_lib$toCount))
        df <- as.data.frame.matrix(table)
        timeUnit_ordered <- factor(rownames(df), levels=c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
        
        output$invPlot <- renderPlotly({
          plot_ly(df, x = timeUnit_ordered, y = ~toCount, name = 'Total Transactions',
                  type = 'scatter',mode = 'lines+markers') %>%
            layout(yaxis = list(title = 'No. of Total Transactions'),title = "Total Transaction Analysis by Week Day (San-Sat)")
          #type = 'bar', barmode = 'stack'
          
        })
        
      }
      
    }
    
    if(timeUnitSelected == "by Month (Jan-Dec)"){
      
      if(anaSelected == "Race Segmentation"){
        TXN_FY13_wPatron_lib <- TXN_FY13_wPatron_lib[!is.na(TXN_FY13_wPatron_lib$Patron.Race),]
        TXN_FY13_wPatron_lib <- TXN_FY13_wPatron_lib[!is.na(TXN_FY13_wPatron_lib$Patron.Race),]
        table <- table((TXN_FY13_wPatron_lib$month), (TXN_FY13_wPatron_lib$Patron.Race))
        df <- as.data.frame.matrix(table)
        timeUnit_ordered <- factor(rownames(df), levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                          "Aug", "Sep", "Oct", "Nov", "Dec"))
        
        output$invPlot <- renderPlotly({
          plot_ly(df, x = timeUnit_ordered, y = ~Chinese, name = 'Chiense',
                  type = 'scatter',mode = 'lines+markers') %>%
            add_trace(y = ~Indian, name = 'Indian') %>%
            add_trace(y = ~Malay, name = 'Malay') %>%
            add_trace(y = ~Others, name = 'Others') %>%
            layout(yaxis = list(title = 'No. of Transactions'),title = "Race Segmentation by Month (Jan-Dec)")
          #type = 'bar', barmode = 'stack'
          
        })
        
      }
      
      if(anaSelected == "Age Distribution"){
        TXN_FY13_wPatron_lib <- TXN_FY13_wPatron_lib[!is.na(TXN_FY13_wPatron_lib$ageBins),]
        table <- table((TXN_FY13_wPatron_lib$month), (TXN_FY13_wPatron_lib$ageBins))
        df <- as.data.frame.matrix(table)
        timeUnit_ordered <- factor(rownames(df), levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                          "Aug", "Sep", "Oct", "Nov", "Dec"))
        
        output$invPlot <- renderPlotly({
          plot_ly(df, x = timeUnit_ordered, y = ~b0t9, name = '0-9',
                  type = 'scatter',mode = 'lines+markers') %>%
            add_trace(y = ~b10t19, name = '10-19') %>%
            add_trace(y = ~b20t29, name = '20-29') %>%
            add_trace(y = ~b30t39, name = '30-39') %>%
            add_trace(y = ~b40t49, name = '40-49') %>%
            add_trace(y = ~b50t59, name = '50-59') %>%
            add_trace(y = ~b60t69, name = '60-69') %>%
            add_trace(y = ~over69, name = 'over 69') %>%
            layout(yaxis = list(title = 'No. of Transactions'),title = "Age Distribution by Month (Jan-Dec)")
          #type = 'bar', barmode = 'stack'
          
        })
        
      }
      
      if(anaSelected == "Gender Distribution"){
        TXN_FY13_wPatron_lib <- TXN_FY13_wPatron_lib[!is.na(TXN_FY13_wPatron_lib$Patron.Gender),]
        table <- table((TXN_FY13_wPatron_lib$month), (TXN_FY13_wPatron_lib$Patron.Gender))
        df <- as.data.frame.matrix(table)
        timeUnit_ordered <- factor(rownames(df), levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                          "Aug", "Sep", "Oct", "Nov", "Dec"))
        
        output$invPlot <- renderPlotly({
          plot_ly(df, x = timeUnit_ordered, y = ~Male, name = 'Male',
                  type = 'scatter',mode = 'lines+markers') %>%
            add_trace(y = ~Female, name = 'Female') %>%
            layout(yaxis = list(title = 'No. of Transactions'),title = "Gender Distribution by Month (Jan-Dec)")
          #type = 'bar', barmode = 'stack'
          
        })
        
      }
      
      if(anaSelected == "Total Transaction Analysis"){
        table <- table((TXN_FY13_wPatron_lib$month), (TXN_FY13_wPatron_lib$toCount))
        df <- as.data.frame.matrix(table)
        timeUnit_ordered <- factor(rownames(df), levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                                          "Aug", "Sep", "Oct", "Nov", "Dec"))
        
        output$invPlot <- renderPlotly({
          plot_ly(df, x = timeUnit_ordered, y = ~toCount, name = 'Total Transactions',
                  type = 'scatter',mode = 'lines+markers') %>%
            layout(yaxis = list(title = 'No. of Total Transactions'),title = "Total Transaction Analysis by Month (Jan-Dec)")
          #type = 'bar', barmode = 'stack'
          
        })
        
      }
      
    }
  })
  
  #---------------------------------------------------------------------------------------------------------------------------------#   
  # Tab 2.1 - {Huff Visualization - Huff Model}
  
  # Plot General lMap
  output$huffMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)
  })
  
  output$actualMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)
  })
  
  output$diffMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)
  })
  
  # calculateAllDistances <- function(lib, subzone){
  #   DistanceDF <- data.frame(matrix(ncol=3, nrow=0))
  #   colnames(DistanceDF) <- c("LibName" , "SubzoneName", "Distance")
  #   print(DistanceDF)
  #   count <- 1;
  #   
  #   for(i in 1:nrow(lib)){
  #     #print(lib[i,"NAME"])
  #     for(j in 1:nrow(subzone)){
  #       distance<-distm (c(lib[i,"X"], lib[i,"Y"]), c(subzone[j,"X"], subzone[j,"Y"]), fun = distHaversine)/1000 
  #       tempVector<-c(toString(lib[i,"NAME"]),toString(subzone[j,"SUBZONE_N"]),distance)
  #       #tempVector<-c("Hello","Hello","Hello")
  #       DistanceDF<-insertRow(DistanceDF, tempVector, count)
  #       count<-count+1
  #     }
  #   }
  #   huffGlobalDistances<<-DistanceDF
  # }
  
  calculateNewLibDist <- function(newLib, subzone){
    newDistanceDF <- data.frame(matrix(ncol=4, nrow=0))
    colnames(newDistanceDF) <- c("from_Subzone" , "to_Branch.Code", "Distance_km", "route")

    count <- 1;
    
    for(i in 1:nrow(subzone)){
      distance <- distm (c(as.numeric(newLib[1]), as.numeric(newLib[2])), c(subzone[i,"X"], subzone[i,"Y"]), fun = distHaversine)/1000
      tempVector <- c(toString(subzone[i,"SUBZONE_C"]), toString(newLib[4]),as.numeric(distance), 
                      paste(toString(subzone[i,"SUBZONE_C"]),toString(newLib[4]),sep="-"))
      
      newDistanceDF <- insertRow(newDistanceDF, tempVector, count)
      count <- count+1
    }
    huffNewDistances <<- newDistanceDF
    return(huffNewDistances)

  }
  
  mciFit <- function(ijmatrix_Full_Txn_Data, variables){
    print("inside mciFit method")
    # The process of MCI Modeling shold be put here
    # With variables defined in advance for later
    
    # {MCI Model}
    
    # # to eliminate zero value in attraction variables for mci.fit
    # ijmatrix_Full_Txn_Data$No..of.MRT.Within.1KM <- ijmatrix_Full_Txn_Data$No..of.MRT.Within.1KM + 1
    # ijmatrix_Full_Txn_Data$No..of.Malls.Within.1KM <- ijmatrix_Full_Txn_Data$No..of.Malls.Within.1KM + 1
    
    # MCI model for the library patronage
    # shares: "p_ij_obs",
    # explanatory variables: "Branch.Gross.Floor.Area", "Collection.Size", , "Distance_km",
    #                        "type_if_mall","type_if_standalone"
    
    
     mcimodel_lib_patronage <- mci.fit(ijmatrix_Full_Txn_Data, "Subzone","Branch.Code", "p_ij_obs", "Distance_km",
                                       variables)
    
     print(summary(mcimodel_lib_patronage))
     
     mci_fit <- c("Distance_km", as.numeric(mcimodel_lib_patronage$coefficients[1]))
                   
     for(i in 1:length(variables)){
       mci_fit <- c(mci_fit, c(variables[i], as.numeric(mcimodel_lib_patronage$coefficients[i+1])))
     }
    
    # Use like lm
    return(mci_fit)
    
  }
  
  mciShare <- function(ijmatrix_Full_Txn_Data, mci_fit){

    print("mci.fit")
    print(mci_fit)
    mci_share <- mci.shares(ijmatrix_Full_Txn_Data, "Subzone", "Branch.Code",mci_fit)
    print("mci_share")
    print(mci_share)
    
    mci_share$p_ij_diff <- mci_share$p_ij_obs - mci_share$p_ij
    
    return(mci_share)
    
  }
  
  huffMapVisualization <- function(subzone_map, ijmatrix_Full_Txn_Data, lib_C_mci, lib_N_mci, variables){

    mci_fit <- mciFit(ijmatrix_Full_Txn_Data, variables)
    assign('mci_fit',mci_fit,env=globalenv())
    
    # Always run mci.shares in case libraries are eidtted
    mci_share <<- mciShare(ijmatrix_Full_Txn_Data, mci_fit)
    
    huffShare <- mci_share[(mci_share$Branch.Code == lib_C_mci),]
    
    # Merge the data, match the probabiliry with the subzone locations
    toMap <- merge(subzone_map, huffShare, by.x="SUBZONE_C", by.y="Subzone", all.x = TRUE)
    toMap <- spTransform(toMap, CRS("+proj=longlat +datum=WGS84 +no_defs")) # to transform the projection
    
    # Save toMap as a shapefile
    # writeOGR(toMap, dsn = 'C:/nlb/toMap.shp', layer = 'toMap', driver = "ESRI Shapefile")
    
    # To calculate the huffShare bins for a proper color classification
    p_max_huff = round(max(huffShare$p_ij+0.005), digits = 2)
    p_gap_huff = round(p_max_huff/5, digits = 2)
    p_bin_huff <- c(0,p_gap_huff,2*p_gap_huff,3*p_gap_huff,4*p_gap_huff,p_max_huff)
    
    # To calculate the actualShare bins for a proper color classification
    p_max_actual = round(max(huffShare$p_ij_obs+0.005), digits = 2)
    p_gap_actual = round(p_max_actual/5, digits = 2)
    p_bin_actual <- c(0,p_gap_actual,2*p_gap_actual,3*p_gap_actual,4*p_gap_actual,p_max_actual)
    
    # To calculate the share diffierence bins for a proper color classification
    p_max_diff = round(max(huffShare$p_ij_diff+0.005), digits = 2)
    p_min_diff = round(min(huffShare$p_ij_diff-0.005), digits = 2)
    p_gap_diff = round((p_max_diff-p_min_diff)/5, digits = 2)
    p_bin_diff <- c(p_min_diff,p_min_diff+p_gap_diff,p_min_diff+2*p_gap_diff,p_min_diff+3*p_gap_diff,p_min_diff+4*p_gap_diff,p_max_diff)
    
    
    
    # Determine Color bins for Huff
    palette_huff <- colorBin(c('#eff3ff',
                              '#bdd7e7',
                              '#6baed6',
                              '#3182bd',
                              '#08519c'),
                            bins = p_bin_huff)
    # Determine Color bins for Actual
    palette_actual <- colorBin(c('#eff3ff',
                               '#bdd7e7',
                               '#6baed6',
                               '#3182bd',
                               '#08519c'),
                             bins = p_bin_actual)
    # Determine Color bins for Difference
    palette_diff <- colorBin(c('#d7191c',
                               '#fdae61',
                               '#ffffbf',
                               '#a6d96a',
                               '#1a9641'),
                             bins = p_bin_diff)
    
    
    # Determine the tooltips for Huff
    popupHuff <- paste0("From Subzone: ",             
                        toMap$SUBZONE_C,         #column containing the district names
                        "<br>To Library: ", 
                        toMap$Branch.Code,          #column that contains the relative amount data
                        "<br>Predicted Patronage Percentage: ", 
                        round(toMap$p_ij, digits=2)   #column that contains the absolute amount data
                        
    )
    # Determine the tooltips for Actual
    popupActual <- paste0("From Subzone: ",             
                        toMap$SUBZONE_C,         #column containing the district names
                        "<br>To Library: ", 
                        toMap$Branch.Code,          #column that contains the relative amount data
                        "<br>Actual Patronage Percentage: ", 
                        round(toMap$p_ij_obs, digits=2)           #column that contains the absolute amount data
    )
    # Determine the tooltips for Diff
    popupDiff <- paste0("From Subzone: ",             
                        toMap$SUBZONE_C,         #column containing the district names
                        "<br>To Library: ",
                        toMap$Branch.Code,          #column that contains the relative amount data
                        "<br>Patronage Percentage Difference: ", 
                        round(toMap$p_ij_diff, digits=2)           #column that contains the absolute amount data
    )
    
    
    # Plot the choropleth map for Huff
    output$huffMap <- renderLeaflet({
      #print("inside huff")
      leaflet()%>%
        setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>% 
        addTiles()%>% 
        
        addPolygons(data = toMap, 
                    fillColor = ~palette_huff(toMap$p_ij),  
                    fillOpacity = 0.7,         ## how transparent do you want the polygon to be?
                    color = "darkgrey",       ## color of borders between districts
                    weight = 1.5,            ## width of borders
                    popup = popupHuff,         ## which popup?
                    group="<span style='color: #7f0000; font-size: 11pt'><strong>2000</strong></span>")%>%  
        
        addMarkers(icon = nlbIcon, lng = as.numeric(subset(DF, outcome == lib_N_mci)$lon), lat = as.numeric(subset(DF, outcome == lib_N_mci)$lat), popup = subset(DF, outcome == lib_N_mci)$outcome, layerId = "selected") %>% 
        
        addLegend(position = 'bottomright', ## choose bottomleft, bottomright, topleft or topright
                  colors = c('#eff3ff',
                             '#bdd7e7',
                             '#6baed6',
                             '#3182bd',
                             '#08519c'), 
                  labels =  c(paste(p_bin_huff[1],p_bin_huff[2],sep="-"),
                              paste(p_bin_huff[2],p_bin_huff[3],sep="-"),
                              paste(p_bin_huff[3],p_bin_huff[4],sep="-"),
                              paste(p_bin_huff[4],p_bin_huff[5],sep="-"),
                              paste(p_bin_huff[5],p_max_huff,sep="-")),  ## legend labels (only min and max)
                  opacity = 0.6,      ##transparency again
                  title = "Predicted Patronage Percentage")   ## title of the legend
     })
    
    
    
    # Plot the choropleth map for Huff
    output$actualMap <- renderLeaflet({
       #print("inside actual")
       leaflet()%>%
          setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>% 
          addTiles()%>% 
        
        addPolygons(data = toMap, 
                    fillColor = ~palette_actual(toMap$p_ij_obs),  
                    fillOpacity = 0.7,         ## how transparent do you want the polygon to be?
                    color = "darkgrey",       ## color of borders between districts
                    weight = 1.5,            ## width of borders
                    popup = popupActual,         ## which popup?
                    group="<span style='color: #7f0000; font-size: 11pt'><strong>2000</strong></span>")%>%  
        
        addMarkers(icon = nlbIcon, lng = as.numeric(subset(DF, outcome == lib_N_mci)$lon), lat = as.numeric(subset(DF, outcome == lib_N_mci)$lat), popup = subset(DF, outcome == lib_N_mci)$outcome, layerId = "selected") %>% 
        
        addLegend(position = 'bottomright', ## choose bottomleft, bottomright, topleft or topright
                  colors = c('#eff3ff',
                             '#bdd7e7',
                             '#6baed6',
                             '#3182bd',
                             '#08519c'), 
                  labels =  c(paste(p_bin_actual[1],p_bin_actual[2],sep="-"),
                              paste(p_bin_actual[2],p_bin_actual[3],sep="-"),
                              paste(p_bin_actual[3],p_bin_actual[4],sep="-"),
                              paste(p_bin_actual[4],p_bin_actual[5],sep="-"),
                              paste(p_bin_actual[5],p_max_actual,sep="-")),  ## legend labels (only min and max)
                  opacity = 0.6,      ##transparency again
                  title = "Actual Patronage Percentage")   ## title of the legend
     })
    
    # Plot the choropleth map for Huff
     output$diffMap <- renderLeaflet({
      #print("inside diff")
      leaflet() %>%
        setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>% 
        addTiles() %>% 
        
        addPolygons(data = toMap, 
                    fillColor = ~palette_diff(toMap$p_ij_diff),  
                    fillOpacity = 0.7,         ## how transparent do you want the polygon to be?
                    color = "darkgrey",       ## color of borders between districts
                    weight = 1.5,            ## width of borders
                    popup = popupDiff,         ## which popup?
                    group="<span style='color: #7f0000; font-size: 11pt'><strong>2000</strong></span>")%>%  
        
        addMarkers(icon = nlbIcon, lng = as.numeric(subset(DF, outcome == lib_N_mci)$lon), lat = as.numeric(subset(DF, outcome == lib_N_mci)$lat), 
                   popup = subset(DF, outcome == lib_N_mci)$outcome, layerId = "selected") %>% 
        
        addLegend(position = 'bottomright', ## choose bottomleft, bottomright, topleft or topright
                  colors = c('#d7191c',
                             '#fdae61',
                             '#ffffbf',
                             '#a6d96a',
                             '#1a9641'),
                  labels =  c(paste(p_bin_diff[1],p_bin_diff[2],sep="-"),
                              paste(p_bin_diff[2],p_bin_diff[3],sep="-"),
                              paste(p_bin_diff[3],p_bin_diff[4],sep="-"),
                              paste(p_bin_diff[4],p_bin_diff[5],sep="-"),
                              paste(p_bin_diff[5],p_max_diff,sep="-")),  ## legend labels (only min and max)
                  opacity = 0.6,      ##transparency again
                  title = "Patronage Percentage Difference")   ## title of the legend
     })

  }
  
  
  observeEvent(input$calibrate,{
    if(is.null(input$toCalibrate)){
      showNotification("Error: Please select attraction variables for MCI Model Calibration", type = "error")  
    }else{
      toValibrate <- input$toCalibrate
      # tmp <- ijmatrix_Full_Txn_Data %>%
      #   mutate(Branch.Type = ifelse(Branch.Type == "Mall", 1, ifelse(Branch.Type == "Stand-Alone", 2, 3)))
      
      mci_summary <- mci.fit(ijmatrix_Full_Txn_Data, "Subzone","Branch.Code", "p_ij_obs", "Distance_km",
                             toValibrate)
      
      output$disSummary <- renderPrint({
        summary(mci_summary)
      })
    }
  })
    
    
  
  observeEvent(input$visualizeHuff,{
    # Need to add if-else condition for location update
    for(i in 1:nrow(lib)){
      if((lib[i,"NAME"]==input$libHuff)){
        lib_C_mci <- lib[i, "DESCRIPTION"]
        lib_N_mci <- lib[i, "NAME"]
      }
    }
     
     if(input$ageBinSelect!="All"){
       if(input$ageBinSelect == "Kindergarten and Preschool"){
         TXN_FY13_wPatron_dist <- TXN_FY13_wPatron_dist[(TXN_FY13_wPatron_dist$Patron.Birthyear > as.numeric(format(Sys.Date(), "%Y"))-7), ]
       }else if(input$ageBinSelect == "Primary School"){
         TXN_FY13_wPatron_dist <- TXN_FY13_wPatron_dist[(TXN_FY13_wPatron_dist$Patron.Birthyear <= as.numeric(format(Sys.Date(), "%Y"))-7)&
                                                          (TXN_FY13_wPatron_dist$Patron.Birthyear >= as.numeric(format(Sys.Date(), "%Y"))-12), ]
       }else if(input$ageBinSelect == "Secondary and Post-Sceondary School"){
         TXN_FY13_wPatron_dist <- TXN_FY13_wPatron_dist[(TXN_FY13_wPatron_dist$Patron.Birthyear <= as.numeric(format(Sys.Date(), "%Y"))-13)&
                                                          (TXN_FY13_wPatron_dist$Patron.Birthyear >= as.numeric(format(Sys.Date(), "%Y"))-19), ]
       }else if(input$ageBinSelect == "Univeristy"){
         TXN_FY13_wPatron_dist <- TXN_FY13_wPatron_dist[(TXN_FY13_wPatron_dist$Patron.Birthyear <= as.numeric(format(Sys.Date(), "%Y"))-20)&
                                                          (TXN_FY13_wPatron_dist$Patron.Birthyear >= as.numeric(format(Sys.Date(), "%Y"))-25), ]
       }else if(input$ageBinSelect == "Working Adults"){
         TXN_FY13_wPatron_dist <- TXN_FY13_wPatron_dist[(TXN_FY13_wPatron_dist$Patron.Birthyear <= as.numeric(format(Sys.Date(), "%Y"))-26)&
                                                          (TXN_FY13_wPatron_dist$Patron.Birthyear >= as.numeric(format(Sys.Date(), "%Y"))-64), ]
       }else if(input$ageBinSelect == "Senior Citizens"){
         TXN_FY13_wPatron_dist <- TXN_FY13_wPatron_dist[(TXN_FY13_wPatron_dist$Patron.Birthyear < as.numeric(format(Sys.Date(), "%Y"))-64), ]
       }
     }
     
     if(input$sexSelect!="Both"){
         TXN_FY13_wPatron_dist <- TXN_FY13_wPatron_dist[(TXN_FY13_wPatron_dist$Patron.Gender == input$sexSelect), ]
     }
     
     if(input$raceSelect!="All"){
       TXN_FY13_wPatron_dist <- TXN_FY13_wPatron_dist[(TXN_FY13_wPatron_dist$Patron.Race == input$raceSelect), ]
     }
     
     if(is.null(input$attractiveness)){
       showNotification("Error: Please select attraction variables for Market Share Calculation", type = "error")  
     }else{
       temp <- input$attractiveness
       variables <- c()
       for(i in 1:length(temp)){
         variables[i] <- temp[i]
       }
    
       ijmatrix_TXN_FY13 <- ijmatrix.create(TXN_FY13_wPatron_dist, "from_Subzone","to_Branch.Code", 
                                            correctVar = TRUE, correctVar.val = 1)
       
       ijmatrix_Full_Txn_Data <- merge(ijmatrix_TXN_FY13, Lib_Info, by.x="to_Branch.Code", by.y="Branch.Code", all.x = TRUE)
       ijmatrix_Full_Txn_Data <- merge(ijmatrix_Full_Txn_Data, Subzone_Distance, by.x="interaction", by.y="route", all.x = TRUE)
       
       # Rename columns for convenience
       names(ijmatrix_Full_Txn_Data)[2] <- "Branch.Code"
       names(ijmatrix_Full_Txn_Data)[3] <- "Subzone"
       
       ijmatrix_Full_Txn_Data$to_Branch.Code.y <- NULL
       ijmatrix_Full_Txn_Data$from_Subzone.y <- NULL
       
       huffMapVisualization(subzone_map, ijmatrix_Full_Txn_Data, lib_C_mci, lib_N_mci, variables)
     
     }

  })
  
  # Tab 2.2 - {Huff Visualization - Update Lib Location (Add/Remove)}
  
  # Observe select on location update type
  observeEvent(input$radio,{
    
    if(input$radio == "newLibLocation"){
      
      #Observer map click
      observe({
        
        click <- input$huffMap_shape_click
        if (!is.null(click)) {
          
          isolate({
            huffMapLat <<- click$lat
            huffMapLng <<- click$lng
          })
          
          
          #add marker on map
          leafletProxy('huffMap') %>% 
            removeMarker(layerId = "newLocationMarker")%>%
            addMarkers(icon = nlbIcon, lng = click$lng, lat = click$lat, popup = paste(click$lng, click$lat), layerId = "newLocationMarker") 
          
        }
      })
      
      output$dynamicUpdateLib <- renderUI({
        newLibName <- textInput("newLibName", h5("Name of new location"), placeholder = "Give a name to the location")
        newLibCode <- textInput("newLibCode", h5("Branch code of new location"), placeholder = "Give a branch code to the location")
        newLibCollectionSize <- numericInput("newLibCollectionSize", h5("Collection Size"), value = 10000000)
        newLibType <- selectizeInput("newLibType", "Branch Type", choices = c("Mall", "Stand-Alone", "Regional"),
                                     multiple = FALSE, selected = "Mall")
        newLibFloorArea <- numericInput("newLibFloorArea", h5("Gross Floor Area"), value = 1890)
        
        # Dynamically assign the branch code by default based on the new lib name input
        observeEvent(input$newLibName, {
          if(input$newLibCode == "" || is.null(input$newLibCode)){
            updateTextInput(session, "newLibCode", value = input$newLibName)
          }
        })
        
        # Dynamically assign the average floor area by default based on the new lib type input
        observeEvent(input$newLibType, {
          
          type <- input$newLibType
          
          if(type == "Stand-Alone"){
            #print("Stand-Alone")
            updateNumericInput(session, "newLibFloorArea", value = 3967)
          }else if(type == "Regional"){
            #print("Regional")
            updateNumericInput(session, "newLibFloorArea", value = 11833)
          }else{
            #print("Mall")
            updateNumericInput(session, "newLibFloorArea", value = 1890)
          }
        })
        
        newLocation <- list(newLibName, newLibCode, newLibCollectionSize, newLibType, newLibFloorArea)
      })
      
      output$dynamicSaveLib <- renderUI({
        actionButton("saveLocation", label = "Save Location")
      })
      
      observeEvent(input$saveLocation, {
        if(is.null(huffMapLng) || is.null(huffMapLat) || input$newLibName == "" || input$newLibCode == "" || is.null(input$newLibCollectionSize) 
           || is.null(input$newLibType) || is.null(input$newLibFloorArea)){
          #print("empty")
          showNotification("Error: Please pin a location on map / input a name for the location", type = "error")  
          
        }
        else{
          barvector <- facilitiesCalculator(huffMapLng, huffMapLat, data.list, name.list, 1000)
          
          isolate(input$newLibName)
          isolate(input$newLibCode)
          isolate(input$newLibCollectionSize)
          isolate(input$newLibType)
          isolate(input$newLibFloorArea)
          isolate(input$radio)

          
          huffMapName <<- input$newLibName
          huffMapCode <<- input$newLibCode
          huffMapSize <<- input$newLibCollectionSize
          huffMapType <<- input$newLibType
          huffMapArea <<- input$newLibFloorArea
          
          
          # Calculate the number of MRT, Malls and Tuition Centre within 1km of the new location
          newBarVector <- facilitiesCalculator(huffMapLng, huffMapLat, data.list, name.list, 1000)
          # Pass the new lib location corresponding attractivenesses
          totalLng <<- c(totalLng, huffMapLng)
          totalLat <<- c(totalLat, huffMapLat)
          totalName <<- c(totalName, huffMapName)
          totalCode <<- c(totalCode, huffMapCode)
          totalCollectionSize <<- c(totalCollectionSize, huffMapSize)
          totalBranchType <<- c(totalBranchType, huffMapType)
          totalFloorArea <<- c(totalFloorArea, huffMapArea)
          totalMRTno <<- c(totalMRTno, newBarVector[1])
          totalMallno <<- c(totalMallno, newBarVector[2])
          totalTCno <<- c(totalTCno, newBarVector[3])
          
          # Create new record of lib data
          newLibData <- c(totalLng, totalLat, totalName, totalCode, totalCollectionSize,
                           totalBranchType, totalFloorArea, totalMRTno, totalMallno, totalTCno)
          # add the new library to lib list
          newLibRow <- data.frame(X=totalLng, Y=totalLat, DESCRIPTION=totalCode, NAME=totalName)
          lib <<- rbind(lib, newLibRow)
          assign('lib', lib, env=globalenv())
          libName <<- paste(lib$NAME)
          assign('libName', libName, env=globalenv())

          
          # Calculate the distance between the new lib location to all subzones
          newDist <- calculateNewLibDist(newLibData, subzoneCentroids)
          # remove the extra 324 row in the newly created matrix
          newDist <- newDist[-c(324), ] 
          
          # Transform new lib branch type to dummy variables
          new_type_if_mall <- transform(ifelse(totalBranchType == "Mall",1,0))
          new_type_if_standalone <- transform(ifelse(totalBranchType == "Stand-Alone",1,0))
          
          # Create new data frame with one single row
          add_ijmatrix <- data.frame("route", totalCode, "from_Subzone", 2, 50, 0.04, totalFloorArea, totalBranchType,
                                     totalCollectionSize, totalMRTno, totalMallno, totalTCno, new_type_if_mall, new_type_if_standalone, "Distance_km")
          
          # create new data frame - column names
          # names(add_ijmatrix) <- c("interaction", "Branch.Code", "Subzone", "freq_ij_abs", "freq_i_total", "p_ij_obs", "Branch.Gross.Floor.Area", 
          #                 "Branch.Type", "Collection.Size", "No..of.MRT.Within.1KM", "No..of.Malls.Within.1KM", "No..of.Tuition.Centres.Within.1KM", 
          #                 "type_if_mall", "type_if_standalone", "Distance_km_x")

          cNames <- replace(colnames(ijmatrix_Full_Txn_Data), colnames(ijmatrix_Full_Txn_Data)=="Distance_km", "Distance_km_x")
          names(add_ijmatrix) <- cNames
          
          add_ijmatrix <- merge(newDist, add_ijmatrix)
          
          # Replace placeholder with data from newDist
          add_ijmatrix["interaction"] <- add_ijmatrix["route"]
          add_ijmatrix["Subzone"] <- add_ijmatrix["from_Subzone"]
          add_ijmatrix["Distance_km_x"] <- add_ijmatrix["Distance_km"]
          add_ijmatrix["route"] <- NULL
          add_ijmatrix["from_Subzone"] <- NULL
          add_ijmatrix["to_Branch.Code"] <- NULL
          add_ijmatrix["Distance_km"] <- NULL
          names(add_ijmatrix)[names(add_ijmatrix) == "Distance_km_x"] <- "Distance_km"
          
          
          # add_ijmatrix <- add_ijmatrix[-c(324), ] 
          
          
          # Add the new lib data to existing ijmatrix
          ijmatrix_Full_Txn_Data <<- rbind(ijmatrix_Full_Txn_Data, add_ijmatrix)
          ijmatrix_Full_Txn_Data[,15] %<>% as.numeric(as.character(.))
          assign('ijmatrix_Full_Txn_Data', ijmatrix_Full_Txn_Data, env=globalenv())

          # Update lib selection input to include the new lib
          updateSelectizeInput(session, "libHuff", label = "Select a Library", choices = libName, selected = totalName)
          updateSelectInput(session, "radio", label = "Library Update Type", 
                            choices = c("None"= "none", "Add new library location" = "newLibLocation",
                                         "Remove existing library location" = "removeLibLocation",
                                         "Change existing Library location" = "changeLibLocation"),
                            selected = "none")
          
          updateNumericInput(session, "radio", value = "None")
          showNotification("New library location has been saved. Please click the button above for updated MCI visualization", type = "message")
          
        }
      })
      
    }
    else if(input$radio == "removeLibLocation"){
      isolate(input$listNewLib)
      #remove marker
      leafletProxy('huffMap') %>% 
        removeMarker(layerId = "newLocationMarker")
      
      output$dynamicUpdateLib <- renderUI({
        selectizeInput("listAllLib", h5("Select a Library to remove"), choices = libName, multiple = FALSE)
      })
      isolate(input$listAllLib)
      
      #Remove location
      output$dynamicSaveLib <- renderUI({
        actionButton("removeLocation", label = "Remove Location")
      })
      
      
      observeEvent(input$removeLocation,{
        #observeEvent(input$listNewLib,{  
        
        isolate(input$listNewLib)
        
        toRemove <- newLibData
        
        removeList <- NULL
        remLng <- NULL
        remLat <- NULL
        remSize <- NULL
        remName <- c()
        
        if(input$listNewLib == ""){
          output$huffErrorText <- renderText({ 
            "Error: Please select a location to remove" 
          })
        }else{
          output$huffErrorText <- renderText({ 
          })
          
          
          if(nrow(toRemove) > 0){
            
            for(i in 1:nrow(toRemove)){
              if(toRemove[i,3] != input$listNewLib){
                remLng <- c(remLng, toRemove[i,1])
                remLat <- c(remLat, toRemove[i,2])
                remName <- c(remName, paste(toRemove[i,3]))
                remSize <- c(remSize, toRemove[i,4])
                removeList <- data.frame(remLng, remLat, remName, remSize)
                
                
              }
            }
          }else{
            output$huffErrorText <- renderText({ 
              "Error: No location available"
            })
          }
          
          
          newLibData <<- removeList
          libAvail <- newLibData

          print(newLibData)
          
          totalLng <<- remLng
          totalLat <<- remLat
          totalName <<- remName
          totalCollectionSize <<- remSize
  

          output$dynamicUpdateLib <- renderUI({
            selectizeInput("listNewLib", h5("Select a location"), choices = newLibData[,3], multiple = FALSE)
          })
          
        }
        #})
      }) 

      
    }
    else if(input$radio == "changeLibLocation"){
      output$huffErrorText <- renderText({ 
      })
      
      #remove marker
      leafletProxy('huffMap') %>% 
        removeMarker(layerId = "newLocationMarker")
      
      #pop the libraries dropdown
      #Use selection to change lib location
      # libLocList <<- lib
      # libLocLibList <- lib$NAME
      
      output$dynamicUpdateLib <- renderUI({
        r1 <- radioButtons("changeDeleteLib", label = h5("Step 1: Select"),
                           choices = c("Change Library location" = "modifyExisting", "Delete Existing Library"= "deleteExisting"),
                           selected = "modifyExisting")
        r2 <- hr()
        rList <- list(r1, r2)
      })
      
      observeEvent(input$changeDeleteLib, {
        if(input$changeDeleteLib == "modifyExisting"){
          #Edit location

          output$dynamicSaveLib <- renderUI({
            i1 <- selectizeInput("changeLibLocation", h5("Step 2: Select a Library to change location"), choices = paste(libLocLibList), multiple = FALSE)
            i2 <- actionButton("saveNewLocation", label = "Save new location")
            inputList<-list(i1,i2)
          })
          
          #pin the marker of library
          observeEvent(input$changeLibLocation, {
            if(nrow(libLocList) > 0){
              
              output$huffErrorText <- renderText({ 
              })
              
              for(i in 1:nrow(libLocList)){
                if(libLocList$NAME[i] == input$changeLibLocation){
                  leafletProxy('huffMap') %>% 
                    removeMarker(layerId = "newLocationMarker")%>%
                    addMarkers(icon = nlbIcon, lng = libLocList$X[i], lat = libLocList$Y[i], popup = paste(libLocList$NAME[i]), layerId = "newLocationMarker") 
                  
                }
              }
            }else{
              output$huffErrorText <- renderText({ 
                "Error: No Library available"
              })
            }
          })
          
          #observe new location for library
          observe({
            click <- input$huffMap_shape_click
            if (!is.null(click)) {

              
              huffMapLngChange <<- click$lng
              huffMapLatChange <<- click$lat
              
              #add marker on map
              leafletProxy('huffMap') %>% 
                removeMarker(layerId = "newLocationMarker")%>%
                addMarkers(icon = nlbIcon, lng = click$lng, lat = click$lat, popup = paste(click$lng, click$lat), layerId = "newLocationMarker") 
            }
          })
          
          observeEvent(input$saveNewLocation,{
            isolate(input$changeLibLocation)
            #replace the location of lib
            
            if(nrow(libLocList) > 0){
              
              output$huffErrorText <- renderText({ 
              })
              
              if(!is.null(huffMapLngChange) && !is.null(huffMapLatChange)){
                for(i in 1:nrow(libLocList)){
                  if(libLocList$NAME[i] == input$changeLibLocation){
                    #replace lat lng
                    libLocList$X[i] <<- huffMapLngChange
                    libLocList$Y[i] <<- huffMapLatChange
                  }
                }
              }else{
                output$huffErrorText <- renderText({
                  "Error: Please select a new location for the Library"
                })
              }
            }else{
              output$huffErrorText <- renderText({ 
                "Error: No Library available"
              })
            }
            #add marker on map
            leafletProxy('huffMap') %>% 
              removeMarker(layerId = "newLocationMarker")%>%
              addMarkers(icon = nlbIcon, lng = huffMapLngChange, lat = huffMapLatChange, popup = paste(input$changeLibLocation), layerId = "newLocationMarker")

          })
          
        }else{
          #add marker on map
          leafletProxy('huffMap') %>% 
            removeMarker(layerId = "newLocationMarker")
          
          output$dynamicSaveLib <- renderUI({
            s1 <- selectizeInput("deleteLibLocation", h5("Select a Library to remove"), choices = paste(libLocList$NAME), multiple = FALSE)
            s2 <- actionButton("removeExistLib", label = "Remove Library")
            inputList<-list(s1,s2)
          })
          
          observeEvent(input$deleteLibLocation, {
            if(nrow(libLocList) > 0) {
              
              output$huffErrorText <- renderText({ 
              })
              
              for(i in 1:nrow(libLocList)){
                if(libLocList$NAME[i] == input$deleteLibLocation){
                  leafletProxy('huffMap') %>% 
                    removeMarker(layerId = "newLocationMarker")%>%
                    addMarkers(icon = nlbIcon, lng = libLocList$X[i], lat = libLocList$Y[i], popup = paste(libLocList$NAME[i]), layerId = "newLocationMarker") 
                  
                }
              }
            }else{
              output$huffErrorText <- renderText({ 
                "Error: Please select a Library to delete"
              })
            }
            
          })
          
          
          observeEvent(input$removeExistLib, {
            isolate(input$changeLibLocation)
            
            finalRemoveList <- NULL
            X <- NULL
            Y <- NULL
            DESCRIPTIO <- NULL
            NAME <- NULL

            #click delete
            
            
            for(i in 1:nrow(libLocList)){
              if(libLocList[i,4] != input$deleteLibLocation){
                X <- c(X, libLocList[i,1])
                Y <- c(Y, libLocList[i,2])
                DESCRIPTIO <- c(DESCRIPTIO, libLocList[i,3])
                NAME <- c(NAME, paste(libLocList[i,4]))
                finalRemoveList <- data.frame(X, Y, DESCRIPTIO, NAME)
              }
              
            }
            
            libLocList <<- finalRemoveList

          })
          
          #})
          
        }
      })
      
      
    }
    else{
      #remove marker
      leafletProxy('huffMap') %>% 
        removeMarker(layerId = "newLocationMarker")
      
      output$dynamicUpdateLib <- renderUI({
      })
      
      output$dynamicSaveLib <- renderUI({
      })
      
      output$huffErrorText <- renderText({ 
      })
    }
    
  })
  
  insertRow <- function(existingDF, newrow, r) {
    existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
    existingDF[r,] <- newrow
    return(existingDF)
  }
  
  
  
  #---------------------------------------------------------------------------------------------------------------------------------#   
  
  # Tab 3 - {Patron Segmentation - Clustering}
  
  # 1. Select interested variables
  observeEvent(input$rawVar, {
    varSelected <- input$rawVar
    numVar <- length(varSelected)
    
    # show dist of selected variable
    output$distPlot <- renderPlot({
      par(mfrow=c(2, 2))
      
      for (i in 1:numVar) {
        n <- as.numeric(varSelected[i]);
        
        hist(cluster[,n], main = colnames(cluster)[n] , prob = TRUE)
        d <- density(cluster[,n])
        lines(d, col="red")
      }
    })
  })
  
  # 2. Select the variables to transform
  observeEvent(input$transVar, {
    
    varSelected <- input$transVar
    numVar <- length(varSelected)
    
    # show dist of selected variable
    output$transformPlot <- renderPlot({
      par(mfrow=c(2, 2))
      
      for(i in 1:numVar){
        n <- as.numeric(varSelected[i]);
        transformed <- signedlog10(cluster[,n])
        
        hist(transformed, main = paste("Transformed (Log10)", colnames(cluster)[n]), prob=TRUE)
        d <- density(transformed)
        lines(d, col="red")
        
      }
    })
  })
  
  # Transform variables
  signedlog10 = function(x) {
    ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
  }
  
  # Observe click on transform
  observeEvent(input$trans, {
    transvarSelected <- input$transformVar
    numtransVar <- length(transvarSelected)
    transclustervarSelected <- input$clusterVar
    numtransclusterVar <- length(transclustervarSelected)
    
    if(numtransVar == 0){
      if(numtransclusterVar == 0){
        output$errorText <- renderText({ 
          "Error: Please input variables" 
        })
        error <- TRUE
      }
    }else{
      if(numtransclusterVar > 0){
        for(i in 1:numtransVar){
          for(c in 1:numtransclusterVar){
            if(transvarSelected[i] == transclustervarSelected[c]){
              output$errorText <- renderText({ 
                "Error: Cannot use the same variables" 
              })
              error <- TRUE
              print(error)
            }
          }
        }
      }
    }
    
    
    #print(error)
    
    if(error == FALSE){
      output$errorText <- renderText({ 
        ""
      })
      
      output$opclusterPlot <-renderPlot({
        h5("10 iterations (3 to 12 clusters) were run to determine the optimal number of clusters. 
           The optimal number of cluster is based on Within Sum of Squares.")
        if(numtransVar > 0){
          for(i in 1:numtransVar){
            
            n <- as.numeric(transvarSelected[i]);
            transformed <- signedlog10(cluster[,n])
            scaletransformed <- scale(transformed)
            print("ScaleTData")
            print(NROW(scaletransformed))
            if(is.null(clusteringData)){
              clusteringData<<-data.frame(matrix(ncol=1,nrow=nrow(scaletransformed)))
              colnames(clusteringData)<<- paste("Transformed ", colnames(cluster[n]), sep="")
              clusteringData[paste("Transformed ", colnames(cluster[n]), sep="")]<<-scaletransformed
            }else{
              clusteringData[paste("Transformed ", colnames(cluster[n]), sep="")]<<-scaletransformed
            }
          }
        }
        
        if(numtransclusterVar > 0){
          for(i in 1:numtransclusterVar){
            
            n <- as.numeric(transclustervarSelected[i]);
            scaledData <- scale(cluster[,n])

            if(is.null(clusteringData)){
              clusteringData<<-data.frame(matrix(ncol=1,nrow=nrow(scaledData)))
              colnames(clusteringData)<<- colnames(cluster[n])
              clusteringData[colnames(cluster[n])]<<-scaledData
            }else{
              clusteringData[colnames(cluster[n])]<<-scaledData
            }
            
          }
          
          
        }
        
        #Determine number of clusters
        wss <- (nrow(clusteringData)-1)*sum(apply(clusteringData,2,var))
        for (i in 3:12) wss[i] <- sum(kmeans(clusteringData, centers=i)$withinss)
        
        plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", xlim=c(3,12))
        
      })
      
      
  }
})
  
  # Observe click on clustering
  observeEvent(input$cluster, {
    
    transvarSelected <- input$transformVar

    numtransVar <- length(transvarSelected)
    transclustervarSelected <- input$clusterVar
    numtransclusterVar <- length(transclustervarSelected)
    
    if(numtransVar == 0 && numtransclusterVar ==0){
      output$errorTextCluster <- renderText({ 
        "Error: Please input variables for clustering" 
      })
    }else{
      output$errorTextCluster <- renderText({ 
      })
      
      if(numtransVar > 0){
        for(i in 1:numtransVar){
          
          n <- as.numeric(transvarSelected[i]);
          transformed <- signedlog10(cluster[,n])
          scaletransformed <- scale(transformed)

          if(is.null(clusteringData)){
            clusteringData<<-data.frame(matrix(ncol=1,nrow=nrow(scaletransformed)))
            colnames(clusteringData)<<- paste("Transformed ", colnames(cluster[n]), sep="")
            clusteringData[paste("Transformed ", colnames(cluster[n]), sep="")]<<-scaletransformed
          }else{
            clusteringData[paste("Transformed ", colnames(cluster[n]), sep="")]<<-scaletransformed
          }
          
        }
        
        if(numtransclusterVar > 0){
          for(i in 1:numtransclusterVar){
            
            n <- as.numeric(transclustervarSelected[i]);
            scaledData <- scale(cluster[,n])

            if(is.null(clusteringData)){
              clusteringData<<-data.frame(matrix(ncol=1,nrow=nrow(scaledData)))
              colnames(clusteringData)<<- colnames(cluster[n])
              clusteringData[colnames(cluster[n])]<<-scaledData
            }else{
              clusteringData[colnames(cluster[n])]<<-scaledData
            }
            
          }
        }
      }
      
    }
    
    output$clusterPlot <-renderPlot({

      kmClustered <- kmeans(clusteringData, input$numCluster)
      
      clusterCenter <- data.frame(kmClustered$centers)
      clustersNum <- unique(kmClustered$cluster)
      sortedCluster <- clustersNum[order(clustersNum)]
      clusterCenter[paste("Cluster.No")] <- sortedCluster

      
      # output$mytable <- DT::renderDataTable({
      #   DT::datatable(clusterCenter, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
      #   
      # })
      
      output$mytable = renderDataTable({
        clusterCenter
      })
      
      
      counts <- table(kmClustered$cluster)
      print(counts)
      bar <- barplot(counts, main = "Clustering",
                     xlab="Cluster",
                     ylab="No. of Patrons",  ylim=c(0,1.25*max(counts)))
      text(x = bar, y = counts, label = counts, pos = 3, cex = 0.8, col = "red")
      
      #assign cluster to mergeClusterPASubzone
      mergclusterPASubzone[paste("ClusterNo")] <- kmClustered$cluster
      paPALib<<-merge(patronLib, mergclusterPASubzone, by.x="Patron.UID.of.RFM.with.Cluster", by.y="Patron.UID",all.x=TRUE)
      
      #output cluster by PA
      observeEvent(input$libClustering, {
        output$clusterPlot2 <-renderPlot({

          paPALib['freq'] <<- 1

          aggdata <- aggregate(paPALib$freq, by=list(paPALib$NAME, paPALib$ClusterNo), FUN=sum)
          dfAgg <- data.frame(aggdata)
          
          subsetLib <- subset(dfAgg, dfAgg$Group.1 == input$libClustering)
          print(subsetLib)
          patronVector <- c()
          patronVector <- subsetLib$x
          print(patronVector)
          
          colVector <- c()
          colVector <- subsetLib$Group.2
          print(colVector)
          
          print(unique(dfAgg$Group.2))
          
          cluster <- matrix(patronVector,ncol=length(unique(dfAgg$Group.2)),byrow=FALSE)
          colnames(cluster) <- colVector
          cluster <- as.table(cluster)
          bar<-barplot(cluster, ylab="Number of Patrons", xlab="Cluster", main="Distribution of Patrons", ylim=c(0,1.25*max(cluster)))
          text(x = bar, y = cluster, label = cluster, pos = 3, cex = 0.8, col = "red")
          
          
        })
      })
    })
    
    
    
  })
  
  #---------------------------------------------------------------------------------------------------------------------------------#    
  
  
  
  
})


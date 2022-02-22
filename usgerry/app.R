library(shiny)
library(shinyBS)
library(leaflet)
library(sf)
library(rmapshaper)
library(graphics)
library(scales)
library(dplyr)

# # # save data instead of reloading it every time
# shape <- tigris::counties(cb=TRUE,resolution="500k",class="sp")
# shape <- ms_simplify(shape)
# shape@data <- shape@data[,c(1:3,6)]
# shape$STATEFP <- as.numeric(shape@data$STATEFP)
# shape$COUNTYFP <- as.numeric(shape@data$COUNTYFP)
# shape <- shape[shape$STATEFP <= 66,]
# saveRDS(shape, file = "shape.rds")

shape <- readRDS(file = "shape.rds")
#https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020-alldata.csv
#https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-20/master/2020_US_County_Level_Presidential_Results.csv
#https://rrhelections.com/index.php/2021/04/13/alaska-presidential-results-by-county-equivalent-1960-2020/9/
countydat <- read.csv("countydat.csv")

#state.list is now just used for the drop-down and palette
state.list <- read.csv("statefips.csv")[1:50,]
state.list <- rbind(c("none","N",0,7), state.list, c("Extra1","E1",81,5), c("Extra2","E2",82,6), c("Extra3","E3",83,3))
state.list$STATEFP <- as.numeric(state.list$STATEFP)
state.list$COLOR <- as.numeric(state.list$COLOR)

shape <- sp::merge(shape,countydat[,c(1:3,5:8)])
shape <- sp::merge(shape,state.list[,c(3,4)])
shape[shape$STATEFP == 66,] <- c(0,0,"01802705","none","none",0,0,0,0,7)
shape[shape$COUNTYNS == "01805241",] <- c(81,0,"01805241","none","Extra1",0,0,0,0,5)
shape[shape$COUNTYNS == "01805242",] <- c(82,0,"01805242","none","Extra2",0,0,0,0,6)
shape[shape$COUNTYNS == "01805240",] <- c(83,0,"01805240","none","Extra3",0,0,0,0,3)
shape$COLOR[shape$STATEFP == 11] <- 6  #give DC a color

#COUNTYFP will have lots of contradictions and is not to be used
#shape$COUNTYNS <- as.factor(shape$COUNTYNS)   #COUNTYNS __MUST_NOT__ be a number
shape$POP <- as.numeric(shape$POP)
shape$R <- as.numeric(shape$R)
shape$D <- as.numeric(shape$D)
shape$TOTAL <- as.numeric(shape$TOTAL)

shape$COLOR <- as.factor(shape$COLOR)
factpal <- colorFactor(c("yellow","green","blue","purple","red","brown","grey"), shape$COLOR)

js <- paste(
    "$(document).on('keydown', function(event){",
    "  var key = event.which;",
    "  if(key === 67){",
    "    Shiny.onInputChange('cKey', new Date());",
    "  } else if(key === 86){",
    "    Shiny.onInputChange('vKey', new Date());",
    "  }",
    "});"
)

ui <- fluidPage(
    tags$head(tags$script(HTML(js))),
    shinydashboard::box(width = 11, title = "click on counties",
                        
        column(width = 2, 
               textOutput("text"),
               fluidRow(
                   column(width=5,
                       bsButton( inputId = "pick",
                                 icon = icon( name = "eye-dropper"),
                                 label = "Picker (C)")),
                   column(width=5,
                       bsButton( inputId = "paint",
                                 icon = icon( name = "paint-brush"), 
                                 label = "Painter (V)"))),
               fluidRow(
                   column(width=5,
                       bsButton( inputId = "info",
                                 icon = icon( name = "receipt"), 
                                 label = "Show Info")),
                   column(width=5,
                       bsButton( inputId = "fill",
                                 icon = icon( name = "square"), 
                                 label = "Fill State"))),
               selectInput("choose","State selection",state.list$STATE),
               bsButton( inputId = "lights",
                         icon = icon( name = "star"),
                         label = "Show City Lights")
        ),
        
        column(width = 9,
               leafletOutput("map", height=800,width=1400)
        )
    )
)

server <- function(input, output, session) {
    
    apportion <<- function(shape){
        #reset for repeat calls
        shape$STATEPOP <- NULL
        shape$EVOTES <- NULL
        shape$PARTY <- NULL
        shape$MARGIN <- NULL
        
        statepops <- aggregate(shape$POP,by=list(Category=shape$STATE),FUN=sum)
        colnames(statepops) <- c("State","Pop")
        
        #all population of the "none" state is disregarded
        statepops$Pop[statepops$State=="none"] <- 0
        
        test <- statepops
        test$Pop[test$State=="District of Columbia"] <- 0
        DCtest <- statepops #this is for finding DC's 'true' number of seats
        
        test <- test[test$Pop!=0,]
        DCtest <- DCtest[DCtest$Pop!=0,]
        
        test$Senate <- 2
        DCtest$Senate <- 2
        
        test$House <- 1
        DCtest$House <- 1
        
        seats <- 435 - sum(test$House)
        
        while ( seats > 0 ) {
            test$A <- test$Pop / (sqrt(test$House*(test$House+1)))
            test$House[test$A == max(test$A)] <- test$House[test$A == max(test$A)] + 1
            seats <- seats - 1
        }
        
        seats <- 435 - sum(DCtest$House)
        
        while ( seats > 0 ) {
            DCtest$A <- DCtest$Pop / (sqrt(DCtest$House*(DCtest$House+1)))
            DCtest$House[DCtest$A == max(DCtest$A)] <- DCtest$House[DCtest$A == max(DCtest$A)] + 1
            seats <- seats - 1
        }
        
        test$Total <- test$Senate + test$House
        DCtest$Total <- DCtest$Senate + DCtest$House
        
        DCscore1 <- min(test$Total)
        DCscore2 <- max(DCtest$Total[DCtest$State=="District of Columbia"],0)
        DCscore <- min(DCscore1,DCscore2)
        
        both <- merge(test,DCtest,by=c("State","Pop"),all=TRUE)[,c(1,2,6,10)]
        both[is.na(both)] <- 0
        both$Total.x[both$State=="District of Columbia"] <- DCscore
        
        result <- both[,c(1:3)]
        colnames(result) <- c("STATE","STATEPOP","EVOTES")
        
        result <- sp::merge(shape,result,by="STATE",all=TRUE)
        result$STATEPOP[is.na(result$STATEPOP)] <- 0
        result$EVOTES[is.na(result$EVOTES)] <- 0
        
        #also find which party wins each state
        stateRs <- aggregate(shape$R,by=list(Category=shape$STATE),FUN=sum)
        stateDs <- aggregate(shape$D,by=list(Category=shape$STATE),FUN=sum)
        stateTs <- aggregate(shape$TOTAL,by=list(Category=shape$STATE),FUN=sum)
        colnames(stateRs) <- c("STATE","R")
        colnames(stateDs) <- c("STATE","D")
        colnames(stateTs) <- c("STATE","TOTAL")
        RD <- merge(merge(stateRs,stateDs,by="STATE"),stateTs,by="STATE")
        RD$PARTY <- "R"
        RD$PARTY[RD$R < RD$D] <- "D"
        RD$MARGIN <- abs((RD$R-RD$D)/RD$TOTAL)
        RD$TOTAL <- NULL
        
        shape <- sp::merge(result,RD[,c(1,4,5)],by="STATE",all=TRUE)
        
        dis <- distinct(shape@data,STATE,.keep_all = TRUE)[,c("STATE","EVOTES","PARTY")]
        Rv <- sum(dis$EVOTES[dis$PARTY=="R"], na.rm=TRUE)
        Rs <- length(which(dis$EVOTES[dis$PARTY=="R"]!=0))
        Dv <- sum(dis$EVOTES[dis$PARTY=="D"], na.rm=TRUE)
        Ds <- length(which(dis$EVOTES[dis$PARTY=="D"]!=0))
        output$text <- renderText(paste0("R: ",Rv," (",Rs,")\n D: ",Dv," (",Ds,")"))
        
        return(shape)
    }
    
    shape <<- apportion(shape)
    
    chosen.tool <<- "pick"
    chosen.state <<- "none"
    lights.on <<- TRUE
    
    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(doubleClickZoom=FALSE,zoomControl = FALSE)) %>%
            setView(lat = 38, lng = -96.5, zoom = 5) %>%
            addPolygons(data = shape, 
                        fillColor = ~factpal(shape$COLOR),
                        color = "black",
                        weight = 1,
                        layerId = ~COUNTYNS)
    })
    
    reset <<- function(session){
        updateButton(session,inputId = "pick",style = "default")
        updateButton(session,inputId = "paint",style = "default")
        updateButton(session,inputId = "info",style = "default")
        updateButton(session,inputId = "fill",style = "default")
        
        #this is makes everything slow for a few seconds
        #its purpose is to remove info boxes when not using the info tool
        if(chosen.tool == "info") {
            leafletProxy(mapId = "map") %>%
                addPolygons(data = shape,
                            fillColor = ~factpal(COLOR),
                            color = "black",
                            weight = 1,
                            layerId = ~COUNTYNS)
        }
    }
    
    # choose a tool
    observeEvent( list(input$paint,input$vKey), {
        reset(session)
        chosen.tool <<- "paint"
        updateButton(session,inputId = "paint",style = "warning")
    })
    observeEvent( list(input$fill), {
        reset(session)
        chosen.tool <<- "fill"
        updateButton(session,inputId = "fill",style = "warning")
    })
    observeEvent( list(input$lights), {
        if (lights.on == FALSE) {
            lights.on <<- TRUE
            updateButton(session,inputId = "lights",style = "warning")
            leafletProxy(mapId = "map") %>%
                addProviderTiles("NASAGIBS.ViirsEarthAtNight2012",options=providerTileOptions(opacity=.5),layerId="light.back")
        } else {
            lights.on <<- FALSE
            updateButton(session,inputId = "lights",style = "default")
            leafletProxy(mapId = "map") %>%
                removeTiles(layerId="light.back")
        }
    })
    observeEvent( list(input$info), {
        chosen.tool <<- "info"
        updateButton(session,inputId = "paint",style = "default")
        updateButton(session,inputId = "pick",style = "default")
        updateButton(session,inputId = "fill",style = "default")
        updateButton(session,inputId = "info",style = "warning")
    })
    observeEvent( list(input$pick,input$cKey), {
        reset(session)
        chosen.tool <<- "pick"
        updateButton(session,inputId = "pick",style = "warning")
    })
    
    #choose a state from the drop-down
    observeEvent( input$choose, {
        chosen.state <<- input$choose
    })
    
    # click on counties
    observeEvent( input$map_shape_click, {
        click <- input$map_shape_click
        
        if (chosen.tool == "pick") {
            chosen.state <<- shape$STATE[shape$COUNTYNS == click$id]
            updateSelectInput(session,inputId = "choose",selected = chosen.state)
        }
        if (chosen.tool == "paint") {
            shape$STATE[shape$COUNTYNS == click$id] <<- chosen.state
            shape$STATEFP[shape$STATE == chosen.state] <<- state.list$STATEFP[state.list$STATE == chosen.state]
            shape$COLOR[shape$STATE == chosen.state] <<- state.list$COLOR[state.list$STATE == chosen.state]
            
            shape <<- apportion(shape)
            
            leafletProxy(mapId = "map") %>%
                addPolygons(data = shape[which(shape$COUNTYNS == click$id), ],
                            fillColor = ~factpal(COLOR),
                            color = "black",
                            weight = 1,
                            layerId = ~COUNTYNS)
        }
        if (chosen.tool == "fill") {
            target.state <- shape$STATE[shape$COUNTYNS == click$id]
            shape$STATE[shape$STATE == target.state] <<- chosen.state
            shape$STATEFP[shape$STATE == chosen.state] <<- state.list$STATEFP[state.list$STATE == chosen.state]
            shape$COLOR[shape$STATE == chosen.state] <<- state.list$COLOR[state.list$STATE == chosen.state]
            
            shape <<- apportion(shape)
            
            leafletProxy(mapId = "map") %>%
                addPolygons(data = shape[which(shape$STATE == chosen.state), ],
                            fillColor = ~factpal(COLOR),
                            color = "black",
                            weight = 1,
                            layerId = ~COUNTYNS)
        }
    })
    
    # hover to see info
    old_id <<- -1
    observeEvent(input$map_shape_mouseover$id, {
        hover <- input$map_shape_mouseover$id
        this <- shape[shape$COUNTYNS == hover,]
        
        pop <- formatC(this$POP,format="f",big.mark=",",digits=0)
        Rp <- label_percent()(this$R/this$TOTAL)
        Dp <- label_percent()(this$D/this$TOTAL)
        spop <- formatC(this$STATEPOP,format="f",big.mark=",",digits=0)
        margin <- label_percent()(this$MARGIN)
        
        label <- paste0("County Name: ",this$NAME,"<br>",
                        "Population: ",pop,"<br>",
                        "R votes: ",this$R," (",Rp,")<br>",
                        "D votes: ",this$D," (",Dp,")<br>",
                        "<br>",
                        "Current State: ",this$STATE,"<br>",
                        "State Population: ",spop,"<br>",
                        "Electoral Votes: ",this$EVOTES,"<br>",
                        "Favored Party: ",this$PARTY," (+",margin,")")
        
        if (old_id != hover & chosen.tool == "info") {
            leafletProxy(mapId = "map") %>%
                addPolygons(data = shape[which(shape$COUNTYNS == hover), ],
                            fillColor = ~factpal(COLOR),
                            color = "black",
                            weight = 1,
                            layerId = ~COUNTYNS,
                            label = HTML(label),
                            highlightOptions=highlightOptions(weight=3))
        }
        old_id <<- hover
        
    })
}

shinyApp(ui = ui, server = server)
#library(rsconnect)
#rsconnect::deployApp()
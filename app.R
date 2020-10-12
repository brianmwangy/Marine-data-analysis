
#############loading required libraries##############
library(shiny)
library(shiny.semantic)
library(shinyWidgets)
library(leaflet)
library(htmltools)
library(readr)
library(dplyr)
library(rsconnect)


##############loading dataset#################
marine_data<-read_csv("./www/ships.csv")
shipport<-unique(marine_data$PORT)


#################module for dropdown inputs##############
selectVarInput<-function(id){
    ns<-NS(id)
    ##############page UI layout############
             tags$div(
                 class="ui grid",
                 tags$div(
                     class="three column row",
                     
                     ###############port dropdown UI###############
                     tags$div(
                         class="column",
                         selectizeInput(ns("vport"),h3("Select port"),
                                        choices =shipport)
                     ),
                     #################Vessel type dropdown UI############
                     tags$div(
                         class="column",
                         selectizeInput(ns("vtype"),h3("Select vessel type"),
                                        choices =c("Cargo","Tanker","Unspecified","Tug","Fishing","Passenger","Pleasure","Navigation","High Special"))
                     ),
                     
                     #################vessel name dropdown UI#############
                     tags$div(
                         class="column",
                         selectizeInput(ns("vname"),h3("Select vessel name"),choices = NULL)
                         
                     )
                 ),
                 ######################port map UI######################
                 tags$div(
                     class="row",
                     tags$div(
                         class="sixteen wide column",
                         leafletOutput(ns("map"),height=300)
                     )
                 ),
                 #####################data table UI###################
                 tags$div(
                     class="row",
                     tags$div(
                         class="sixteen wide column",
                         dataTableOutput(ns("table"))
                         
                     )
                 )
             )

}


#######################server module#####################
selectvaroutput<-function(id){
    moduleServer(id, function(input, output,session){
        
        ##################### vessel type reactive function#################
        shiptype<-reactive({
            marine_data %>% filter(ship_type==input$vtype & PORT==input$vport)
        })
        
        ####################vessel name reactive function#####################
        shipname<-reactive({
            shiptype() %>% filter(SHIPNAME==input$vname)
        })
        
        ######### vessel name update event function############################
       observeEvent(input$vtype,{
           updateSelectizeInput(session, "vname", choices = unique(shiptype()$SHIPNAME[shiptype()$ship_type==input$vtype]))
       })
       
      #########color function for vessel type########################
       pal=colorFactor("plasma",domain = marine_data$ship_type)
       
       #############vessel map rendering#####################
       output$map<-renderLeaflet({
           leaflet() %>%
               addTiles() %>%
               addCircleMarkers(
                   data = shipname(),
                   fillColor = ~pal(ship_type),
                   fillOpacity = 1.0,
                   radius = 8,
                   stroke = FALSE,
                   lat = ~LAT,
                   lng = ~LON,
                   label=paste(
                       "<strong>Vessel name: </strong>",shipname()$SHIPNAME,"<br>",
                       "<strong>Vessel type: </strong>",shipname()$ship_type,"<br>"
                   ) %>% lapply(htmltools::HTML)
              )
       })
       
       ################port data table output function###############
       output$table<-renderDataTable(
           shipname(),
           options = list(pageLength = 10)
       )
       
    })
}



# Define UI for application 
ui <- semanticPage(
    h1("Marine data analysis"),
    selectVarInput("var")
)

# Define server logic 
server <- function(input, output,session) {
   selectvaroutput("var")
}

# Run the application 
shinyApp(ui = ui, server = server)

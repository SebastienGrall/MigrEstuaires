#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)
library(stringr)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(spsComps)
library(sf)

df<-sf::read_sf("data/Migrateurs_estuaires.gpkg")
data_df<-as.data.frame(df)
data_df<-select(data_df,1:3)
data_df$Enjeux<-factor(data_df$Enjeux, levels = c("Très fort","Fort" ,"Moyen","Faible","Très faible"))
reco_pertu<-read.csv2("data/Tableau_activites_perturbations_recommandations.csv")


ui <-shinydashboard::dashboardPage( title="MigrEstuaires",
                                    header = shinydashboardPlus::dashboardHeader(title= tags$a(href="https://seinormigr.fr", target="_blank",
                                                                                               tags$img(height = "40px", alt="MigrEstuaires",  src="favicon.ico", width = "40px") ),
                                                                                 
                                                                                 leftUi = tagList()
                                    ),
                                    body=shinydashboard::dashboardBody(
                                      tags$script(
                                        HTML(
                                          'var e = document.querySelector("body > div.wrapper > header > nav > div:nth-child(4) > ul > li > a > i");
           e.setAttribute("style", "display: none;");'
                                        )
                                      ),
                                      
                                      tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #568ef6;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #568ef6;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #568ef6;
                              }

                              ')),
                                                tags$link(rel = "shortcut icon", href = "favicon.ico")),
                                      
                                      fluidRow(
                                        shinydashboardPlus::box(title ="Cartographie",
                                                                                          collapsible = TRUE,
                                                                                          width = 6,
                                                                
                                                                leaflet::leafletOutput("map"),height = 800),
                                                                  
                                                                  shinydashboardPlus::box(title ="Enjeux migrateurs",
                                                                                          collapsible = TRUE,
                                                                                          width = 4,
                                                                                          DTOutput("myTable")),
                                                                  shinydashboardPlus::box(title ="Calendrier des migrations",
                                                                                          collapsible = TRUE,
                                                                                          width = 8,
                                                                                          imageOutput("myImage")),
                                                                  shinydashboardPlus::box(title ="Perturbations et recommandations de gestion",
                                                                                          collapsible = TRUE,
                                                                                          width = 10,
                                                                                          DTOutput("table_reco"))
                                          )),
                                    
                                    sidebar =   shinydashboard::dashboardSidebar(disable = TRUE)
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
    data <- reactiveValues(clickedMarker=NULL)
  
  # produce the basic leaflet map with single marker
  
  FishIcon <- leaflet::makeIcon(
    iconUrl = "www/fish_works.png",
    iconWidth = 50,
  )
  
  
  output$map <- leaflet::renderLeaflet(
    leaflet::leaflet(df) %>%
      leaflet::addTiles() %>%
      leaflet::addControl(actionButton("reset_button","Vue générale"),position="topright") %>%
      leaflet::setView(lat=49,lng=0.3,zoom=7) %>%
      leaflet::addMarkers(icon=FishIcon, layerId = df$Estuaire,
                          popup = paste("Esuaires : ", df$Estuaire#, "<br>",
                                        #"Localisation : ", df$ouv_localisation
                          ))
  )
  
  observe({
    click <- input$map_marker_click
    zoom <- isolate(input$map_zoom)
    if(is.null(click))
      return()
    
    leaflet::leafletProxy('map') %>%
      leaflet::setView(click$lng, click$lat, zoom = 15)
  })
  
  observeEvent(input$map_marker_click,{
    print("observed map_marker_click")
    data$clickedMarker <- input$map_marker_click
    print(data$clickedMarker)
    
  })
  
  observeEvent(input$reset_button,{
    leaflet::leafletProxy('map') %>%
      leaflet::setView(lat=49,lng=0.3,zoom=7)
  })
  
  output$myTable <- renderDT({
    return(if(is.null(data$clickedMarker$id)){NULL}else{
      df<-subset(data_df,Estuaire==data$clickedMarker$id)
      df<-df[order(df$Enjeux),]
      df %>% 
        datatable(rownames=F,options = list(
          dom='t'
        )
          ) %>%
        formatStyle(
        'Enjeux',
        target = 'row',
        backgroundColor = styleEqual(c("Très fort","Fort","Moyen","Faible","Très faible"), c('red','orange','yellow','green','lightskyblue')))}
    )
  })
  
  output$myImage <- renderImage({
    
    # Return a list containing the filename
    list(src = "data/Calendrier_migration.png",
         contentType = 'image/png',
         width = 1000,
         height = 350,
         alt = "This is alternate text")
  }, deleteFile = FALSE)

  output$table_reco <- renderDT({
    reco_pertu%>% 
      datatable(rownames=F,options = list(
        dom='t'
      ),escape = F)
  })
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)

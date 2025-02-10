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
library(ggplot2)
library(cowplot)
library(reactable)

df<-sf::read_sf("data/Migrateurs_estuaires_wgs84.gpkg")
liaison_estuaires<-sf::read_sf("data/Liaisons_petits_estuaires.gpkg")

df <- df %>% st_as_sf( coords = c("X", "Y")) %>%
  st_set_crs("EPSG:2154") %>%
  st_transform(4326)

df <- df %>% mutate(
  SPP = case_when(
    SPP == "SAT" ~ "Saumon atlantique",
    SPP == "TRM" ~ "Truite de mer",
    SPP == "ALA" ~ "Aloses",
    SPP == "LPM" ~ "Lamproie marine",
    SPP == "LPF" ~ "Lamproie fluviatile",
    SPP == "ANG" ~ "Anguille européenne"
  )
)

liaison_estuaires <- liaison_estuaires %>%
  st_as_sf( coords = c("X", "Y")) %>%
  st_cast("MULTILINESTRING") %>%
  st_set_crs("EPSG:2154") %>%
  st_transform(4326)
data_df<-as.data.frame(df)
data_df<-select(data_df,1:3)
data_df <- data_df %>%  rename(Espèces = SPP)
data_df$Enjeux<-factor(data_df$Enjeux, levels = c("Très fort","Fort" ,"Moyen","Faible","Très faible"))
reco_pertu<-read.csv2("data/Tableau_activites_perturbations_recommandations.csv")
colnames(reco_pertu)<-c("Activité","Facteurs de perturbation","Impacts potentiels","Recommandations et Orientations","Type d'activités")
liste_estuaires<-unique(data_df$Estuaire)


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
                                        shinydashboardPlus::box(title ="Fonctionnement",
                                                                collapsible = TRUE,
                                                                width = 3,
                                                                uiOutput("fonctionnement"),
                                                                downloadButton("download_notice", "Télécharger la note méthodologique")),
                                        shinydashboardPlus::box(title ="Cartographie",
                                                                collapsible = TRUE,
                                                                width = 4,
                                                                leaflet::leafletOutput("map"),
                                                                selectizeInput("estuaires",
                                                                               label = "Choix de l'estuaire",
                                                                               choices = sort(liste_estuaires),
                                                                               selected = NULL,
                                                                               multiple = FALSE,
                                                                               options = list(
                                                                                 placeholder = 'Choisissez un estuaire',
                                                                                 onInitialize = I('function() { this.setValue(""); }')
                                                                               ))),
                                        
                                        shinydashboardPlus::box(title ="Enjeux migrateurs",
                                                                collapsible = TRUE,
                                                                width = 3,
                                                                DTOutput("myTable")),
                                        shinydashboardPlus::box(title ="Calendrier des migrations",
                                                                collapsible = TRUE,
                                                                width = 10,
                                                                downloadButton("download_graphique", "Télécharger"),
                                                                plotOutput("calendrier_mig")
                                        ),
                                        shinydashboardPlus::box(title ="Perturbations et recommandations de gestion",
                                                                collapsible = TRUE,
                                                                width = 10,
                                                                downloadButton("download_table_perturbation", "Télécharger"),
                                                                #DTOutput("table_reco")
                                                                reactable::reactableOutput("table_reco")
                                        )
                                      )),
                                    
                                    sidebar =   shinydashboard::dashboardSidebar(disable = TRUE)
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  data <- reactiveValues(clickedMarker=NULL)
  
  # produce the basic leaflet map with single marker
  
  # FishIcon <- leaflet::makeIcon(
  #   iconUrl = "www/fish_works.png",
  #   iconWidth = 50,
  # )
  
  output$fonctionnement<-renderText(HTML(paste("
<b>Cette application a été développée et réalisée par SEINORMIGR.</b><br><br>
Les estuaires, et les embouchures plus généralement, sont des passages obligés pour les poissons grands migrateurs dans l’accomplissement de leurs cycles biologiques. Certains individus (Anguilles, Lamproies fluviatiles, Truites de mer, Aloses) peuvent même être présents dans les estuaires de façon quasi permanente (hors reproduction) pour se nourrir.
<br><br>
Cet outil est principalement destiné à orienter les porteurs de projets en estuaires afin de définir au mieux les enjeux associés aux poissons migrateurs dans le but de limiter au maximum les impacts sur ces populations.
<br><br>
Pour une bonne utilisation, consultez les documents de la manière suivante :<br>

 <b> 1.Choisissez un estuaire pour connaître les niveaux d’enjeux par espèce associés <br>
     2.Consulter le calendrier de présence des espèces <br>
     3.Consulter le tableau des recommandations en fonction de l’activité</b><br><br>


Une notice méthodologique décrivant les étapes de réalisation des différents documents est disponible en téléchargement ci-dessous.<br>

Cette notice contient également la carte globale des enjeux migrateurs des estuaires normands.")))
  
  output$map <- leaflet::renderLeaflet(
    leaflet::leaflet() %>%
      leaflet::addPolylines(data=liaison_estuaires,color = "black",opacity = 1,weight = 2) %>%
      leaflet::addTiles() %>%
      leaflet::addControl(actionButton("reset_button","Vue générale"),position="topright") %>%
      leaflet::setView(lat=49.5,lng=0.3,zoom=7) %>%
      leaflet::addMarkers(data=df,
                          # icon=FishIcon,
                          layerId = df$Estuaire,
                          popup = if_else(df$Estuaire == "Petits havres du Cotentin",paste("Estuaire : ", df$Estuaire, "<br>",
                                                                                       "Cours d'eau : la Gerfleur, l'Olonde, la Dure,
                                                                                       l'Ay, le Thar"),
                          if_else(df$Estuaire == "Côtiers Seino-Marins",paste("Estuaire : ", df$Estuaire, "<br>",
                                                                               "Cours d'eau : la Valmont, la Durdent, la Saâne, la Scie, l'Yères"),
                          paste("Estuaire : ", df$Estuaire))
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
      leaflet::setView(lat=49.5,lng=0.3,zoom=7)
  })
  
  observeEvent(input$estuaires,{
    df <- df %>% mutate(lon = sf::st_coordinates(.)[,1],
                        lat = sf::st_coordinates(.)[,2])
    choix<-subset(df,Estuaire == input$estuaires)
    choix<- choix %>% distinct(Estuaire,lat,lon)
    leaflet::leafletProxy('map') %>%
      leaflet::setView(lat = choix$lat, lng = choix$lon ,zoom=10)
  },ignoreInit = TRUE)
  
  observeEvent(input$map_marker_click,{
    if(!is.null(data$clickedMarker$id)){
      updateSelectizeInput(session,"estuaires",choices = sort(liste_estuaires),selected = data$clickedMarker$id)
    }
  })
  
  output$myTable <- renderDT({
    
    return(if(input$estuaires==""){NULL}else{
      df<-subset(data_df,Estuaire==input$estuaires)
      df<-df[order(df$Enjeux),]
      df %>% 
        datatable(rownames=F,extensions = "Buttons",options = list(
          dom='Bt',
          buttons=list(
          list(extend="excel",
               filename = "table enjeux"))
        )) %>%
        formatStyle(
          'Enjeux',
          backgroundColor = styleEqual(c("Très fort","Fort","Moyen","Faible","Très faible"), c('red','orange','yellow','green','lightskyblue')))}
    )
  })
  
  output$calendrier_mig<-renderPlot({
    #cowplot::ggsave2(file="data/calendrier_migration.png", plot = plotInput(), device = "png",width = 50, height = 21, units= "cm")
    print(plotInput())
  })
  
  
   plotInput<- reactive({
    
    calendrier<-read.csv2("data/calendrier_migration.csv",fileEncoding = "WINDOWS-1252")
    calendrier$Type.de.migration<-as.factor(calendrier$Type.de.migration)
    
    calendrier$debut.periode <- as.Date(calendrier$debut.periode,format = "%d/%m/%Y")
    calendrier$fin.periode <- as.Date(calendrier$fin.periode,format = "%d/%m/%Y")
    
    # calendrier <- calendrier %>% 
    #   mutate(niveau.de.presence = str_wrap(niveau.de.presence, width = 15))
    
    fills<-c("présence anectodique"="white","présence faible"="#B9DDF1","présence forte"="#5586B3","présence maximale"="#2A5783",
             "présence annuelle dans le \"grand estuaire\""="#98D688")
    # fills<-str_wrap(fills, width = 15)
    
    esp_plot<-as.data.frame(unique(calendrier$Especes))
    esp_plot$niveau <-c("15","12","9.5","7","4.5","2")
    esp_plot$niveau<-as.numeric(esp_plot$niveau)
    colnames(esp_plot)<-c("Especes","niveau")
    
    sens_mig<- calendrier %>% distinct(Type.de.migration,niveau)
    
    p_right <-  ggplot(calendrier ,aes(xmin=debut.periode,xmax=fin.periode,
                                       ymin=niveau-1,
                                       ymax=niveau ))+
      geom_rect(aes(fill=niveau.de.presence),color="black")+ theme_bw()+
      geom_hline(yintercept = c(0:16),color="black")+
      guides(fill=guide_legend("",label.theme = element_text(size = 10, lineheight = 0.8)))+
      scale_fill_manual(values = fills) + scale_x_date(breaks = "month",date_labels = "%d %b",
                                                       expand = (add = c(0.02, 0.2)))+
      scale_y_discrete(breaks = "null")+
      theme(legend.position="bottom")+
      xlab(NULL) + ylim(0,17) + 
      theme(axis.text.y = element_blank(),
            text = element_text(size=15),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.grid.major.y  = element_blank(),
            panel.grid.minor.y  = element_blank(),
            plot.margin = margin(0, -5, 5, -5, "pt"),
            panel.border = element_blank(),
            legend.text = element_text(
              margin = margin(r = 30, l = 5, unit = "pt")))
    
    p_middle <- ggplot(sens_mig, aes(x = 1, y = niveau-0.5)) +
      geom_hline(yintercept = c(0:16),color="black")+
      geom_text(aes(label = Type.de.migration), hjust = 0) + theme_bw() +
      scale_y_discrete() + ylim(0,17) +
      scale_x_continuous(expand = expansion(add = c(0.5, 1))) +
      labs(y = NULL, x = NULL) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.grid = element_blank(),
            plot.margin = margin(0, -5, 5, -5, "pt"),
            panel.border = element_blank())
    
    p_left <- ggplot(esp_plot, aes(x = 1, y = niveau-0.5)) +
      geom_hline(yintercept = c(0,3,5,8,10,13,16),color="black")+
      geom_text(aes(label = Especes), hjust = 0) + theme_bw()+
      scale_y_discrete() + ylim(0,17) +
      scale_x_continuous(expand = expansion(add = c(0.05, 0.5)))+
      labs(x = NULL, y = NULL) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            panel.grid = element_blank(),
            plot.margin = margin(0, -5, 5, -5, "pt"),
            panel.border = element_blank())
    
    calendar <- plot_grid(p_left, p_middle, p_right,
                          nrow = 1, rel_widths = c(0.1, 0.1, 0.8),
                          align = "h"
    )
    
    
    return(calendar)
    
  })
  
  
  output$table_reco <- reactable::renderReactable(
    reco_pertu %>%
      reactable::reactable(
        groupBy = "Type d'activités",
        columns = list(
          `Facteurs de perturbation` = colDef(html = TRUE),
          `Impacts potentiels` = colDef(html = TRUE),
          `Recommandations et Orientations` = colDef(html = TRUE)
        )
      )
  )
  
  output$download_table_perturbation <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      "tableau perturbations.xlsx"
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      file.copy("data/Activites_recommandations_VF.xlsx", file, overwrite=TRUE)
    }
  )
  
  
  output$download_graphique <- downloadHandler(
    filename = function() { "calendrier_migration.png" },
    content = function(file) {
      file.copy("data/calendrier_migration.png", file, overwrite=TRUE)
    }
  )
  
  output$download_notice <- downloadHandler(
    filename = function() { "Note_methodologique.pdf" },
    content = function(file) {
      file.copy("data/Note_methodologique_VF.pdf", file, overwrite=TRUE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)


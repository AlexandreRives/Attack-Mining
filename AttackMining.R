##################################
# Attack Mining                  #
#                                #
# by Elisa Frintz                #
#    Jacky Madi-Corodji          #
#    Alexandre Rives             #
#                                #
##################################

# vidage de la memoire
rm(list=ls())

# Fonction de verification pour installation des packages
packages = c("leaflet", "shinydashboard", "shinycssloaders", "shiny", "shinyWidgets", "DT", "leaflet.extras", 
             "readr", "rAmCharts", "dplyr", "wordcloud")

#Check des packages
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Partie front
ui <- shinyUI(fluidPage(
  
  # Charger le style
  includeCSS("www/style.css"),
  
  # Page complete
  dashboardPage(
    
    skin = "black",
    
    # Header
    dashboardHeader(title="Attack Mining", titleWidth = 300),
    
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       menuItem("Importation des données", tabName = "tableau", icon = icon("table")),
                       menuItem("Carte des IP", tabName = "carte", icon = icon("map-marked-alt")),
                       menuItem("Statistiques descriptives", tabName = "analyse", icon = icon("stats", lib="glyphicon")),
                       menuItem("Analyse par IP source", tabName = "ZoomIP", icon = icon("stats", lib="glyphicon")),
                       HTML(paste0(
                         "<br><br><br><br><br><br><br><br><br>",
                         "<table style='margin-left:auto; margin-right:auto;'>",
                         "<tr>",
                         "<p style = 'text-align: center;'><small>LinkedIn :</a></small></p>",
                         "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/elisa-frintz' target='_blank'>Elisa Frintz</a></small></p>",
                         "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/jacky-madi-corodji-4a6b9a209' target='_blank'>Jacky Madi Corodji</a></small></p>",
                         "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/rives-alexandre/' target='_blank'>Alexandre Rives</a></small></p>",
                         "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/quentinthomasson/' target='_blank'>Quentin Thomasson</a></small></p>",
                         "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/baptiste-ourdouillié-63aa9b12a/' target='_blank'>Baptiste Ourdouillé</a></small></p>",
                         "</tr>",
                         "</table>",
                         "<br>"),
                       )
                     )),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "tableau",
                h1("Importation des données"),
                fileInput("dataFile",label = NULL,  buttonLabel = "Browse...", placeholder = "No file selected"),
                h2("Aperçu des données"),
                DT::dataTableOutput("apercu_data")
        ),
        
        tabItem(tabName = "carte",
          addSpinner(leafletOutput("carte"), spin = "circle", color = "black")
        ),
        
        tabItem(tabName= "analyse",
                h1("Statistiques descriptives", align="center"),
                fluidRow(
                  column(6, h3("Action réalisées par le firewall", align="center"), amChartsOutput(outputId = "statdesc1")),
                  column(6, h3("Top 10 des ports inférieurs à 1024 avec un accès autorisé", align="center"), amChartsOutput(outputId = "statdesc3"))
                )
        ),
        tabItem(tabName = "ZoomIP",
                h1("Analyse par IP source", align="center"),
                fluidRow(
                  column(6, h3("Top 5 des IP source les plus emetrices", align="center"), amChartsOutput(outputId = "statdesc2")),
                  column(6, h3("Lister les acces des adresses IP non inclues dans le plan d'adressage de l'Université", align="center"), plotOutput(outputId = "wordcloud"))
                )
        )
      )
    )
  )
))

# Partie serveur
server <- shinyServer(function(input, output, session) {
  
  #Affichage des donnees
  df_secu <- reactive({
    if (is.null(input$dataFile))
      return(NULL)
    df <- read.csv(input$dataFile$datapath, sep=";")
    return(df)
  })
  
  output$apercu_data <-  DT::renderDataTable({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    DT::datatable(df, filter = "top") %>% formatStyle(
      'action',
      backgroundColor = styleEqual(c("Permit", "Deny"), c("#85C17E", "#DE2916"))
    )
  })
  
  latitude = c(45.5, 47.5)
  longitude = c(7.5, 7.8)
  
  # Carte
  output$carte <- renderLeaflet({
    # if (is.null(input$dataFile))
    #   return(NULL)
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(lat = latitude, lng = longitude) %>%
      addLayersControl(
        baseGroups = c("Open Street Map"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  output$statdesc1 <- renderAmCharts({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    df_ok = as.data.frame(table(df$action))
    colnames(df_ok) = c("label", "value")
    df_ok$color = c("#85C17E", "#DE2916")
    amPie(data = df_ok, inner_radius = 50, depth = 10, show_values = TRUE)
  })
  
  output$statdesc2 <- renderAmCharts({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    df_ok = as.data.frame(sort(table(df$ipsrc), decreasing = TRUE))
    head(df_ok, 5)
    amBarplot(x = "Var1", y = "Freq", data = head(df_ok, 5), depth = 15, labelRotation = -90)
  })
  
  output$statdesc3 <- renderAmCharts({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    df_ok = df %>% filter(portdst < 1024) %>% filter(action == "Permit")
    df_ok = as.data.frame(sort(table(df$portdst), decreasing = TRUE))
    amBarplot(x = "Var1", y = "Freq", data = head(df_ok, 10), depth = 15, labelRotation = -90)
  })
  
  output$wordcloud <- renderPlot({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    
    adressage = rep("172.20.10.", 255)
    num = 1:255
    adressage_num = paste(adressage,num,sep="")
    
    df_ok = df %>% filter(ipsrc != adressage_num)
    df_ok_ok = as.data.frame(sort(table(df_ok$ipsrc), decreasing = TRUE))
    set.seed(0)
    wordcloud(word = df_ok_ok$Var1, freq = df_ok_ok$Freq, colors = brewer.pal(8, "Dark2"))
  })
  
  
})

shinyApp(ui = ui, server = server)

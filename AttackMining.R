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
packages = c("leaflet", "shinydashboard", "shinycssloaders", "shiny", "shinyWidgets", "DT", "leaflet.extras")

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
                       menuItem("Tableau récapitulatif", tabName = "tableau", icon = icon("table")),
                       menuItem("Carte des IP", tabName = "carte", icon = icon("map-marked-alt")),
                       menuItem("Analyse", tabName = "analyse", icon = icon("file")),
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
        tabItem(tabName = "carte",
          addSpinner(leafletOutput("carte"), spin = "circle", color = "black")
        )
      )
    )
  )
))

# Partie serveur
server <- shinyServer(function(input, output, session) {
  
  latitude = c(45.5, 47.5)
  longitude = c(7.5, 7.8)
  
  # Carte
  output$carte <- renderLeaflet({leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(lat = latitude, lng = longitude) %>%
      addLayersControl(
        baseGroups = c("Open Street Map"),
        position = c("topleft"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
})

shinyApp(ui = ui, server = server)

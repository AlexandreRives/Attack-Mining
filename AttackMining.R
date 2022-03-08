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
packages = c("shinydashboard", "shinycssloaders", "shiny", "shinyWidgets", "DT", 
             "rAmCharts", "dplyr", "highcharter", "lubridate")

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
                       menuItem("Analyse temporelle", tabName = "Time", icon = icon("fal fa-chart-line")),
                       menuItem("Statistiques descriptives", tabName = "analyse", icon = icon("stats", lib="glyphicon")),
                       menuItem("Analyse par IP source", tabName = "ZoomIP", icon = icon("fa-solid fa-circle")),
                       HTML(paste0(
                         "<br><br><br><br><br><br><br><br><br>",
                         "<table style='margin-left:auto; margin-right:auto;'>",
                         "<tr>",
                         "<p style = 'text-align: center;'><small>LinkedIn :</a></small></p>",
                         "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/elisa-frintz' target='_blank'>Elisa Frintz</a></small></p>",
                         "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/jacky-madi-corodji-4a6b9a209' target='_blank'>Jacky Madi Corodji</a></small></p>",
                         "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/rives-alexandre/' target='_blank'>Alexandre Rives</a></small></p>",
                         "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/quentinthomasson/' target='_blank'>Quentin Thomasson</a></small></p>",
                         "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/baptiste-ourdouillié-63aa9b12a/' target='_blank'>Baptiste Ourdouillié</a></small></p>",
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
        
        tabItem(tabName = "Time",
                h1("Analyses temporelles", align="center"),
                fluidRow(
                  column(12, h3("Nombre d'actions en fonction du temps", align="center"), amChartsOutput(outputId = "plottime")),
                  column(12, h3("Ports attaqués en fonction du temps", align="center"), amChartsOutput(outputId = "plottime2"))
                )
                
        ),
        
        tabItem(tabName= "analyse",
                h1("Statistiques descriptives", align="center"),
                fluidRow(
                  column(6, h3("Actions réalisées par le firewall", align="center"), amChartsOutput(outputId = "statdesc1")),
                  column(6, h3("Top 10 des ports inférieurs à 1024 avec un accès autorisé", align="center"), amChartsOutput(outputId = "statdesc3")),
                  column(6, h3("Rapprochements entre les règles et les ports de destination", align = "center"), highchartOutput("wheel")),
                  column(6, h3("Rapprochements entre les règles et les actions (PERMIT/DENY)", align = "center"), highchartOutput("wheel3", width = "100%", height = "400px"))
                  
                )
        ),
        tabItem(tabName = "ZoomIP",
                h1("Analyse par IP source", align="center"),
                fluidRow(
                  column(6, 
                         h3("Top des IP source les plus émettrices", align="center"),
                         sliderInput("top1", "Top :", min = 1, max = 30, value = 5), 
                         highchartOutput("statdesc2")),
                  column(6, 
                         h3("IP source non inclues dans le plan d'adressage", align="center"), 
                         sliderInput("top2", "Top :", min = 1, max = 30, value = 5), 
                         highchartOutput("wordcloud"))
                ),
                h2("Zoom sur une IP source", align="center"),
                fluidRow(
                  column(12, DT::dataTableOutput("apercu_data2")),
                  column(12, highchartOutput("wheel2", width = "100%", height = "600px"))
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
    df = df %>% filter(ipdst == "192.168.0.33")
    return(df)
  })
  
  output$apercu_data <-  DT::renderDataTable({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    
    DT::datatable(df, filter = "top", option = list(pageLenght = 5, autoWidth = TRUE)) %>% 
      formatStyle('action', backgroundColor = styleEqual(c("Permit", "Deny"), c("#85C17E", "#DE2916"))) %>% 
      formatStyle('proto', backgroundColor = styleEqual(c("TCP", "UDP"), c("#FBF2B7", "#D0C07A")))
  })
  
  latitude = c(45.5, 47.5)
  longitude = c(7.5, 7.8)
  
  # Time plot
  
  output$plottime <- renderAmCharts({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    df_ok <- df %>%
      select(date, action)
    
    df_ok <- df_ok %>%
      count(date, action)
    
    tab <- xtabs(n~date + action, data = df_ok)
    typeof(tab)
    tab <- as.data.frame.matrix(tab)
    tab$Date <- rownames(tab)
    rownames(tab) <- NULL
    tab$Date <- ymd_hms(tab$Date)
    amTimeSeries(tab, "Date", c("Deny", "Permit"))
    
  })
  
  output$plottime2 <- renderAmCharts({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    df_ok <- df %>%
      select(date, action)
    
    df_ok <- df %>%
      select(date, portdst)
    
    df_ok <- df_ok %>%
      count(date, portdst)
    
    tab <- xtabs(n~date + portdst, data = df_ok)
    typeof(tab)
    tab <- as.data.frame.matrix(tab)
    tab$Date <- rownames(tab)
    rownames(tab) <- NULL
    tab$Date <- ymd_hms(tab$Date)
    amTimeSeries(tab, "Date", c("22", "21", "80", "3306", "8080", "443", "445", "20", "23"))
    
    
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
  
  output$statdesc2 <- renderHighchart({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    df_ok = as.data.frame(sort(table(df$ipsrc), decreasing = TRUE))
    head(df_ok, 5)
    ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.Var1);}")
    
    df_ok <- head(df_ok, input$top1)
    hchart(df_ok, "bar", hcaes(x = Var1, y = Freq), color = 'navyblue', name = "Fréquence d'apparition", pointWidth = 15) %>%
      hc_yAxis(title = list(text = "Mots")) %>%
      hc_xAxis(title = list(text = "Fréquence")) %>%
      hc_title(text = "Top des IP source les plus fréquentes") %>%
      hc_caption(text = "Ce diagramme peut être trié par région et par catégorie de métiers") %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_plotOptions(series = list(events = list(click = ClickFunction)))
  })
  
  output$statdesc3 <- renderAmCharts({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    df_ok = df %>% filter(portdst < 1024) %>% filter(action == "Permit")
    df_ok = as.data.frame(sort(table(df$portdst), decreasing = TRUE))
    amBarplot(x = "Var1", y = "Freq", data = head(df_ok, 10), depth = 15, labelRotation = -90)
  })
  

  
  output$wordcloud <- renderHighchart({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    
    adressage = rep("192.168.0.", 255)
    num = 1:255
    adressage_num = paste(adressage,num,sep="")
    
    df_ok = df[!df$ipsrc %in% adressage_num,] #%>% filter(ipsrc != adressage_num)
    df_ok_ok = as.data.frame(sort(table(df_ok$ipsrc), decreasing = TRUE))
    df_ok_ok <- head(df_ok_ok, input$top2)
    set.seed(0)
    
    ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.Var1);}")
    
    hchart(df_ok_ok, "wordcloud", hcaes(name = Var1, weight = Freq), name = "Occurence") %>%
      hc_title(text = "IP source non inclues dans le plan d'adressage") %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_plotOptions(
        series = list(events = list(click = ClickFunction)),
        dataLabels = list(enabled = TRUE,
                          format = "{point.Freq}"), 
        minFontSize = 10)
    
  })
  
  output$wheel <- renderHighchart({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    
    dw <- df %>%
      select(ruleid,portdst)
    
    
    dw$count <- 1
    dw <- aggregate(count ~ ., dw, FUN = sum)
    dw_ok <- head(dw[order(dw$count, decreasing = TRUE),], 30)
    
    names(dw_ok)[names(dw_ok) == 'ruleid'] <- 'from'
    names(dw_ok)[names(dw_ok) == 'portdst'] <- 'to'
    names(dw_ok)[names(dw_ok) == 'count'] <- 'weight'
    
    
    highchart()%>%
      hc_add_series(data = dw_ok,
                    name = "COOCCURENCES",
                    type = 'dependencywheel') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_chart(zoomType = "xy")
    
  })
  
  output$wheel2 <- renderHighchart({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    
    dw <- df %>%
      select(ipsrc,portdst)
    
    
    dw$count <- 1
    dw <- aggregate(count ~ ., dw, FUN = sum)
    
    dw_ok_filter <- dw %>%
      filter(ipsrc %in% input$Clicked)
    
    names(dw_ok_filter)[names(dw_ok_filter) == 'ipsrc'] <- 'from'
    names(dw_ok_filter)[names(dw_ok_filter) == 'portdst'] <- 'to'
    names(dw_ok_filter)[names(dw_ok_filter) == 'count'] <- 'weight'
    
    
    highchart()%>%
      hc_add_series(data = dw_ok_filter,
                    name = "COOCCURENCES",
                    type = 'dependencywheel') %>%
      hc_title(text = "Ports atteints par cette adresse IP") %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_chart(zoomType = "xy")
    
  })
  
  output$apercu_data2 <-  DT::renderDataTable({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    
    df_drill <- df %>%
      filter(ipsrc %in% input$Clicked)
    
    DT::datatable(df_drill, filter = "top", option = list(pageLenght = 5, autoWidth = TRUE, lengthMenu = c(5, 10, 15, 20))) %>% 
      formatStyle('action', backgroundColor = styleEqual(c("Permit", "Deny"), c("#85C17E", "#DE2916"))) %>% 
      formatStyle('proto', backgroundColor = styleEqual(c("TCP", "UDP"), c("#FBF2B7", "#D0C07A")))
  })
  
  output$wheel3 <- renderHighchart({
    if (is.null(input$dataFile))
      return(NULL)
    df <- df_secu()
    
    dw <- df %>%
      select(ruleid,action)
    
    
    dw$count <- 1
    dw <- aggregate(count ~ ., dw, FUN = sum)
    dw_ok <- head(dw[order(dw$count, decreasing = TRUE),], 30)
    
    names(dw_ok)[names(dw_ok) == 'ruleid'] <- 'from'
    names(dw_ok)[names(dw_ok) == 'action'] <- 'to'
    names(dw_ok)[names(dw_ok) == 'count'] <- 'weight'
    
    
    highchart()%>%
      hc_add_series(data = dw_ok,
                    name = "COOCCURENCES",
                    type = 'dependencywheel') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_chart(zoomType = "xy")
    
  })
  
  
})


shinyApp(ui = ui, server = server)
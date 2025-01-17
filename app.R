# Instalacija potrebnih paketa
install.packages(c("shiny", "ggplot2", "dplyr", "plotly", "leaflet"))

# Ucitavanje paketa
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)

# UI - Korisnicko sucelje
ui <- fluidPage(
  titlePanel("World Happiness Report Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload World Happiness Report Dataset (CSV)", accept = ".csv"),
      selectInput("response_var", "Select Response Variable:", ""),
      selectInput("predictor_var", "Select Predictor Variable:", ""),
      selectInput("region", "Select Region:", c("All")),
      numericInput("num_bins", "Number of Bins for Histogram (if applicable):", value = 10, min = 1),
      actionButton("analyze", "Analyze")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset Overview", tableOutput("data_preview")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary")),
        tabPanel("Visualization", plotlyOutput("plot")),
        tabPanel("Bivariate Analysis", verbatimTextOutput("bivariate")),
        tabPanel("Top 5/Bottom 5 Countries", 
                 plotlyOutput("top5"),
                 plotlyOutput("bottom5")),
        tabPanel("Regional Summary", tableOutput("region_summary")),
        tabPanel("Map", leafletOutput("map"))
      )
    )
  )
)

# Server - Logika aplikacije
server <- function(input, output, session) {
  
  # Ucitavanje dataset-a
  dataset <- reactive({
    req(input$file)
    tryCatch({
      read.csv(input$file$datapath, sep = ",", encoding = "UTF-8")
    }, error = function(e) {
      showNotification("Error loading file. Please ensure it's a valid CSV file.", type = "error")
      NULL
    })
  })
  
  # Azuriranje opcija za odabir varijabli
  observeEvent(dataset(), {
    req(dataset())
    updateSelectInput(session, "response_var", choices = names(dataset()))
    updateSelectInput(session, "predictor_var", choices = names(dataset()))
    if ("Regional indicator" %in% names(dataset())) {
      updateSelectInput(session, "region", choices = c("All", unique(dataset()$`Regional indicator`)))
    } else {
      showNotification("Column 'Regional indicator' not found in dataset.", type = "error")
    }
  })
  
  # Filtriranje podataka po regiji
  filtered_data <- reactive({
    req(dataset())
    if (input$region == "All") {
      dataset()
    } else {
      dataset() %>% filter(`Regional indicator` == input$region)
    }
  })
  
  # Pregled dataset-a
  output$data_preview <- renderTable({
    req(dataset())
    head(dataset())
  })
  
  # Statistika
  output$summary <- renderPrint({
    req(input$response_var, input$predictor_var)
    data <- filtered_data()
    
    cat("Summary for Response Variable:", input$response_var, "\n")
    print(summary(data[[input$response_var]]))
    
    cat("\nSummary for Predictor Variable:", input$predictor_var, "\n")
    print(summary(data[[input$predictor_var]]))
  })
  
  # Vizualizacije
  output$plot <- renderPlotly({
    req(input$response_var, input$predictor_var)
    data <- filtered_data()
    
    if (is.numeric(data[[input$response_var]]) && is.numeric(data[[input$predictor_var]])) {
      p <- ggplot(data, aes_string(x = input$predictor_var, y = input$response_var)) +
        geom_point(color = "blue", alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE, color = "red") +
        labs(title = paste("Scatterplot:", input$response_var, "vs", input$predictor_var),
             x = input$predictor_var, y = input$response_var) +
        theme_minimal()
      ggplotly(p)
    } else if (is.factor(data[[input$predictor_var]])) {
      p <- ggplot(data, aes_string(x = input$predictor_var, y = input$response_var)) +
        geom_boxplot(fill = "lightblue") +
        labs(title = paste("Boxplot:", input$response_var, "by", input$predictor_var),
             x = input$predictor_var, y = input$response_var) +
        theme_minimal()
      ggplotly(p)
    } else {
      plot_ly(data, x = data[[input$response_var]], type = "histogram", nbinsx = input$num_bins)
    }
  })
  
  # Top 5 i Bottom 5 zemlje
  output$top5 <- renderPlotly({
    req(filtered_data())
    data <- filtered_data() %>%
      arrange(desc(Ladder.score)) %>%
      slice_head(n = 5)
    p <- ggplot(data, aes(x = reorder(`Country name`, -Ladder.score), y = Ladder.score)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 5 Happiest Countries", x = "Country", y = "Happiness Score")
    ggplotly(p)
  })
  
  output$bottom5 <- renderPlotly({
    req(filtered_data())
    data <- filtered_data() %>%
      arrange(Ladder.score) %>%
      slice_head(n = 5)
    p <- ggplot(data, aes(x = reorder(`Country name`, Ladder.score), y = Ladder.score)) +
      geom_bar(stat = "identity", fill = "red") +
      coord_flip() +
      labs(title = "Bottom 5 Least Happy Countries", x = "Country", y = "Happiness Score")
    ggplotly(p)
  })
  
  # Regionalni sazetak
  output$region_summary <- renderTable({
    req(filtered_data())
    filtered_data() %>%
      group_by(`Regional indicator`) %>%
      summarise(
        Avg_Score = mean(Ladder.score, na.rm = TRUE),
        SD_Score = sd(Ladder.score, na.rm = TRUE)
      )
  })
  
  # Mapa
  output$map <- renderLeaflet({
    req(filtered_data())
    data <- filtered_data()
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = data$Longitude, lat = data$Latitude,
        color = ~colorBin("RdYlGn", Ladder.score)(Ladder.score),
        label = ~paste(`Country name`, ":", Ladder.score),
        radius = 5
      )
  })
}

# Pokretanje aplikacije
shinyApp(ui = ui, server = server)



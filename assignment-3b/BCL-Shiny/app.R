library(shiny)
library(tidyverse)
options(shiny.autoreload = TRUE)

# YOU CAN IGNORE THIS: metadata for when sharing the app on facebook/twitter
share <- list(
  title = "BC Liquor Store prices",
  url = "http://daattali.com/shiny/bcl/",
  image = "http://daattali.com/shiny/img/bcl.png",
  description = "Had a long day? This app will help you find the right drink for tonight!",
  twitter_user = "daattali"
)

# load the data (retrieve and clean raw data if this is the first time)
filename <- file.path("data", "bcl-data.csv")
if (file.exists(filename)) {
  bcl <- read.csv(filename, stringsAsFactors = FALSE)
} else {
  bcl <- read.csv("http://pub.data.gov.bc.ca/datasets/176284/BC_Liquor_Store_Product_Price_List.csv",
                  stringsAsFactors = FALSE)
  products <- c("BEER", "REFRESHMENT BEVERAGE", "SPIRITS", "WINE")
  bcl <- dplyr::filter(bcl, PRODUCT_CLASS_NAME %in% products) %>%
    dplyr::select(-PRODUCT_TYPE_NAME, -PRODUCT_SKU_NO, -PRODUCT_BASE_UPC_NO,
                  -PRODUCT_LITRES_PER_CONTAINER, -PRD_CONTAINER_PER_SELL_UNIT,
                  -PRODUCT_SUB_CLASS_NAME) %>%
    rename(Type = PRODUCT_CLASS_NAME,
           Subtype = PRODUCT_MINOR_CLASS_NAME,
           Name = PRODUCT_LONG_NAME,
           Country = PRODUCT_COUNTRY_ORIGIN_NAME,
           Alcohol_Content = PRODUCT_ALCOHOL_PERCENT,
           Price = CURRENT_DISPLAY_PRICE,
           Sweetness = SWEETNESS_CODE)
  bcl$Type <- sub("^REFRESHMENT BEVERAGE$", "REFRESHMENT", bcl$Type)
  dir.create("data", showWarnings = FALSE)
  write.csv(bcl, filename, row.names = FALSE)
}

ui <- fluidPage(
  shinydisconnect::disconnectMessage2(),
  # Ignore this tags$head section, just adding metadata for facebook/twitter sharing
  tags$head(
    tags$link(rel = "shortcut icon", type="image/x-icon", href="http://daattali.com/shiny/img/favicon.ico"),
    # Facebook OpenGraph tags
    tags$meta(property = "og:title", content = share$title),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:url", content = share$url),
    tags$meta(property = "og:image", content = share$image),
    tags$meta(property = "og:description", content = share$description),
    
    # Twitter summary cards
    tags$meta(name = "twitter:card", content = "summary"),
    tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:title", content = share$title),
    tags$meta(name = "twitter:description", content = share$description),
    tags$meta(name = "twitter:image", content = share$image)
  ),
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      h4(
        "Had a long day?  This app will help you find the right drink for tonight! Just use the filters below..."
      ),
      #img(src = "blcLOGO.jpg", height="50%", width="85%", align="left"),
      #br(), br(), br(), br(), br(), br(), br(),
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("sweetnessChoice", label = "Select sweetness", choices = c("All", "High (7-10)", "Medium (4-6)", "Low (0-3)")),
      uiOutput("typeSelectOutput"),
      checkboxInput("filterCountry", "Filter by country", FALSE),
      textInput("searchName", "Search by product name (try to type a word)"),
      conditionalPanel(
        condition = "input.filterCountry",
        uiOutput("countrySelectorOutput")
      ),
      hr(),
      span("Data source:", 
           tags$a("OpenDataBC",
                  href = "https://www.opendatabc.ca/dataset/bc-liquor-store-product-price-list-current-prices")),
      br(),
      span("Learn how to build this app", a(href = "http://deanattali.com/blog/building-shiny-apps-tutorial/", "with Shiny tutorial")),
      br(), br(),
      em(
        HTML("&bull;"),
        span("Originally Created by", a(href = "http://deanattali.com", "Dean Attali")),
        br(),
        HTML("&bull;"),
        span("Edited by", a(href = "https://github.com/tianyica", "Tianyi Zheng")),
        br(),
        HTML("&bull;"),
        span("Code", a(href = "https://github.com/stat545ubc-2020/stat-545b-assignments-tianyica/tree/master/assignment-3b", "on GitHub"))
      )
    ),
    mainPanel(
      h3(textOutput("summaryText")),
      downloadButton("download", "Download results"),
      br(),
      plotOutput("plot"),
      br(), br(),
      plotOutput("splot"),
      br(), br(),
      #tableOutput("filtered")
      DT::dataTableOutput("filtered")
    )
  )
)

server <- function(input, output, session) {
  output$countrySelectorOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })
  
  output$typeSelectOutput <- renderUI({
    selectInput("typeInput", "Product type",
                sort(unique(bcl$Type)),
                multiple = TRUE,
                selected = c("BEER", "WINE"))
  })
   
  output$summaryText <- renderText({
    numOptions <- nrow(filtered())
    startfrom<- min(filtered()$Price)
    if (is.null(numOptions)) {
      numOptions <- 0
    }
    paste0("We found ", numOptions, " options for you", " starting from $", startfrom)
  })
  
  filtered <- reactive({
    filtered <- bcl
    
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    
    filtered <- dplyr::filter(filtered, Type %in% input$typeInput)
    if (input$filterCountry) {
      filtered <- dplyr::filter(filtered, Country == input$countryInput)
    }
    filtered <- dplyr::filter(filtered, Price >= input$priceInput[1],
                            Price <= input$priceInput[2])
    if (input$sweetnessChoice=="Low (0-3)") {
      filtered <- dplyr::filter(filtered, Sweetness >= 0,
                              Sweetness <= 3)
    }
    if (input$sweetnessChoice=="Medium (4-6)") {
      filtered <- dplyr::filter(filtered, Sweetness >= 4,
                              Sweetness <= 6)
    }
    if (input$sweetnessChoice=="High (7-10)") {
      filtered <- dplyr::filter(filtered, Sweetness >= 7,
                              Sweetness <= 10)
    }
    if (input$sweetnessChoice=="All") {
      filtered
    }
    
    if (!is.null(input$searchName)) {
      filtered <- dplyr::filter(filtered, grepl(toupper(input$searchName),Name))
    }
    
    if(nrow(filtered) == 0) {
      return(NULL)
    }
    filtered
  })
  
  output$plot <- renderPlot({
    if (is.null(filtered())) {
      return(NULL)
    }
    
    ggplot(filtered(), aes(Alcohol_Content, fill = Type)) +
      geom_histogram(colour = "black") +
      theme_classic(20)
  })
  
  output$filtered <- DT::renderDataTable({
    filtered()
  })
  
  output$splot <- renderPlot({
    if (is.null(filtered())) {
      return(NULL)
    }
    
    ggplot(filtered(), aes(Alcohol_Content, Sweetness, fill = Type, color = Type)) +
      geom_point() +
      theme_classic(20)
  })
  
  output$filtered <- DT::renderDataTable({
    filtered()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      "bcl-results.csv"
    },
    content = function(con) {
      write.csv(filtered(), con)
    }
  )
}

shinyApp(ui = ui, server = server)

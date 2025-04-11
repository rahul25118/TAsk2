#Install and load required packages
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(readr)
library(lubridate)
library(DT)

#Load and preprocess data
#Convert Order Date to proper date format for filtering and plotting
data_raw <- read_csv("Sample_-_Superstore[1].csv")
data_raw$`Order Date` <- mdy(data_raw$`Order Date`)

#Define the User Interface (UI)
#This UI consists of four dashboard tabs with sidebar menu and input controls
ui <- dashboardPage(
  dashboardHeader(title = "Business Intelligence Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("chart-pie")),
      menuItem("Geographical View", tabName = "geoview", icon = icon("globe")),
      menuItem("Product Performance", tabName = "products", icon = icon("boxes")),
      menuItem("Dataset Explorer", tabName = "explorer", icon = icon("database"))
    ),
    selectInput("input_region", "Choose Region:", choices = c("All Regions", unique(data_raw$Region))),
    dateRangeInput("input_date", "Filter by Order Date:",
                   start = min(data_raw$`Order Date`),
                   end = max(data_raw$`Order Date`))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("salesBox"),
                valueBoxOutput("profitBox"),
                valueBoxOutput("discountBox")
              ),
              fluidRow(
                box(title = "Monthly Sales Trend", plotlyOutput("trendPlot"), width = 12)
              )
      ),
      tabItem(tabName = "geoview",
              fluidRow(
                box(title = "Sales by Region", plotlyOutput("regionSalesPlot"), width = 6),
                box(title = "Profit by Region", plotlyOutput("regionProfitPlot"), width = 6)
              )
      ),
      tabItem(tabName = "products",
              fluidRow(
                box(title = "Category-wise Sales", plotlyOutput("categoryPlot"), width = 12)
              ),
              fluidRow(
                box(title = "Top 10 Products by Profit", DTOutput("topProductsTable"), width = 12)
              )
      ),
      tabItem(tabName = "explorer",
              fluidRow(
                box(title = "Complete Dataset View", DTOutput("dataTable"), width = 12)
              )
      )
    )
  )
)

#Define Server Logic
# This function filters the dataset and produces all outputs for the UI
server <- function(input, output) {
#Reactive dataset filtered by region and date
  dataset_filtered <- reactive({
    data <- data_raw
    if (input$input_region != "All Regions") {
      data <- data %>% filter(Region == input$input_region)
    }
    data %>% filter(`Order Date` >= input$input_date[1], `Order Date` <= input$input_date[2])
  })
  
#KPI Boxes
  output$salesBox <- renderValueBox({
    valueBox(
      value = paste0("$", formatC(sum(dataset_filtered()$Sales, na.rm = TRUE), format = "f", digits = 2)),
      subtitle = "Total Sales",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$profitBox <- renderValueBox({
    valueBox(
      value = paste0("$", formatC(sum(dataset_filtered()$Profit, na.rm = TRUE), format = "f", digits = 2)),
      subtitle = "Total Profit",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$discountBox <- renderValueBox({
    valueBox(
      value = paste0(round(mean(dataset_filtered()$Discount, na.rm = TRUE) * 100, 2), "%"),
      subtitle = "Average Discount",
      icon = icon("percent"),
      color = "orange"
    )
  })
  
#Time-series Plot
  output$trendPlot <- renderPlotly({
    monthly_summary <- dataset_filtered() %>%
      group_by(Month = floor_date(`Order Date`, "month")) %>%
      summarise(Sales = sum(Sales))
    plot_ly(data = monthly_summary, x = ~Month, y = ~Sales, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'forestgreen'))
  })
  
#Region-wise Charts
  output$regionSalesPlot <- renderPlotly({
    sales_by_region <- dataset_filtered() %>%
      group_by(Region) %>%
      summarise(Sales = sum(Sales))
    plot_ly(data = sales_by_region, x = ~Region, y = ~Sales, type = 'bar', marker = list(color = 'skyblue'))
  })
  
  output$regionProfitPlot <- renderPlotly({
    profit_by_region <- dataset_filtered() %>%
      group_by(Region) %>%
      summarise(Profit = sum(Profit))
    plot_ly(data = profit_by_region, x = ~Region, y = ~Profit, type = 'bar', marker = list(color = 'violet'))
  })
  
#Category Pie Chart
  output$categoryPlot <- renderPlotly({
    category_summary <- dataset_filtered() %>%
      group_by(Category) %>%
      summarise(Sales = sum(Sales))
    plot_ly(data = category_summary, labels = ~Category, values = ~Sales, type = 'pie')
  })
  
#Top Products Table
  output$topProductsTable <- renderDT({
    top_products <- dataset_filtered() %>%
      group_by(`Product Name`) %>%
      summarise(Profit = sum(Profit)) %>%
      arrange(desc(Profit)) %>%
      slice_head(n = 10)
    datatable(top_products)
  })
  
#Raw Dataset View
  output$dataTable <- renderDT({
    datatable(dataset_filtered())
  })
}

#Launch the Shiny App
shinyApp(ui, server)

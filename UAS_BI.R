#UAS BI
#Data Analyst at an e-commerce company
#MULTPLE REGRESSION
data_uas <- read.csv("D:\\rahmi\\Business intelligence\\Data_Uas.csv", sep = ";")
View(data_uas)
model <- lm(y ~ x1 * x5, data = data_uas)
summary(model)
model.fix <- lm(y ~ x2 + x5, data = data_uas)
summary(model.fix)

#RSHINNY
# Load Packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(car)
library(plotly)
library(DT)

# Data
data <- data.frame(
  month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

# Shiny User Interface
Ui <- dashboardPage(
  dashboardHeader(title = "Prediction Data in Company Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Dataset", tabName = "data", icon = icon("database")),
      menuItem("Prediction", tabName = "prediction", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # Home tab
      tabItem(tabName = "home",
              fluidPage(
                box(
                  title = "Sales Prediction Dashboard",
                  width = 12, solidHeader = TRUE,
                  plotOutput("monthly_sales_plot")
                ),
                box(
                  title = "Relationship Plot",
                  width = 12, solidHeader = TRUE,
                  plotOutput("relationship_plot")
                )
              )
      ),
      # Prediction tab
      tabItem(tabName = "prediction",
              fluidPage(
                box(
                  title = "Sales Prediction Data in Company",
                  width = 12, solidHeader = TRUE,
                  numericInput("input_x2", "Number of Transactions:", value = 10000, min = 8000, max = 15000),
                  numericInput("input_x5", "Number of Advertisements:", value = 25000, min = 20000, max = 60000),
                  actionButton("predict_button", "Predict", icon("calculator")),
                  plotOutput("prediction_plot"),
                  dataTableOutput("prediction_table"),
                  box(
                    title = "Residual Plot",
                    width = 6, solidHeader = TRUE,
                    plotOutput("residual_plot")
                  ),
                  box(
                    title = "Scatterplot",
                    width = 6, solidHeader = TRUE,
                    plotOutput("scatterplot")
                  )
                )
              )
      ),
      # Data tab
      tabItem(tabName = "data",
              fluidPage(
                box(
                  title = "Dataset",
                  width = 12, solidHeader = TRUE,
                  dataTableOutput("data_table")
                )
              )
      )
    )
  ),
  skin = "purple",
)

# Server
server <- function(input, output) {
  # Monthly sales plot
  output$monthly_sales_plot <- renderPlot({
    ggplot(data, aes(x = month, y = y, fill = month)) +
      geom_bar(stat = "identity") +
      labs(title = "Monthly Sales Volume", x = "Month", y = "Sales Volume") +
      theme_minimal()
  })

  # Relationship plot
  output$relationship_plot <- renderPlot({
    ggplot(data, aes(x = x2, y = x5)) +
      geom_point() +
      labs(title = "Relationship between Number of Transactions and Advertisements",
           x = "Number of Transactions",
           y = "Number of Advertisements") +
      theme_minimal()
  })
  output$scatterplot <- renderPlot({
    ggplot(data, aes(x = x2, y = x5)) +
      geom_point() +
      labs(title = "scatterplot", x = "Number of Transactions", y = "Number of Advertisements")+
      theme_minimal()
  })

  # Data table
  output$data_table <- renderDataTable({
    data
  })
  # Regression model
  model <- lm(y ~ x2 + x5, data = data)

  # Function to predict sales
  predict_sales <- function(x2, x5) {
    new_data <- data.frame(x2 = x2, x5 = x5)
    predicted_sales <- predict(model, newdata = new_data)
    return(predicted_sales)
  }

  # Observe button click and update plots and tables
  observeEvent(input$predict_button, {
    x2_value <- input$input_x2
    x5_value <- input$input_x5
    result <- predict_sales(x2_value, x5_value)

    # Update plot
    output$prediction_plot <- renderPlot({
      ggplot(data, aes(x = y, y = result)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
        labs(title = "Actual vs Predicted Monthly Sales Volume",
             x = "Actual Sales Volume",
             y = "Predicted Sales Volume") +
        theme_minimal()
    })

    # Update table
    output$prediction_table <- renderDataTable({
      data.frame(Actual = data$y, Predicted = result)
    })

    # Update residual plot
    output$residual_plot <- renderPlot({
      residuals_plot <- plot(residuals(model), main = "Residuals Plot")
      return(residuals_plot)
    })
  })
}

# Run the app
shinyApp(Ui, server)


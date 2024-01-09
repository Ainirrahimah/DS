# Load library
library(tidyr)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(rstatix)
library(plotly)

# data for table
data <- data.frame(
  day = 1:10,
  left = c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7),
  center = c(3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  right = c(3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)
# data for ANOVA
ad_placement_data <- data.frame(
  Ad_Placement = rep(c("left", "center", "right"), each = 10),
  CTR = c(
    2.5, 2.7, 2.8, 2.6, 3, 2.4, 2.9, 2.5, 2.6, 2.7,
    3.8, 3.5, 4, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9,
    3.1, 2.9, 3, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5
  )
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Analisis Click-Trough Rates"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input Data", tabName = "input_data", icon = icon ("barplot")),
      menuItem("Analysis", tabName = "analisisAd", icon = icon("chart-line")),
      menuItem("Visualisasi", tabName = "visualisasi", icon = icon ("pie-chart"))
    ),
    tags$style(
      HTML("
        .main-sidebar {
          background-color: red;
        }
        .main-sidebar a {
          color: white;
        }
      ")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body {
          background-color: #F0FFFF;
        }
        .content-wrapper, .right-side {
          background-color: #F0FFFF;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "input_data",
        h2("Input Data"),
        fluidRow(
          column(
            width = 6,
            textInput("left_input", "Masukkan data left", ""),
            textInput("center_input", "Masukkan data center", ""),
            textInput("right_input", "Masukkan data right", ""),
            actionButton("submit_btn", "Tambah Data")
          ),
          column(
            width = 6,
            DTOutput("data_table"),
            actionButton("edit_data_btn", "Edit Data Terpilih"),
            actionButton("hapus_data_btn", "Hapus Data Terpilih")
          ),
          column(
            width = 12,
            plotlyOutput("bar_plot")
          )
        )
      ),
      # Tab 2: Analisis Ad
      tabItem("analisisAd",
              fluidRow(
                column(
                  width = 12,
                  box(
                    title = "Ad Placement Analysis",
                    solidHeader = TRUE,
                    DTOutput("userDataTable"),
                    fluidRow(
                      column(4, numericInput("left_sidebar", "Left Sidebar:", value = 3)),
                      column(4, numericInput("center_page", "Center Page:", value = 3)),
                      column(4, numericInput("right_sidebar", "Right Sidebar:", value = 3))
                    ),
                    actionButton("add_row", "Add Row"),
                    actionButton("delete_row", "Delete Row"),  # Added delete row button
                    actionButton("run_anova", "Run ANOVA"),
                    verbatimTextOutput("anovaResultAd"),
                    plotOutput("boxplotUserInput", height = 300)  # Boxplot for user input
                  )
                )
              )
           ),
      #Visualisasi
      tabItem(
        tabName = "visualisasi",
        h2("Visualisasi"),
        plotlyOutput("bar_plot_visualisasi")
     )
   )
 )
)


# Server
server <- function(input, output, session) {
  # Inisialisasi data default
  rv <- reactiveValues(data = data, selected_rows = NULL, editing_row = NULL)

  # Menampilkan data tabel
  output$data_table <- renderDT({
    datatable(rv$data, editable = TRUE, selection = "multiple")
  })

  # Menambahkan data baru ke tabel
  observeEvent(input$submit_btn, {
    new_row <- data.frame(
      day = nrow(rv$data) + 1,
      left = as.numeric(input$left_input),
      center = as.numeric(input$center_input),
      right = as.numeric(input$right_input)
    )
    rv$data <- rbind(rv$data, new_row)
  })

  # Mengatur baris yang dipilih
  observeEvent(input$data_table_rows_selected, {
    rv$selected_rows <- input$data_table_rows_selected
  })

  # Hapus data terpilih
  observeEvent(input$hapus_data_btn, {
    if (!is.null(rv$selected_rows)) {
      rv$data <- rv$data[-rv$selected_rows, ]
      rv$selected_rows <- NULL
    }
  })

  # Edit data terpilih
  observeEvent(input$edit_data_btn, {
    if (!is.null(rv$selected_rows) && length(rv$selected_rows) == 1) {
      rv$editing_row <- rv$selected_rows
      updateTextInput(session, "left_input", value = as.character(rv$data$left[rv$editing_row]))
      updateTextInput(session, "center_input", value = as.character(rv$data$center[rv$editing_row]))
      updateTextInput(session, "right_input", value = as.character(rv$data$right[rv$editing_row]))
    }
  })

  # Simpan perubahan setelah mengedit
  observeEvent(input$submit_btn, {
    if (!is.null(rv$editing_row)) {
      rv$data$left[rv$editing_row] <- as.numeric(input$left_input)
      rv$data$center[rv$editing_row] <- as.numeric(input$center_input)
      rv$data$right[rv$editing_row] <- as.numeric(input$right_input)
      rv$editing_row <- NULL
    }
  })
  # Reactive values to store user input data
  user_data <- reactiveValues(data = NULL)

  # Render datatable for user input
  output$userDataTable <- renderDT({
    datatable(user_data$data, editable = TRUE, options = list(lengthChange = FALSE))
  })
  # Analisis statistik
  output$output_anova <- renderPrint({
    if (is.null(rv$data)) return(NULL)  # Hindari analisis jika data kosong
    result_anova <- aov(cbind(left, center, right) ~ day, data = rv$data)
    print(summary(result_anova))
  })
  # Add row button
  observeEvent(input$add_row, {
    new_row <- data.frame(
      Ad_Placement = c("Left Sidebar", "Center Page", "Right Sidebar"),
      CTR = c(input$left_sidebar, input$center_page, input$right_sidebar)
    )

    # If user_data$data is NULL, initialize with new_row
    if (is.null(user_data$data)) {
      user_data$data <- new_row
    } else {
      # Otherwise, add new_row to existing data
      user_data$data <- rbind(user_data$data, new_row)
    }
  })

  # Delete row button
  observeEvent(input$delete_row, {
    selected_rows <- input$userDataTable_rows_selected
    if (length(selected_rows) > 0) {
      user_data$data <- user_data$data[-selected_rows, ]
    }
  })

  # Run ANOVA on user input
  observeEvent(input$run_anova, {
    if (!is.null(user_data$data)) {
      # Fit ANOVA model
      anova_result_ad <- aov(CTR ~ Ad_Placement, data = user_data$data)

      # Print ANOVA summary
      output$anovaResultAd <- renderPrint({
        summary(anova_result_ad)
      })
    }
  })

  # Run ANOVA on ad_placement_data
  output$anovaResultMainDashboard <- renderPrint({
    # Fit ANOVA model
    anova_result_main_dashboard <- aov(CTR ~ Ad_Placement, data = ad_placement_data)

    # Print ANOVA summary
    summary(anova_result_main_dashboard)
  })
  # Visualisasi diagram batang
  output$bar_plot_visualisasi <- renderPlotly({
    if (is.null(rv$data)) return(NULL)

    # Data untuk plot
    data_plot <- rv$data %>%
      pivot_longer(cols = c(left, center, right), names_to = "Group", values_to = "Value")

    # Warna untuk setiap kelompok
    colors <- c("cornsilk4", "lightcyan2", "pink")

    # Plot
    p <- plot_ly(data_plot, x = ~Group, y = ~Value, type = "bar", color = ~Group, colors = colors) %>%
      layout(title = "Sum of Click Based on Location",
             xaxis = list(title = "Location"),
             yaxis = list(title = "Sum"),
             showlegend = FALSE)

    return(p)
  })

}

# Run the application
shinyApp(ui, server)

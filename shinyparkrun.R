# app.R
library(shiny)
library(rvest)
library(httr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(hms)
library(DT)
library(bslib)

# Helper Functions

# Shiny app that uses the local server

# Function to fetch Park Run stats from the website
fetch_parkrun_stats <- function(athlete_id) {
  url <- paste0("https://www.parkrun.org.uk/parkrunner/", athlete_id, "/all/")
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
  
  page <- GET(url, add_headers(`User-Agent` = user_agent))
  
  if (status_code(page) == 200) {
    parsed_content <- content(page, as = "parsed")
    stats_table <- parsed_content %>%
      html_node(xpath = "//table[contains(., 'Run Number')]") %>%
      html_table(fill = TRUE)
    
    if (!is.null(stats_table) && nrow(stats_table) > 0) {
      return(stats_table)
    } else {
      stop("Failed to find the 'All Results' stats table in the page content.")
    }
  } else {
    stop("Failed to retrieve Park Run stats. HTTP status code: ", status_code(page))
  }
}

# Function to clean and process the stats data
clean_parkrun_stats <- function(stats_table) {
  if (!all(c("Run Date", "Time") %in% colnames(stats_table))) {
    stop("Required columns 'Run Date' and 'Time' not found in the stats table.")
  }
  
  stats_table <- stats_table %>%
    rename(
      Event = `Event`,
      Date = `Run Date`,
      Run_No = `Run Number`,
      Position = `Pos`,
      Time = `Time`,
      Age_Grade = `AgeGrade`,
      PB = `PB?`
    )
  
  stats_table <- stats_table %>%
    mutate(Date = parse_date_time(Date, orders = c("dmy", "mdy", "ymd"))) %>%
    mutate(Time = ifelse(nchar(Time) == 5, paste0("00:", Time), Time)) %>%
    filter(!is.na(Date) & grepl("^\\d{2}:\\d{2}:\\d{2}$", Time)) %>%
    mutate(Time = hms::as_hms(Time))
  
  return(stats_table)
}

# Function to plot the Park Run times over weeks with trend analysis
plot_parkrun_times <- function(stats_table) {
  if (nrow(stats_table) == 0 || all(is.na(stats_table$Date))) {
    stop("No valid data found to plot.")
  }
  
  # Convert Date column to Date class
  stats_table$Date <- as.Date(stats_table$Date)
  
  all_weeks <- seq.Date(from = min(stats_table$Date, na.rm = TRUE), 
                        to = max(stats_table$Date, na.rm = TRUE), 
                        by = "week")
  
  full_data <- data.frame(Date = all_weeks) %>%
    left_join(stats_table, by = "Date")
  
  y_breaks <- seq(as.numeric(hms::as_hms("00:25:00")), 
                  as.numeric(hms::as_hms("00:45:00")), 
                  by = 300)
  y_labels <- format(hms::as_hms(y_breaks), "%M:%S")
  
  vertical_lines <- data.frame(Date = all_weeks)
  
  segment_data <- full_data %>%
    filter(!is.na(Time)) %>%
    mutate(NextDate = lead(Date), NextTime = lead(Time)) %>%
    filter(!is.na(NextDate))
  
  # Fit a linear trend line
  trend_line <- lm(Time ~ as.numeric(Date), data = stats_table)
  
  g <- ggplot(full_data, aes(x = Date, y = Time)) +
    geom_vline(data = vertical_lines, aes(xintercept = as.numeric(Date)), 
               linetype = "solid", color = "grey", alpha = 0.5) +
    geom_segment(data = segment_data, 
                 aes(x = Date, y = Time, xend = NextDate, yend = NextTime), 
                 linetype = "dotted") +
    geom_point(size = 3, color = "blue") +
    geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
    scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
    scale_y_time(limits = c(hms::as_hms("00:25:00"), hms::as_hms("00:45:00")), 
                 breaks = hms::as_hms(y_breaks),
                 labels = function(x) format(x, "%M:%S")) +
    labs(title = "Park Run Times",
         x = "Date",
         y = "Time (MM:SS)") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "bottom"
    )
  
  return(g)
}

# UI definition
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  titlePanel("Park Run Statistics Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("athlete_id", 
                "Enter Athlete ID:", 
                value = "9306344"),
      actionButton("fetch_btn", "Fetch Data",
                   class = "btn-primary",
                   style = "width: 100%"),
      br(), br(),
      downloadButton("download_csv", "Download CSV",
                     style = "width: 100%"),
      br(), br(),
      downloadButton("download_plot", "Download Plot",
                     style = "width: 100%")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 plotOutput("parkrun_plot", height = "600px"),
                 br(),
                 div(style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                     textOutput("trend_analysis"))
        ),
        tabPanel("Data", 
                 br(),
                 DT::dataTableOutput("parkrun_table"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value to store the processed data
  parkrun_data <- reactiveVal(NULL)
  
  # Fetch and process data when button is clicked
  observeEvent(input$fetch_btn, {
    withProgress(message = 'Fetching data...', {
      tryCatch({
        stats_table <- fetch_parkrun_stats(input$athlete_id)
        cleaned_stats <- clean_parkrun_stats(stats_table)
        parkrun_data(cleaned_stats)
        showNotification("Data fetched successfully!", 
                         type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), 
                         type = "error",
                         duration = 10)
      })
    })
  })
  
  # Generate plot
  output$parkrun_plot <- renderPlot({
    req(parkrun_data())
    plot_parkrun_times(parkrun_data())
  })
  
  # Display data table
  output$parkrun_table <- DT::renderDataTable({
    req(parkrun_data())
    DT::datatable(
      parkrun_data(),
      options = list(
        pageLength = 10,
        ordering = TRUE,
        searching = TRUE,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      extensions = 'Buttons',
      class = 'cell-border stripe'
    )
  })
  
  # Download handlers
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("parkrun_stats_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(parkrun_data())
      write.csv(parkrun_data(), file, row.names = FALSE)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("parkrun_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(parkrun_data())
      plot <- plot_parkrun_times(parkrun_data())
      ggsave(file, plot, width = 10, height = 6, dpi = 300)
    }
  )
  
  # Calculate and display trend analysis
  output$trend_analysis <- renderText({
    req(parkrun_data())
    data <- parkrun_data()
    
    # Fit linear model
    model <- lm(as.numeric(Time) ~ as.numeric(Date), data = data)
    slope <- coef(model)[2]
    
    # Convert slope to seconds per day
    seconds_per_day <- slope
    
    # Calculate improvement over 30 days
    improvement_30_days <- seconds_per_day * 30
    
    paste0("Trend Analysis: ",
           sprintf("%.1f", abs(improvement_30_days)), 
           " seconds ", 
           ifelse(improvement_30_days < 0, "improvement", "increase"),
           " per month")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
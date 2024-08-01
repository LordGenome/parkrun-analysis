library(rvest)
library(httr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(hms)

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
    geom_vline(data = vertical_lines, aes(xintercept = as.numeric(Date)), linetype = "solid", color = "grey") +
    geom_segment(data = segment_data, aes(x = Date, y = Time, xend = NextDate, yend = NextTime), linetype = "dotted") +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
    scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
    scale_y_time(limits = c(hms::as_hms("00:25:00"), hms::as_hms("00:45:00")), 
                 breaks = hms::as_hms(y_breaks),
                 labels = function(x) format(x, "%M:%S")) +
    labs(title = "Park Run Times",
         x = "Date",
         y = "Time (MM:SS)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(g)
}

# Main function to fetch, process, and plot Park Run stats
process_parkrun_stats <- function(athlete_id) {
  stats_table <- fetch_parkrun_stats(athlete_id)
  stats_table <- clean_parkrun_stats(stats_table)
  g <- plot_parkrun_times(stats_table)
  
  ggsave("parkrun_times_over_weeks.png", plot = g, width = 10, height = 6)
  write.csv(stats_table, "parkrun_stats.csv", row.names = FALSE)
  
  return(list(stats_table = stats_table, plot = g))
}

# Example usage
athlete_id <- "9306344"
result <- process_parkrun_stats(athlete_id)
print(head(result$stats_table))
print(result$plot)


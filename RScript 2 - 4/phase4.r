# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readr)
library(rpart)
library(rpart.plot)
library(reshape2)
library(corrplot)
library(plotly)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(bslib)

# Load and preprocess data
data <- read_csv("cleaned_student_habits_performance_data.csv") %>%
  mutate(
    screen_time = social_media_hours + netflix_hours,
    performance_level = case_when(
      exam_score >= 90 ~ "Excellent",
      exam_score >= 80 ~ "High",
      exam_score >= 70 ~ "Satisfactory",
      exam_score >= 60 ~ "Medium",
      exam_score >= 50 ~ "Fair",
      exam_score < 50 ~ "Low"
    ),
    performance_level = factor(performance_level, levels = c("Low", "Fair", "Medium", "Satisfactory", "High", "Excellent")),
    age_group = cut(age, breaks = c(15, 17, 19, 21, 23), labels = c("15–17", "18–19", "20–21", "22–23"))
  )

# Colorblind-friendly palette
performance_colors <- c(
  "Low" = "#D73027",
  "Fair" = "#FC8D59",
  "Medium" = "#FEE08B",
  "Satisfactory" = "#D9EF8B",
  "High" = "#91CF60",
  "Excellent" = "#1A9850"
)

# Define custom themes for light and dark mode with Bootstrap 5
light_theme <- bs_theme(version = 5, bootswatch = "flatly", bg = "#ffffff", fg = "#222")
dark_theme  <- bs_theme(version = 5, bootswatch = "darkly", bg = "#222222", fg = "#fff")

# UI (theme injected via tagList and bs_theme_dependencies)
ui <- tagList(
  bslib::bs_theme_dependencies(light_theme),
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = tagList(icon("graduation-cap"), "Student Habits & Performance Dashboard")),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
        menuItem("Key Insights", tabName = "insights", icon = icon("lightbulb")),
        menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
        menuItem("Correlations", tabName = "correlations", icon = icon("braille")),
        menuItem("Habits vs Exam Score", tabName = "scatter", icon = icon("line-chart")),
        menuItem("Habits by Performance", tabName = "boxplot", icon = icon("th-large")),
        menuItem("Decision Tree", tabName = "tree", icon = icon("sitemap")),
        menuItem("Heatmaps", tabName = "heatmap", icon = icon("th")),
        menuItem("Demographics", tabName = "barplots", icon = icon("users")),
        menuItem("Data Explorer", tabName = "data", icon = icon("table"))
      ),
      hr(),
      selectInput("gender_filter", "Gender", choices = c("All", unique(data$gender)), selected = "All"),
      selectInput("level_filter", "Performance Level", choices = c("All", levels(data$performance_level)), selected = "All"),
      sliderInput("age_filter", "Age Range", min(data$age, na.rm=TRUE), max(data$age, na.rm=TRUE), value = c(min(data$age, na.rm=TRUE), max(data$age, na.rm=TRUE))),
      actionButton("reset_filters", "Reset Filters", icon = icon("redo"))
    ),
    dashboardBody(
      tags$head(tags$style(HTML("
        .small-note { color: #888; font-size: 12px; }
        .shiny-output-error-validation { color: red; }
        .dashboard-header { font-weight: bold; }
      "))),
      # Light/Dark mode toggle
      fluidRow(
        column(12, div(style = "float:right; margin:8px 12px 0 0;",
                       materialSwitch(
                         inputId = "dark_mode", label = "Dark Mode", status = "primary", right = TRUE
                       )
        ))
      ),
      tabItems(
        tabItem(tabName = "overview",
                h2("Welcome!"),
                p("This interactive dashboard helps you explore how student habits influence academic performance."),
                p("Use the sidebar to navigate, and the filters below to customize your view."),
                br(),
                p("Created by Team <b>Standout</b>", class="small-note")
        ),
        tabItem(tabName = "insights",
                h2("Key Insights & Recommendations"),
                verbatimTextOutput("insights"),
                br(),
                p("Tips: Hover on charts for more detail, and use filters on the sidebar to refine the analysis.", class="small-note")
        ),
        tabItem(tabName = "summary",
                fluidRow(
                  valueBoxOutput("avg_exam_score", width = 3),
                  valueBoxOutput("n_students", width = 3),
                  valueBoxOutput("top_perf", width = 3),
                  valueBoxOutput("most_common_gender", width = 3)
                ),
                br(),
                h4("Quick Data Summary"),
                tableOutput("summary_table")
        ),
        tabItem(tabName = "correlations",
                box(title = "Correlation Matrix", width = 12, solidHeader = TRUE, status = "primary",
                    withSpinner(plotOutput("corrplot", height = "450px")),
                    p("Shows relationships between behavioral variables and exam score.", class="small-note")
                )
        ),
        tabItem(tabName = "scatter",
                h4("Interactive Plots: Habits vs Exam Score"),
                fluidRow(
                  box(withSpinner(plotlyOutput("p1")), width = 6),
                  box(withSpinner(plotlyOutput("p2")), width = 6)
                ),
                fluidRow(
                  box(withSpinner(plotlyOutput("p3")), width = 6),
                  box(withSpinner(plotlyOutput("p4")), width = 6)
                )
        ),
        tabItem(tabName = "boxplot",
                box(title = "Student Habits by Performance Level", width = 12, solidHeader = TRUE, status = "info",
                    withSpinner(plotlyOutput("habitBoxplot", height = "550px")),
                    p("Boxplots compare key habits for each performance level.", class="small-note")
                )
        ),
        tabItem(tabName = "tree",
                box(title = "Decision Tree", width = 12, solidHeader = TRUE, status = "success",
                    withSpinner(plotOutput("treePlot", height = "600px")),
                    p("Tree shows key predictors for performance.", class="small-note")
                )
        ),
        tabItem(tabName = "heatmap",
                fluidRow(
                  box(withSpinner(plotlyOutput("heatmap1")), width = 6),
                  box(withSpinner(plotlyOutput("heatmap2")), width = 6)
                ),
                br(),
                p("Heatmaps summarize average screen time and sleep hours by group.", class="small-note")
        ),
        tabItem(tabName = "barplots",
                h4("Demographic Analysis"),
                fluidRow(
                  box(withSpinner(plotlyOutput("bar1")), width = 6),
                  box(withSpinner(plotlyOutput("bar2")), width = 6)
                ),
                fluidRow(
                  box(withSpinner(plotlyOutput("bar3")), width = 6),
                  box(withSpinner(plotlyOutput("bar4")), width = 6)
                )
        ),
        tabItem(tabName = "data",
                h4("Explore Raw Data"),
                DTOutput("datatable"),
                downloadButton("downloadData", "Download Filtered Data")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Theme switching for light/dark mode
  observe({
    if (isTRUE(input$dark_mode)) {
      session$setCurrentTheme(dark_theme)
    } else {
      session$setCurrentTheme(light_theme)
    }
  })
  
  # Filtering
  filtered_data <- reactive({
    d <- data
    if (input$gender_filter != "All") d <- d[d$gender == input$gender_filter,]
    if (input$level_filter != "All") d <- d[d$performance_level == input$level_filter,]
    d <- d[d$age >= input$age_filter[1] & d$age <= input$age_filter[2],]
    d
  })
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "gender_filter", selected = "All")
    updateSelectInput(session, "level_filter", selected = "All")
    updateSliderInput(session, "age_filter", value = c(min(data$age, na.rm=TRUE), max(data$age, na.rm=TRUE)))
  })
  
  # Value boxes
  output$n_students <- renderValueBox({
    valueBox(nrow(filtered_data()), "Students (filtered)", icon = icon("users"), color = "teal")
  })
  output$avg_exam_score <- renderValueBox({
    valueBox(round(mean(filtered_data()$exam_score, na.rm = TRUE), 2), "Avg Exam Score", icon = icon("chart-line"), color = "aqua")
  })
  output$top_perf <- renderValueBox({
    tbl <- table(filtered_data()$performance_level)
    top <- names(tbl)[which.max(tbl)]
    valueBox(top, "Most Common Performance", icon = icon("star"), color = "green")
  })
  output$most_common_gender <- renderValueBox({
    tbl <- table(filtered_data()$gender)
    top <- names(tbl)[which.max(tbl)]
    valueBox(top, "Most Common Gender", icon = icon("venus-mars"), color = "yellow")
  })
  
  # Summary Table
  output$summary_table <- renderTable({
    d <- filtered_data()
    tibble(
      "Exam Score (Mean)" = round(mean(d$exam_score, na.rm=TRUE), 2),
      "Study Hours (Mean)" = round(mean(d$study_hours_per_day, na.rm=TRUE),2),
      "Screen Time (Mean)" = round(mean(d$screen_time, na.rm=TRUE),2),
      "Sleep Hours (Mean)" = round(mean(d$sleep_hours, na.rm=TRUE),2),
      "Attendance (%)" = round(mean(d$attendance_percentage, na.rm=TRUE),2)
    )
  })
  
  # Key Insights (auto-generated)
  output$insights <- renderPrint({
    d <- filtered_data()
    n <- nrow(d)
    if (n == 0) return("No data in current filter selection.")
    avg_score <- round(mean(d$exam_score, na.rm=TRUE),2)
    top_perf <- names(sort(table(d$performance_level), decreasing = TRUE))[1]
    low_perf <- names(sort(table(d$performance_level), decreasing = TRUE))[length(table(d$performance_level))]
    high_study <- round(mean(d$study_hours_per_day[d$performance_level %in% c("Excellent","High")], na.rm=TRUE),2)
    low_study <- round(mean(d$study_hours_per_day[d$performance_level %in% c("Low","Fair")], na.rm=TRUE),2)
    high_screen <- round(mean(d$screen_time[d$performance_level %in% c("Low","Fair")], na.rm=TRUE),2)
    low_screen <- round(mean(d$screen_time[d$performance_level %in% c("Excellent","High")], na.rm=TRUE),2)
    paste(
      sprintf("• The average exam score (filtered) is %s.", avg_score),
      sprintf("\n• Most students are '%s' performers; least are '%s'.", top_perf, low_perf),
      sprintf("\n• 'Excellent/High' performers study %.1f hours/day and have ~%.1f hours screen time.", high_study, low_screen),
      sprintf("\n• 'Low/Fair' performers study %.1f hours/day and have ~%.1f hours screen time.", low_study, high_screen),
      "\n• Recommendation: Increase study hours, reduce screen time, and maintain regular sleep for better performance.",
      sep = ""
    )
  })
  
  # Correlation plot
  output$corrplot <- renderPlot({
    cor_data <- filtered_data() %>%
      select(exam_score, study_hours_per_day, screen_time, sleep_hours, exercise_frequency)
    cor_matrix <- cor(cor_data, use = "complete.obs")
    corrplot(cor_matrix, method = "circle", type = "upper",
             tl.cex = 0.9, addCoef.col = "black",
             title = "Correlation Between Behavioral Variables and Exam Score",
             mar = c(0, 0, 1, 0))
  })
  
  # Scatterplots (plotly)
  output$p1 <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = study_hours_per_day, y = exam_score, color = performance_level, text = paste("ID:", student_id))) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      scale_color_manual(values = performance_colors) +
      labs(title = "Study Hours vs Exam Score", x = "Study Hours per Day", y = "Exam Score", color = "Performance Level") +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "color", "text"))
  })
  output$p2 <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = screen_time, y = exam_score, color = performance_level, text = paste("ID:", student_id))) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", color = "blue", se = FALSE) +
      scale_color_manual(values = performance_colors) +
      labs(title = "Screen Time vs Exam Score", x = "Screen Time (hrs)", y = "Exam Score", color = "Performance Level") +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "color", "text"))
  })
  output$p3 <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = sleep_hours, y = exam_score, color = performance_level, text = paste("ID:", student_id))) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", color = "purple", se = FALSE) +
      scale_color_manual(values = performance_colors) +
      labs(title = "Sleep Hours vs Exam Score", x = "Sleep Hours per Day", y = "Exam Score", color = "Performance Level") +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "color", "text"))
  })
  output$p4 <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = exercise_frequency, y = exam_score, color = performance_level, text = paste("ID:", student_id))) +
      geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", color = "darkgreen", se = FALSE) +
      scale_color_manual(values = performance_colors) +
      labs(title = "Exercise Frequency vs Exam Score", x = "Exercise Sessions per Week", y = "Exam Score", color = "Performance Level") +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "color", "text"))
  })
  
  # Boxplot (plotly)
  output$habitBoxplot <- renderPlotly({
    data_long <- filtered_data() %>%
      select(performance_level, study_hours_per_day, screen_time, sleep_hours, attendance_percentage) %>%
      pivot_longer(cols = -performance_level, names_to = "Habit", values_to = "Value")
    p <- ggplot(data_long, aes(x = performance_level, y = Value, fill = performance_level)) +
      geom_boxplot() +
      facet_wrap(~ Habit, scales = "free_y") +
      scale_fill_manual(values = performance_colors) +
      labs(title = "Comparison of Student Habits by Performance Level",
           x = "Performance Level",
           y = "Value (hours or percentage)") +
      theme_minimal() +
      theme(strip.text = element_text(face = "bold"))
    ggplotly(p)
  })
  
  # Decision tree
  output$treePlot <- renderPlot({
    d <- filtered_data()
    d$internet_quality <- as.factor(d$internet_quality)
    model <- rpart(performance_level ~ study_hours_per_day + screen_time + sleep_hours + internet_quality,
                   data = d, method = "class")
    rpart.plot(model, type = 4, extra = 104, box.palette = "Blues")
    title(main = "Decision Tree: Factors Influencing Performance Level", line = 2.4, cex.main = 1)
  })
  
  # Heatmaps (plotly)
  output$heatmap1 <- renderPlotly({
    heatmap_data <- filtered_data() %>%
      group_by(performance_level) %>%
      summarise(
        avg_screen_time = mean(screen_time, na.rm = TRUE),
        avg_sleep_hours = mean(sleep_hours, na.rm = TRUE)
      )
    melted <- melt(heatmap_data, id.vars = "performance_level")
    p <- ggplot(melted, aes(x = variable, y = performance_level, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightyellow", high = "firebrick") +
      labs(title = "Avg Screen Time & Sleep Hours by Performance Level",
           x = "Behavioral Metric", y = "Performance Level", fill = "Avg Value") +
      theme_minimal()
    ggplotly(p)
  })
  output$heatmap2 <- renderPlotly({
    heatmap_data <- filtered_data() %>%
      group_by(mental_health_rating) %>%
      summarise(
        avg_screen_time = mean(screen_time, na.rm = TRUE),
        avg_sleep_hours = mean(sleep_hours, na.rm = TRUE)
      )
    melted <- melt(heatmap_data, id.vars = "mental_health_rating")
    p <- ggplot(melted, aes(x = variable, y = as.factor(mental_health_rating), fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightyellow", high = "firebrick") +
      labs(title = "Avg Screen Time & Sleep Hours by Mental Health Rating",
           x = "Behavioral Metric",
           y = "Mental Health Rating (1 = Poor, 10 = Excellent)",
           fill = "Avg Value") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Demographic barplots (plotly)
  output$bar1 <- renderPlotly({
    d <- filtered_data()
    grp <- d %>%
      group_by(performance_level, age_group) %>%
      summarise(count = n())
    p <- ggplot(grp, aes(x = performance_level, y = count, fill = age_group)) +
      geom_col(position = "dodge") +
      labs(title = "Performance Level by Age Group", x = "Performance Level", y = "Number of Students", fill = "Age Group") +
      theme_minimal()
    ggplotly(p)
  })
  output$bar2 <- renderPlotly({
    d <- filtered_data()
    grp <- d %>%
      group_by(performance_level, gender) %>%
      summarise(count = n())
    p <- ggplot(grp, aes(x = performance_level, y = count, fill = gender)) +
      geom_col(position = "dodge") +
      labs(title = "Performance Level by Gender", x = "Performance Level", y = "Number of Students", fill = "Gender") +
      theme_minimal()
    ggplotly(p)
  })
  output$bar3 <- renderPlotly({
    d <- filtered_data()
    grp <- d %>%
      group_by(performance_level, diet_quality) %>%
      summarise(count = n())
    p <- ggplot(grp, aes(x = performance_level, y = count, fill = diet_quality)) +
      geom_col(position = "dodge") +
      labs(title = "Performance Level by Diet Quality", x = "Performance Level", y = "Number of Students", fill = "Diet Quality") +
      theme_minimal()
    ggplotly(p)
  })
  output$bar4 <- renderPlotly({
    d <- filtered_data()
    grp <- d %>%
      group_by(performance_level, internet_quality) %>%
      summarise(count = n())
    p <- ggplot(grp, aes(x = performance_level, y = count, fill = internet_quality)) +
      geom_col(position = "dodge") +
      labs(title = "Performance Level by Internet Quality", x = "Performance Level", y = "Number of Students", fill = "Internet Quality") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Data table
  output$datatable <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      filter = "top",
      rownames = FALSE
    )
  })
  output$downloadData <- downloadHandler(
    filename = function() { paste0("student_data_filtered-", Sys.Date(), ".csv") },
    content = function(file) {
      write_csv(filtered_data(), file)
    }
  )
}

shinyApp(ui, server)
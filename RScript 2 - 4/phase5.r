# ========== LIBRARIES ========== #
library(shiny)
library(shinydashboard)
library(shinyjs)
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

# ========== CONFIGURATION ========== #
APP_TITLE <- "Student Performance Dashboard"
APP_CREATOR <- "Group 6"

performance_colors <- c(
  "Low" = "#D73027",
  "Fair" = "#FC8D59",
  "Medium" = "#FEE08B",
  "Satisfactory" = "#D9EF8B",
  "High" = "#91CF60",
  "Excellent" = "#1A9850"
)

# ========== DATA LOADING & PREPROCESSING ========== #
load_student_data <- function(path = "cleaned_student_habits_performance_data.csv") {
  read_csv(path) %>%
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
}
data <- load_student_data()

# ========== UI COMPONENTS ========== #
dashboard_css <- HTML(sprintf('
  .main-header .logo { font-family: "Montserrat", "Arial", sans-serif; font-weight: bold; }
  .box { border-radius: 10px; }
  .small-note { color: #888; font-size: 13px;}
  .info-box-icon { border-radius:10px 0 0 10px !important; }
  .value-box { border-radius:10px !important; min-height: 120px; }
  .skin-blue .main-header .logo { background-color: #1a2236 !important; color: #fff !important; }
  .skin-blue .main-header .navbar { background-color: #1a2236 !important; }
  .skin-blue .main-sidebar { background-color: #232d44 !important; }
  .skin-blue .sidebar-menu > li.active > a { background-color: #285c8f !important; }
  body.dark-mode, .dark-mode .content-wrapper, .dark-mode .main-sidebar, .dark-mode .main-header { background-color: #181d1f !important; color: #fff !important; }
  .dark-mode .box, .dark-mode .info-box, .dark-mode .value-box { background: #212531 !important; color: #fff !important; }
  .dark-mode .sidebar-menu > li.active > a { background-color: #1abc9c !important; }
  .dark-mode .dataTables_wrapper, 
  .dark-mode .dataTable, 
  .dark-mode table.dataTable,
  .dark-mode table.dataTable th,
  .dark-mode table.dataTable td {
    background-color: #232531 !important;
    color: #fff !important;
  }
  .dark-mode .paginate_button,
  .dark-mode .dataTables_info,
  .dark-mode .dataTables_filter,
  .dark-mode .dataTables_length,
  .dark-mode .dataTables_paginate {
    color: #fff !important;
  }
  .dark-mode .dataTables_wrapper input,
  .dark-mode .dataTables_wrapper select {
    background-color: #232531 !important;
    color: #fff !important;
    border: 1px solid #888 !important;
  }
  .dark-mode .dataTables_wrapper .dataTables_filter input {
    background-color: #232531 !important;
    color: #fff !important;
  }
  #dark_mode_label { color: #fff !important; font-weight:500; }
  .dark-mode #dark_mode_label { color: #fff !important; }
  /* Slightly lower the dark mode button */
  .main-header .dropdown { margin-top: 12px !important; }
'))

# ========== DASHBOARD LAYOUT ========== #
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = tagList(
      span(icon("graduation-cap"), APP_TITLE)
    ),
    tags$li(
      class = "dropdown",
      style = "margin-top: 12px; margin-right: 12px; position: relative;",
      materialSwitch(
        inputId = "dark_mode",
        label = span(id="dark_mode_label", "Dark Mode"),
        status = "primary",
        inline = TRUE,
        value = FALSE
      )
    )
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Summary", tabName = "summary", icon = icon("chart-bar")),
      menuItem("Correlations", tabName = "correlations", icon = icon("braille")),
      menuItem("Habits vs Exam Score", tabName = "scatter", icon = icon("chart-line")),
      menuItem("Habits by Performance", tabName = "boxplot", icon = icon("th-large")),
      menuItem("Decision Tree", tabName = "tree", icon = icon("sitemap")),
      menuItem("Heatmaps", tabName = "heatmap", icon = icon("th")),
      menuItem("Demographics", tabName = "barplots", icon = icon("users")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      hr(),
      h5("Global Filters"),
      selectInput("gender_filter", "Gender", choices = c("All", unique(data$gender)), selected = "All"),
      selectInput("level_filter", "Performance Level", choices = c("All", levels(data$performance_level)), selected = "All"),
      sliderInput("age_filter", "Age Range", min(data$age, na.rm=TRUE), max(data$age, na.rm=TRUE),
                  value = c(min(data$age, na.rm=TRUE), max(data$age, na.rm=TRUE))),
      actionButton("reset_filters", "Reset Filters", icon = icon("redo"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(dashboard_css)),
    tabItems(
      tabItem("overview",
              fluidRow(
                box(width = 12, title = "Welcome!", status = "primary", solidHeader = TRUE,
                    h3(APP_TITLE),
                    p("This interactive dashboard helps you explore how student habits influence academic performance."),
                    p("Use the sidebar for navigation and filters."),
                    br(),
                    p(sprintf("Created by %s", APP_CREATOR), class = "small-note")
                )
              )
      ),
      tabItem("summary",
              fluidRow(
                valueBoxOutput("avg_exam_score", width = 3),
                valueBoxOutput("n_students", width = 3),
                valueBoxOutput("top_perf", width = 3),
                valueBoxOutput("most_common_gender", width = 3)
              ),
              box(width = 12, title = "Quick Data Summary", status = "primary", solidHeader = TRUE,
                  tableOutput("summary_table")
              )
      ),
      tabItem("correlations",
              box(width = 12, title = "Correlation Matrix", status = "primary", solidHeader = TRUE,
                  withSpinner(plotOutput("corrplot", height = "450px")),
                  p("Shows relationships between behavioral variables and exam score.", class = "small-note")
              )
      ),
      tabItem("scatter",
              tabBox(width = 12, title = "Habits vs Exam Score",
                     tabPanel("Study Hours", withSpinner(plotlyOutput("p1"))),
                     tabPanel("Screen Time", withSpinner(plotlyOutput("p2"))),
                     tabPanel("Sleep Hours", withSpinner(plotlyOutput("p3"))),
                     tabPanel("Exercise Frequency", withSpinner(plotlyOutput("p4")))
              )
      ),
      tabItem("boxplot",
              box(width = 12, title = "Student Habits by Performance Level", status = "primary", solidHeader = TRUE,
                  withSpinner(plotlyOutput("habitBoxplot", height = "550px")),
                  p("Boxplots compare key habits for each performance level.", class = "small-note")
              )
      ),
      tabItem("tree",
              box(width = 12, title = "Decision Tree", status = "info", solidHeader = TRUE,
                  withSpinner(plotOutput("treePlot", height = "600px")),
                  p("Tree shows key predictors for performance.", class = "small-note")
              )
      ),
      tabItem("heatmap",
              fluidRow(
                box(width = 6, title = "By Performance Level", status = "primary", solidHeader = TRUE,
                    withSpinner(plotlyOutput("heatmap1"))
                ),
                box(width = 6, title = "By Mental Health Rating", status = "primary", solidHeader = TRUE,
                    withSpinner(plotlyOutput("heatmap2"))
                )
              ),
              p("Heatmaps summarize average screen time and sleep hours by group.", class = "small-note")
      ),
      tabItem("barplots",
              tabBox(width = 12, title = "Demographic Analysis",
                     tabPanel("By Age Group", withSpinner(plotlyOutput("bar1"))),
                     tabPanel("By Gender", withSpinner(plotlyOutput("bar2"))),
                     tabPanel("By Diet Quality", withSpinner(plotlyOutput("bar3"))),
                     tabPanel("By Internet Quality", withSpinner(plotlyOutput("bar4")))
              )
      ),
      tabItem("data",
              box(width = 12, title = "Explore Raw Data", status = "primary", solidHeader = TRUE,
                  DTOutput("datatable"),
                  downloadButton("downloadData", "Download Filtered Data")
              )
      )
    )
  )
)

# ========== SERVER LOGIC ========== #
server <- function(input, output, session) {
  # ---- THEME TOGGLE ----
  observe({
    if (isTRUE(input$dark_mode)) {
      shinyjs::addClass(selector = "body", class = "dark-mode")
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-mode")
    }
  })
  
  # ---- REACTIVE FILTER ----
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
  
  # ---- VALUE BOXES ----
  output$n_students <- renderValueBox({
    valueBox(nrow(filtered_data()), "Students (filtered)", icon = icon("users"), color = "blue")
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
  
  # ---- SUMMARY TABLE ----
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
  
  # ---- CORRELATION ----
  output$corrplot <- renderPlot({
    cor_data <- filtered_data() %>%
      select(exam_score, study_hours_per_day, screen_time, sleep_hours, exercise_frequency)
    cor_matrix <- cor(cor_data, use = "complete.obs")
    corrplot(cor_matrix, method = "circle", type = "upper",
             tl.cex = 0.9, addCoef.col = "black",
             title = "Correlation Between Behavioral Variables and Exam Score",
             mar = c(0, 0, 1, 0))
  })
  
  # ---- SCATTER PLOTS ----
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
  
  # ---- BOXPLOT ----
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
  
  # ---- DECISION TREE ----
  output$treePlot <- renderPlot({
    d <- filtered_data()
    d$internet_quality <- as.factor(d$internet_quality)
    model <- rpart(performance_level ~ study_hours_per_day + screen_time + sleep_hours + internet_quality,
                   data = d, method = "class")
    rpart.plot(model, type = 4, extra = 104, box.palette = "Blues")
    title(main = "Decision Tree: Factors Influencing Performance Level", line = 2.4, cex.main = 1)
  })
  
  # ---- HEATMAPS ----
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
  
  # ---- DEMOGRAPHIC BARPLOTS ----
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
  
  # ---- DATA TABLE ----
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

# ========== APP LAUNCH ========== #
shinyApp(ui, server)
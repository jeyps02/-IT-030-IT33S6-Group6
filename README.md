## 🚀 Features

- Interactive visualizations (bar charts, heatmaps, scatter plots, decision tree)
- Correlation analysis of habits like study time, sleep, screen time, and exercise
- Demographic filtering (gender, age range, performance level)
- Decision tree to explore predictors of academic performance
- Data explorer for raw data viewing

## 🧰 Libraries Used

- `shiny` – Main web application framework
- `shinydashboard` – UI layout components
- `shinyjs`, `shinyWidgets`, `shinycssloaders` – Enhanced interactivity and visuals
- `ggplot2`, `plotly` – Static and interactive visualizations
- `readr`, `dplyr`, `tidyverse` – Data manipulation
- `rpart`, `rpart.plot` – Decision tree modeling
- `corrplot`, `reshape2` – Correlation matrices and data reshaping
- `DT` – Interactive tables

## 🌐 Dashboard Navigation

The application contains the following tabs for ease of exploration:

- **Overview** – Welcome screen and project intro
- **Summary** – Key metrics and statistics
- **Correlations** – Visual correlation matrix of variables
- **Habits vs Exam Score** – Charts linking habits with performance
- **Habits by Performance** – Bar charts showing patterns per performance group
- **Decision Tree** – Predictive model for academic performance
- **Heatmaps** – Relationship between hours and performance level
- **Demographics** – Breakdown by age, gender, diet, and internet quality
- **Data Explorer** – Browse and filter raw data

### 🎛️ Global Filters
Located on the main dashboard panel, users can filter the entire dashboard by:
- **Gender**
- **Age Range**
- **Performance Level**

This enables focused analysis for specific student groups.

## 📊 Highlighted Visualizations

1. **Study Hours vs Exam Score (Scatter Plot)**  
   Displays correlation between study effort and academic results.

2. **Heatmap of Sleep & Screen Time by Performance Level**  
   Reveals combined effect of digital usage and rest on performance.

3. **Decision Tree Model**  
   Visual representation of key predictors and paths to different performance outcomes.

4. **Bar Chart Comparison by Gender, Age, Diet, Internet Quality**  
   Four grouped bar plots help identify disparities in performance based on demographic factors.

5. **Correlation Matrix**  
   Highlights how student habits interrelate and which behaviors cluster together.

## ⚙️ How It Works

The `app.R` file initializes the UI and server logic:
- Loads the CSV dataset
- Uses reactive inputs for filtering and visual updates
- Outputs charts and tables to the main dashboard based on user interaction

Filters are managed via input selectors and update charts in real-time.

## 👥 Creators

> **IT-030 – IT33S6 | Group 6**

- BALUYUT, Jerome  
- MEJIA, Igel  
- NOVENARIO, Jose Miguel  
- PASCUAL, John Paul  
- VASQUEZ, Christian Lloyd

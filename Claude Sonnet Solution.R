# Stacked Bar Plot: Global Deaths from Particulate Matter Air Pollution (2023)
# By Causes of Death and Risk Factor

# Step 1: Load required packages
tryCatch({
  if (!requireNamespace("pacman", quietly = TRUE)) {
    install.packages("pacman")
  }
  pacman::p_load(readr, dplyr, ggplot2, janitor, forcats)
  message("Step 1: Packages loaded successfully")
}, error = function(e) {
  stop("Step 1 Error - Failed to load packages: ", e$message)
})

# Step 2: Create figures directory
tryCatch({
  if (!dir.exists("figures")) {
    dir.create("figures")
    message("Step 2: 'figures' directory created")
  } else {
    message("Step 2: 'figures' directory already exists")
  }
}, error = function(e) {
  stop("Step 2 Error - Failed to create figures directory: ", e$message)
})

# Step 3: Read and clean data
tryCatch({
  df <- read_csv("data/IHME GBD Compare - Air Pollution Deaths.csv", 
                 show_col_types = FALSE) |>
    clean_names()
  message("Step 3: Data loaded and column names cleaned")
  message("  Columns: ", paste(names(df), collapse = ", "))
}, error = function(e) {
  stop("Step 3 Error - Failed to read/clean data: ", e$message)
})

# Step 4: Filter and prepare data
tryCatch({
  plot_data <- df |>
    filter(
      measure == "Percent of total deaths",
      year == 2023,
      location == "Global"
    ) |>
    mutate(
      deaths_pct = value * 100
    ) |>
    filter(deaths_pct >= 0.1) |>
    select(cause_of_death = cause_of_death_or_injury, 
           risk_factor = risk_factor, deaths_pct)
  
  if (nrow(plot_data) == 0) {
    stop("No data remaining after filtering")
  }
  message("Step 4: Data filtered successfully")
  message("  Rows after filtering: ", nrow(plot_data))
}, error = function(e) {
  stop("Step 4 Error - Failed to filter data: ", e$message)
})

# Step 5: Calculate totals and order causes
tryCatch({
  cause_totals <- plot_data |>
    group_by(cause_of_death) |>
    summarise(total = sum(deaths_pct), .groups = "drop") |>
    arrange(desc(total))
  
  plot_data <- plot_data |>
    mutate(
      cause_of_death = factor(cause_of_death, 
                              levels = rev(cause_totals$cause_of_death))
    )
  message("Step 5: Causes ordered by total deaths (descending)")
}, error = function(e) {
  stop("Step 5 Error - Failed to order causes: ", e$message)
})

# Step 6: Create the stacked bar plot
tryCatch({
  n_factors <- length(unique(plot_data$risk_factor))
  pastel_colors <- c("#FFB3BA", "#BAE1FF", "#FFDFbA", "#E0BBE4")[1:n_factors]
  
  p <- ggplot(plot_data, aes(x = deaths_pct, 
                             y = cause_of_death, 
                             fill = risk_factor)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    geom_text(
      aes(label = sprintf("%.1f", deaths_pct)),
      position = position_stack(vjust = 0.95),
      hjust = 1,
      size = 2.5,
      color = "gray20"
    ) +
    scale_fill_manual(values = pastel_colors, name = "Risk Factor") +
    scale_x_continuous(
      breaks = seq(0, ceiling(max(cause_totals$total)), by = 1),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = "Global Deaths from Particulate Matter Air Pollution (2023)",
      subtitle = "By Cause of Death and Risk Factor (Deaths ≥ 0.1%)",
      x = "Deaths (%)",
      y = "Cause of Death",
      caption = paste("Source:",
                      "Institute for Health Metrics and Evaluation (IHME).",
                      "GBD Compare Data Visualization.",
                      "Global Burden of Disease (GBD) Study 2023.",
                      "Seattle, WA: IHME, University of Washington, 2025.")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      plot.caption = element_text(hjust = 1, size = 9, color = "gray50"),
      axis.text.y = element_text(size = 10, color = "gray20"),
      axis.text.x = element_text(size = 10, color = "gray20"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  message("Step 6: Plot created successfully")
}, error = function(e) {
  stop("Step 6 Error - Failed to create plot: ", e$message)
})

# Step 7: Save the plot
tryCatch({
  ggsave(
    filename = "figures/air_pollution_deaths_stacked_bar.png",
    plot = p,
    width = 12,
    height = 8,
    dpi = 300,
    bg = "white"
  )
  message(
    "Step 7: Plot saved to 'figures/air_pollution_deaths_stacked_bar.png'")
}, error = function(e) {
  stop("Step 7 Error - Failed to save plot: ", e$message)
})

# Step 8: Display the plot
tryCatch({
  print(p)
  message("Step 8: Plot displayed successfully")
  message("\nScript completed successfully!")
}, error = function(e) {
  stop("Step 8 Error - Failed to display plot: ", e$message)
})
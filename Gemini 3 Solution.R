# R Script to create a stacked bar plot of Global Deaths from Particulate Matter Air Pollution (2023)

# 1. Load necessary packages with error handling
tryCatch({
  if (!require("pacman")) {
    install.packages("pacman", repos = "http://cran.us.r-project.org")
  }
  pacman::p_load(readr, janitor, ggplot2, dplyr, here)
  
  if (!all(sapply(c("readr", "janitor", "ggplot2", "dplyr", "here"), 
                  requireNamespace, quietly = TRUE))) {
    stop("One or more required packages failed to load.")
  }
  
  # 2. Define file and directory names
  input_dir <- "data"
  input_file <- here(input_dir, "IHME GBD Compare - Air Pollution Deaths.csv")
  output_dir <- "figures"
  output_file <- here(output_dir, "global_air_pollution_deaths_stacked_bar.png")
  
  # 3. Read the data with error handling
  data <- tryCatch({
    readr::read_csv(input_file, show_col_types = FALSE) %>%
      janitor::clean_names()
  }, error = function(e) {
    stop(paste("Error reading or cleaning data:", e$message))
  })
  
  # 4. Data Preparation and Filtering with error handling
  plot_data <- tryCatch({
    # Filter for "Percent of total deaths"
    data_filtered <- data %>%
      filter(measure == "Percent of total deaths")
    
    if (nrow(data_filtered) == 0) {
      stop("No data found for 'Percent of total deaths'.")
    }
    
    # Multiply 'value' by 100 to get actual percentage
    plot_data_prep <- data_filtered %>%
      mutate(deaths_percent = value * 100) %>%
      
      # Filter for percentage >= 0.1 and relevant risk factors
      filter(
        deaths_percent >= 0.1,
        risk_factor %in% c("Ambient particulate matter pollution", "Household air pollution from solid fuels")
      )
    
    if (nrow(plot_data_prep) == 0) {
      stop("No data remaining after filtering (Deaths (%) >= 0.1 and relevant risk factors). Plot cannot be generated.")
    }
    
    # Calculate total deaths for sorting (longest to shortest, so descending total_deaths)
    plot_data_prep %>%
      group_by(cause_of_death_or_injury) %>%
      mutate(total_deaths = sum(deaths_percent)) %>%
      ungroup() %>%
      
      # Order the Cause of Death factor
      # The factor levels are ordered by total_deaths in DESCENDING order for the longest bar to be on top.
      mutate(cause_of_death_or_injury = factor(
        cause_of_death_or_injury,
        levels = unique(cause_of_death_or_injury[order(total_deaths, decreasing = TRUE)])
      )) %>%
      
      # Prepare data for annotations and arrange for predictable stacking
      arrange(cause_of_death_or_injury, risk_factor) %>%
      group_by(cause_of_death_or_injury) %>%
      mutate(
        cumu_deaths_percent = cumsum(deaths_percent) # Right edge of the segment
      ) %>%
      ungroup()
    
  }, error = function(e) {
    stop(paste("Error during data preparation and filtering:", e$message))
  })
  
  # 5. Create figures directory with error handling
  tryCatch({
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
  }, error = function(e) {
    stop(paste("Error creating output directory:", e$message))
  })
  
  # 6. Create the ggplot2 stacked bar plot with error handling
  p <- tryCatch({
    ggplot(
      plot_data,
      aes(x = deaths_percent, 
          y = reorder(cause_of_death_or_injury, deaths_percent), 
          fill = risk_factor)
    ) +
      
      # Stacked Bar Plot
      geom_bar(stat = "identity", color = "white", linewidth = 0.5) +
      
      # Labels on bars (rounded to 1 decimal place, placed just inside the right end)
      geom_text(
        aes(label = format(round(deaths_percent, 1), nsmall = 1)),
        # Place label near the right end
        position = position_stack(vjust = 0.95),
        color = "black",
        size = 2.5,
        hjust = 1 # Align to the right
      ) +
      
      # Custom pastel colors
      scale_fill_brewer(palette = "Pastel1", name = "Risk Factor") +
      
      # X-axis formatting: sequential integers (e.g., 1, 2, 3)
      scale_x_continuous(
        breaks = function(limits) seq.int(0, floor(limits[2] + 0.5)),
        expand = expansion(mult = c(0, 0.05)), # Small expansion on the right
        limits = c(0, NA)
      ) +
      
      # Labels and Titles
      labs(
        title = paste("Global Deaths Attributable to Particulate Matter",
                      "Air Pollution (2023)"),
        subtitle = "Causes of Death with Deaths (%) >= 0.1",
        x = "Deaths (%)",
        y = "Cause of Death",
        caption = paste("Source:",
                        "Institute for Health Metrics and Evaluation (IHME).",
                        "GBD Compare Data Visualization.",
                        "Global Burden of Disease (GBD) Study 2023.")
      ) +
      
      # Theme settings
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 1, face = "italic", margin = margin(t = 10)), # Lower right margin
        legend.position = "bottom",
        # Ensure axis text and label text are similar in font properties
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 11, face = "bold")
      )
    
  }, error = function(e) {
    stop(paste("Error creating ggplot:", e$message))
  })
  
  # 7. Save the plot to a PNG file
  tryCatch({
    ggsave(
      output_file,
      plot = p,
      width = 10,
      height = 7,
      units = "in",
      dpi = 300
    )
    cat(paste("Successfully created and saved the plot to", output_file, "\n"))
  }, error = function(e) {
    stop(paste("Error saving the plot:", e$message))
  })
  
}, error = function(e) {
  # General error handling for the entire script
  cat(paste("An unrecoverable error occurred:", e$message, "\n"))
  cat("The script execution failed.\n")
})

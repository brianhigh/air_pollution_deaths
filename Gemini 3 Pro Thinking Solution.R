# Create an R script to generate a stacked bar plot of Global Deaths from 
# Particulate Matter Air Pollution (2023)

# Produced by GenAI with only minor edits by hand:
# - package loading modified to use pacman::p_load() and to add forcats & here
# - use of here() to improve flexibility in relative file path access
# - output file path stored as a variable to remove repetition path string
# - code line-wrapping modified to keep lines <= 80 characters long
# - plot annotation formatting modified to use a smaller font size

# This variation was created by Gemini 3 Pro Thinking from the following prompt:

# Goal: Create an R script to generate a stacked bar plot of Global Deaths from 
#       Particulate Matter Air Pollution (2023).
# 
# Data: Use the CSV file provided (possibly found within a "data" folder).
# 
# Plot Type: The plot should be a horizontal stacked bar plot x-axis is Deaths, 
#            y-axis is Cause of Death).
# 
# Data Filtering:
#   
#   Include only data where the Measure is "Percent of total deaths."
# 
# Only include death percentages >= 0.1 (after converting the raw value to a 
# percentage).
# 
# Aesthetics and Formatting:
#   
#   Stack the bars by the Risk Factor and use a pastel color palette.
# 
# Sort the bars from longest (top) to shortest (bottom).
# 
# Annotate each bar segment with its corresponding death percentage, rounded to 
# one decimal place.
# 
# Include a caption in the lower right margin stating the data source.
# 
# Output: Save the final plot as a PNG file in a newly created figures folder.
# 
# Note: Structure the script with clear steps and appropriate error handling.

# Load necessary libraries
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, forcats, here)

# 1. Setup and Data Loading
# Create 'figures' folder if it doesn't exist
if (!dir.exists("figures")) {
  dir.create("figures")
}

# Load the dataset
# Assuming the file is in a 'data' subfolder as specified
file_path <- here("data/IHME GBD Compare - Air Pollution Deaths.csv")

# Fallback: Check if file is in current directory if not found in 'data'
if (!file.exists(file_path)) {
  file_path <- here("IHME GBD Compare - Air Pollution Deaths.csv")
}

data <- read_csv(file_path, show_col_types = FALSE)

# 2. Data Filtering and Transformation
plot_data <- data %>%
  # Filter for the correct measure
  filter(Measure == "Percent of total deaths") %>%
  # Convert raw value to percentage
  mutate(Percent = Value * 100) %>%
  # Filter for significant percentages (>= 0.1%)
  filter(Percent >= 0.1)

# 3. Ordering Factors for Plotting
# We want to sort the bars from longest (top) to shortest (bottom).
# In ggplot, the y-axis fills from bottom to top. To put the longest bar at the 
# top, we need the factor level with the highest total percentage to be the 
# "last" level.
plot_data <- plot_data %>%
  group_by(`Cause of death or injury`) %>%
  mutate(Total_Percent = sum(Percent)) %>%
  ungroup() %>%
  mutate(`Cause of death or injury` = 
           fct_reorder(`Cause of death or injury`, Total_Percent))

# 4. Plotting
p <- ggplot(plot_data, aes(x = Percent, 
                           y = `Cause of death or injury`, 
                           fill = `Risk factor`)) +
  geom_col(width = 0.7) + # Stacked bar plot
  # Annotations: Percentage rounded to 1 decimal place
  geom_text(aes(label = sprintf("%.1f", Percent)), 
            position = position_stack(vjust = 0.5), 
            size = 2.5, 
            color = "black") +
  # Aesthetics
  scale_fill_brewer(palette = "Pastel1") + # Pastel color palette
  labs(title = "Global Deaths from Particulate Matter Air Pollution (2023)",
       x = "Deaths (% of Total)",
       y = "Cause of Death",
       caption = paste("Source: Institute for Health Metrics and Evaluation", 
                       "(IHME), GBD 2023")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    panel.grid.major.y = element_blank() # Clean up y-axis grid lines
  )

# 5. Save Output
output_file <- here("figures/global_deaths_particulate_matter_2023.png")
ggsave(output_file, plot = p, width = 10, height = 6, dpi = 300)
message(paste("Script completed. Plot saved to", output_file, "."))

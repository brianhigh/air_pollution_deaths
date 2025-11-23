# Create a stacked bar plot showing global deaths from particulate matter
# air pollution by causes of death and risk factor for deaths (%) >= 0.1.

# This script was developed on Windows using VS Code with Cline and Continue
# extensions. The GenAI LLM was qwen3-coder:30b hosted locally via Ollama.
# The context length was set to 128K tokens. See prompt.txt for details.

# Load required packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(readr, dplyr, ggplot2, janitor, scales, forcats, here)

# Read the data
data <- read_csv(here("data/IHME GBD Compare - Air Pollution Deaths.csv"))

# Clean column names
data <- janitor::clean_names(data)

# Filter data for "Percent of total deaths" measure
data_filtered <- data %>%
  filter(measure == "Percent of total deaths") %>%
  # Multiply by 100 to convert to percentage and filter for >= 0.1
  mutate(value = value * 100) %>%
  filter(value >= 0.1)

# Check if we have data to plot
if (nrow(data_filtered) == 0) {
  stop("No data found with Percent of total deaths >= 0.1")
}

# Find maximum value for x-axis scaling
max_value <- data_filtered %>%
  group_by(cause_of_death_or_injury) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  pull(value) %>%
  max()

# Sort by total value (descending) to ensure proper stacking
data_filtered <- data_filtered %>%
  group_by(cause_of_death_or_injury) %>%
  summarise(total_value = sum(value), .groups = "drop") %>%
  arrange(desc(total_value)) %>%
  left_join(data_filtered, by = "cause_of_death_or_injury") %>%
  ungroup()

# Create the stacked bar plot
p <- ggplot(data_filtered,
            aes(x = value,
                y = reorder(cause_of_death_or_injury, total_value),
                fill = risk_factor)) +
  geom_col() +
  # Add value labels inside each bar segment
  geom_text(aes(label = round(value, 1)),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  # Set x-axis to be sequential integers
  scale_x_continuous(breaks = seq(0, max_value, by = 1)) +
  # Set labels and title
  labs(x = "Deaths (%)",
       y = "Cause of Death",
       title = "Global Deaths from Particulate Matter Air Pollution (2023)",
       subtitle = "by Causes of Death and Risk Factor, for Deaths (%) >= 0.1",
       fill = "Risk Factor",
       caption = paste("Source:",
                       "Institute for Health Metrics and Evaluation (IHME).",
                       "GBD Compare Data Visualization.",
                       "Global Burden of Disease (GBD) Study 2023.",
                       "Seattle, WA: IHME, University of Washington, 2025.")) +
  # Use pastel colors
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 8, hjust = 1),
    legend.position = "bottom"
  )

# Save the plot
dir.create(here("figures"), showWarnings = FALSE)
ggsave(here("figures/air_pollution_deaths.png"), plot = p,
       width = 10, height = 8, dpi = 300)

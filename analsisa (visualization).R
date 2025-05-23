# Create a data frame with the provided information
sneaker_data <- data.frame(
  brands = c("Brooks", "Mizuno", "UnderArmour", "Vans", "ASICS", "Puma", 
             "Converse", "NewBalance", "Adidas", "Nike"),
  Owned = c(0.06, 0.09, 0.15, 0.60, 0.30, 0.82, 1.50, 0.69, 2.19, 3.24),
  CurrentlyOwn = c(0.00, 0.04, 0.04, 0.24, 0.17, 0.26, 0.43, 0.42, 0.88, 1.39),
  Consider = c(0.06, 0.09, 0.13, 0.17, 0.35, 0.41, 0.44, 0.61, 0.78, 0.80)
)

# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Bar chart for Owned and Currently Owned (Nike, Adidas, New Balance, Converse)
selected_brands <- c("Nike", "Adidas", "NewBalance", "Converse")

# Filter data and reshape for plotting
ownership_data <- sneaker_data %>%
  filter(brands %in% selected_brands) %>%
  select(brands, Owned, CurrentlyOwn) %>%
  pivot_longer(cols = c("Owned", "CurrentlyOwn"), 
               names_to = "Ownership", 
               values_to = "Value")

# Create ownership bar chart
ownership_plot <- ggplot(ownership_data, aes(x = brands, y = Value, fill = Ownership)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Sneaker Brand Ownership",
    subtitle = "Previously Owned vs Currently Owned",
    x = "Brand",
    y = "Value",
    fill = "Status"
  ) +
  scale_fill_manual(values = c("Owned" = "#3498db", "CurrentlyOwn" = "#2ecc71")) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3.5))

# Display the ownership plot
print(ownership_plot)

# 2. Bar chart for Consideration Rate (converting to percentage)
consideration_data <- sneaker_data %>%
  filter(brands %in% selected_brands) %>%
  mutate(ConsiderPercent = Consider * 100) %>%
  select(brands, ConsiderPercent)

# Create consideration bar chart
consideration_plot <- ggplot(consideration_data, aes(x = brands, y = ConsiderPercent, fill = brands)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Sneaker Brand Consideration Rate",
    x = "Brand",
    y = "Consideration Rate (%)",
    fill = "Brand"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  ) +
  geom_text(aes(label = sprintf("%.1f%%", ConsiderPercent)), 
            position = position_stack(vjust = 0.5), 
            color = "black") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 85))

# Display the consideration plot
print(consideration_plot)

# Save the plots (optional)
# ggsave("sneaker_ownership_chart.png", ownership_plot, width = 10, height = 6)
# ggsave("sneaker_consideration_chart.png", consideration_plot, width = 10, height = 6)
# Create a data frame with the provided information
sneaker_data <- data.frame(
  brands = c("Brooks", "Mizuno", "UnderArmour", "Vans", "ASICS", "Puma", 
             "Converse", "NewBalance", "Adidas", "Nike"),
  Owned = c(0.00, 0.04, 0.04, 0.24, 0.17, 0.26, 0.43, 0.42, 0.88, 1.39)
)

# Load required packages
library(ggplot2)
library(dplyr)
library(scales)  # For percentage formatting

# Calculate market share percentages
total_owned <- sum(sneaker_data$Owned)
sneaker_data <- sneaker_data %>%
  mutate(market_share = Owned / total_owned,
         market_share_pct = market_share * 100,
         # Create labels for the pie chart
         label = paste0(brands, "\n", round(market_share_pct, 1), "%"))

# Sort data by market share (descending)
sneaker_data <- sneaker_data %>%
  arrange(desc(market_share))

# Create a basic pie chart
pie_chart <- ggplot(sneaker_data, aes(x = "", y = market_share, fill = brands)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(
    title = "Sneaker Brand Market Share",
    subtitle = "Based on Previously Owned Sneakers",
    fill = "Brand"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_fill_brewer(palette = "Paired")

print(pie_chart)

# Create an improved pie chart with percentages
pie_chart_with_labels <- ggplot(sneaker_data, aes(x = "", y = market_share, fill = brands)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_minimal() +
  labs(
    title = "Sneaker Brand Market Share",
    subtitle = "Based on Currently Owned Sneakers",
    fill = "Brand"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  scale_fill_brewer(palette = "Paired") +
  geom_text(aes(label = ifelse(market_share >= 0.05, 
                               paste0(round(market_share_pct, 1), "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 3.5)

print(pie_chart_with_labels)

# Create a donut chart (alternative visualization)
donut_chart <- ggplot(sneaker_data, aes(x = 2, y = market_share, fill = brands)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(
    title = "Sneaker Brand Market Share",
    subtitle = "Based on Previously Owned Sneakers",
    fill = "Brand"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  scale_fill_brewer(palette = "Paired") +
  geom_text(aes(label = ifelse(market_share >= 0.05, 
                               paste0(round(market_share_pct, 1), "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 3.5) +
  xlim(0.5, 2.5) # This creates the hole in the donut

print(donut_chart)

# Create a data table of market share percentages
market_share_table <- sneaker_data %>%
  select(brands, Owned, market_share_pct) %>%
  arrange(desc(market_share_pct)) %>%
  mutate(market_share_pct = paste0(round(market_share_pct, 1), "%"))

colnames(market_share_table) <- c("Brand", "Owned Value", "Market Share")
print(market_share_table)





# Load necessary libraries
library(dplyr)
library(ggplot2)


# Create a dataframe from the sample data
# Note: In a real scenario, you would load this from a CSV or Excel file
data <- data.frame(
  Age = c("25-27", "28-30", "24", "28-30", "25-27", "23", "28-30", "25-27", "24", "25-27", 
          "24", "23", "23", "22", "21", "28-30", "23", "23", "28-30", "28-30", "23", "23", 
          "24", "22", "23", "28-30", "25-27", "25-27", "25-27", "≥31", "≥31", "25-27", 
          "28-30", "28-30", "25-27", "25-27", "25-27", "23", "22", "21", "21", "22", "23", 
          "22", "22", "20", "24", "21", "25-27", "23", "22", "25-27"),
  Gender = c("Male", "Male", "Female", "Female", "Female", "Female", "Female", "Female", "Male", "Female", 
             "Female", "Female", "Male", "Male", "Male", "Female", "Female", "Female", "Female", "Female", 
             "Male", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", "Female", 
             "Male", "Female", "Male", "Female", "Female", "Female", "Female", "Female", "Male", "Female", 
             "Female", "Male", "Female", "Male", "Female", "Female", "Male", "Male", "Female", "Female", 
             "Female", "Female"),
  Consider_NewBalance = c("", "", "Would consider", "Would consider", "Would consider", "", "Would consider", 
                          "Would consider", "", "", "Would consider", "Would consider", "", "", "Would consider", 
                          "", "Would consider", "Would consider", "Would consider", "Would consider", "Would consider", 
                          "Would consider", "Would consider", "", "", "Would consider", "", "Would consider", "", 
                          "Would consider", "", "", "", "Would consider", "Would consider", "Would consider", 
                          "Would consider", "Would consider", "Would consider", "", "Would consider", "Would consider", 
                          "Would consider", "Would consider", "Would consider", "Would consider", "", "", "Would consider", 
                          "", "Would consider", "Would consider")
)

# Function to convert age ranges to numeric values
convert_age <- function(age) {
  if (age == "≥31") {
    return(31)  # Minimum value for this category
  } else if (grepl("-", age)) {
    # Extract the average of the range
    range_parts <- as.numeric(strsplit(age, "-")[[1]])
    return(mean(range_parts))
  } else {
    # Direct numeric age
    return(as.numeric(age))
  }
}

# Apply age conversion and create generation groups
data <- data %>%
  mutate(
    Age_Numeric = sapply(Age, convert_age),
    Generation = ifelse(Age_Numeric <= 28, 1, 2),
    Generation_Label = ifelse(Age_Numeric <= 28, "Gen Z", "Millennial")
  )

# Summary statistics
gen_summary <- data %>%
  group_by(Generation_Label) %>%
  summarise(
    Count = n(),
    Percent = n() / nrow(data) * 100,
    Consider_Count = sum(Consider_NewBalance == "Would consider", na.rm = TRUE),
    Consider_Percent = sum(Consider_NewBalance == "Would consider", na.rm = TRUE) / n() * 100
  )

# Gender breakdown by generation
gender_gen_summary <- data %>%
  group_by(Generation_Label, Gender) %>%
  summarise(
    Count = n(),
    Percent = n() / sum(data$Generation_Label == first(Generation_Label)) * 100,
    Consider_Count = sum(Consider_NewBalance == "Would consider", na.rm = TRUE),
    Consider_Percent = sum(Consider_NewBalance == "Would consider", na.rm = TRUE) / n() * 100
  )

# Create a bar chart of generations
gen_plot <- ggplot(data, aes(x = Generation_Label, fill = Generation_Label)) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "Distribution of Respondents by Generation",
    x = "Generation",
    y = "Count",
    fill = "Generation"
  ) +
  scale_fill_manual(values = c("Gen Z" = "#4ECDC4", "Millennial" = "#FF6B6B"))

# Create a bar chart of New Balance consideration by generation
consider_plot <- ggplot(
  data %>% filter(Consider_NewBalance == "Would consider"),
  aes(x = Generation_Label, fill = Generation_Label)
) +
  geom_bar() +
  theme_minimal() +
  labs(
    title = "New Balance Consideration by Generation",
    x = "Generation",
    y = "Count of 'Would Consider'",
    fill = "Generation"
  ) +
  scale_fill_manual(values = c("Gen Z" = "#4ECDC4", "Millennial" = "#FF6B6B"))

# Create a percentage plot of consideration rate by generation
consider_percent_data <- data %>%
  group_by(Generation_Label) %>%
  summarise(
    Total = n(),
    Consider = sum(Consider_NewBalance == "Would consider", na.rm = TRUE),
    Consider_Percent = Consider / Total * 100
  )

consider_percent_plot <- ggplot(consider_percent_data, 
                                aes(x = Generation_Label, y = Consider_Percent, fill = Generation_Label)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "New Balance Consideration Rate by Generation",
    x = "Generation",
    y = "Consideration Rate (%)",
    fill = "Generation"
  ) +
  scale_fill_manual(values = c("Gen Z" = "#4ECDC4", "Millennial" = "#FF6B6B")) +
  geom_text(aes(label = sprintf("%.1f%%", Consider_Percent)), 
            position = position_stack(vjust = 0.5),
            size = 4)

# Create a plot of gender distribution by generation
gender_plot <- ggplot(data, aes(x = Generation_Label, fill = Gender)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Gender Distribution by Generation",
    x = "Generation",
    y = "Count",
    fill = "Gender"
  ) +
  scale_fill_manual(values = c("Female" = "#FF9F9F", "Male" = "#9FDFFF"))

# Print results
print("Generation Summary:")
print(gen_summary)
print("\nGender by Generation Summary:")
print(gender_gen_summary)

# Display plots
print(gen_plot)
print(consider_plot)
print(consider_percent_plot)
print(gender_plot)

# If you want to save to Excel, you can use the writexl package
# library(writexl)
# write_xlsx(list(
#   "Original Data" = data,
#   "Generation Summary" = gen_summary,
#   "Gender Generation Summary" = gender_gen_summary
# ), path = "age_generation_analysis.xlsx")


# Load necessary libraries
library(dplyr)
library(ggplot2)

# Create a dataframe from the sample data
data <- data.frame(
  Age = c("25-27", "28-30", "24", "28-30", "25-27", "23", "28-30", "25-27", "24", "25-27", 
          "24", "23", "23", "22", "21", "28-30", "23", "23", "28-30", "28-30", "23", "23", 
          "24", "22", "23", "28-30", "25-27", "25-27", "25-27", "≥31", "≥31", "25-27", 
          "28-30", "28-30", "25-27", "25-27", "25-27", "23", "22", "21", "21", "22", "23", 
          "22", "22", "20", "24", "21", "25-27", "23", "22", "25-27"),
  Consider_NewBalance = c("", "", "Would consider", "Would consider", "Would consider", "", "Would consider", 
                          "Would consider", "", "", "Would consider", "Would consider", "", "", "Would consider", 
                          "", "Would consider", "Would consider", "Would consider", "Would consider", "Would consider", 
                          "Would consider", "Would consider", "", "", "Would consider", "", "Would consider", "", 
                          "Would consider", "", "", "", "Would consider", "Would consider", "Would consider", 
                          "Would consider", "Would consider", "Would consider", "", "Would consider", "Would consider", 
                          "Would consider", "Would consider", "Would consider", "Would consider", "", "", "Would consider", 
                          "", "Would consider", "Would consider")
)

# Function to convert age ranges to numeric values
convert_age <- function(age) {
  if (age == "≥31") {
    return(31)  # Minimum value for this category
  } else if (grepl("-", age)) {
    # Extract the average of the range
    range_parts <- as.numeric(strsplit(age, "-")[[1]])
    return(mean(range_parts))
  } else {
    # Direct numeric age
    return(as.numeric(age))
  }
}

# Apply age conversion and create generation groups
data <- data %>%
  mutate(
    Age_Numeric = sapply(Age, convert_age),
    Generation = ifelse(Age_Numeric <= 28, 1, 2),
    Generation_Label = ifelse(Age_Numeric <= 28, "Gen Z", "Millennial")
  )

# Calculate the summary statistics for generations
generation_summary <- data %>%
  group_by(Generation, Generation_Label) %>%
  summarise(
    Count = n(),
    Percentage = n() / nrow(data) * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(Generation_Label))  # Sort for consistent colors

# Create a basic pie chart of generation distribution
gen_pie <- ggplot(generation_summary, aes(x = "", y = Count, fill = Generation_Label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Distribution of Respondents by Generation",
    fill = "Generation"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("Gen Z" = "#4ECDC4", "Millennial" = "#FF6B6B"))

# Add percentage labels
gen_pie_with_labels <- ggplot(generation_summary, aes(x = "", y = Count, fill = Generation_Label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Distribution of Respondents by Generation",
    fill = "Generation"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("Gen Z" = "#4ECDC4", "Millennial" = "#FF6B6B")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%\n(", Count, ")")),
            position = position_stack(vjust = 0.5),
            size = 4)

# Create a donut chart (alternative visualization)
gen_donut <- ggplot(generation_summary, aes(x = 2, y = Count, fill = Generation_Label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Distribution of Respondents by Generation",
    fill = "Generation"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("Gen Z" = "#4ECDC4", "Millennial" = "#FF6B6B")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 4) +
  xlim(0.5, 2.5)  # This creates the hole in the donut

# Create a pie chart for New Balance consideration by generation
# First, calculate consideration rates
consideration_by_gen <- data %>%
  group_by(Generation_Label) %>%
  summarise(
    Total = n(),
    Consider = sum(Consider_NewBalance == "Would consider", na.rm = TRUE),
    Percentage = Consider / Total * 100,
    .groups = 'drop'
  )

# Create a data frame for the pie chart
# This shows what percentage of all "Would consider" responses come from each generation
consider_pie_data <- data %>%
  filter(Consider_NewBalance == "Would consider") %>%
  group_by(Generation_Label) %>%
  summarise(
    Count = n(),
    Percentage = n() / sum(Consider_NewBalance == "Would consider", na.rm = TRUE) * 100,
    .groups = 'drop'
  )

# Create the New Balance consideration pie chart
consider_pie <- ggplot(consider_pie_data, aes(x = "", y = Count, fill = Generation_Label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "New Balance Consideration by Generation",
    subtitle = "Distribution of 'Would Consider' Responses",
    fill = "Generation"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("Gen Z" = "#4ECDC4", "Millennial" = "#FF6B6B")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 4)

# Print the summary tables
print("Generation Distribution:")
print(generation_summary)

print("\nNew Balance Consideration by Generation:")
print(consideration_by_gen)

print("\nDistribution of 'Would Consider' Responses:")
print(consider_pie_data)

# Display the pie charts
print(gen_pie_with_labels)
print(gen_donut)
print(consider_pie)






###fdfdfds saya 



# Load necessary libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)

# Create data frame for market share and reach
market_data <- data.frame(
  Category = c("Market Share (%)", "Market Share (%)", "Population Reach", "Population Reach"),
  Stage = c("Current", "Target", "Current", "Target"),
  Value = c(10.9, 25.9, 828400, 1968400)
)

# Split the data for different visualizations
market_share_data <- market_data %>% filter(Category == "Market Share (%)")
population_data <- market_data %>% filter(Category == "Population Reach")

# Calculate growth percentages
market_share_growth <- (25.9 - 10.9) / 10.9 * 100
population_growth <- (1968400 - 828400) / 828400 * 100

# Create market share bar chart
market_share_plot <- ggplot(market_share_data, aes(x = Stage, y = Value, fill = Stage)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(Value, "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, 
            fontface = "bold") +
  labs(
    title = "Current vs Target Market Share",
    subtitle = paste0("Projected Growth: +", round(market_share_growth, 1), "%"),
    x = "",
    y = "Market Share (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_manual(values = c("Current" = "#5D8CA8", "Target" = "#1F4E79")) +
  ylim(0, 30)


# Create population reach bar chart
population_plot <- ggplot(population_data, aes(x = Stage, y = Value/1000000, fill = Stage)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(format(Value, big.mark = ","))), 
            position = position_stack(vjust = 0.5), 
            size = 4.5, 
            fontface = "bold") +
  labs(
    title = "Current vs Target Population Reach",
    subtitle = paste0("Projected Growth: +", round(population_growth, 1), "%"),
    x = "",
    y = "Population (millions)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_manual(values = c("Current" = "#5D8CA8", "Target" = "#1F4E79")) +
  scale_y_continuous(labels = label_number(suffix = "M"))

# Create a combined visualization with both charts
grid.arrange(market_share_plot, population_plot, ncol = 2)

# Create data for a more detailed stacked bar chart showing the strategy
strategy_data <- data.frame(
  Stage = c("Current", "Target"),
  Value = c(10.9, 25.9),
  Nike_Adidas = c(0, 5),
  Converse_Vans = c(0, 5),
  Original = c(10.9, 15.9)
)

# Create a stacked bar chart showing the strategy components
strategy_plot <- ggplot(strategy_data, aes(x = Stage)) +
  geom_bar(aes(y = Original, fill = "Current Base"), stat = "identity", position = "stack") +
  geom_bar(aes(y = Nike_Adidas, fill = "From Nike & Adidas"), stat = "identity", position = "stack") +
  geom_bar(aes(y = Converse_Vans, fill = "From Converse & Vans"), stat = "identity", position = "stack") +
  geom_text(aes(y = Original/2, label = ifelse(Stage == "Current", paste0(Original, "%"), "15.9%")), 
            size = 4, fontface = "bold") +
  geom_text(aes(y = Original + Nike_Adidas/2, 
                label = ifelse(Nike_Adidas > 0, paste0("+", Nike_Adidas, "%"), "")), 
            size = 4, fontface = "bold") +
  geom_text(aes(y = Original + Nike_Adidas + Converse_Vans/2, 
                label = ifelse(Converse_Vans > 0, paste0("+", Converse_Vans, "%"), "")), 
            size = 4, fontface = "bold") +
  labs(
    title = "New Balance Market Growth Strategy",
    subtitle = "Breaking Down the 15% Market Share Growth",
    x = "",
    y = "Market Share (%)",
    fill = "Source"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_manual(values = c("Current Base" = "#5D8CA8", 
                               "From Nike & Adidas" = "#D55E00", 
                               "From Converse & Vans" = "#009E73")) +
  ylim(0, 30)

# Print the strategy chart
print(strategy_plot)






# CLEANER WAY----------

library(ggplot2)
library(dplyr)
library(tidyr)

# 1) Define wide data with four components
strategy_data <- tibble(
  Stage          = factor(c("Current","Target"), levels = c("Current","Target")),
  Original       = c(10.9, 10.9),   # base NB share stays 10.9 in both
  Nike           = c( 0  ,  5  ),   # +5 in Target
  Converse_Vans  = c( 0  ,  5  ),   # +5 in Target
  Adidas         = c( 0  ,  5  )    # +5 in Target
)

# 2) Pivot to long for automatic stacking
strategy_long <- strategy_data %>%
  pivot_longer(-Stage, names_to = "Source", values_to = "Value") %>%
  # set the stacking order and friendly labels
  mutate(Source = factor(Source,
                         levels = c("Original","Nike","Converse_Vans","Adidas"),
                         labels = c("Current Base", "From Nike", "From Converse & Vans", "From Adidas")
  ))

# 3) Plot
ggplot(strategy_long, aes(x = Stage, y = Value, fill = Source)) +
  geom_col(width = 0.7) +
  # labels: base gets "10.9%", helpers get "+5%"
  geom_text(aes(
    label = ifelse(Value > 0,
                   ifelse(Source == "Current Base",
                          paste0(Value, "%"),
                          paste0("+", Value, "%")
                   ),
                   "")
  ),
  position = position_stack(vjust = 0.5),
  color    = "white",
  size     = 4,
  fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Current Base"         = "#5D8CA8",
    "From Nike"            = "#D55E00",
    "From Converse & Vans" = "#009E73",
    "From Adidas"          = "#E69F00"  # pick any distinct color for Adidas
  )) +
  labs(
    title    = "New Balance Market Growth Strategy",
    subtitle = "Breaking Down the 15% Market Share Growth",
    x        = NULL,
    y        = "Market Share (%)",
    fill     = "Source"
  ) +
  ylim(0, 30) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle    = element_text(hjust = 0.5),
    legend.position  = "bottom",
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "#f0f0f0"),
    panel.background = element_rect(fill = "#f0f0f0")
  )








#------------------------------------------------------------------------------------------------------
# Create data for a funnel chart showing the target markets
funnel_data <- data.frame(
  Stage = c("Total Gen Z & Millennial Population", 
            "Total Sneaker Market", 
            "Current New Balance Customers", 
            "Target New Balance Customers"),
  Value = c(7600000, 7600000 * 0.85, 828400, 1968400),
  Position = 4:1
)

# Create a funnel chart
funnel_plot <- ggplot(funnel_data, aes(x = Position, y = Value, fill = Stage)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = paste0(format(Value, big.mark = ","), 
                               ifelse(Stage == "Current New Balance Customers", 
                                      " (10.9%)", 
                                      ifelse(Stage == "Target New Balance Customers", 
                                             " (25.9%)", "")))), 
            hjust = -0.1, size = 3.5) +
  labs(
    title = "Taiwan Market Opportunity",
    subtitle = "Gen Z & Millennial Market",
    x = "",
    y = "Population"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10),
    legend.position = "right",
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.background = element_rect(fill = "#f0f0f0"),
    panel.background = element_rect(fill = "#f0f0f0")
  ) +
  scale_fill_brewer(palette = "Blues") +
  xlim(0.5, 4.5)

# Print the funnel chart
print(funnel_plot)







# FINAL PLOT-------------------------------------------------------------------------
# Create a single consolidated visualization for presentation
# This is a simplified version showing just the key market share growth


final_data <- data.frame(
  Category = c("Current Market Share", "Target Market Share"),
  Value = c(10.9, 25.9),
  Population = c(828400, 1968400),
  Label = c("10.9% (828,400)", "25.9% (1,968,400)")
)

final_plot <- ggplot(final_data, aes(x = Category, y = Value, fill = Category)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5), 
            size = 4, 
            fontface = "bold", 
            color = "white") +
  labs(
    title = "NB'S MARKET GROWTH PROJECTION ",
    subtitle = paste0("Projected Growth: +", round(market_share_growth, 1)),
    x = "",
    y = "Market Share (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "#f0f0f0"),
    panel.background = element_rect(fill = "#f0f0f0")
  ) +
  scale_fill_manual(values = c("Current Market Share" = "black", 
                               "Target Market Share" = "grey")) +
  ylim(0, 30)

# Print the final plot
print(final_plot)

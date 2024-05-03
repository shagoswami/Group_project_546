library(tidyverse)

# Loading the dataset
data <- read.csv('D:/2024 Spring/546/Re_ Project/fig2_modified.csv')
View(data)
# Selecting the relevant elements
relevant_elements <- c('Na', 'S', 'Ca44', 'Fe', 'Cu', 'Zn', 'Sr')
data <- data %>% select(Sample, Ear, all_of(relevant_elements))

# Normalizing the elemental data by the mean of the ear for each element
data_normalized <- data %>% 
  group_by(Ear) %>% 
  mutate(across(all_of(relevant_elements), ~./mean(.), .names = 'norm_{.col}')) %>% 
  ungroup()

# Calculating the mean for each Sample position
data_means <- data_normalized %>% 
  group_by(Sample) %>% 
  summarise(across(starts_with('norm'), mean))

# Reshaping the data to long format for plotting with ggplot2
data_long <- pivot_longer(data_means, 
                          cols = starts_with('norm'), 
                          names_to = 'Element', 
                          values_to = 'Value',
                          names_prefix = 'norm_')

# Creating a mapping from sample position to a numeric value
position_mapping <- data.frame(Sample = c('base', 'middle', 'tip'),
                               Position = c(1, 2, 3))

# Joining this with our long data frame
data_long <- data_long %>% 
  left_join(position_mapping, by = 'Sample')

# Defining custom colors for the elements
custom_colors <- c('Na' = "blue", 'S' = "maroon", 'Ca44' = "yellow", 
                   'Fe' = "red", 'Cu' = "green", 'Zn' = "cyan", 'Sr' = "pink")

# Plot using ggplot2 with customizations
p <- ggplot(data_long, aes(x = Position, y = Value, group = Element, color = Element)) +
  geom_line(size = 1.5) +  # Increase line thickness
  scale_color_manual(values = custom_colors) +  # Use custom colors
  scale_x_continuous(breaks = position_mapping$Position, labels = position_mapping$Sample) +
    labs(title = 'Gradients in Elemental Accumulation Along the Cob',
       x = 'Cob Position', y = 'Ratio of Cob Location to Average',
       color = 'Element') +
  theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),   
        plot.title.position = "plot")             

# Print the plot
p <- p + theme(panel.background = element_rect(fill = "white"), plot.background = element_rect(fill = "white"))
ggsave("elemental_accumulation_plot_V1.png", p, width = 10, height = 6, dpi = 300, device = 'png')




















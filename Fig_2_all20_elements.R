library(tidyverse)

# Loading the dataset
data <- read.csv('D:/2024 Spring/546/Re_ Project/fig2_modified.csv')

# Defining the relevant elements and check for duplicates or misalignments
relevant_elements <- c('Na', 'S', 'Ca44','Ca43', 'Fe', 'Cu', 'Zn', 'Sr', 'B', 'Mg', 'Al', 'P', 'K', 'Mn', 'Co', 'Ni', 'As', 'Rb', 'Mo', 'Cd')

# Selecting only the relevant columns based on the elements and key identifiers
data <- data %>% select(Sample, Ear, all_of(relevant_elements))

# Normalizing the elemental data by the mean of the ear for each element
data_normalized <- data %>%
  group_by(Ear) %>%
  mutate(across(all_of(relevant_elements), ~ ./mean(.), .names = 'norm_{.col}')) %>%
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

# Joining this with  long data frame
data_long <- data_long %>%
  left_join(position_mapping, by = 'Sample')

# Defining colors for specific elements and set others to gray
highlight_elements <- c('Na', 'S', 'Ca44', 'Fe', 'Cu', 'Zn', 'Sr')
colors <- setNames(rep("gray", length(relevant_elements)), relevant_elements)
colors[highlight_elements] <- RColorBrewer::brewer.pal(length(highlight_elements), "Set1")

# Plot using ggplot2
p <- ggplot(data_long, aes(x = Position, y = Value, group = Element, color = Element)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = position_mapping$Position, labels = position_mapping$Sample) +
  #scale_y_continuous(breaks = seq(0.6, 1.7, by = 0.2), limits = c(0.1, 2.0)) +
  labs(title = 'Gradients in Elemental Accumulation Along the Cob',
       x = 'Cob Position', y = 'Ratio of Cob Location to Average',
       color = 'Element') +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"),  
        plot.caption.position = "plot", 
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"))+
theme(plot.title = element_text(hjust = 0.5),    
      plot.title.position = "plot")              

# Print the plot
print(p)

# Saving the plot ensuring a white background and correct title position
ggsave("elemental_accumulation_plot_all_elements_v4.png", p, width = 12, height = 8, dpi = 300, device = 'png')

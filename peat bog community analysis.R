# Creating new tutorial about community analysis
# Data Science in EES 2024
# Auther: Siya-Qin
# 27th November 2024

# Install necessary packages (if not already installed)
install.packages("readr")
install.packages("vegan")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggrepel")
install.packages("ggpattern")
install.packages("ragg")

# Load required libraries
library(readr)     # For read csv file
library(vegan)     # For ecological diversity calculations
library(ggplot2)   # For data visualization
library(tidyr)     # For data manipulation
library(dplyr)     # For data wrangling
library(ggrepel)   # For non-overlapping text labels in plots
library(ggpattern) # For adding patterns to bar plots
library(ragg)      # For high quality plot

# Load the dataset
peat_bog_data <- read_csv("peat bog data.csv")

# Have a basic understanding of the structure and content of the dataset
View(peat_bog_data)  # Open the dataset
head(peat_bog_data)  # Preview the first few rows of the dataset
str(peat_bog_data)   # Display the structure (e.g., data types, column names)




# --- Plot 1: Alpha, Beta, and Gamma Diversity ---
# Calculate Alpha diversity (Shannon's index for each habitat)
alpha_diversity <- peat_bog_data %>%
  group_by(habitat) %>%
  summarize(Shannon = diversity(count, index = "shannon")) 

# Calculate Gamma diversity (pooled overall diversity across all three habitats)
gamma_diversity <- diversity(colSums(
  peat_bog_data %>%
    pivot_wider(names_from = organism, values_from = count, values_fill = 0) %>%
    select(-habitat)
))

# Calculate Beta diversity (ratio of gamma to mean alpha diversity)
beta_diversity <- gamma_diversity / mean(alpha_diversity$Shannon)

# Combine Alpha, Beta, and Gamma diversity metrics into one dataset 
alpha_diversity <- alpha_diversity %>% mutate(Type = "Alpha") # Add type column
combined_diversity <- bind_rows( # Combined Beta and Gamma dataa with the Alpha data
  alpha_diversity %>% rename(Value = Shannon),
  data.frame(habitat = "Beta", Value = beta_diversity, Type = "Beta"),
  data.frame(habitat = "Gamma", Value = gamma_diversity, Type = "Gamma")
)

# Plot diversity metrics
diversity_metrics <- ggplot(
  combined_diversity, aes(x = habitat, y = Value, fill = Type, pattern = Type)
  ) +
  geom_bar_pattern(    # Set the bars pattern
    stat = "identity", 
    position = "dodge", 
    width = 0.6, 
    color = "black",              
    pattern_density = 0.5,        
    pattern_fill = "white",       
    pattern_spacing = 0.05        
  ) +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 4) + # Add values on top of bars
  theme_minimal(base_size = 14) +
  labs(
    x = "Diversity Metric",
    y = "Shannon Index",
    fill = "Diversity Type",  
    caption = "Figure 1: Box plot compared Alpha, Beta, and Gamma diversity metrics across sediment, sphagum, and water microhabitats in peat bog mesocosms."
  ) +
  scale_fill_manual(  # Colors for diversity types
    values = c("Alpha" = "red", "Beta" = "blue", "Gamma" = "orange")
  ) + 
  scale_pattern_manual(  # Custum patterns for diversity types for user-friendly
    values = c("Alpha" = "stripe", "Beta" = "crosshatch", "Gamma" = "circle")
  ) + 
  guides(  # Applied patterns on legend
    fill = guide_legend(override.aes = list(pattern = c("stripe", "crosshatch", "circle"))), 
    pattern = "none"  
  ) +
  theme(  # Adjust the element and text size
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    panel.grid = element_blank(), 
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks.y = element_line(color = "black", size = 0.5),
    axis.ticks.length = unit(0.2, "cm")
  )
print(diversity_metrics)




# --- Plot 2: Rank-Abundance Plot ---
# Calculate overall abundance for all species
overall_abundance <- peat_bog_data %>%
  group_by(organism) %>%
  summarize(habitat = "overall", count = sum(count)) %>%
  ungroup()

# Add overall group to the dataset and rank species' abundance within each habitat
ranked_data <- bind_rows(peat_bog_data, overall_abundance) %>%
  group_by(habitat) %>%
  arrange(habitat, desc(count)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  filter(count > 0)

# Plot rank-abundance 
rank_abundance <- ggplot(
  ranked_data, aes(x = rank, y = count, color = habitat, shape = habitat, group = habitat)
  ) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Species Rank",
    y = "Log base 10 Abundance",
    color = "Habitat",
    shape = "Habitat",
    caption = "Figure 2: Rank-abundance plot showing the species abundance of three microhabitats (sediment, sphagnum, water) and overall group."
  ) +
  scale_y_log10() +  # Log base 10 abundance help to visualize and compare them between different groups
  scale_x_continuous(
    breaks = 1:20  
  ) +
  scale_color_manual(values = c("red", "orange", "green", "blue")) +
  scale_shape_manual(values = c(16, 17, 18, 15)) +
  theme(  # Adjust the element and text size
    panel.grid = element_blank(),       
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.ticks.x = element_line(color = "black", size = 0.5),  
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
print(rank_abundance)




# --- Plot 3: Relative Abundance overview (Multi-panel Plot) ---
# Calculate relative abundance for each habitat
abundance_data <- bind_rows(peat_bog_data, overall_abundance) %>%
  group_by(habitat) %>%
  mutate(relative_abundance = count / sum(count)) %>%
  ungroup()

# Plot multi-panel bar plot with relative abundance
relative_abundance <- ggplot(
  abundance_data, aes(x = organism, y = relative_abundance, fill = habitat, pattern = habitat)
  ) +
  geom_bar_pattern(
    stat = "identity", 
    position = "dodge", 
    color = "black", 
    width = 0.8,  
    pattern_density = 0.5,  
    pattern_fill = "white", 
    pattern_spacing = 0.05  
  ) +
  geom_text(aes(label = count), vjust = -0.5, size = 3) + # Add original counts on top of bars
  facet_wrap(~ habitat, scales = "free_y", ncol = 2) + # Create multi-panel plots
  theme_minimal(base_size = 14) +
  labs(
    x = "Species",
    y = "Relative Abundance",
    caption = "Figure 3: A 4-panel bar plot showing relative abundance of species across three microhabitats (sediment, sphagnum, and water) and overall data, \nincluding original total counts displayed above each bar."
  ) +
  theme(  # Adjust element and text
    panel.grid = element_blank(),              
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
    axis.text.y = element_text(size = 12),     
    axis.title.x = element_text(size = 14),    
    axis.title.y = element_text(size = 14),    
    strip.text = element_blank(),              
    axis.ticks.y = element_line(color = "black", size = 0.5), 
    axis.line.y = element_line(color = "black", size = 0.5),  
    axis.line.x = element_line(color = "black", size = 0.5),  
    axis.ticks.length = unit(0.2, "cm") 
  ) +
  scale_fill_manual(  
    values = c("red", "orange", "green", "blue") 
  ) +
  scale_pattern_manual(  # Custom habitats' patterns for user-friendly
    values = c("stripe", "crosshatch", "circle", "none") 
  )
print(relative_abundance)




# --- Plot 4: PCA Analysis ---
# Transform data into wide format for PCA
organism_data <- peat_bog_data %>%
  pivot_wider(names_from = habitat, values_from = count, values_fill = 0)

# Convert to data frame and set rownames to organism names
organism_data <- as.data.frame(organism_data)
rownames(organism_data) <- organism_data$organism
organism_data <- organism_data[, -1]

# Perform PCA
pca_result <- prcomp(organism_data, scale. = TRUE)

# Extract PCA scores and add organism names
pca_data <- as.data.frame(pca_result$x)
pca_data$Organism <- rownames(pca_data)

# Plot PCA plot
PCA <- ggplot(
  pca_data, aes(x = PC1, y = PC2, color = Organism, shape = Organism)
  ) +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_text_repel(aes(label = Organism), size = 4, alpha = 0.8, max.overlaps = 20) +
  theme_minimal(base_size = 14) +
  labs(
    x = "PC1",
    y = "PC2",
    caption = "Figure 4: A PCA plot of species showing clustering and separation of sepcies."
  ) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    legend.position = "none"
  ) +
  scale_shape_manual(values = 1:length(unique(pca_data$Organism)))
print(PCA)




# Save the plots
ggsave("peat bog diversity metrics.png", plot = diversity_metrics, width = 13, height = 9, dpi = 300, device = ragg::agg_png)
ggsave("peat bog rank abundance.png", plot = rank_abundance, width = 13, height = 9, dpi = 300, device = "jpeg")
ggsave("peat bog relative abundance.png", plot = relative_abundance, width = 13, height = 9, dpi = 300, device = ragg::agg_png)
ggsave("peat bog PCA.png", plot = PCA, width = 13, height = 9, dpi = 300, device = "jpeg")

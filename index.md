<center><img src="plots/peat bog mesocosms.jpg" alt="Img"></center>

### Tutorial Aims

#### <a href="#section1"> 1. Diversity Analysis: Alpha, Beta, and Gamma Diversity</a>

#### <a href="#section2"> 2. Rank-Abundance Plot</a>

#### <a href="#section3"> 3. Relative Abundance Overview</a>

#### <a href="#section4"> 4. Principal Component Analysis (PCA)</a>

In this tutorial, we will learn how to analyze community composition data from peat bog mesocosms using R. Specifically, we will calculate and visualize different diversity metrics, create rank-abundance plots, visualize relative abundance, and perform PCA to explore species clustering. These skills are important for understanding ecological diversity and species distributions within peatland ecosystems.

You can get all of the resources for this tutorial from <a href="https://github.com//EdDataScienceEES/tutorial-Siya741.git" target="_blank">this GitHub repository</a>. Clone and download the repo as a zip file, then unzip it.

<a name="section1"></a>

## 1. Diversity Analysis: Alpha, Beta, and Gamma Diversity

Diversity analysis helps us understand the complexity of the community in different habitats. Alpha diversity provides a measure of species richness within each habitat, beta diversity compares the diversity between habitats, and gamma diversity represents the overall diversity across all habitats. By calculating and comparing these metrics, we can better understand how species are distributed within and between different habitats, and identify which habitats are more diverse or unique.

Start by opening RStudio and creating a new script by clicking on `File/ New File/ R Script`. Set the working directory, install necessary packages and load them:

```r
# Set the working directory
setwd("your_filepath")

# Install necessary packages (if not already installed)
install.packages("readr")
install.packages("vegan")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("ggrepel")
install.packages("ggpattern")

# Load required libraries
library(readr)
library(vegan)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggrepel)
library(ggpattern)
```

### Load the Dataset

Load the dataset containing peat bog mesocosm data:

```r
# Load the dataset
peat_bog_data <- read_csv("peat bog data.csv")

# Preview the dataset
View(peat_bog_data)
head(peat_bog_data)
str(peat_bog_data)
```

### Calculate Diversity Metrics

We will start by calculating alpha, beta, and gamma diversity metrics for different peat bog habitats:

```r
# Calculate Alpha diversity (Shannon's index for each habitat)
alpha_diversity <- peat_bog_data %>%
  group_by(habitat) %>%
  summarize(Shannon = diversity(count, index = "shannon"))

# Calculate Gamma diversity (pooled overall diversity across all habitats)
gamma_diversity <- diversity(colSums(
  peat_bog_data %>%
    pivot_wider(names_from = organism, values_from = count, values_fill = 0) %>%
    select(-habitat)
))

# Calculate Beta diversity (ratio of gamma to mean alpha diversity)
beta_diversity <- gamma_diversity / mean(alpha_diversity$Shannon)

# Combine Alpha, Beta, and Gamma diversity metrics into one dataset
combined_diversity <- bind_rows(
  alpha_diversity %>% mutate(Type = "Alpha") %>% rename(Value = Shannon),
  data.frame(habitat = "Beta", Value = beta_diversity, Type = "Beta"),
  data.frame(habitat = "Gamma", Value = gamma_diversity, Type = "Gamma")
)
```

### Plot Diversity Metrics

To visualize the calculated diversity metrics, use the following code:

```r
# Plot diversity metrics
diversity_metrics <- ggplot(combined_diversity, aes(x = habitat, y = Value, fill = Type, pattern = Type)) +
  geom_bar_pattern(stat = "identity", position = "dodge", width = 0.6, color = "black") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 4) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Diversity Metric",
    y = "Shannon Index",
    fill = "Diversity Type",
    caption = "Figure 1: Diversity metrics across habitats in peat bog mesocosms."
  ) +
  scale_fill_manual(values = c("Alpha" = "red", "Beta" = "blue", "Gamma" = "orange")) +
  scale_pattern_manual(values = c("Alpha" = "stripe", "Beta" = "crosshatch", "Gamma" = "circle"))

print(diversity_metrics)
```
<center> <img src="plots/peat bog diverity metrics.svg" alt="Img" style="width: 800px;"/> </center>

**Analysis**: The diversity metrics plot provides a comparison of the Shannon diversity index across three metrics: alpha, beta, and gamma diversity. Each habitat is also represented, with diversity broken down by sediment, sphagnum, and water. 

- **Alpha Diversity** (represented in red stripes) indicates the diversity within individual habitats. The values for sediment, sphagnum, and water are relatively similar, with slight variations suggesting some habitats have slightly higher species richness.
- **Beta Diversity** (in blue crosshatch) represents the differences in species composition between habitats. The beta diversity value is lower than the gamma diversity, suggesting that the habitats share some common species, but there is still moderate differentiation among them.
- **Gamma Diversity** (in orange dots) shows the overall diversity across all habitats combined, which is higher than alpha diversity, emphasizing the cumulative effect of pooling species from all habitats.

These metrics help us understand the complexity within individual habitats (alpha), the differences among habitats (beta), and the overall richness (gamma). This allows us to identify which habitats might be contributing most to the overall diversity.

<a name="section2"></a>

## 2. Rank-Abundance Plot

Rank-abundance plots help us understand the relative dominance of species within each habitat by ranking species from most to least abundant. These plots provide insights into community evenness—whether a few species dominate or many species are relatively equally abundant—and can reveal important ecological differences between habitats.

### Calculate Rank-Abundance Data

```r
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
```

### Plot Rank-Abundance

To create a rank-abundance plot, use the following code:

```r
# Plot rank-abundance
rank_abundance <- ggplot(ranked_data, aes(x = rank, y = count, color = habitat, shape = habitat, group = habitat)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Species Rank",
    y = "Log base 10 Abundance",
    color = "Habitat",
    shape = "Habitat",
    caption = "Figure 2: Rank-abundance plot for peat bog mesocosms."
  ) +
  scale_y_log10()

print(rank_abundance)
```
<center> <img src="plots/peat bog rank abundance.png" alt="Img" style="width: 800px;"/> </center>

**Analysis**: The rank-abundance plot shows the distribution of species' abundances across different habitats. The slope of the lines indicates species evenness; a steeper slope suggests a few dominant species, while a flatter line indicates a more even community. For example, in the plot above, we can see that the overall habitat has a more evenly distributed species composition compared to individual habitats like sediment, sphagnum, or water. The difference in slopes among habitats can help identify which habitats have dominant species and which have a more balanced community structure.

<a name="section3"></a>

## 3. Relative Abundance Overview

Relative abundance plots allow us to compare the contribution of different species to the total community in each habitat. These plots are helpful in identifying which species are most prevalent within a habitat and how the composition of the community differs between habitats, providing insights into the unique characteristics of each habitat.

### Calculate Relative Abundance

```r
# Calculate relative abundance for each habitat
abundance_data <- bind_rows(peat_bog_data, overall_abundance) %>%
  group_by(habitat) %>%
  mutate(relative_abundance = count / sum(count)) %>%
  ungroup()
```

### Plot Relative Abundance

To create a multi-panel bar plot of relative abundance, use the following code:

```r
# Plot multi-panel bar plot with relative abundance
relative_abundance <- ggplot(abundance_data, aes(x = organism, y = relative_abundance, fill = habitat, pattern = habitat)) +
  geom_bar_pattern(stat = "identity", position = "dodge", color = "black", width = 0.8) +
  geom_text(aes(label = count), vjust = -0.5, size = 3) + # Add original counts on top of bars
  facet_wrap(~ habitat, scales = "free_y", ncol = 2) + # Create multi-panel plots
  theme_minimal(base_size = 14) +
  labs(
    x = "Species",
    y = "Relative Abundance",
    caption = "Figure 3: A multi-panel bar plot showing relative abundance of species across different habitats in peat bog mesocosms."
  ) +
  theme(
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
  scale_fill_manual(values = c("red", "orange", "green", "blue")) +
  scale_pattern_manual(values = c("stripe", "crosshatch", "circle", "none"))

print(relative_abundance)
```
<center> <img src="plots/peat bog relative abundance.svg" alt="Img" style="width: 800px;"/> </center>

**Analysis**: The relative abundance plot shows how individual species contribute to the overall community within each habitat. By comparing the panels, we can see the different dominant species in each habitat. For instance, in the overall panel, **Algae** and **Cladocera** dominate, whereas in the sediment habitat, **Diatoms** and **Heliozoans** are prominent.

- **Species Dominance**: Different species are clearly dominant in each habitat. For example, in the overall habitat, algae and cladocera dominate, while other habitats show differences in their dominant species.
- **Habitat Differences**: By comparing all four panels, it becomes clear that each habitat has a unique species composition. This helps in understanding which habitats support greater biodiversity and which species are more specialized to particular environments.

This visualization provides insights into the uniqueness of each habitat in terms of its species composition and the dominant species in each habitat.

<a name="section4"></a>

## 4. Principal Component Analysis (PCA)

Principal Component Analysis (PCA) is a powerful tool for reducing the dimensionality of a dataset and identifying patterns. In this context, PCA helps us explore how species cluster together based on their abundance across different habitats. By plotting the first two principal components, we can visualize similarities and differences between species, which can provide insights into the community structure and species associations.

### Perform PCA

```r
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
```

### Plot PCA

To visualize the results of the PCA, use the following code:

```r
# Plot PCA plot
PCA <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Organism, shape = Organism)) +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_text_repel(aes(label = Organism), size = 4, alpha = 0.8, max.overlaps = 20) +
  theme_minimal(base_size = 14) +
  labs(
    x = "PC1",
    y = "PC2",
    caption = "Figure 4: PCA plot showing species clustering in peat bog mesocosms."
  )

print(PCA)
```
<center> <img src="plots/peat bog PCA.png" alt="Img" style="width: 800px;"/> </center>

**Analysis**: The PCA plot helps visualize the relationships between species based on their abundance in different habitats. Species that are closer together have similar habitat preferences or characteristics.

- **Clustering of Species**: Species that are closer together in the PCA plot, such as **Insect Larvae**, **Copepods**, and **Rotifers**, indicate similar habitat preferences or associations. On the other hand, species like **Algae** and **Flagellates** are located far from most other species, suggesting unique characteristics or specific habitat requirements.
- **Axes Interpretation**: The first two principal components (PC1 and PC2) explain the most variability in the dataset, with PC1 separating species based on their overall abundance and habitat preference. For instance, **Algae** has a positive value along PC2, suggesting a distinct preference or behavior compared to other species.
- **Habitat Influence**: The spread of species along both principal components gives insights into which habitats they are most commonly associated with. For example, species like **Ciliates** and **Microturbellarians** are positioned far from **Flagellates** and **Desmids**, indicating different habitat affinities.

The PCA plot is useful for identifying groups of species that are closely related in terms of their habitat usage, and for detecting unique species that might require specific environmental conditions.

### Summary

In this tutorial, we learned:

##### - How to calculate and visualize alpha, beta, and gamma diversity.
##### - How to create a rank-abundance plot.
##### - How to visualize relative abundance of species across habitats.
##### - How to perform and interpret a Principal Component Analysis (PCA).

For more information on the `vegan` package, check out the <a href="https://cran.r-project.org/web/packages/vegan/vegan.pdf" target="_blank">official documentation</a>.

<hr>
<hr>

#### Check out our <a href="https://ourcodingclub.github.io/links/" target="_blank">Useful links</a> page where you can find loads of guides and cheatsheets.

#### If you have any questions about completing this tutorial, please contact us on ourcodingclub@gmail.com

####

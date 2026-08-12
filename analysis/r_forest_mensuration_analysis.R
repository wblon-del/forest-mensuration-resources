# Forest Mensuration — Basic R Analysis
# Teaching script for the sample tree inventory.

# Install/load packages as needed:
# install.packages(c("readr", "dplyr", "ggplot2"))
library(readr)
library(dplyr)
library(ggplot2)

data <- read_csv("data/sample_tree_inventory.csv", show_col_types = FALSE)

# Calculate basal area from DBH in cm.
data <- data %>%
  mutate(BasalArea_m2 = pi * DBH_cm^2 / 40000)

# Basic summaries by plot.
plot_summary <- data %>%
  group_by(Plot) %>%
  summarise(
    Trees = n(),
    Mean_DBH_cm = mean(DBH_cm),
    QuadraticMean_DBH_cm = sqrt(mean(DBH_cm^2)),
    BasalArea_m2 = sum(BasalArea_m2),
    .groups = "drop"
  )

print(plot_summary)

# If each plot represents 0.05 ha, expand plot results to per-hectare values.
plot_area_ha <- 0.05
plot_summary <- plot_summary %>%
  mutate(
    Trees_per_ha = Trees / plot_area_ha,
    BasalArea_m2_per_ha = BasalArea_m2 / plot_area_ha
  )

print(plot_summary)

# Diameter distribution.
ggplot(data, aes(x = DBH_cm)) +
  geom_histogram(binwidth = 5, boundary = 0) +
  labs(
    title = "Tree Diameter Distribution",
    x = "DBH (cm)",
    y = "Number of trees"
  )

# Height versus DBH.
ggplot(data, aes(x = DBH_cm, y = Height_m, shape = Species)) +
  geom_point(size = 3) +
  labs(
    title = "Tree Height versus DBH",
    x = "DBH (cm)",
    y = "Height (m)"
  )

# Species counts.
species_summary <- data %>% count(Species, sort = TRUE)
print(species_summary)

# Load necessary libraries
library(here)
library(tidyverse)
library(vroom)
library(vtable)

# Read the data
measured_growth_parameters <- vroom::vroom(
  file = here:::here("data", "measured_growth_parameters.csv"),
  col_types = "ffiddi"
)

# Filter the data for "Frisian Dew" cultivar only
frisian_dew_data <-
  measured_growth_parameters |>
  dplyr::filter(Cultivar == "Frisian Dew")

# Summary statistics for the UV and control groups
vtable::sumtable(
  frisian_dew_data,
  group = "Group",
  title = "Summary statistics for the UV and control groups (Frisian Dew cultivar)",
  vars = c("Height (cm)", "Stem cir. (cm)", "# Internodes"),
  digits = 2,
  group.long = TRUE,
  file = here:::here("results", "summary_statistics_frisian-dew")
)


# Analysis plant height comparison ---------------------------------------------

# Boxplot
ggplot2::ggplot(
  frisian_dew_data,
  ggplot2::aes(x = Group, y = `Height (cm)`, fill = Group)
) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(
    title = "Plant height comparison",
    x = "Group",
    y = "Height (cm)"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = here::here("results", "boxplot_height_comparison.svg"),
  units = "cm",
  width = 9,
  height = 6
)

# Welch's t-test
t.test(`Height (cm)` ~ Group, data = frisian_dew_data)


# Analysis stem circumference comparison ----------------------------------

# Boxplot
ggplot2::ggplot(
  frisian_dew_data,
  ggplot2::aes(x = Group, y = `Stem cir. (cm)`, fill = Group)
) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(
    title = "Stem circumference comparison",
    x = "Group",
    y = "Stem circumference (cm)"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = here::here("results", "boxplot_stem-circumference_comparison.svg"),
  units = "cm",
  width = 9,
  height = 6
)

# Welch's t-test
t.test(`Stem cir. (cm)` ~ Group, data = frisian_dew_data)


# Analysis number of internodes -------------------------------------------

# Boxplot
ggplot2::ggplot(
  frisian_dew_data,
  ggplot2::aes(x = Group, y = `# Internodes`, fill = Group)
) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(
    title = "Number of internodes comparison",
    x = "Group",
    y = "Number of internodes"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  filename = here::here("results", "boxplot_no-internodes_comparison.svg"),
  units = "cm",
  width = 9,
  height = 6
)

# Welch's t-test
t.test(`# Internodes` ~ Group, data = frisian_dew_data)

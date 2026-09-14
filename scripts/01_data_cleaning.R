# 01_data_cleaning.R
# Basic data-cleaning workflow for a child-development research project.

library(tidyverse)

# Replace with a de-identified or simulated dataset.
dat <- read_csv("data/example_data.csv")

# Inspect structure and missingness.
glimpse(dat)
summary(dat)

# Example cleaning steps.
clean_dat <- dat %>%
  distinct() %>%
  mutate(
    age = as.numeric(age),
    condition = factor(condition)
  ) %>%
  filter(!is.na(age))

# Save cleaned data locally. This file is ignored by Git by default.
write_csv(clean_dat, "data/clean_data.csv")

# 02_analysis.R
# Example analysis workflow for binary child-development outcomes.

library(tidyverse)
library(lme4)
library(broom.mixed)

# Load cleaned data created by 01_data_cleaning.R.
dat <- read_csv("data/clean_data.csv")

# Descriptive statistics.
descriptives <- dat %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    .groups = "drop"
  )

print(descriptives)

# Example mixed-effects logistic regression.
# Expected variables: response (0/1), condition, measure, participant_id.
model <- glmer(
  response ~ condition * measure + (1 | participant_id),
  data = dat,
  family = binomial
)

summary(model)

# Export tidy fixed-effect estimates.
model_results <- tidy(model, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
write_csv(model_results, "results/model_results.csv")

# Example visualization.
plot_data <- dat %>%
  group_by(condition, measure) %>%
  summarise(
    proportion = mean(response, na.rm = TRUE),
    .groups = "drop"
  )

p <- ggplot(plot_data, aes(x = measure, y = proportion, group = condition)) +
  geom_point(aes(shape = condition), size = 3) +
  geom_line(aes(linetype = condition)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = NULL,
    y = "Proportion selecting target response",
    title = "Responses by Condition and Measure"
  ) +
  theme_minimal()

ggsave("figures/response_by_condition.png", p, width = 8, height = 5, dpi = 300)

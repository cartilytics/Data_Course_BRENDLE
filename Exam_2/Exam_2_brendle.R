library(tidyverse)

unicef <- read_csv("unicef-u5mr.csv")
glimpse(unicef)

library(tidyverse)

unicef <- read_csv("unicef-u5mr.csv")

unicef_tidy <- unicef %>%
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    values_to = "U5MR"
  ) %>%
  mutate(
    Year = as.integer(str_remove(Year, "U5MR\\."))
  )

glimpse(unicef_tidy)

ggplot(unicef_tidy, aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line(alpha = 0.6) +
  facet_wrap(~ Continent) +
  labs(
    x = "Year",
    y = "U5MR (deaths per 1000 live births)",
    title = "Under-5 Mortality Rate Over Time by Country and Continent"
  ) +
  theme_bw()

unicef_continent_mean <- unicef_tidy %>%
  group_by(Continent, Year) %>%
  summarise(
    Mean_U5MR = mean(U5MR, na.rm = TRUE),
    .groups = "drop"
  )

p2 <- ggplot(unicef_continent_mean,
             aes(x = Year, y = Mean_U5MR, color = Continent)) +
  geom_line(size = 1.1) +
  labs(
    x = "Year",
    y = "Mean U5MR",
    title = "Mean Under-5 Mortality Rate by Continent Over Time"
  ) +
  theme_bw()

p2

mod1 <- lm(U5MR ~ Year, data = unicef_tidy)

mod2 <- lm(U5MR ~ Year + Continent, data = unicef_tidy)

mod3 <- lm(U5MR ~ Year * Continent, data = unicef_tidy)

# Compare AIC values
AIC(mod1, mod2, mod3)

# Compare R-squared values
summary(mod1)$r.squared
summary(mod2)$r.squared
summary(mod3)$r.squared

library(dplyr)
library(ggplot2)
library(purrr)   # for map
library(broom)   # for augment (if not installed: install.packages("broom"))

pred_grid <- expand.grid(
  Year = seq(min(unicef_tidy$Year), max(unicef_tidy$Year), by = 5),
  Continent = unique(unicef_tidy$Continent)
)

pred1 <- augment(mod1, newdata = pred_grid) %>%
  mutate(model = "mod1")

pred2 <- augment(mod2, newdata = pred_grid) %>%
  mutate(model = "mod2")

pred3 <- augment(mod3, newdata = pred_grid) %>%
  mutate(model = "mod3")

pred_all <- bind_rows(pred1, pred2, pred3)

p_models <- ggplot(pred_all,
                   aes(x = Year, y = .fitted, color = Continent)) +
  geom_line(size = 1.1) +
  
  ecuador_2020 <- data.frame(
    CountryName = "Ecuador",
    Continent = "Americas",
    Year = 2020
  )

# Use mod3 (your preferred model)
ecuador_pred <- predict(mod3, newdata = ecuador_2020)

ecuador_pred
difference <- ecuador_pred - 13
difference
data.frame(
  Model = "mod3",
  Prediction = ecuador_pred,
  Reality = 13,
  Difference = difference
)
# BONUS
ecuador_2020 <- data.frame(CountryName="Ecuador", Continent="Americas", Year=2020)

ecuador_pred <- predict(mod3, newdata=ecuador_2020)
ecuador_pred - 13   # error

mod4 <- lm(log(U5MR) ~ Year * Continent, data = unicef_tidy)
ecuador_pred_mod4 <- exp(predict(mod4, newdata=ecuador_2020))
ecuador_pred_mod4 - 13  # improved error

data.frame(
  Model = "mod4",
  Prediction = ecuador_pred_mod4,
  Reality = 13,
  Difference = ecuador_pred_mod4 - 13
)
#mod4 gave the best prediction at around 1/1000 deaths
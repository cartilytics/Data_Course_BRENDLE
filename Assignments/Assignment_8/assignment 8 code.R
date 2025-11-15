rm(list = ls())
library(tidyverse)
mush <- read.csv("Data/mushroom_growth.csv")
mush$Species <- as.factor(mush$Species)
mush$Temperature <- as.factor(mush$Temperature)
model1 <- lm(GrowthRate ~ Light, data = mush)

model2 <- lm(GrowthRate ~ Light + Nitrogen, data = mush)

model3 <- lm(GrowthRate ~ Light + Nitrogen + Humidity + Temperature,
             data = mush)

model4 <- lm(GrowthRate ~ Species + Temperature + Light + Nitrogen + Humidity,
             data = mush)
model1
model2
model3
model4
mse1 <- mean(residuals(model1)^2)
mse2 <- mean(residuals(model2)^2)
mse3 <- mean(residuals(model3)^2)
mse4 <- mean(residuals(model4)^2)

mse1; mse2; mse3; mse4
best_model <- model4
newdata <- data.frame(
  Light      = seq(min(mush$Light), max(mush$Light), length.out = 50),
  Nitrogen   = mean(mush$Nitrogen),
  Humidity   = mean(mush$Humidity),
  Temperature = factor("20", levels = levels(mush$Temperature)),
  Species    = mush$Species[1]
)
# 0. (You already did this, but it’s safe to keep here)
mush$Species     <- as.factor(mush$Species)
mush$Temperature <- as.factor(mush$Temperature)

# 1: Light only
model1 <- lm(GrowthRate ~ Light, data = mush)

# 2: Light + Nitrogen
model2 <- lm(GrowthRate ~ Light + Nitrogen, data = mush)

# 3: Light + Nitrogen + Temperature (20 vs 25)
model3 <- lm(GrowthRate ~ Light + Nitrogen + Temperature, data = mush)

# 4: Full-ish model: Species + Temperature + Light + Nitrogen
model4 <- lm(GrowthRate ~ Species + Temperature + Light + Nitrogen, data = mush)
mse1 <- mean(residuals(model1)^2)
mse2 <- mean(residuals(model2)^2)
mse3 <- mean(residuals(model3)^2)
mse4 <- mean(residuals(model4)^2)

mse1; mse2; mse3; mse4
best_model <- model4

newdata <- data.frame(
  Light      = seq(min(mush$Light, na.rm = TRUE),
                   max(mush$Light, na.rm = TRUE),
                   length.out = 50),
  Nitrogen   = mean(mush$Nitrogen, na.rm = TRUE),
  Temperature = factor("20", levels = levels(mush$Temperature)),
  Species    = mush$Species[1]
)

newdata$pred_GrowthRate <- predict(best_model, newdata)

range(mush$GrowthRate, na.rm = TRUE)
range(newdata$pred_GrowthRate, na.rm = TRUE)
ggplot() +
  geom_point(data = mush,
             aes(x = Light, y = GrowthRate),
             alpha = 0.6) +
  geom_line(data = newdata,
            aes(x = Light, y = pred_GrowthRate),
            linewidth = 1.1) +
  theme_bw()
#1. all the values are positive ad within range so none are meaningless really. the extreme light values are
# extapolated though so they're less reliable.
#2. i dont think i saw any non linear relationships, my keyboard is so sticky i spilled soda on it.
# BIOL 3100 Exam 1 redo  –  Carter Brendle
# All tasks done in R, using cleaned_covid_data.csv
library(tidyverse)
# I. Read the cleaned_covid_data.csv file into an R data frame
covid <- read_csv("cleaned_covid_data.csv", show_col_types = FALSE)

glimpse(covid)
# II. Subset to just states that begin with “A”  -> A_states
# 'Province_State' is the state column in the cleaned data
A_states <- covid %>%
  filter(str_starts(Province_State, "A"))

# Check the states included
A_states %>% distinct(Province_State)
# III. Plot of that subset: Deaths over time, faceted by state
#   - scatterplot
#   - loess curves, no standard error shading
#   - free y-scales in each facet

plot_A_states <- ggplot(A_states,
                        aes(x = Last_Update, y = Deaths)) +
  geom_point(alpha = 0.5, size = 1.2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  facet_wrap(~ Province_State, scales = "free_y") +
  labs(
    title = "Deaths over time for A states",
    x = "Date",
    y = "Deaths"
  ) +
  theme_bw()

plot_A_states
ggsave("Exam1_plot_A_states.png", plot_A_states,
       width = 8, height = 6, dpi = 300)
# IV. For the full dataset:
#     Find the *peak* Case_Fatality_Ratio for each state
#     -> state_max_fatality_rate
state_max_fatality_rate <- covid %>%
  group_by(Province_State) %>%
  summarise(
    Maximum_Fatality_Ratio = max(Case_Fatality_Ratio,
                                 na.rm = TRUE)
  ) %>%
  arrange(desc(Maximum_Fatality_Ratio))

state_max_fatality_rate
# V. Use that new data frame to make another plot:
#    Bar plot of Maximum_Fatality_Ratio by Province_State
#    - x axis in descending order of max CFR
#    - x-labels rotated 90 degrees
state_max_fatality_rate <- state_max_fatality_rate %>%
  mutate(
    Province_State = fct_reorder(
      Province_State, Maximum_Fatality_Ratio, .desc = TRUE
    )
  )

plot_max_cfr <- ggplot(state_max_fatality_rate,
                       aes(x = Province_State,
                           y = Maximum_Fatality_Ratio)) +
  geom_col() +
  labs(
    title = "Maximum Case Fatality Ratio by State",
    x = "State",
    y = "Maximum Case Fatality Ratio"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 90, vjust = 0.5, hjust = 1
  ))

plot_max_cfr
ggsave("Exam1_plot_max_fatality_ratio.png", plot_max_cfr,
       width = 10, height = 6, dpi = 300)
# VI. BONUS (optional): cumulative deaths for the entire US
# Sum deaths over all states for each date,
# then compute cumulative sum over time
us_cum_deaths <- covid %>%
  group_by(Last_Update) %>%
  summarise(daily_deaths = sum(Deaths, na.rm = TRUE)) %>%
  arrange(Last_Update) %>%
  mutate(cumulative_deaths = cumsum(daily_deaths))

plot_us_cum <- ggplot(us_cum_deaths,
                      aes(x = Last_Update, y = cumulative_deaths)) +
  geom_line() +
  labs(
    title = "Cumulative US deaths over time",
    x = "Date",
    y = "Cumulative deaths"
  ) +
  theme_bw()

plot_us_cum

ggsave("Exam1_plot_US_cumulative_deaths.png", plot_us_cum,
       width = 8, height = 5, dpi = 300)
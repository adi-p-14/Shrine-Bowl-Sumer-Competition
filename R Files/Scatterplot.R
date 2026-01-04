library(readxl)
library(dplyr)
library(ggplot2)


df <- read.csv("C:/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/Player Tiers.csv")

df <- df %>%
  mutate(
    quadrant = case_when(
      Speed >= 0 & Production >= 0 ~ "Q1: Projectable Athleticism / Higher Production",
      Speed >= 0 & Production <  0 ~ "Q2: Projectable Athleticism / Lower Production",
      Speed <  0 & Production <  0 ~ "Q3: Non-Projectable Athleticism / Lower Production",
      Speed <  0 & Production >= 0 ~ "Q4: Non Projectable Athletcism / Higher Production"
    )
  )

ggplot(df, aes(y = Speed, x = Production, color = forecast_tier)) +
  geom_point(size = 2.8, alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  annotate("text", x =  2, y =  2, label = "Q1\nProjectable Athleticism / Higher Production", alpha = 0.75) +
  annotate("text", x = 2, y =  -2, label = "Q4\nNon Projectable Athleticism / Higher Production", alpha = 0.75) +
  annotate("text", x = -2, y = -2, label = "Q3\nNon-Projectable Athleticism / Lower Production", alpha = 0.75) +
  annotate("text", x =  -2, y = 2, label = "Q2\nProjectable Athleticism / Lower Production", alpha = 0.75) +
  theme_minimal()
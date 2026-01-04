library(readxl)
library(dplyr)
library(ggplot2)


df <- read_excel("C:/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/Player Speed Data/All Speed Data.xlsx", sheet = "Full Roster")

df <- df %>%
  rename(
    Speed = "Average Speed Z-Score",
    Production = "Average Production Z-Score"
  )

df <- df %>%
  mutate(
    quadrant = case_when(
      Speed >= 0 & Production >= 0 ~ "Q1: Projectable Athleticism / Higher Production",
      Speed >= 0 & Production <  0 ~ "Q2: Projectable Athleticism / Lower Production",
      Speed <  0 & Production <  0 ~ "Q3: Non-Projectable Athleticism / Lower Production",
      Speed <  0 & Production >= 0 ~ "Q4: Non Projectable Athletcism / Higher Production"
    )
  )

ggplot(df, aes(y = Speed, x = Production, color = Position)) +
  geom_point(size = 2.8, alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  annotate("text", x =  1.25, y =  2, label = "Q1\nProjectable Athleticism / Higher Production", alpha = 0.75) +
  annotate("text", x = 1.25, y =  -2, label = "Q4\nNon Projectable Athletcism / Higher Production", alpha = 0.75) +
  annotate("text", x = -1.125, y = -2, label = "Q3\nNon-Projectable Athleticism / Lower Production", alpha = 0.75) +
  annotate("text", x =  -1.125, y = 2, label = "Q2\nProjectable Athleticism / Lower Production", alpha = 0.75) +
  theme_minimal()
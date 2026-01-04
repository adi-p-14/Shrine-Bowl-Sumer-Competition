library(readxl)
library(dplyr)

df <- read_excel("C:/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/Player Speed Data/All Speed Data.xlsx", sheet = "Full Roster")

df <- df %>%
  rename(
    Speed = "Average Speed Z-Score",
    Production = "Average Production Z-Score"
  )

df <- df %>%
  mutate(
    Projection_Score = 0.75 * Speed + 0.25 * Production
  )

df <- df %>%
  group_by(Position) %>%
  mutate(
    projection_pct = round(percent_rank(Projection_Score), digits = 2)*100
  ) %>%
  ungroup()

df <- df %>%
  mutate(
    forecast_tier = case_when(
      projection_pct >= 80 ~ "Tier 1: Draftable Player + Expected Contributor",
      projection_pct >= 60 ~ "Tier 2a: Draftable Player + Needs Development to Consistently Produce",
      projection_pct >= 40 ~ "Tier 2b: PFA + Limited Athletic Ceiling",
      projection_pct >= 20 ~ "Tier 3: Routine Practice Squad Player",
      TRUE                ~ "Tier 4: Camp Invite/90-Man Roster Player"
    )
  )

df <- df %>%
  select(`GSIS ID`, `Player Name`, Position, Speed, Production, projection_pct, forecast_tier)

write.csv(df, "/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/Player Tiers.csv", row.names = FALSE)

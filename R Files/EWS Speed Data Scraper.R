library(arrow)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(ggplot2)
library(slider)
library(readxl)


#Skim Dataset
ds <- open_dataset("C:/Users/jayap/OneDrive - University of Massachusetts/Shrine Bowl Competition/2022_West_Practice_2.snappy.parquet")
available_ids <- ds %>%
  select(gsis_id) %>%
  distinct() %>%
  collect()

#Identify Team Periods
slice <- ds %>%
  distinct(drill_type) %>%
  collect()

#Read in Player IDs
player_df <- read.csv(player_file) %>%
  select(college_gsis_id, position, player_name)
valid_df <- player_df %>%
  mutate(college_gsis_id = as.character(college_gsis_id)) %>%
  semi_join(
    available_ids,
    by = c("college_gsis_id" = "gsis_id")
  )


# Example list of gsis_ids by position
player_file <- "C:/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/shrine_bowl_players_nfl_rookie_stats.csv"
rb_df <- read_excel(player_file, sheet = "RB") %>%
  select(college_gsis_id, player_name)
wr_df <- read_excel(player_file, sheet = "WR") %>%
  select(college_gsis_id, player_name)
te_df <- read_excel(player_file, sheet = "TE") %>%
  select(college_gsis_id, player_name)
ol_df <- read_excel(player_file, sheet = "OL") %>%
  select(college_gsis_id, player_name)
dt_df <- read_excel(player_file, sheet = "DT") %>%
  select(college_gsis_id, player_name)
de_df <- read_excel(player_file, sheet = "DE") %>%
  select(college_gsis_id, player_name)
ib_df <- read_excel(player_file, sheet = "IB") %>%
  select(college_gsis_id, player_name)
ob_df <- read_excel(player_file, sheet = "OB") %>%
  select(college_gsis_id, player_name)
dc_df <- read_excel(player_file, sheet = "DC") %>%
  select(college_gsis_id, player_name)
ds_df <- read_excel(player_file, sheet = "DS") %>%
  select(college_gsis_id, player_name)

valid_rb <- rb_df %>%
  mutate(college_gsis_id = as.character(college_gsis_id)) %>%
  semi_join(
    available_ids,
    by = c("college_gsis_id" = "gsis_id")
  )
valid_wr <- wr_df %>%
  mutate(college_gsis_id = as.character(college_gsis_id)) %>%
  semi_join(
    available_ids,
    by = c("college_gsis_id" = "gsis_id")
  )
valid_te <- te_df %>%
  mutate(college_gsis_id = as.character(college_gsis_id)) %>%
  semi_join(
    available_ids,
    by = c("college_gsis_id" = "gsis_id")
  )
valid_ol <- ol_df %>%
  mutate(college_gsis_id = as.character(college_gsis_id)) %>%
  semi_join(
    available_ids,
    by = c("college_gsis_id" = "gsis_id")
  )
valid_dt <- dt_df %>%
  mutate(college_gsis_id = as.character(college_gsis_id)) %>%
  semi_join(
    available_ids,
    by = c("college_gsis_id" = "gsis_id")
  )
valid_de <- de_df %>%
  mutate(college_gsis_id = as.character(college_gsis_id)) %>%
  semi_join(
    available_ids,
    by = c("college_gsis_id" = "gsis_id")
  )
valid_ib <- ib_df %>%
  mutate(college_gsis_id = as.character(college_gsis_id)) %>%
  semi_join(
    available_ids,
    by = c("college_gsis_id" = "gsis_id")
  )
valid_ob <- ob_df %>%
  mutate(college_gsis_id = as.character(college_gsis_id)) %>%
  semi_join(
    available_ids,
    by = c("college_gsis_id" = "gsis_id")
  )
valid_dc <- dc_df %>%
  mutate(college_gsis_id = as.character(college_gsis_id)) %>%
  semi_join(
    available_ids,
    by = c("college_gsis_id" = "gsis_id")
  )
valid_ds <- ds_df %>%
  mutate(college_gsis_id = as.character(college_gsis_id)) %>%
  semi_join(
    available_ids,
    by = c("college_gsis_id" = "gsis_id")
  )


# --------------------------------------------------
# 1. Load + prep player data
# -------------------------------------------------
drills <- paste0("D", 1:10)
drills <- paste0("Team ", 1:3)
drills <- paste("Team")

#Function for Player Tracking Data
compute_player_route_metrics <- function(ds_path, col_gsis_id, drive_levels, min_frames = 12, min_duration = 1, max_extraneous = 20) {
  
  player_df <- ds %>%
    filter(gsis_id == col_gsis_id,
           drill_type %in% drive_levels) %>%
    select(gsis_id, drill_type, ts, x, y, z, a, s) %>%
    collect() %>%
    arrange(drill_type, ts)
  
  player_df <- player_df %>%
    mutate(ts = as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")) %>%
    arrange(drill_type, ts)
  
  # 2. Smooth coordinates
  player_df <- player_df %>%
    group_by(drill_type) %>%
    mutate(
      x_s = slide_dbl(x, median, .before = 1, .after = 1, .complete = TRUE),
      y_s = slide_dbl(y, median, .before = 1, .after = 1, .complete = TRUE)
    ) %>%
    ungroup()
  
  # 3. Displacement + kinematics
  route_df <- player_df %>%
    group_by(drill_type) %>%
    mutate(
      dx = x_s - lag(x_s),
      dy = y_s - lag(y_s),
      dt = as.numeric(difftime(ts, lag(ts), units = "secs")),
      xy_disp = sqrt(dx^2 + dy^2),
      speed = xy_disp / dt,
      heading = atan2(dy, dx),
      delta_heading = abs(heading - lag(heading)),
      forward_dir = sign(median(dy, na.rm = TRUE)),
      forward_progress = dy * forward_dir
    ) %>%
    ungroup()
  
  # 4. Valid motion
  route_df <- route_df %>%
    mutate(
      heading_ok = if_else(speed > 0.9, delta_heading < 0.6, delta_heading < 1.2),
      valid_motion = !is.na(speed) & speed < 9 & xy_disp > 0.1 & xy_disp < 1.2 &
        heading_ok & (speed > 0.9 | forward_progress > 0.2)
    )
  
  # 5. Route termination
  route_df <- route_df %>%
    group_by(drill_type) %>%
    mutate(
      stop_low_speed = (speed < 0.5) & coalesce(lag(speed < 0.5), FALSE) & coalesce(lag(speed < 0.7, 2), FALSE),
      stop_backward = sign(dy) != forward_dir & abs(dy) > 0.3,
      stop_gap = dt > 0.25,
      stop_drill = drill_type != lag(drill_type),
      stop_route = stop_low_speed | stop_backward | stop_gap | stop_drill
    ) %>%
    ungroup()
  
  # 6. Route segmentation
  route_df <- route_df %>%
    arrange(drill_type, ts) %>%
    mutate(
      route_start = replace_na(valid_motion, FALSE) & 
        (replace_na(stop_route, TRUE) | !replace_na(lag(valid_motion, default = FALSE), FALSE)),
      route_id = cumsum(route_start),
      route_id = if_else(replace_na(valid_motion, FALSE), route_id, NA_integer_)
    )
  
  route_df <- route_df %>%
    group_by(route_id) %>%
    mutate(
      accel = (speed - lag(speed)) / dt
    ) %>%
    ungroup()
  
  # 7. Filter small fragments
  route_summary <- route_df %>%
    filter(!is.na(route_id)) %>%
    group_by(route_id) %>%
    summarise(
      frames = n(),
      valid_speed_frames = sum(!is.na(speed)),
      valid_accel_frames = sum(!is.na(accel)),
      duration = sum(dt, na.rm = TRUE),
      total_disp = sum(xy_disp, na.rm = TRUE),
      .groups = "drop"
    )
  
  good_routes <- route_summary %>%
    filter(
      frames >= 12,
      valid_speed_frames >= 2,
      duration >= .5,
      total_disp >= 6
    )
  
  route_df <- route_df %>% semi_join(good_routes, by = "route_id")
  
  
  # 8. Compute route-level metrics
  print("About to summarise route metrics")
  
  route_metrics <- route_df %>%
    group_by(gsis_id, drill_type, route_id) %>%
    summarise(
      frames = n(),
      duration = sum(dt, na.rm = TRUE),
      
      avg_speed = mean(speed, na.rm = TRUE),
      p75_speed = if (any(!is.na(speed))) {
        quantile(speed, 0.75, na.rm = TRUE)
      } else {
        NA_real_
      },
      max_speed = if (any(!is.na(speed))) {
        max(speed, na.rm = TRUE)
      } else {
        NA_real_
      },
      
      p95_accel = if_else(any(!is.na(accel)), quantile(accel, 0.95, na.rm = TRUE), NA_real_),
      max_accel = if (any(!is.na(accel))) {
        max(accel, na.rm = TRUE)
      } else {
        NA_real_
      },
      
      total_disp = sum(xy_disp, na.rm = TRUE),
      net_disp = sqrt((last(x_s) - first(x_s))^2 + (last(y_s) - first(y_s))^2),
      extraneous_dist = total_disp - net_disp,
      waste_ratio = extraneous_dist / net_disp,
      
      .groups = "drop"
    )
  
  print("Summarise complete")
  
  return(route_metrics)
}

# Apply function to all players
drills <- paste0("Drive ", 1:13)
all_metrics_rb <- lapply(1:nrow(valid_rb), function(i) {
  res <- compute_player_route_metrics(ds, valid_rb$college_gsis_id[i], drills) %>%
    mutate(player_name = valid_rb$player_name[i])%>%
    group_by(route_id)
  # Print progress
  print(paste0(valid_rb$college_gsis_id[i], ": Done"))
  gc()  # free memory before next iteration
  return(res)
}) %>% bind_rows()

all_metrics_wr <- lapply(1:nrow(valid_wr), function(i) {
  res <- compute_player_route_metrics(ds, valid_wr$college_gsis_id[i], drills) %>%
    mutate(player_name = valid_wr$player_name[i])%>%
    group_by(route_id)
  # Print progress
  print(paste0(valid_wr$college_gsis_id[i], ": Done"))
  gc()  # free memory before next iteration
  return(res)
}) %>% bind_rows()

all_metrics_te <- lapply(1:nrow(valid_te), function(i) {
  res <- compute_player_route_metrics(ds, valid_te$college_gsis_id[i], drills) %>%
    mutate(player_name = valid_te$player_name[i])%>%
    group_by(route_id)
  # Print progress
  print(paste0(valid_te$college_gsis_id[i], ": Done"))
  gc()  # free memory before next iteration
  return(res)
}) %>% bind_rows()

all_metrics_ol <- lapply(1:nrow(valid_ol), function(i) {
  res <- compute_player_route_metrics(ds, valid_ol$college_gsis_id[i], drills) %>%
    mutate(player_name = valid_ol$player_name[i])%>%
    group_by(route_id)
  # Print progress
  print(paste0(valid_ol$college_gsis_id[i], ": Done"))
  gc()  # free memory before next iteration
  return(res)
}) %>% bind_rows()

all_metrics_dt <- lapply(1:nrow(valid_dt), function(i) {
  res <- compute_player_route_metrics(ds, valid_dt$college_gsis_id[i], drills) %>%
    mutate(player_name = valid_dt$player_name[i])%>%
    group_by(route_id)
  # Print progress
  print(paste0(valid_dt$college_gsis_id[i], ": Done"))
  gc()  # free memory before next iteration
  return(res)
}) %>% bind_rows()

all_metrics_de <- lapply(1:nrow(valid_de), function(i) {
  res <- compute_player_route_metrics(ds, valid_de$college_gsis_id[i], drills) %>%
    mutate(player_name = valid_de$player_name[i])%>%
    group_by(route_id)
  # Print progress
  print(paste0(valid_de$college_gsis_id[i], ": Done"))
  gc()  # free memory before next iteration
  return(res)
}) %>% bind_rows()

all_metrics_ib <- lapply(1:nrow(valid_ib), function(i) {
  res <- compute_player_route_metrics(ds, valid_ib$college_gsis_id[i], drills) %>%
    mutate(player_name = valid_ib$player_name[i])%>%
    group_by(route_id)
  # Print progress
  print(paste0(valid_ib$college_gsis_id[i], ": Done"))
  gc()  # free memory before next iteration
  return(res)
}) %>% bind_rows()

all_metrics_ob <- lapply(1:nrow(valid_ob), function(i) {
  res <- compute_player_route_metrics(ds, valid_ob$college_gsis_id[i], drills) %>%
    mutate(player_name = valid_ob$player_name[i])%>%
    group_by(route_id)
  # Print progress
  print(paste0(valid_ob$college_gsis_id[i], ": Done"))
  gc()  # free memory before next iteration
  return(res)
}) %>% bind_rows()

all_metrics_dc <- lapply(1:nrow(valid_dc), function(i) {
  res <- compute_player_route_metrics(ds, valid_dc$college_gsis_id[i], drills) %>%
    mutate(player_name = valid_dc$player_name[i])%>%
    group_by(route_id)
  # Print progress
  print(paste0(valid_dc$college_gsis_id[i], ": Done"))
  gc()  # free memory before next iteration
  return(res)
}) %>% bind_rows()

all_metrics_ds <- lapply(1:nrow(valid_ds), function(i) {
  res <- compute_player_route_metrics(ds, valid_ds$college_gsis_id[i], drills) %>%
    mutate(player_name = valid_ds$player_name[i])%>%
    group_by(route_id)
  # Print progress
  print(paste0(valid_ds$college_gsis_id[i], ": Done"))
  gc()  # free memory before next iteration
  return(res)
}) %>% bind_rows()

all_data_rb <- all_metrics_rb
all_data_wr <- all_metrics_wr
all_data_te <- all_metrics_te
all_data_ol <- all_metrics_ol
all_data_dt <- all_metrics_dt
all_data_de <- all_metrics_de
all_data_ib <- all_metrics_ib
all_data_ob <- all_metrics_ob
all_data_dc <- all_metrics_dc
all_data_ds <- all_metrics_ds


all_data_rb <- bind_rows(all_data_rb, all_metrics_rb)
all_data_wr <- bind_rows(all_metrics_wr, all_data_wr)
all_data_te <- bind_rows(all_data_te, all_metrics_te)
all_data_ol <- bind_rows(all_data_ol, all_metrics_ol)
all_data_ib <- bind_rows(all_data_ib, all_metrics_ib)
all_data_dc <- bind_rows(all_data_dc, all_metrics_dc)
all_data_ds <- bind_rows(all_data_ds, all_metrics_ds)

write.csv(all_data_rb, "/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/rb_practice_data.csv", row.names = FALSE)
write.csv(all_data_wr, "/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/wr_practice_data.csv", row.names = FALSE)
write.csv(all_data_te, "/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/te_practice_data.csv", row.names = FALSE)
write.csv(all_data_ol, "/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/ol_practice_data.csv", row.names = FALSE)
write.csv(all_data_dt, "/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/dt_practice_data.csv", row.names = FALSE)
write.csv(all_data_de, "/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/de_practice_data.csv", row.names = FALSE)
write.csv(all_data_ib, "/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/ib_practice_data.csv", row.names = FALSE)
write.csv(all_data_dc, "/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/dc_practice_data.csv", row.names = FALSE)
write.csv(all_data_ds, "/Users/jayap/OneDrive/Documents/Shrine Bowl Analytics Competition/ds_practice_data.csv", row.names = FALSE)

# --------------------------------------------------
# 8. Evaluating Reasonable Movement
# --------------------------------------------------

# Example: plot route with route_id = 3
single_route <- route_df %>% filter(route_id == 2241)

ggplot(single_route, aes(x = y_s, y = x_s)) +
  geom_path(linewidth = 1, color = "blue") +
  geom_point(size = 2, color = "red") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Single Route: route_id = 2132")
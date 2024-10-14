# ==============================================================================
# This script pre-processes the dataset used in Han et al. "Gambling Content Regulation: Evidence from Twitch.tv"
# This version: November 2023
# R Version: 4.2.3
# ==============================================================================

rm(list = ls())
set.seed(2024)

if(!require(dplyr)){install.packages('dplyr');library(dplyr)}
if(!require(tidyr)){install.packages('tidyr');library(tidyr)}
if(!require(lubridate)){install.packages('lubridate');library(lubridate)}
if(!require(rlang)){install.packages('rlang');library(rlang)}
if(!require(PanelMatch)){install.packages('PanelMatch');library(PanelMatch)}

# ==============================================================================
# Load Dataframes

setwd("/projectnb/seteff/qfhan94/Twitch Gambling/data")
plotpath = "/projectnb/seteff/qfhan94/Twitch Gambling/data/fig"
data <- read.csv("daily_data_final_merged2.csv", header = TRUE)
slotslist <- read.csv("slotslist_515.csv", header = TRUE)
kicklist  <- read.csv("streamscharts_kick_accounts.csv", header = TRUE)
baninfolist <- read.csv("streamscharts_bans_cleaned.csv", header = TRUE)
revenue_data <- read.csv("revenue_data_daily.csv", header = TRUE)

# Generate treated groups.
banlist <- subset(slotslist, banslots == 1)
data$slots <- ifelse(data$user_login %in% slotslist$user_login, 1, 0) # 475 streamers
data$banslots <- ifelse(data$user_login %in% banlist$user_login, 1, 0) # 158 streamers
data$unbanslots <- data$slots - data$banslots # 317 streamers

# Set date/week indicators
data$date <- as.Date(data$date)
data$week <- as.Date(cut(data$date, "week"))
data <- data %>%
  filter(date < as.Date("2023-01-01"))

# Language Indexing
language_freq <- data %>%
  group_by(user_login, daily_language) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  group_by(user_login) %>%
  filter(n == max(n)) %>%
  distinct(user_login, .keep_all = TRUE) %>%
  ungroup()

language_freq[["lan_ind"]] <- case_when(language_freq[["daily_language"]] == "en" ~ 1,
                                     language_freq[["daily_language"]] == "es" ~ 2,
                                     language_freq[["daily_language"]] == "pt" ~ 3,
                                     TRUE ~ 4)
language_freq <- language_freq[,c("user_login", "lan_ind")]

# Merge language and daily revenue data.
data <- merge(data, language_freq, by = "user_login", all.x = TRUE)
data <- merge(data, revenue_data, by = c("user_login", "date"), all.x = TRUE)

# ==============================================================================
# Construct weekly-level dataframe.

data_weekly <- data %>% group_by(user_login, week) %>%
  dplyr::summarise(broadcast_hrs  = sum(daily_broadcast_hrs, na.rm = TRUE),
            hrs_watched           = sum(daily_hrs_watched, na.rm = TRUE),
            slots_hrs             = sum(daily_slots_hrs, na.rm = TRUE),
            lootbox_hrs           = sum(daily_lootboxes_hrs, na.rm = TRUE),
            others_hrs            = sum(daily_others_hrs, na.rm = TRUE),
            nongaming_hrs         = sum(daily_non_gaming_hrs, na.rm = TRUE),
            banslots_hrs          = sum(daily_banned_game_hrs, na.rm = TRUE),
            acv                   = sum(daily_acv, na.rm = TRUE),
            SubsGain              = sum(Subs.gain, na.rm = TRUE),
            NewSubs               = sum(New.subs, na.rm = TRUE),
            Resubs                = sum(Resubs, na.rm = TRUE),
            GiftedSubs            = sum(Gifted.subs, na.rm = TRUE),
            Prime                 = sum(Prime, na.rm = TRUE),
            Tier1                 = sum(Tier1, na.rm = TRUE),
            Tier2                 = sum(Tier2, na.rm = TRUE),
            Tier3                 = sum(Tier3, na.rm = TRUE),
            slots                 = mean(slots),
            banslots              = mean(banslots),
            unbanslots            = mean(unbanslots),
            total_games_played    = mean(total_games_played),
            stream_count          = mean(total_stream_count),
            lan_ind               = mean(lan_ind)) %>%
  mutate(week_n = as.integer(isoweek(week) - isoweek("2022-08-01")+1))
data_weekly$week_n <- ifelse(data_weekly$week_n < 0, 52+data_weekly$week_n, data_weekly$week_n)
#names(data_weekly)[names(data_weekly) == "nongaming_hours"] <- "nongaming_hrs"

# Merge account ban & kick account info.
df_list <- list(data_weekly, subset(baninfolist, select = -c(X)), subset(kicklist, select = c(user_login, kick_ind)))
data_weekly <- Reduce(function(x, y) merge(x, y, by = "user_login", all.x = TRUE), df_list, accumulate=FALSE)

# Generate indicators for post-announcement and post-treatment periods.
data_weekly <- data_weekly %>%
  mutate(between_ind = ifelse(week_n > 7 & week_n < 12, 1, 0),
         post_ind    = ifelse(week_n > 11, 1, 0))

# Generate interaction indicators for DiD regression.
data_weekly$z_1 = data_weekly$post_ind * (data_weekly$banslots == 1)
data_weekly$z_2 = data_weekly$post_ind * (data_weekly$unbanslots == 1)
data_weekly$z_between_1 = data_weekly$between_ind * (data_weekly$banslots == 1)
data_weekly$z_between_2 = data_weekly$between_ind * (data_weekly$unbanslots == 1)

# Generate log variables
data_weekly$log_slots_hrs <- log(data_weekly$slots_hrs+1)
data_weekly$log_banslots_hrs <- log(data_weekly$banslots_hrs+1)
data_weekly$log_broadcast_hrs <- log(data_weekly$broadcast_hrs+1)
data_weekly$log_lootbox_hrs <- log(data_weekly$lootbox_hrs+1)
data_weekly$log_others_hrs <- log(data_weekly$others_hrs+1)
data_weekly$log_nongaming_hrs <- log(data_weekly$nongaming_hrs+1)
data_weekly$log_hrs_watched <- log(data_weekly$hrs_watched+1)
data_weekly$log_acv <- log(data_weekly$acv+1)

# Generate indicator of reliance on banned gambling content
data_weekly <- data_weekly %>% group_by(user_login) %>% 
  mutate(
    banshare = {
      total_banslots_hrs = sum(banslots_hrs)
      total_slots_hrs = sum(slots_hrs)
      ifelse(total_slots_hrs == 0, NaN, total_banslots_hrs / total_slots_hrs)
    }
  )

data_weekly$banshare <- pmin(data_weekly$banshare, 1)

# Generate user id for privacy
#data_weekly$id <- as.numeric(factor(data_weekly$user_login, levels=unique(data_weekly$user_login)))

# Generate mean acv and quartiles based on pre-announcement observations (use hrs_watched as a robustness check)
filtered_data_weekly <- data_weekly %>% filter(post_ind + between_ind == 0)
mean_acv <- filtered_data_weekly %>%
  group_by(user_login) %>%
  dplyr::summarise(mean_acv = mean(acv, na.rm =TRUE))
mean_hrs_watched <- filtered_data_weekly %>%
  group_by(user_login) %>%
  dplyr::summarise(mean_hrs_watched = mean(hrs_watched, na.rm =TRUE))

data_weekly <- merge(data_weekly, mean_acv, by = "user_login", all.x = TRUE)
data_weekly <- merge(data_weekly, mean_hrs_watched, by = "user_login", all.x = TRUE)

data_weekly <- data_weekly %>% group_by(week_n) %>%
  mutate(popularity_quartile = cut(mean_acv, 
                            breaks = quantile(mean_acv, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 
                            labels = c(1, 2, 3, 4), include.lowest = TRUE),
         banslots_quartile   = ntile(banshare, 4))

data_weekly$popularity_quartile <- as.numeric(data_weekly$popularity_quartile)


# Construct Balanced Panel for Synthetic DiD
data_weekly_balanced <- data_weekly %>% dplyr::select(-week)
template <- expand.grid(
  user_login = unique(data_weekly$user_login),
  week_n = seq(min(data_weekly$week_n), max(data_weekly$week_n))
)
data_weekly_balanced <- template %>%
  left_join(data_weekly_balanced, by = c("user_login", "week_n"))
data_weekly_balanced <- data_weekly_balanced %>%
  complete(user_login, week_n, fill = list(value = 0))
#data_weekly_balanced <- data_weekly_balanced[,-3]
data_weekly_balanced[is.na(data_weekly_balanced)] <- 0

# Fix NaN entries of slots, period indicators and treatment indicators.
data_weekly_balanced <- data_weekly_balanced %>%
  group_by(user_login) %>%
  mutate(#id = max(id),                                    # Fix id from zero
         banshare = max(banshare, na.rm = TRUE),
         slots    = if_else(sum(slots) != 0, 1, 0),        # Fix indicator for treated groups
         banslots = if_else(sum(banslots) != 0, 1, 0),     # Fix indicator for banned group
         unbanslots = if_else(sum(unbanslots) != 0, 1, 0), # Fix indicator for unbanned group
         popularity_quartile = max(popularity_quartile),   # Fix quartile to be the highest number of quantile assignment for each ID.
         banslots_quartile = max(banslots_quartile, na.rm = TRUE),
         #slots_group  = max(slots_group),                  # Fix slots group to be the highest number of slots group for each ID.
         lan_ind = max(lan_ind))                           # Fix language indicator.
data_weekly_balanced <- data_weekly_balanced %>%
  mutate(between_ind = ifelse(week_n > 7 & week_n < 12, 1, 0),
         post_ind    = ifelse(week_n > 11, 1, 0)) %>%
  mutate(z_1 = if_else(banslots == 1 & post_ind == 1, 1, 0),
         z_2 = if_else(unbanslots == 1 & post_ind == 1, 1, 0),
         z_between_1 = if_else(banslots == 1 & between_ind == 1, 1, 0),
         z_between_2 = if_else(unbanslots == 1 & between_ind == 1, 1, 0))

#data_weekly_balanced <- subset(data_weekly_balanced, week_n < 22)

# Export the pre-processed dataset.
write.csv(data_weekly_balanced, "data_weekly_balanced_621.csv", row.names=FALSE)

# ==============================================================================
# Summary Statistics.

data_banslots = subset(data_weekly_balanced, banslots == 1 & between_ind + post_ind == 0)
data_unbanslots = subset(data_weekly_balanced, unbanslots == 1 & between_ind + post_ind == 0)
data_nonslots = subset(data_weekly_balanced, banslots + unbanslots == 0 & between_ind + post_ind == 0)

sum_mat <- data.frame(
  Variable = character(),
  Mean_Slots = numeric(),
  Sd_Slots = numeric(),
  Mean_Nonslots = numeric(),
  Sd_Nonslots = numeric(),
  stringsAsFactors = FALSE
)

sum_var <- c("broadcast_hrs", "slots_hrs", "acv", "hrs_watched", "total_games_played", "SubsGain", "GiftedSubs", "Prime", "Tier1", "Tier2", "Tier3")

for (var in sum_var) {
  mean_banslots      <- mean(data_banslots[[var]], na.rm = TRUE)
  sd_banslots        <- sd(data_banslots[[var]], na.rm = TRUE)
  mean_unbanslots    <- mean(data_unbanslots[[var]], na.rm = TRUE)
  sd_unbanslots      <- sd(data_unbanslots[[var]], na.rm = TRUE)
  mean_nonslots      <- mean(data_nonslots[[var]], na.rm = TRUE)
  sd_nonslots        <- sd(data_nonslots[[var]], na.rm = TRUE)
  sum_mat <- rbind(sum_mat, data.frame(Variable = var, Mean_BanSlots = mean_banslots, Sd_BanSlots = sd_banslots,
                                       Mean_Unbanslots = mean_unbanslots, Sd_Unbanslots = sd_unbanslots,
                                       Mean_Nonslots = mean_nonslots, Sd_Nonslots = sd_nonslots))
}

sum_mat[,2:7] <- round(sum_mat[,2:dim(sum_mat)[2]], 3)

# ==============================================================================
# Descriptive plot on slots hours
data_weekly_balanced$group <- with(data_weekly_balanced,
                                   ifelse(banslots == 1, "Banned",
                                          ifelse(unbanslots == 1, "Unbanned", "Control")))

aggregated_data_plot <- data_weekly_balanced %>%
  group_by(week_n, group) %>%
  dplyr::summarise(avg_slots_hrs = mean(slots_hrs, na.rm = TRUE),
            avg_broadcast_hrs = mean(broadcast_hrs, na.rm = TRUE),
            avg_others_hrs = mean(others_hrs, na.rm = TRUE))

# Line plot of slots hrs.
plot_slots_hrs <- ggplot(aggregated_data_plot, aes(x = week_n, y = avg_slots_hrs, linetype = group)) +
  geom_line() +
  geom_vline(xintercept = c(8, 11), color = "gray50", linetype = "dashed", linewidth = 1) +
  labs(title = NULL,
       x = "Week",
       y = "Streaming Hours of Online Gambling",
       linetype = "Group") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.25)
  )
ggsave("line_slots_hrs.png", plot = plot_slots_hrs, dpi = 300, width = 10, height = 6)

# Line plot of total streaming hours.
plot_streaming_hrs <- ggplot(aggregated_data_plot, aes(x = week_n, y = avg_broadcast_hrs, linetype = group)) +
  geom_line() +
  geom_vline(xintercept = c(8, 11), color = "gray50", linetype = "dashed", linewidth = 1) +
  labs(title = NULL,
       x = "Week",
       y = "Total Streaming Hours",
       linetype = "Group") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.25)
  )
ggsave("line_streaming_hrs.png", plot = plot_streaming_hrs, dpi = 300, width = 10, height = 6)

# Line plot of total streaming hours.
plot_others_hrs <- ggplot(aggregated_data_plot, aes(x = week_n, y = avg_others_hrs, linetype = group)) +
  geom_line() +
  geom_vline(xintercept = c(8, 11), color = "gray50", linetype = "dashed", linewidth = 1) +
  labs(title = NULL,
       x = "Week",
       y = "Total Streaming Hours",
       linetype = "Group") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.25)
  )
ggsave("line_others_hrs.png", plot = plot_streaming_hrs, dpi = 300, width = 10, height = 6)







        
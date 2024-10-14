# =============================================================
# This script implements panel regressions in Han et al. "Gambling Content Regulation: Evidence from Twitch.tv"
# This version: November 2023
# R Version: 4.2.2
# =============================================================

rm(list = ls())
set.seed(20231109)

if(!require(eventstudyr)){install.packages('eventstudyr');library(eventstudyr)}
if(!require(did)){install.packages('did');library(did)}
if(!require(dplyr)){install.packages('dplyr');library(dplyr)}
if(!require(tidyr)){install.packages('tidyr');library(tidyr)}
if(!require(purrr)){install.packages('purrr');library(purrr)}
if(!require(broom)){install.packages('broom');library(broom)}
if(!require(lubridate)){install.packages('lubridate');library(lubridate)}
if(!require(ggplot2)){install.packages('ggplot2');library(ggplot2)}
if(!require(png)){install.packages('png');library(png)}
if(!require(grid)){install.packages('grid');library(grid)}
if(!require(gridExtra)){install.packages('gridExtra');library(gridExtra)}
if(!require(sandwich)){install.packages('sandwich');library(sandwich)}
#if(!require(rlang)){install.packages('rlang');library(rlang)}
if(!require(PanelMatch)){install.packages('PanelMatch');library(PanelMatch)}
if(!require(plm)){install.packages('plm');library(plm)}
if(!require(lmtest)){install.packages('lmtest');library(lmtest)}
if(!require(synthdid)) devtools:: install_github("synth-inference/synthdid")

# Install xsynthdid
options(repos = c(skranz = 'https://skranz.r-universe.dev',
                  CRAN = 'https://cloud.r-project.org'))
if(!require(xsynthdid)){install.packages('xsynthdid');library(xsynthdid)}

setwd("/projectnb/seteff/qfhan94/Twitch Gambling/data")
plotpath = "/projectnb/seteff/qfhan94/Twitch Gambling/data/fig"
data_weekly_balanced <- read.csv("data_weekly_balanced_621.csv", header = TRUE)
slotslist <- read.csv("slotslist_515.csv", header = TRUE)
# 
# Check whether the numbers are correct.
num_unique_users <- data_weekly_balanced %>%
   filter(banslots == 1) %>%
   summarize(num_users = n_distinct(user_login))
#
# Print the result, which should be 158 + 317
print(num_unique_users)

# banlist <- subset(slotslist, banslots == 1)
# unbanlist <- subset(slotslist, unbanslots == 1)
# data_weekly_balanced$banslots <- ifelse(data_weekly_balanced$user_login %in% banlist$user_login, 1, 0)
# data_weekly_balanced$unbanslots <- ifelse(data_weekly_balanced$user_login %in% unbanlist$user_login, 1, 0)
# data_weekly_balanced$slots <- 1 - data_weekly_balanced$banslots - data_weekly_balanced$unbanslots
# 
# #REDO Zs
data_weekly_balanced$z_1 = data_weekly_balanced$post_ind * (data_weekly_balanced$banslots == 1)
data_weekly_balanced$z_2 = data_weekly_balanced$post_ind * (data_weekly_balanced$unbanslots == 1)
data_weekly_balanced$z_between_1 = data_weekly_balanced$between_ind * (data_weekly_balanced$banslots == 1)
data_weekly_balanced$z_between_2 = data_weekly_balanced$between_ind * (data_weekly_balanced$unbanslots == 1)

# ==============================================================================
# 1. Simple TWFE Difference-in-Differences
# ==============================================================================

# READJUST DATA
# Function to assign quartiles
assign_quartiles <- function(data) {
  data %>%
    mutate(banslots_quartile = ntile(banslots_hrs, 4)) # Assign quartiles based on banslots_hrs
}

# Demand-Side Estimates
revenue_var <- c("SubsGain", "NewSubs", "Resubs", "GiftedSubs", "Prime", "Tier1", "Tier2", "Tier3")
for (var in revenue_var) {
  log_var <- paste0("log_", var)  # Create the name for the new variable
  data_weekly_balanced[[log_var]] <- log(data_weekly_balanced[[var]] + 1)  # Calculate log(var + 1)
}

# Step 1: Filter out rows where banslots == 1 and assign quartiles
data_weekly_balanced <- data_weekly_balanced %>%
  group_by(user_login) %>%
  mutate(slots_quartile = ifelse(banslots + unbanslots == 1, ntile(slots_hrs, 4), 0)) %>%
  ungroup()

# Step 2: Set banslots_quartile to 0 where banslots == 0
data_weekly_balanced <- data_weekly_balanced %>%
  mutate(banslots_quartile = ifelse(banslots == 0, 0, banslots_quartile))

# Supply Side Estimates
data_weekly_balanced$nonslots_hrs <- data_weekly_balanced$broadcast_hrs - data_weekly_balanced$slots_hrs
data_weekly_balanced$log_nonslots_hrs <- log(data_weekly_balanced$nonslots_hrs+1)
data_weekly_balanced <- subset(data_weekly_balanced, week_n <= 22)
pdata <- pdata.frame(data_weekly_balanced, index = c("user_login", "week_n"))

didvar_supply <- c("slots", "lootbox", "others", "broadcast")
did_est_supply <- list()

for (var in didvar_supply){
  fdid <- as.formula(paste0("log_", var, "_hrs ~ z_1 + z_between_1 + z_2 + z_between_2"))
  did <- plm(fdid, data = pdata, model = "within", effect = "twoways")
  did_est_supply[[paste("did_", var, sep = "")]] <- summary(did)
  did_est_supply[[paste("did_cluster_", var, sep = "")]] <- coeftest(did, vcov = vcovHC(did, type = "sss", cluster = "group"))
}

didvar_demand <- c("log_hrs_watched", "log_Tier1", "log_Tier2", "log_Tier3")
did_est_demand <- list()

for (var in didvar_demand){
  fdid <- as.formula(paste0(var, " ~ z_1 + z_between_1 + z_2 + z_between_2"))
  did <- plm(fdid, data = pdata, model = "within", effect = "twoways")
  did_est_demand[[paste("did_", var, sep = "")]] <- summary(did)
  did_est_demand[[paste("did_cluster_", var, sep = "")]] <- coeftest(did, vcov = vcovHC(did, type = "sss", cluster = "group"))
}

# Results Table

# Define the extract_and_format function
did_table <- function(coeftest_obj, name) {
  tidy_coeftest <- tryCatch({
    tidy(coeftest_obj)
  }, error = function(e) {
    message(paste("Error in tidy for", name, ":", e$message))
    return(NULL)
  })
  
  # Return NULL if tidy failed
  if (is.null(tidy_coeftest)) return(NULL)
  
  # Add significance stars based on p-value
  tidy_coeftest <- tidy_coeftest %>%
    mutate(
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        p.value < 0.1 ~ ".",
        TRUE ~ ""
      ),
      result = paste0(estimate, " (", std.error, ")", significance)
    ) %>%
    dplyr::select(term, result) %>%
    #rename(!!name := result)
    dplyr::rename(!!rlang::sym(name) := result)
  
  return(tidy_coeftest)
}

results_supply <- list(
  did_cluster_slots     = did_est_supply$did_cluster_slots,
  did_cluster_lootbox   = did_est_supply$did_cluster_lootbox,
  did_cluster_others    = did_est_supply$did_cluster_others,
  did_cluster_stream    = did_est_supply$did_cluster_broadcast
)

supply_table <- lapply(names(results_supply), function(name) {
  did_table(results_supply[[name]], name)
}) %>%
  reduce(full_join, by = "term")

# Print TWFE-DiD results on supply-side variables.
print(supply_table)

results_demand <- list(
  did_cluster_log_hrs_watched = did_est_demand$did_cluster_log_hrs_watched,
  did_cluster_Tier1           = did_est_demand$did_cluster_log_Tier1,
  did_cluster_Tier2           = did_est_demand$did_cluster_log_Tier2,
  did_cluster_Tier3           = did_est_demand$did_cluster_log_Tier3
)

demand_table <- lapply(names(results_demand), function(name) {
  did_table(results_demand[[name]], name)
}) %>%
  reduce(full_join, by = "term")

# Print TWFE-DiD results on demand-side variables.
print(demand_table)

# ==============================================================================
# Heterogeneous Treatment Effects: Popularity
# ==============================================================================

hdid_est_supply <- hdid_est_demand <- list()

# Supply-side
for (var in didvar_supply){
  for (i in 1:max(data_weekly_balanced$popularity_quartile)){
    fdid <- as.formula(paste0("log_", var, "_hrs ~ z_1 + z_between_1 + z_2 + z_between_2"))
    hdid <- plm(fdid, data = subset(pdata, popularity_quartile == i), model = "within", effect = "twoways")
    hdid_est_supply[[paste("hdid_", var, "_Q", i, sep = "")]] <- summary(hdid)
    hdid_est_supply[[paste("hdid_cluster_", var, "_Q", i, sep = "")]] <- coeftest(hdid, vcov = vcovHC(hdid, type = "sss", cluster = "group"))
  }
  
  # Appendix Results: DiD with interaction
  fdid_int <- as.formula(paste0("log_", var, "_hrs ~ z_1 + z_between_1 + z_2 + z_between_2 + 
                                z_1:popularity_quartile + z_between_1:popularity_quartile + z_2:popularity_quartile + z_between_2:popularity_quartile"))
  hdid_int <- plm(fdid_int, data = pdata, model = "within", effect = "twoways")
  hdid_est_supply[[paste("hdid_int_", var, sep = "")]] <- summary(hdid_int)
  
}

# Demand-side
for (var in didvar_demand){
   for (i in 1:max(data_weekly_balanced$popularity_quartile)){
    fdid <- as.formula(paste0(var, " ~ z_1 + z_between_1 + z_2 + z_between_2"))
    hdid <- plm(fdid, data = subset(pdata, popularity_quartile == i), model = "within", effect = "twoways")
    hdid_est_demand[[paste("hdid_", var, "_Q", i, sep = "")]] <- summary(hdid)
    hdid_est_demand[[paste("hdid_cluster_", var, "_Q", i, sep = "")]] <- coeftest(hdid, vcov = vcovHC(hdid, type = "sss", cluster = "group"))
  }
  
  # Appendix Results: DiD with interaction
  fdid_int <- as.formula(paste0(var, " ~ z_1 + z_between_1 + z_2 + z_between_2 + 
                                z_1:popularity_quartile + z_between_1:popularity_quartile + z_2:popularity_quartile + z_between_2:popularity_quartile"))
  hdid_int <- plm(fdid_int, data = pdata, model = "within", effect = "twoways")
  hdid_est_demand[[paste("hdid_int_", var, sep = "")]] <- summary(hdid_int)
}


# ==============================================================================
# Heterogeneous Treatment Effects: Reliance on Banned Content
# ==============================================================================

hdid2_est_supply <- hdid2_est_demand <- list()

for (i in 1:max(data_weekly_balanced$banslots_quartile)) {
  for (var in didvar_supply) {
    fdid <- as.formula(paste0("log_", var, "_hrs ~ z_1 + z_between_1 + z_2 + z_between_2"))
    hdid2 <- plm(fdid, data = subset(pdata, banslots_quartile == 0 | banslots_quartile == i), model = "within", effect = "twoways")
    hdid2_est_supply[[paste("hdid2_", var, "_G", i, sep = "")]] <- summary(hdid2)
    hdid2_est_supply[[paste("hdid2_cluster_", var, "_G", i, sep = "")]] <- coeftest(hdid2, vcov = vcovHC(hdid2, type = "sss", cluster = "group"))
  }
  
  # Appendix Results: DiD with interaction
  fdid_int <- as.formula(paste0("log_", var, "_hrs ~ z_1:banslots_hrs + z_between_1:banslots_hrs + z_2 + z_between_2"))
  hdid_int <- plm(fdid_int, data = pdata, model = "within", effect = "twoways")
  hdid2_est_supply[[paste("hdid2_int_", var, sep = "")]] <- summary(hdid_int)
}

for (i in 1:max(data_weekly_balanced$banslots_quartile)) {
  for (var in didvar_demand) {
    fdid <- as.formula(paste0(var, " ~ z_1 + z_between_1 + z_2 + z_between_2"))
    hdid2 <- plm(fdid, data = subset(pdata, banslots_quartile == 0 | banslots_quartile == i), model = "within", effect = "twoways")
    hdid2_est_demand[[paste("hdid2_", var, "_G", i, sep = "")]] <- summary(hdid2)
    hdid2_est_demand[[paste("hdid2_cluster_", var, "_G", i, sep = "")]] <- coeftest(hdid2, vcov = vcovHC(hdid2, type = "sss", cluster = "group"))
  }
  
  # Appendix Results: DiD with interaction
  fdid_int <- as.formula(paste0(var, " ~ z_1 + z_between_1 + z_1:banslots_quartile + z_between_1:banslots_quartile + z_2 + z_between_2"))
  hdid_int <- plm(fdid_int, data = pdata, model = "within", effect = "twoways")
  hdid2_est_demand[[paste("hdid2_int_", var, sep = "")]] <- summary(hdid_int)
}

# ==============================================================================
# Heterogeneous Treatment Effects: Language
# "en" ~ 1, "es" ~ 2, "pt" ~ 3, others" ~ 4
# ==============================================================================

hdid3_est_supply <- hdid3_est_demand <- list()

for (var in didvar_supply){
  for (i in 1:max(data_weekly_balanced$lan_ind)){
    fdid <- as.formula(paste0("log_", var, "_hrs ~ z_1 + z_between_1 + z_2 + z_between_2"))
    hdid3 <- plm(fdid, data = subset(pdata, lan_ind == i), model = "within", effect = "twoways")
    hdid3_est_supply[[paste("hdid3_", var, "_L", i, sep = "")]] <- summary(hdid3)
    hdid3_est_supply[[paste("hdid3_cluster_", var, "_L", i, sep = "")]] <- coeftest(hdid3, vcov = vcovHC(hdid3, type = "sss", cluster = "group"))
  }
}

for (var in didvar_demand){
  for (i in 1:max(data_weekly_balanced$lan_ind)){
    fdid <- as.formula(paste0(var, " ~ z_1 + z_between_1 + z_2 + z_between_2"))
    hdid3 <- plm(fdid, data = subset(pdata, lan_ind == i), model = "within", effect = "twoways")
    hdid3_est_demand[[paste("hdid3_", var, "_L", i, sep = "")]] <- summary(hdid3)
    hdid3_est_demand[[paste("hdid3_cluster_", var, "_L", i, sep = "")]] <- coeftest(hdid3, vcov = vcovHC(hdid3, type = "sss", cluster = "group"))
  }
}

# ==============================================================================
# Heterogeneous Treatment Effect Plots: Popularity
# ==============================================================================

hest_plot_supply <- hest_plot_demand <- data.frame(matrix(0, nrow = max(data_weekly_balanced$popularity_quartile), ncol = (length(didvar_supply))*6+1))
colnames(hest_plot_supply)[1] <- "Q"
colnames(hest_plot_demand)[1] <- "Q"
hest_plot_supply[,1] <- c(1:max(data_weekly_balanced$popularity_quartile))
hest_plot_demand[,1] <- c(1:max(data_weekly_balanced$popularity_quartile))

# Plots: quartile TE of log streaming hours 
k = 1 # Indicator for loop
for (var in didvar_supply) {
  for (j in 1:max(data_weekly_balanced$popularity_quartile)) {
    hest_plot_supply[j,k*3-1] <- hdid_est_supply[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][1,1]
    hest_plot_supply[j,k*3] <- hdid_est_supply[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][1,1] -
      hdid_est_supply[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][1,2]*1.96
    hest_plot_supply[j,k*3+1] <- hdid_est_supply[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][1,1] +
      hdid_est_supply[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][1,2]*1.96
    
    hest_plot_supply[j,k*3+14] <- hdid_est_supply[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,1]
    hest_plot_supply[j,k*3+15] <- hdid_est_supply[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,1] -
      hdid_est_supply[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,2]*1.96
    hest_plot_supply[j,k*3+16] <- hdid_est_supply[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,1] +
      hdid_est_supply[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,2]*1.96
    
    colnames(hest_plot_supply)[k*3-1] <- paste(var, "_est_ban", sep = "")
    colnames(hest_plot_supply)[k*3] <- paste(var, "_lb_ban", sep = "")
    colnames(hest_plot_supply)[k*3+1] <- paste(var, "_ub_ban", sep ="")
    colnames(hest_plot_supply)[k*3+14] <- paste(var, "_est_unban", sep = "")
    colnames(hest_plot_supply)[k*3+15] <- paste(var, "_lb_unban", sep = "")
    colnames(hest_plot_supply)[k*3+16] <- paste(var, "_ub_unban", sep ="")
  
    # Plot
    for (type in c("ban", "unban")) {
      plot <- ggplot(hest_plot_supply, aes(x = Q, y = .data[[paste0(var, "_est_", type)]])) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
        geom_errorbar(aes(ymin = .data[[paste0(var, "_lb_", type)]], ymax = .data[[paste0(var, "_ub_", type)]]), width = 0.2) +
        geom_line(color = "black", linewidth = 1) +
        geom_point() +
        labs(x = "Quartile", y = "Treatment Effect") +
        theme_minimal() +
        theme(
          axis.title = element_text(size = 30),
          axis.text = element_text(size = 30)
        )
      ggsave(paste0("plot_hdid_", var, "_", type, ".png"), plot, path = plotpath, height = 7, width = 10)
    }
  }
  k <- k+1
}

k = 1 # Indicator for loop
for (var in didvar_demand) {
  for (j in 1:max(data_weekly_balanced$popularity_quartile)) {
    hest_plot_demand[j,k*3-1] <- hdid_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][1,1]
    hest_plot_demand[j,k*3] <- hdid_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][1,1] -
      hdid_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][1,2]*1.96
    hest_plot_demand[j,k*3+1] <- hdid_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][1,1] +
      hdid_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][1,2]*1.96
    
    hest_plot_demand[j,k*3+14] <- hdid_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,1]
    hest_plot_demand[j,k*3+15] <- hdid_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,1] -
      hdid_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,2]*1.96
    hest_plot_demand[j,k*3+16] <- hdid_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,1] +
      hdid_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,2]*1.96
    
    colnames(hest_plot_demand)[k*3-1] <- paste(var, "_est_ban", sep = "")
    colnames(hest_plot_demand)[k*3] <- paste(var, "_lb_ban", sep = "")
    colnames(hest_plot_demand)[k*3+1] <- paste(var, "_ub_ban", sep ="")
    colnames(hest_plot_demand)[k*3+14] <- paste(var, "_est_unban", sep = "")
    colnames(hest_plot_demand)[k*3+15] <- paste(var, "_lb_unban", sep = "")
    colnames(hest_plot_demand)[k*3+16] <- paste(var, "_ub_unban", sep ="")
    
    # Plot
    for (type in c("ban", "unban")) {
      plot <- ggplot(hest_plot_demand, aes(x = Q, y = .data[[paste0(var, "_est_", type)]])) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
        geom_errorbar(aes(ymin = .data[[paste0(var, "_lb_", type)]], ymax = .data[[paste0(var, "_ub_", type)]]), width = 0.2) +
        geom_line(color = "black", linewidth = 1) +
        geom_point() +
        labs(x = "Quartile", y = "Treatment Effect") +
        theme_minimal() +
        theme(
          axis.title = element_text(size = 30),
          axis.text = element_text(size = 30)
        )
      ggsave(paste0("plot_hdid_", var, "_", type, ".png"), plot, path = plotpath, height = 7, width = 10)
    }
  }
  k <- k+1
}

# ==============================================================================
# Heterogeneous Treatment Effect Plots: Reliance on Ban Content
# ==============================================================================

# Only plot for banned group of streamers.
# Supply-side
gest_plot_supply <- data.frame(matrix(0, nrow = max(data_weekly_balanced$banslots_quartile), ncol = (length(didvar_supply))*3+1))
gest_plot_demand <- data.frame(matrix(0, nrow = max(data_weekly_balanced$banslots_quartile), ncol = (length(didvar_demand))*3+1))
colnames(gest_plot_supply)[1] <- "Q"
colnames(gest_plot_demand)[1] <- "Q"
gest_plot_supply[,1] <- c(1:max(data_weekly_balanced$banslots_quartile))
gest_plot_demand[,1] <- c(1:max(data_weekly_balanced$banslots_quartile))

# Plots: quartile TE of log streaming hours 
t = 1 # Indicator for loop
for (var in didvar_supply) {
  for (j in 1:max(data_weekly_balanced$banslots_quartile)) {
    gest_plot_supply[j,t*3-1] <- hdid2_est_supply[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][1,1]
    gest_plot_supply[j,t*3] <- hdid2_est_supply[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][1,1] -
      hdid2_est_supply[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][1,2]*1.96
    gest_plot_supply[j,t*3+1] <- hdid2_est_supply[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][1,1] +
      hdid2_est_supply[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][1,2]*1.96
    # 
    # gest_plot_supply[j,t*3+14] <- hdid2_est_supply[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][3,1]
    # gest_plot_supply[j,t*3+15] <- hdid2_est_supply[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][3,1] -
    #   hdid2_est_supply[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][3,2]*1.96
    # gest_plot_supply[j,t*3+16] <- hdid2_est_supply[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][3,1] +
    #   hdid2_est_supply[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][3,2]*1.96
    # 
    colnames(gest_plot_supply)[t*3-1] <- paste(var, "_est_ban", sep = "")
    colnames(gest_plot_supply)[t*3] <- paste(var, "_lb_ban", sep = "")
    colnames(gest_plot_supply)[t*3+1] <- paste(var, "_ub_ban", sep ="")
    # colnames(gest_plot_supply)[t*3+14] <- paste(var, "_est_unban", sep = "")
    # colnames(gest_plot_supply)[t*3+15] <- paste(var, "_lb_unban", sep = "")
    # colnames(gest_plot_supply)[t*3+16] <- paste(var, "_ub_unban", sep ="")
    
    # Plot
    for (type in c("ban")) {
      plot <- ggplot(gest_plot_supply, aes(x = Q, y = .data[[paste0(var, "_est_", type)]])) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
        geom_errorbar(aes(ymin = .data[[paste0(var, "_lb_", type)]], ymax = .data[[paste0(var, "_ub_", type)]]), width = 0.2) +
        geom_line(color = "black", linewidth = 1) +
        geom_point() +
        labs(x = "Quartile", y = "Treatment Effect") +
        theme_minimal() +
        theme(
          axis.title = element_text(size = 30),
          axis.text = element_text(size = 30)
        )
      ggsave(paste0("plot_reliance_did_", var, "_", type, ".png"), plot, path = plotpath, height = 7, width = 10)
    }
  }
  t <- t+1
}

# ==============================================================================
# Demand
t = 1 # Indicator for loop
for (var in didvar_demand) {
  for (j in 1:max(data_weekly_balanced$banslots_quartile)) {
    gest_plot_demand[j,t*3-1] <- hdid2_est_demand[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][1,1]
    gest_plot_demand[j,t*3] <- hdid2_est_demand[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][1,1] -
      hdid2_est_demand[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][1,2]*1.96
    gest_plot_demand[j,t*3+1] <- hdid2_est_demand[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][1,1] +
      hdid2_est_demand[[paste("hdid2_cluster_", var, "_G", j, sep = "")]][1,2]*1.96
    # 
    # gest_plot_demand[j,t*3+14] <- hdid2_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,1]
    # gest_plot_demand[j,t*3+15] <- hdid2_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,1] -
    #   hdid2_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,2]*1.96
    # gest_plot_demand[j,t*3+16] <- hdid2_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,1] +
    #   hdid2_est_demand[[paste("hdid_cluster_", var, "_Q", j, sep = "")]][3,2]*1.96
    
    colnames(gest_plot_demand)[t*3-1] <- paste(var, "_est_ban", sep = "")
    colnames(gest_plot_demand)[t*3] <- paste(var, "_lb_ban", sep = "")
    colnames(gest_plot_demand)[t*3+1] <- paste(var, "_ub_ban", sep ="")
    # colnames(gest_plot_demand)[t*3+14] <- paste(var, "_est_unban", sep = "")
    # colnames(gest_plot_demand)[t*3+15] <- paste(var, "_lb_unban", sep = "")
    # colnames(gest_plot_demand)[t*3+16] <- paste(var, "_ub_unban", sep ="")
    # 
    # Plot
    for (type in c("ban")) {
      plot <- ggplot(gest_plot_demand, aes(x = Q, y = .data[[paste0(var, "_est_", type)]])) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
        geom_errorbar(aes(ymin = .data[[paste0(var, "_lb_", type)]], ymax = .data[[paste0(var, "_ub_", type)]]), width = 0.2) +
        geom_line(color = "black", linewidth = 1) +
        geom_point() +
        labs(x = "Quartile", y = "Treatment Effect") +
        theme_minimal() +
        theme(
          axis.title = element_text(size = 30),
          axis.text = element_text(size = 30)
        )
      ggsave(paste0("plot_reliance_did_", var, "_", type, ".png"), plot, path = plotpath, height = 7, width = 10)
    }
  }
  t <- t+1
}

# ==============================================================================
# Heterogeneous Treatment Effect Plots: Language
# ==============================================================================

lest_plot <- data.frame(matrix(0, nrow = max(data_weekly_balanced$lan_ind), ncol = (length(didvar_supply))*3+1))
colnames(lest_plot)[1] <- "L"
lest_plot[,1] <- c(1:max(data_weekly_balanced$lan_ind))

# Plots: quantile TE of log streaming hours 
k = 1 # Indicator for loop
for (var in didvar_supply) {
  for (j in 1:max(data_weekly_balanced$lan_ind)) {
    # lest_plot[j,k*3-1] <- hdid3_est_supply[[paste("hdid3_cluster_", var, "_L", j, sep = "")]][1,1]
    # lest_plot[j,k*3] <- hdid3_est_supply[[paste("hdid3_cluster_", var, "_L", j, sep = "")]][1,1] -
    #   hdid3_est_supply[[paste("hdid3_cluster_", var, "_L", j, sep = "")]][1,2]*1.96
    # lest_plot[j,k*3+1] <- hdid3_est_supply[[paste("hdid3_cluster_", var, "_L", j, sep = "")]][1,1] +
    #   hdid3_est_supply[[paste("hdid3_cluster_", var, "_L", j, sep = "")]][1,2]*1.96
    lest_plot[j,k*3-1] <- hdid3_est_supply[[paste("hdid3_cluster_", var, "_L", j, sep = "")]][3,1]
    lest_plot[j,k*3] <- hdid3_est_supply[[paste("hdid3_cluster_", var, "_L", j, sep = "")]][3,1] -
      hdid3_est_supply[[paste("hdid3_cluster_", var, "_L", j, sep = "")]][3,2]*1.96
    lest_plot[j,k*3+1] <- hdid3_est_supply[[paste("hdid3_cluster_", var, "_L", j, sep = "")]][3,1] +
      hdid3_est_supply[[paste("hdid3_cluster_", var, "_L", j, sep = "")]][3,2]*1.96
    
    colnames(lest_plot)[k*3-1] <- paste(var, "_est", sep = "")
    colnames(lest_plot)[k*3] <- paste(var, "_lb", sep = "")
    colnames(lest_plot)[k*3+1] <- paste(var, "_ub", sep ="")
    
    # Plot
    #lest_plot$L <- factor(lest_plot$L, levels = c(1, 2, 3, 4), labels = c("EN", "ES", "PT", "Others"))
    plot <- ggplot(lest_plot, aes(x = L, y = .data[[paste0(var,"_est",sep="")]])) +
      geom_hline(yintercept=0, linetype="dashed", color = "red", linewidth = 0.8) +
      geom_errorbar(aes(ymin = .data[[paste0(var,"_lb",sep="")]], ymax = .data[[paste0(var,"_ub",sep="")]]), width = 0.2) +
      geom_line(color = "black", linewidth = 1) +
      geom_point() +
      labs(x = "Language", y = "Treatment Effect") +
      theme_minimal() +
      scale_x_continuous(breaks = c(1, 2, 3, 4), labels = c("EN", "ES", "PT", "Others")) +
      theme(
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30)
      )
    ggsave(paste0("plot_ldid_unban_", var, ".png"), plot, path = plotpath, height = 7, width = 10)
  }
  k <- k+1
}

# ==============================================================================
# Total Aggregate-Level Estimates.
# ==============================================================================

# Regenerate treatment indicators.
data_weekly_balanced <- data_weekly_balanced %>%
  group_by(user_login) %>%  # Replace 'ID' with the actual identifier column name
  mutate(treatment = case_when(
    any(z_1 == 1) ~ 1,
    any(z_2 == 1) ~ 2,
    TRUE ~ 0
  )) %>%
  ungroup()

# Aggregate data of all streamers within a treated group.
data_weekly_agg <- data_weekly_balanced %>%
  group_by(treatment, week_n) %>%
  dplyr::summarise(across(all_of(paste0("log_",didvar_supply, "_hrs")), mean, na.rm = TRUE),
            across(all_of(didvar_demand), mean, na.rm = TRUE),
            z_1 = mean(z_1),
            z_2 = mean(z_2),
            z_between_1 = mean(z_between_1),
            z_between_2 = mean(z_between_2)) %>%
  dplyr::mutate(between_ind = ifelse(week_n > 7 & week_n < 12, 1, 0),
         post_ind    = ifelse(week_n > 11, 1, 0)) %>%
  ungroup()
pdata_agg <- pdata.frame(data_weekly_agg, index = c("treatment", "week_n"))


did_est_supply_agg <- did_est_demand_agg <- list()
did_est_supply_agg$z_1

for (var in didvar_supply){
  fdid <- as.formula(paste0("log_", var, "_hrs ~ z_1 + z_between_1 + z_2 + z_between_2"))
  did <- plm(fdid, data = pdata_agg, model = "within", effect = "twoways")
  did_est_supply_agg[[paste("did_", var, sep = "")]] <- summary(did)
  did_est_supply_agg[[paste("did_cluster_", var, sep = "")]] <- coeftest(did, vcov = vcovHC(did, type = "sss", cluster = "group"))
}
did_broadcast   <- plm(log_broadcast_hrs ~ z_1 + z_between_1 + z_2 + z_between_2 , 
                       data = pdata_agg, model = "within", effect = "twoways")
did_est_supply_agg[["did_broadcast"]] <- summary(did_broadcast)
did_est_supply_agg[["did_cluster_broadcast"]] <- coeftest(did_broadcast, vcov = vcovHC(did_broadcast, type = "sss", cluster = "group"))

for (var in didvar_demand){
  fdid <- as.formula(paste0(var, " ~ z_1 + z_between_1 + z_2 + z_between_2"))
  did <- plm(fdid, data = pdata_agg, model = "within", effect = "twoways")
  did_est_demand_agg[[paste("did_", var, sep = "")]] <- summary(did)
  did_est_demand_agg[[paste("did_cluster_", var, sep = "")]] <- coeftest(did, vcov = vcovHC(did, type = "sss", cluster = "group"))
}

# Function to extract and format results
format_and_combine_results <- function(plm_models) {
  results <- map2(plm_models, names(plm_models), ~ {
    tidy_model <- tidy(.x)
    
    tidy_model %>%
      mutate(estimate = round(estimate, 3),
             std.error = round(std.error, 3),
             significance = case_when(
               p.value < 0.001 ~ "***",
               p.value < 0.01 ~ "**",
               p.value < 0.05 ~ "*",
               p.value < 0.1 ~ ".",
               TRUE ~ ""
             )) %>%
      transmute(
        term,
        !!.y := paste0(estimate, " (", std.error, ")", significance)
      )
  })
  
  combined_results <- reduce(results, full_join, by = "term")
  return(combined_results)
}

# ==============================================================================
# Streamers in Isolated Networks
# ==============================================================================

isostreamers <- read.csv("demand_streamers_1270_aug.csv", header = TRUE)
isostreamers_new <- read.csv("demand_streamers_1270.csv", header = TRUE)
data_weekly_iso <- subset(data_weekly_balanced, user_login %in% isostreamers$user_login,)

# Aggregate data of all streamers within a treated group after filtering by network.
data_weekly_agg_iso <- data_weekly_iso %>%
  group_by(treatment, week_n) %>%
  summarise(across(all_of(paste0("log_",didvar_supply, "_hrs")), mean, na.rm = TRUE),
            across(all_of(didvar_demand), mean, na.rm = TRUE),
            z_1 = mean(z_1),
            z_2 = mean(z_2),
            z_between_1 = mean(z_between_1),
            z_between_2 = mean(z_between_2)) %>%
  mutate(between_ind = ifelse(week_n > 7 & week_n < 12, 1, 0),
         post_ind    = ifelse(week_n > 11, 1, 0)) %>%
  ungroup()
pdata_agg_iso <- pdata.frame(data_weekly_agg_iso, index = c("treatment", "week_n"))


did_est_supply_agg_iso <- did_est_demand_agg_iso <- list()

for (var in didvar_supply){
  fdid <- as.formula(paste0("log_", var, "_hrs ~ z_1 + z_between_1 + z_2 + z_between_2"))
  did <- plm(fdid, data = pdata_agg_iso, model = "within", effect = "twoways")
  did_est_supply_agg_iso[[paste("did_", var, sep = "")]] <- summary(did)
  #did_est_supply_agg_iso[[paste("did_cluster_", var, sep = "")]] <- coeftest(did, vcov = vcovHC(did, type = "sss", cluster = "group"))
}
did_broadcast   <- plm(log_broadcast_hrs ~ z_1 + z_between_1 + z_2 + z_between_2 , 
                       data = pdata_agg_iso, model = "within", effect = "twoways")
did_est_supply_agg_iso[["did_broadcast"]] <- summary(did_broadcast)
#did_est_supply_agg_iso[["did_cluster_broadcast"]] <- coeftest(did_broadcast, vcov = vcovHC(did_broadcast, type = "sss", cluster = "group"))

for (var in didvar_demand){
  fdid <- as.formula(paste0(var, " ~ z_1 + z_between_1 + z_2 + z_between_2"))
  did <- plm(fdid, data = pdata_agg_iso, model = "within", effect = "twoways")
  did_est_demand_agg_iso[[paste("did_", var, sep = "")]] <- summary(did)
  #did_est_demand_agg_iso[[paste("did_cluster_", var, sep = "")]] <- coeftest(did, vcov = vcovHC(did, type = "sss", cluster = "group"))
}

# Results Table: Aggregate-level
results_supply_agg_iso <- list(
  did_slots     = did_est_supply_agg_iso$did_slots,
  did_lootbox   = did_est_supply_agg_iso$did_lootbox,
  did_others    = did_est_supply_agg_iso$did_others,
  did_broacast  = did_est_supply_agg_iso$did_broadcast
)

supply_table_agg_iso <- lapply(names(results_supply_agg_iso), function(name) {
  format_and_combine_results(results[[name]], name)
}) %>%
  reduce(full_join, by = "term")

results_demand_agg_iso <- list(
  did_cluster_log_hrs_watched = did_est_demand_agg_iso$did_log_hrs_watched,
  did_cluster_Tier1           = did_est_demand_agg_iso$did_log_Tier1,
  did_cluster_Tier2           = did_est_demand_agg_iso$did_log_Tier2,
  did_cluster_Tier3           = did_est_demand_agg_iso$did_log_Tier3
)

demand_table_agg_iso <- lapply(names(results_demand_agg_iso), function(name) {
  format_and_combine_results(results[[name]], name)
}) %>%
  reduce(full_join, by = "term")

# ==============================================================================
# 2. EventStudy as Check of Parallel Trend Assumption
# ==============================================================================

# Partial out periods between announcement and treatment to check the parallel trend assumption.
# Supply
data_weekly_balanced$log_broadcast_hrs_adj = adjust.outcome.for.x(data_weekly_balanced,
                                                                  unit="user_login", time = "week_n", outcome = "log_broadcast_hrs", treatment = "z_1",
                                                                  x = c("z_between_1", "z_1", "z_between_2"))
data_weekly_balanced$log_slots_hrs_adj = adjust.outcome.for.x(data_weekly_balanced,
                                                              unit="user_login", time = "week_n", outcome = "log_slots_hrs", treatment = "z_1",
                                                              x = c("z_between_1", "z_1", "z_between_2"))
data_weekly_balanced$log_lootbox_hrs_adj = adjust.outcome.for.x(data_weekly_balanced,
                                                                unit="user_login", time = "week_n", outcome = "log_lootbox_hrs", treatment = "z_1",
                                                                x = c("z_between_1", "z_1", "z_between_2"))
data_weekly_balanced$log_others_hrs_adj = adjust.outcome.for.x(data_weekly_balanced,
                                                                unit="user_login", time = "week_n", outcome = "log_others_hrs", treatment = "z_1",
                                                               x = c("z_between_1", "z_1", "z_between_2"))
data_weekly_balanced$log_nonslots_hrs_adj = adjust.outcome.for.x(data_weekly_balanced,
                                                                unit="user_login", time = "week_n", outcome = "log_nonslots_hrs", treatment = "z_1",
                                                                x = c("z_between_1", "z_1", "z_between_2"))

# Demand
data_weekly_balanced$log_hrs_watched_adj  = adjust.outcome.for.x(data_weekly_balanced,
                                                                 unit="user_login", time = "week_n", outcome = "log_hrs_watched", treatment = "z_1",
                                                                 x = c("z_between_1", "z_1", "z_between_2"))

eventstudy <- function(var, data=data_weekly_balanced){
  event <- EventStudy(
    estimator = "OLS",
    data = data,   # Use package sample data
    outcomevar = var,
    policyvar = "z_2",
    idvar = "user_login",
    timevar = "new_week_n",
    pre = 0,  post = 2
  )
  
  return(event)
}

varlist <- c("log_slots_hrs")
#varlist <- c("log_broadcast_hrs_adj", "log_slots_hrs_adj", "log_lootbox_hrs_adj", "log_nonslots_hrs_adj", "log_others_hrs_adj", "log_hrs_watched_adj")
for (i in 1:length(varlist)){
  event <- eventstudy(varlist[i], data_test)
  plt <- EventStudyPlot(estimates = event) +
    geom_point(color = "black", size = 1.2) +
    geom_hline(yintercept = 0,
               color = "red", linetype = "dashed", linewidth = 0.8) +
    theme_minimal() + 
    labs(x = "Event time", y = "Treatment Effect") +
    theme(
      axis.title = element_text(size = 25),
      axis.text = element_text(size = 25),
      plot.caption = element_text(size = 15),
      plot.background = element_rect(fill = "white")
    )
  ggsave(paste0("Event_unban_test_", varlist[i], ".jpeg"), plt, path = plotpath, height = 7, width = 10, dpi = 1000)
}

# ==============================================================================
# 3. Synthetic Difference-in-Differences
# ==============================================================================

# We report the s.e. based on the reported 95% CI.

# Synthetic DiD: log Slots Hours
setup_shrs = panel.matrices(as.data.frame(data_weekly_balanced), 
                            unit="user_login",time = "week_n",outcome = "log_slots_hrs_adj",treatment = "z_1")
tau.hat.shrs = synthdid_estimate(setup_shrs$Y, setup_shrs$N0, setup_shrs$T0)
tau.sc.shrs   = sc_estimate(setup_shrs$Y, setup_shrs$N0, setup_shrs$T0)
tau.did.shrs  = did_estimate(setup_shrs$Y, setup_shrs$N0, setup_shrs$T0)
estimates.shrs = list(tau.did.shrs, tau.sc.shrs, tau.hat.shrs)
names(estimates.shrs) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
print(unlist(estimates.shrs))
print(estimates.shrs$`Synthetic Diff-in-Diff`)

# Synthetic DiD: log Lootbox Hours
setup_lhrs = panel.matrices(as.data.frame(data_weekly_balanced), 
                            unit="user_login",time = "week_n",outcome = "log_lootbox_hrs_adj",treatment = "z_1")
tau.hat.lhrs = synthdid_estimate(setup_lhrs$Y, setup_lhrs$N0, setup_lhrs$T0)
tau.sc.lhrs   = sc_estimate(setup_lhrs$Y, setup_lhrs$N0, setup_lhrs$T0)
tau.did.lhrs  = did_estimate(setup_lhrs$Y, setup_lhrs$N0, setup_lhrs$T0)
estimates.lhrs = list(tau.did.lhrs, tau.sc.lhrs, tau.hat.lhrs)
names(estimates.lhrs) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
#print(unlist(estimates.lhrs))
print(estimates.lhrs$`Synthetic Diff-in-Diff`)

# Synthetic DiD: log Nonslots Hours
setup_nhrs = panel.matrices(as.data.frame(data_weekly_balanced), 
                            unit="user_login",time = "week_n",outcome = "log_nonslots_hrs_adj",treatment = "z_1")
tau.hat.nhrs = synthdid_estimate(setup_nhrs$Y, setup_nhrs$N0, setup_nhrs$T0)
tau.sc.nhrs   = sc_estimate(setup_nhrs$Y, setup_nhrs$N0, setup_nhrs$T0)
tau.did.nhrs  = did_estimate(setup_nhrs$Y, setup_nhrs$N0, setup_nhrs$T0)
estimates.nhrs = list(tau.did.nhrs, tau.sc.nhrs, tau.hat.nhrs)
names(estimates.nhrs) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
#print(unlist(estimates.nhrs))
print(estimates.nhrs$`Synthetic Diff-in-Diff`)

# Synthetic DiD: log Other Games Hours
setup_ohrs = panel.matrices(as.data.frame(data_weekly_balanced), 
                            unit="user_login",time = "week_n",outcome = "log_others_hrs_adj",treatment = "z_1")
tau.hat.ohrs = synthdid_estimate(setup_ohrs$Y, setup_ohrs$N0, setup_ohrs$T0)
tau.sc.ohrs   = sc_estimate(setup_ohrs$Y, setup_ohrs$N0, setup_ohrs$T0)
tau.did.ohrs  = did_estimate(setup_ohrs$Y, setup_ohrs$N0, setup_ohrs$T0)
estimates.ohrs = list(tau.did.ohrs, tau.sc.ohrs, tau.hat.ohrs)
names(estimates.ohrs) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
#print(unlist(estimates.nhrs))
print(estimates.ohrs$`Synthetic Diff-in-Diff`)

# Synthetic DiD: log Broadcast Hours
setup_bhrs = panel.matrices(as.data.frame(data_weekly_balanced), 
                            unit="user_login",time = "week_n",outcome = "log_broadcast_hrs_adj",treatment = "z_1")
tau.hat.bhrs = synthdid_estimate(setup_bhrs$Y, setup_bhrs$N0, setup_bhrs$T0)
tau.sc.bhrs   = sc_estimate(setup_bhrs$Y, setup_bhrs$N0, setup_bhrs$T0)
tau.did.bhrs  = did_estimate(setup_bhrs$Y, setup_bhrs$N0, setup_bhrs$T0)
estimates.bhrs = list(tau.did.bhrs, tau.sc.bhrs, tau.hat.bhrs)
names(estimates.bhrs) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
#print(unlist(estimates.whrs))
print(estimates.bhrs$`Synthetic Diff-in-Diff`)

# Synthetic DiD: log Hours Watched
setup_hhrs = panel.matrices(as.data.frame(data_weekly_balanced), 
                            unit="user_login",time = "week_n",outcome = "log_hrs_watched_adj",treatment = "z_1")
tau.hat.hhrs = synthdid_estimate(setup_hhrs$Y, setup_hhrs$N0, setup_hhrs$T0)
tau.sc.hhrs   = sc_estimate(setup_hhrs$Y, setup_hhrs$N0, setup_hhrs$T0)
tau.did.hhrs  = did_estimate(setup_hhrs$Y, setup_hhrs$N0, setup_hhrs$T0)
estimates.hhrs = list(tau.did.hhrs, tau.sc.hhrs, tau.hat.hhrs)
names(estimates.hhrs) = c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
#print(unlist(estimates.hhrs))
print(estimates.hhrs$`Synthetic Diff-in-Diff`)

# Define syndid function
syndid <- function(data, variable) {
  treatments <- c("z_1", "z_2")
  other_vars <- c("z_between_1", "z_between_2")
  
  # Iterate over each treatment variable
  for (treatment in treatments) {
    # Define the other variables to be used as covariates
    remaining_z <- setdiff(treatments, treatment)
    x_vars <- c(other_vars, remaining_z)
    
    # Adjust the outcome for the current treatment
    adjusted_outcome_name <- paste0(variable, "_adj_", treatment)
    data[[adjusted_outcome_name]] <- adjust.outcome.for.x(data,
                                                          unit = "user_login",
                                                          time = "week_n",
                                                          outcome = variable,
                                                          treatment = treatment,
                                                          x = x_vars)
    
    # Set up the panel matrices using the adjusted outcome
    setup <- panel.matrices(as.data.frame(data),
                            unit = "user_login",
                            time = "week_n",
                            outcome = adjusted_outcome_name,
                            treatment = treatment)
    
    # Estimate the effects
    tau.hat <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
    tau.sc <- sc_estimate(setup$Y, setup$N0, setup$T0)
    tau.did <- did_estimate(setup$Y, setup$N0, setup$T0)
    
    # Store the estimates
    estimates <- list(tau.did, tau.sc, tau.hat)
    names(estimates) <- c('Diff-in-Diff', 'Synthetic Control', 'Synthetic Diff-in-Diff')
    
    # Print the result for the current treatment
    cat("\nResults for treatment:", treatment, "\n")
    print(estimates$`Synthetic Diff-in-Diff`)
  }
}

#varlist <- c("log_broadcast_hrs_adj", "log_slots_hrs_adj", "log_lootbox_hrs_adj", "log_nonslots_hrs_adj", "log_others_hrs_adj", "log_hrs_watched_adj")
varlist <- c("log_Tier1_adj", "log_Tier2_adj", "log_Tier3_adj")
# Remove _adj suffix to get the base variable names
base_vars <- gsub("_adj$", "", varlist)

# Initialize an empty list to store all results
all_results <- list()

# Loop over the base variables and apply syndid function
for (variable in base_vars) {
  cat("\nAnalyzing variable:", variable, "\n")
  result <- syndid(data_weekly_balanced, variable)
  all_results[[variable]] <- result
}

# Combine the results into a table
combined_results <- do.call(cbind, lapply(all_results, function(res) {
  do.call(rbind, res)
}))

# Convert to data frame for better presentation
combined_results_df <- as.data.frame(combined_results)

# Print the combined results
print(combined_results_df)

#######################################################################################################
# 4. Group-Time Average Treatment Effects
# Create a new group indicator
data_weekly_balanced$id <- as.numeric(factor(data_weekly_balanced$user_login, levels=unique(data_weekly_balanced$user_login)))
data_weekly_balanced$Group = case_when(data_weekly_balanced$banslots == 1 ~ 12,
                                       data_weekly_balanced$unbanslots == 1 ~ 1,
                                       data_weekly_balanced$banslots + data_weekly_balanced$unbanslots == 0 ~ 0)
# Specification: no staggered adoption and with covariates
att_gt_broadcast <- att_gt(yname = "log_nonslots_hrs_adj",
                           tname = "week_n",
                           idname = "id",
                           gname = "Group",
                           #xformla = ~,
                           data = data_weekly_balanced)
aggte_broadcast <- aggte(att_gt_broadcast, type = "group")

att_gt_slots <- att_gt(yname = "log_slots_hrs_adj",
                       tname = "week_n",
                       idname = "id",
                       gname = "Group",
                       #xformla = ,
                       data = data_weekly_balanced)
aggte_slots <- aggte(att_gt_slots, type = "group")

att_gt_lootbox <- att_gt(yname = "log_lootbox_hrs_adj_z_2",
                         tname = "week_n",
                         idname = "id",
                         gname = "Group",
                         #xformla = ~,
                         data = data_weekly_balanced)
aggte_lootbox <- aggte(att_gt_lootbox, type = "group")

att_gt_hrs_watched <- att_gt(yname = "log_hrs_watched_adj",
                         tname = "week_n",
                         idname = "id",
                         gname = "Group",
                         #xformla = ~,
                         data = data_weekly_balanced)
aggte_hrs_watched <- aggte(att_gt_hrs_watched, type = "group")

att_gt_others <- att_gt(yname = "log_others_hrs_adj",
                        tname = "week_n",
                        idname = "id",
                        gname = "Group",
                        #xformla = ~,
                        data = data_weekly_balanced)
att_gt_others <- aggte(att_gt_others, type = "group")

aggte_1 = list(aggte_slots$overall.att, aggte_slots$overall.se,
               aggte_broadcast$overall.att, aggte_broadcast$overall.se,
               aggte_lootbox$overall.att, aggte_lootbox$overall.se,
               )
names(aggte_1) = c('AGGTE_slots', 'SE_slots', 'AGGTE_broadcast', 'SE_broadcast', 'AGGTE_lootbox', 'SE_lootbox')
print(unlist(aggte_1))




test <- plm(log_slots_hrs ~ z_1 + z_between_1 + z_2 + z_between_2 + z_1:slots_share + z_between_1:slots_share + z_2:slots_share + z_between_2:slots_share, data = pdata, model = "within", effect = "twoways")

test <- data_weekly_balanced %>%
  dplyr::group_by(user_login) %>%
  dplyr::summarise(Tier2_nonzero = any(Tier3 != 0)) %>%
  dplyr::filter(Tier2_nonzero)

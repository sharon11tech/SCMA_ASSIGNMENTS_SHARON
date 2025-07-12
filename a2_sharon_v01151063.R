
library(tidyverse)
library(readxl)
library(lubridate)
library(fitdistrplus)
library(stats4)
library(dplyr)
library(stringdist)


setwd("C:/vcu extra/assignment/data")

ipl_bbb <- read_csv("IPL_ball_by_ball_updated till 2024.csv")
ipl_salary <- read_excel("IPL SALARIES 2024.xlsx")

grouped_data <- ipl_bbb %>%
  group_by(Season, `Innings No`, Striker, Bowler) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE),
            wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE), .groups = "drop")

player_runs <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored), .groups = "drop")

player_wickets <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation), .groups = "drop")

# Top 3 performers each year
top_run_getters <- player_runs %>%
  group_by(Season) %>%
  slice_max(runs_scored, n = 3) %>%
  ungroup()

top_wicket_takers <- player_wickets %>%
  group_by(Season) %>%
  slice_max(wicket_confirmation, n = 3) %>%
  ungroup()

print("Top Three Run Getters")
print(top_run_getters)

print("Top Three Wicket Takers")
print(top_wicket_takers)


ipl_bbbc <- ipl_bbb
ipl_bbbc$year <- year(dmy(ipl_bbbc$Date))


total_run_each_year <- ipl_bbbc %>%
  group_by(year, Striker) %>%
  summarise(runs_scored = sum(runs_scored), .groups = "drop") %>%
  arrange(year, desc(runs_scored))

print(total_run_each_year)

list_top_batsman_last_three_year <- total_run_each_year %>%
  group_by(year) %>%
  slice_max(runs_scored, n = 3) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(top_batsmen = list(unique(Striker))) %>%
  deframe()


# Helper function to get best distribution
get_best_distribution <- function(data) {
  distributions <- c("norm", "lnorm", "gamma", "weibull", "exp")
  results <- list()
  for (dist_name in distributions) {
    fit <- tryCatch(fitdist(data, dist_name), error = function(e) NULL)
    if (!is.null(fit)) {
      ks <- ks.test(data, dist_name, fit$estimate)
      cat("Distribution:", dist_name, "- p-value:", ks$p.value, "\n")
      results[[dist_name]] <- ks$p.value
    }
  }
  best <- names(which.max(unlist(results)))
  cat("\nBest fitting distribution:", best, "\n")
}

runs <- ipl_bbbc %>%
  group_by(Striker, `Match id`) %>%
  summarise(runs_scored = sum(runs_scored), .groups = "drop")

for (yr in names(list_top_batsman_last_three_year)) {
  for (batsman in list_top_batsman_last_three_year[[yr]]) {
    cat("\n************************\n")
    cat("Year:", yr, " Batsman:", batsman, "\n")
    get_best_distribution(runs %>% filter(Striker == batsman) %>% pull(runs_scored))
  }
}

batsman <- "Mustafizur Rahman"
cat("Analyzing for Batsman:", batsman, "\n")
get_best_distribution(runs %>% filter(Striker == batsman) %>% pull(runs_scored))


total_wicket_each_year <- ipl_bbbc %>%
  group_by(year, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation), .groups = "drop") %>%
  arrange(year, desc(wicket_confirmation))

print(total_wicket_each_year)

list_top_bowler_last_three_year <- total_wicket_each_year %>%
  group_by(year) %>%
  slice_max(wicket_confirmation, n = 3) %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(top_bowlers = list(unique(Bowler))) %>%
  deframe()

wickets <- ipl_bbbc %>%
  group_by(Bowler, `Match id`) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation), .groups = "drop")

for (yr in names(list_top_bowler_last_three_year)) {
  for (bowler in list_top_bowler_last_three_year[[yr]]) {
    cat("\n************************\n")
    cat("Year:", yr, " Bowler:", bowler, "\n")
    get_best_distribution(wickets %>% filter(Bowler == bowler) %>% pull(wicket_confirmation))
  }
}


# 2024 Run Data
R2024 <- total_run_each_year %>% filter(year == 2024)

# Match player names using fuzzy string matching
match_names <- function(name, list_names) {
  scores <- stringsim(name, list_names, method = "jw")
  best_match <- which.max(scores)
  if (scores[best_match] >= 0.8) return(list_names[best_match]) else return(NA)
}

df_salary <- ipl_salary
df_salary$Matched_Player <- sapply(df_salary$Player, match_names, list_names = R2024$Striker)

df_merged <- df_salary %>%
  left_join(R2024, by = c("Matched_Player" = "Striker"))

# Correlation
correlation <- cor(df_merged$Rs, df_merged$runs_scored, use = "complete.obs")
cat("Correlation between Salary and Runs:", correlation, "\n")




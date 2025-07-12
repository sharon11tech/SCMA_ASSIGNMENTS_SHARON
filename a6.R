library(tidyverse)


df <- read.csv("C:/vcu extra/assignment/data/NSSO68.csv")


df_kar <- df %>%
  filter(state == 29) %>%
  rename(
    district_code = District,
    mpce_urp = MPCE_URP,
    mpce_mrp = MPCE_MRP,
    cereal = cerealtot_v,
    pulses = pulsestot_v,
    milk = Milktotal_v,
    nonveg = nonvegtotal_v,
    veg = vegtt_v
  )

df_kar <- df_kar %>%
  mutate(total_food_consumption = cereal + pulses + milk + nonveg + veg)

district_map <- c(
  "1" = "Belgaum", "2" = "Bagalkot", "3" = "Bijapur", "4" = "Bidar", "5" = "Raichur",
  "6" = "Gulbarga", "7" = "Koppal", "8" = "Bellary", "9" = "Chitradurga", "10" = "Davanagere"
  
)

df_kar$district_name <- district_map[as.character(df_kar$district_code)]

district_summary <- df_kar %>%
  group_by(district_name) %>%
  summarise(
    avg_mpce_urp = mean(mpce_urp, na.rm = TRUE),
    avg_food_consumption = mean(total_food_consumption, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(df_kar, aes(x = total_food_consumption)) +
  geom_histogram(bins = 30, fill = "orange", color = "white", alpha = 0.8) +
  geom_density(aes(y = ..density..), color = "black", size = 1) +
  labs(
    title = "Distribution of Household Food Consumption in Karnataka",
    x = "Total Food Consumption (₹)",
    y = "Number of Households"
  ) +
  theme_minimal()

ggplot(
  district_summary %>% arrange(desc(avg_food_consumption)),
  aes(x = avg_food_consumption, y = fct_reorder(district_name, avg_food_consumption))
) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Food Consumption by District",
    x = "Average Food Consumption (₹)",
    y = "District"
  ) +
  theme_minimal()


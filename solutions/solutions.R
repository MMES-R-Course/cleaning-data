library(tidyverse)
library(summarytools)
library(janitor)

messy <- read_xlsx("data/messy_mangrove_data.xlsx")

view(dfSummary(messy))

clean_namgrove_data <- function(data) {
  
  a <- data %>% 
    clean_names() %>% 
    pivot_longer(
      cols = starts_with("week"),
      names_to = "week_number",
      values_to = "height_mm"
    ) %>% 
    mutate(
      week_number = parse_number(week_number),
      height_mm = parse_number(height_mm),
      location = case_when(
        str_to_lower(location) == "site a" ~ "Site_A",
        str_to_lower(location) == "site b" ~ "Site_B",
        TRUE ~ "Unknown_Location"),
      treatment = case_when(
        str_to_lower(treatment) %in% c("control", "ctrl") ~ "Control",
        str_to_lower(treatment) %in% c("treatment 1", "t1") ~ "Treatment_1",
        str_to_lower(treatment) %in% c("treatment 2", "t2") ~ "Treatment_2",
        TRUE ~ "Unknown_Treatment")
      ) %>% 
    select(propagule_id, location, treatment, week_number, height_mm, notes)
  
  return(a)
}

plot_ht_over_time <- function(df) {
  
  df %>%
    group_by(location, treatment, week_number) %>%
    summarise(mean_ht = mean(height_mm, na.rm = T)) %>%
    ggplot(aes(x = week_number, y = mean_ht, color = location)) +
    geom_point() +
    geom_line() +
    facet_wrap(~treatment)
  
}

clean_data <- clean_namgrove_data(messy)

plot_ht_over_time(clean_data)

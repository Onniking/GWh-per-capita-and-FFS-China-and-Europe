#Fossil Fuel Subsidies and per capita kWh, with correlation
#Use Datasets: IMF Fossil Fuel Subsidies 2010-2024 and Ember and Energy Institute 

library(tidyverse)

#Code for FFS and per capita energy consumption + correlation 

Subsidy <- read.csv("Data/Fossil Fuel Subidies 2015-2036.csv")

kwh <- read.csv("Data/EUCHI_00-25.csv")

View(Subsidy)

european_countries <- c(
  "Albania", "Austria", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
  "Croatia, Republic of", "Cyprus", "Czech Republic", "Denmark",
  "Estonia, Republic of", "Finland", "France", "Germany", "Greece",
  "Hungary", "Iceland", "Ireland", "Italy", "Kosovo, Republic of",
  "Latvia, Republic of", "Lithuania, Republic of", "Luxembourg", "Malta",
  "Moldova, Republic of", "Montenegro", "Netherlands", "North Macedonia, Republic of",
  "Norway", "Poland, Republic of", "Portugal", "Romania", "Serbia, Republic of",
  "Slovak Republic", "Slovenia, Republic of", "Spain", "Sweden",
  "Switzerland", "Ukraine", "United Kingdom"
)

european_countries[european_countries %in% unique(Subsidy$COUNTRY)]

europe_subsidy <- Subsidy |>
  filter(COUNTRY %in% european_countries) |>
  select(COUNTRY, FFS, X2015:X2025) |>
  pivot_longer(cols = X2015:X2025, names_to = "Year", values_to = "value") |>
  mutate(Year = as.integer(str_remove(Year, "X"))) |>
  group_by(Year) |>
  summarise(total_subsidy = sum(value, na.rm = TRUE))

europe_kwh <- kwh |>
  filter(Entity == "Europe", Year >= 2015, Year <= 2025)

europe_combined <- left_join(europe_kwh, europe_subsidy, by = "Year")

k <- 2
b <- 5500

#Europe: Elec Use per Capita vs. FOssil Fuel Subsidy
ggplot(europe_combined, aes(x = Year)) +
  geom_line(aes(y = Per.capita.electricity.use, color = "kWh per capita"), linewidth = 1.1) +
  geom_point(aes(y = Per.capita.electricity.use, color = "kWh per capita"), size = 2.5) +
  geom_line(aes(y = total_subsidy * k + b, color = "Fossil Fuel Subsidies"), linewidth = 1.1, linetype = "dashed") +
  geom_point(aes(y = total_subsidy * k + b, color = "Fossil Fuel Subsidies"), size = 2.5) +
  scale_y_continuous(
    name = "Electricity Use per Capita (kWh)",
    sec.axis = sec_axis(~ (. - b) / k,
                        name = "Total Fossil Fuel Subsidies (Billion USD, 2021 prices)")
  ) +
  scale_x_continuous(breaks = 2015:2025) +
  scale_color_manual(
    values = c("kWh per capita" = "#2166ac", "Fossil Fuel Subsidies" = "#d6604d"),
    name = NULL
  ) +
  labs(
    title = "Europe: Electricity Use per Capita vs. Fossil Fuel Subsidies",
    subtitle = "2015–2025",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.title.y.left  = element_text(color = "#2166ac"),
    axis.title.y.right = element_text(color = "#d6604d"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#China: Elec Use per Capita vs. FOssil Fuel Subsidy
china_subsidy <- Subsidy |>
  filter(COUNTRY == "China, People's Republic of") |>
  select(FFS, X2015:X2025) |>
  pivot_longer(cols = X2015:X2025, names_to = "Year", values_to = "value") |>
  mutate(Year = as.integer(str_remove(Year, "X"))) |>
  group_by(Year) |>
  summarise(total_subsidy = sum(value, na.rm = TRUE))

china_kwh <- kwh |>
  filter(Entity == "China", Year >= 2015, Year <= 2025)

china_combined <- left_join(china_kwh, china_subsidy, by = "Year")
china_combined

k_cn <- 2.9
b_cn <- -1655

ggplot(china_combined, aes(x = Year)) +
  geom_line(aes(y = Per.capita.electricity.use, color = "kWh per capita"), linewidth = 1.1) +
  geom_point(aes(y = Per.capita.electricity.use, color = "kWh per capita"), size = 2.5) +
  geom_line(aes(y = total_subsidy * k_cn + b_cn, color = "Fossil Fuel Subsidies"), linewidth = 1.1, linetype = "dashed") +
  geom_point(aes(y = total_subsidy * k_cn + b_cn, color = "Fossil Fuel Subsidies"), size = 2.5) +
  scale_y_continuous(
    name = "Electricity Use per Capita (kWh)",
    sec.axis = sec_axis(~ (. - b_cn) / k_cn,
                        name = "Total Fossil Fuel Subsidies (Billion USD, 2021 prices)")
  ) +
  scale_x_continuous(breaks = 2015:2025) +
  scale_color_manual(
    values = c("kWh per capita" = "#d6604d", "Fossil Fuel Subsidies" = "#4d9221"),
    name = NULL
  ) +
  labs(
    title = "China: Electricity Use per Capita vs. Fossil Fuel Subsidies",
    subtitle = "2015–2025",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.title.y.left  = element_text(color = "#d6604d"),
    axis.title.y.right = element_text(color = "#4d9221"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#China Correlation
cor_china_pearson  <- cor.test(china_combined$total_subsidy, china_combined$Per.capita.electricity.use, method = "pearson")
cor_china_spearman <- cor.test(china_combined$total_subsidy, china_combined$Per.capita.electricity.use, method = "spearman")

#Europe Correlation
cor_europe_pearson  <- cor.test(europe_combined$total_subsidy, europe_combined$Per.capita.electricity.use, method = "pearson")
cor_europe_spearman <- cor.test(europe_combined$total_subsidy, europe_combined$Per.capita.electricity.use, method = "spearman")

results <- tibble(
  Region   = rep(c("China", "Europe"), each = 2),
  Method   = rep(c("Pearson", "Spearman"), 2),
  r        = c(cor_china_pearson$estimate,  cor_china_spearman$estimate,
               cor_europe_pearson$estimate, cor_europe_spearman$estimate),
  p_value  = c(cor_china_pearson$p.value,   cor_china_spearman$p.value,
               cor_europe_pearson$p.value,  cor_europe_spearman$p.value),
  n        = 11
)

results |> mutate(across(c(r, p_value), ~ round(.x, 4)))

cat("China Pearson p-value: ", format(cor_china_pearson$p.value, scientific = TRUE), "\n")
cat("China Spearman p-value:", format(cor_china_spearman$p.value, scientific = TRUE), "\n")
cat("China Pearson 95% CI: [",
    round(cor_china_pearson$conf.int[1], 3), ",",
    round(cor_china_pearson$conf.int[2], 3), "]\n")
cat("Europe Pearson 95% CI: [",
    round(cor_europe_pearson$conf.int[1], 3), ",",
    round(cor_europe_pearson$conf.int[2], 3), "]\n")

#China and Europe kwh per capita
ggplot(data = kwh, aes(x = Year, y = Per.capita.electricity.use, color = Entity, group = Entity)) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 2) + 
  labs(
    title = "Per Capita Electricity Consumption (2000-2025)",
    subtitle = "Comparing trends between China and Europe",
    x = "Year",
    y = "Electricity Use (kWh per capita)",
    color = "Region"
  ) +
  scale_color_manual(values = c("China" = "#de2d26", "Europe" = "#3182bd")) + 
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

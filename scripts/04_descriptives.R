############################## ОПИСАТЕЛЬНЫЕ СТАТИСТИКИ #############################

# Таблица 1
var_labels <- c(
  Income = "Среднедушевые доходы, руб.",
  Poverty = "Уровень бедности, %",
  Competition = "Объекты розничной торговли, ед.",
  Diesel = "Цена дизеля, руб./л",
  Wages = "Реальная заработная плата, руб.",
  Cars = "Автомобили на 1000 чел."
)

desc_table <- filtered_data %>%
  summarise(across(
    c(Income, Poverty, Competition, Diesel, Wages, Cars),
    list(
      mean = ~mean(., na.rm = TRUE),
      sd   = ~sd(., na.rm = TRUE),
      min  = ~min(., na.rm = TRUE),
      max  = ~max(., na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to = c("Показатель", "Статистика"),
               names_sep = "_",
               values_to = "Значение") %>%
  pivot_wider(names_from = Статистика, values_from = Значение) %>%
  mutate(Показатель = recode(Показатель, !!!var_labels)) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  select(Показатель, mean, sd, min, max) %>%
  rename(
    `Среднее` = mean,
    `Ст. отклонение` = sd,
    `Минимум` = min,
    `Максимум` = max
  )

# Таблица 2
table2 <- tibble(
  `Показатель` = c(
    "Всего наблюдений",
    "Уникальные города",
    "Уникальные регионы",
    "Уникальные недели",
    "Уникальные ритейлеры",
    "Уникальные товары",
    "Уникальные категории",
    "Доля социально значимых товаров"
  ),
  `Значение` = c(
    nrow(filtered_data),
    n_distinct(filtered_data$City),
    n_distinct(filtered_data$Region),
    n_distinct(filtered_data$Week),
    n_distinct(filtered_data$Retailer),
    n_distinct(filtered_data$Product),
    n_distinct(filtered_data$Категория),
    round(mean(filtered_data$Соц_значимость, na.rm = TRUE), 3)
  )
)

# Таблица 3
table3 <- filtered_data %>%
  group_by(Retailer) %>%
  summarise(
    `Число регионов` = n_distinct(Region),
    `Число городов` = n_distinct(City),
    `Число наблюдений` = n()
  ) %>%
  arrange(desc(`Число наблюдений`))

# Таблица 4
table4 <- filtered_data %>%
  group_by(Категория) %>%
  summarise(
    `Число наблюдений` = n(),
    `Доля от выборки` = round(n() / nrow(filtered_data), 3),
    `Доля СЗТ в категории` = round(mean(Соц_значимость, na.rm = TRUE), 5),
    `Средняя цена` = round(mean(EffectivePrice, na.rm = TRUE), 2),
    `Медиана цены` = round(median(EffectivePrice, na.rm = TRUE), 2),
    `Мин. цена` = round(min(EffectivePrice, na.rm = TRUE), 2),
    `Макс. цена` = round(max(EffectivePrice, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(`Число наблюдений`))

# Таблица 5
coverage_by_region <- filtered_data %>%
  group_by(Region) %>%
  summarise(
    n_cities = n_distinct(City),
    n_products = n_distinct(Product),
    n_categories = n_distinct(Категория),
    n_retailers = n_distinct(Retailer),
    n_obs = n()
  ) %>%
  arrange(desc(n_obs))

# Таблица 6
socio_by_region <- filtered_data %>%
  group_by(Region) %>%
  summarise(
    avg_income = mean(Income, na.rm = TRUE),
    avg_poverty = mean(Poverty, na.rm = TRUE),
    avg_wages = mean(Wages, na.rm = TRUE),
    avg_cars = mean(Cars, na.rm = TRUE),
    avg_competition = mean(Competition, na.rm = TRUE),
    avg_diesel = mean(Diesel, na.rm = TRUE)
  )

write.xlsx(list(Coverage = coverage_by_region, Socio = socio_by_region),
           file = "region_summary.xlsx", overwrite = TRUE)

# VIF
vif_raw_model <- lm(
  EffectivePrice ~ Cars + Wages + Competition + Diesel + Poverty,
  data = filtered_data
)
vif(vif_raw_model)

unique(filtered_data$Соц_категория)

############################## ПРОВЕРКА УСТОЙЧИВОСТИ #############################

# Подвыборка без Ленты
data_no_lenta <- filtered_data %>%
  filter(Retailer != "Lenta")

# СЗТ без Ленты
models_by_cat_szt_nolenta <- data_no_lenta %>%
  filter(Соц_значимость == TRUE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )
for (i in seq_len(nrow(models_by_cat_szt_nolenta))) {
  cat("\n\n=== СЗТ / Категория (без Ленты):", models_by_cat_szt_nolenta$Категория[i], "===\n")
  print(summary(models_by_cat_szt_nolenta$model[[i]]))
}

# неСЗТ без Ленты
models_by_cat_nonszt_nolenta <- data_no_lenta %>%
  filter(Соц_значимость == FALSE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )
for (i in seq_len(nrow(models_by_cat_nonszt_nolenta))) {
  cat("\n\n=== неСЗТ / Категория (без Ленты):", models_by_cat_nonszt_nolenta$Категория[i], "===\n")
  print(summary(models_by_cat_nonszt_nolenta$model[[i]]))
}

# Driscoll–Kraay
filtered_data$panel_id <- interaction(filtered_data$Product, filtered_data$City, filtered_data$Retailer)

model_dk <- feols(
  log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
    Product + Retailer + Week,
  data = filtered_data,
  vcov = "DK",
  panel.id = ~ panel_id + Week
)
model_dk

# DK по категориям СЗТ
models_by_cat_szt <- filtered_data %>%
  filter(Соц_значимость == TRUE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      vcov = "DK",
      panel.id = ~panel_id + Week
    ))
  )
for (i in seq_len(nrow(models_by_cat_szt))) {
  cat("\n\n=== СЗТ / Категория (DK):", models_by_cat_szt$Категория[i], "===\n")
  print(summary(models_by_cat_szt$model[[i]]))
}

# DK по категориям неСЗТ
models_by_cat_nonszt <- filtered_data %>%
  filter(Соц_значимость == FALSE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      vcov = "DK",
      panel.id = ~panel_id + Week
    ))
  )
for (i in seq_len(nrow(models_by_cat_nonszt))) {
  cat("\n\n=== неСЗТ / Категория (DK):", models_by_cat_nonszt$Категория[i], "===\n")
  print(summary(models_by_cat_nonszt$model[[i]]))
}

# DK по ритейлерам (бедность)
models_ret_pov <- filtered_data %>%
  group_by(Retailer) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product,
      data = .x,
      vcov = "DK",
      panel.id = ~panel_id + Week
    ))
  )
for (i in seq_len(nrow(models_ret_pov))) {
  cat("\n\n=== Ретейлер (DK, бедность):", models_ret_pov$Retailer[i], "===\n")
  print(summary(models_ret_pov$model[[i]]))
}

# DK по ритейлерам (доход)
models_ret_inc <- filtered_data %>%
  group_by(Retailer) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ log(Income) + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product,
      data = .x,
      vcov = "DK",
      panel.id = ~panel_id + Week
    ))
  )
for (i in seq_len(nrow(models_ret_inc))) {
  cat("\n\n=== Ретейлер (DK, доход):", models_ret_inc$Retailer[i], "===\n")
  print(summary(models_ret_inc$model[[i]]))
}

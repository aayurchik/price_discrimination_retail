# Вся выборка по категориям
models_by_cat <- filtered_data %>%
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
for (i in seq_len(nrow(models_by_cat))) {
  cat("\n\n=== Категория:", models_by_cat$Категория[i], "===\n")
  print(summary(models_by_cat$model[[i]]))
}

# СЗТ
models_by_cat_szt <- filtered_data %>%
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
for (i in seq_len(nrow(models_by_cat_szt))) {
  cat("\n\n=== СЗТ / Категория:", models_by_cat_szt$Категория[i], "===\n")
  print(summary(models_by_cat_szt$model[[i]]))
}

# неСЗТ
models_by_cat_nonszt <- filtered_data %>%
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
for (i in seq_len(nrow(models_by_cat_nonszt))) {
  cat("\n\n=== неСЗТ / Категория:", models_by_cat_nonszt$Категория[i], "===\n")
  print(summary(models_by_cat_nonszt$model[[i]]))
}

# СЗТ базовая цена
models_by_cat_szt <- filtered_data %>%
  filter(Соц_значимость == TRUE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(Price) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )
for (i in seq_len(nrow(models_by_cat_szt))) {
  cat("\n\n=== СЗТ / Категория (базовая цена):", models_by_cat_szt$Категория[i], "===\n")
  print(summary(models_by_cat_szt$model[[i]]))
}

# неСЗТ базовая цена
models_by_cat_nonszt <- filtered_data %>%
  filter(Соц_значимость == FALSE) %>%
  group_by(Категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(Price) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )
for (i in seq_len(nrow(models_by_cat_nonszt))) {
  cat("\n\n=== неСЗТ / Категория (базовая цена):", models_by_cat_nonszt$Категория[i], "===\n")
  print(summary(models_by_cat_nonszt$model[[i]]))
}

# Модели по Соц_категория (бедность)
models_by_subcat_szt_eff <- filtered_data %>%
  filter(Соц_значимость == TRUE) %>%
  group_by(Соц_категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )
for (i in seq_len(nrow(models_by_subcat_szt_eff))) {
  cat("\n\n=== СЗТ / Соц. категория:", models_by_subcat_szt_eff$Соц_категория[i], "===\n")
  print(summary(models_by_subcat_szt_eff$model[[i]]))
}

# Модели по Соц_категория (доход)
models_by_subcat_szt_eff <- filtered_data %>%
  filter(Соц_значимость == TRUE) %>%
  group_by(Соц_категория) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ log(Income)+ log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product + Retailer,
      data = .x,
      cluster = ~Region
    ))
  )
for (i in seq_len(nrow(models_by_subcat_szt_eff))) {
  cat("\n\n=== СЗТ / Соц. категория (доход):", models_by_subcat_szt_eff$Соц_категория[i], "===\n")
  print(summary(models_by_subcat_szt_eff$model[[i]]))
}

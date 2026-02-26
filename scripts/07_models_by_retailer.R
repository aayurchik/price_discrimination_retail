# БЕДНОСТЬ
models_ret <- filtered_data %>%
  group_by(Retailer) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product,
      data = .x,
      cluster = ~Region
    ))
  )
for (i in seq_len(nrow(models_ret))) {
  cat("\n\n=== Ретейлер:", models_ret$Retailer[i], "===\n")
  print(summary(models_ret$model[[i]]))
}

# ДОХОД
models_ret <- filtered_data %>%
  group_by(Retailer) %>%
  nest() %>%
  mutate(
    model = map(data, ~ feols(
      log(EffectivePrice) ~ log(Income) + log(Wages) + log(Competition) + log(Diesel) + log(Cars) |
        Week + Product,
      data = .x,
      cluster = ~Region
    ))
  )
for (i in seq_len(nrow(models_ret))) {
  cat("\n\n=== Ретейлер (доход):", models_ret$Retailer[i], "===\n")
  print(summary(models_ret$model[[i]]))
}

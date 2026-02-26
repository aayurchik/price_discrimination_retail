############################## МОДЕЛИРОВАНИЕ #############################

# Базовая модель (бедность)
model1 <- feols(
  log(EffectivePrice) ~ Poverty + log(Wages) + log(Competition) + log(Diesel) + log(Cars) | 
    Product + Retailer + Week,
  data = filtered_data,
  cluster = ~Region
)
summary(model1)

# Модель с доходом
model_income <- feols(
  log(EffectivePrice) ~ log(Income) + log(Wages) + log(Competition) + log(Diesel) + log(Cars) | 
    Product + Retailer + Week,
  data = filtered_data,
  cluster = ~Region
)
summary(model_income)

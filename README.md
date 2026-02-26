# Diploma thesis: Spatial Price Discrimination in Russian Grocery Retail

This repository contains the replication code for my undergraduate thesis at HSE University (2025). The thesis investigates whether major Russian grocery chains adjust prices based on regional poverty levels, using a panel dataset of over 5 million price observations.  
It was awarded the **Laureate of the HSE Best Research Paper Competition 2025**.

## 📁 Data
- Original dataset: [Yandex.Disk](https://disk.yandex.ru/d/-KSMQUN3Vf8htQ)
- Thesis page (HSE repository): [spb.hse.ru/ba/economics/students/diplomas/1048202003](https://spb.hse.ru/ba/economics/students/diplomas/1048202003)

## 📦 Requirements
R packages: `tidyverse`, `fixest`, `kableExtra`, `openxlsx`, `car`

Install with:
```r
install.packages(c("tidyverse", "fixest", "kableExtra", "openxlsx", "car"))
```

## About the work
- Data: 5+ million weekly price observations from five federal retailers across 28 regions (2021–2022)
- Methods: Fixed-effects regressions with clustered standard errors; robustness checks (Driscoll–Kraay, subsample analysis)
- Main findings:
  - In non-regulated categories, prices are significantly lower in poorer regions
  - Socially important goods show weaker effects (compensatory pricing)
  - Heterogeneous strategies across retail chains
  - Lower base prices in poor regions are offset by less frequent discounts

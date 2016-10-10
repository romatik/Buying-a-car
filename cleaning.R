library(tidyverse)

df <- readRDS(file = "./Data/interim/first_pass.Rds")
df <- df %>% 
  filter(!is.na(id)) %>%
  separate(category, c("category1", "category2"), sep = ",") %>%
  separate(eng_pow, c("eng_pow_kw", "eng_pow_ps"), sep = "\\(") %>%
  separate(fuel_cons, c("fuel_cons_comb", "fuel_cons_city", "fuel_cons_hwy"), sep = ";")

df <- type_convert(df, 
    cols(
      .default = col_number(),
      carmake = col_factor(levels = levels(as.factor(df$carmake))),
      title = col_character(),
      category1 = col_factor(levels = levels(as.factor(df$category1))),
      category2 = col_character(),
      fuel_type = col_factor(levels = levels(as.factor(df$fuel_type))),
      doors = col_factor(levels = levels(as.factor(df$doors))),
      transm = col_factor(levels = levels(as.factor(df$transm))),
      co2_class = col_factor(levels = levels(as.factor(df$co2_class))),
      fr = col_date(format = "%m/%Y"),
      hu = col_character(),
      clima = col_factor(levels = levels(as.factor(df$clima))),
      extra = col_character()
    ), locale = locale(grouping_mark = "."))

saveRDS(df, file = "./Data/interim/after_cleaning.Rds")

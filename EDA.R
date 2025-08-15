library(vroom)
library(tidyverse)
library(corrplot)

# id: A unique identifier of the data set
# target: The transformed ratio of loss to total insured value
# dummy: Nuisance variable used to control the model, but not working as a predictor
# var1-var17: A set of normalized variables representing policy characteristics (note: var11 is the weight used in the weighted gini score calculation)
# crimeVar1-crimeVar9: A set of normalized Crime Rate variables
# geodemVar1-geodemVar37: A set of normalized geodemographic variables
# weatherVar1-weatherVar236: A set of normalized weather station variables

df_raw <- vroom("train.csv")
df <- df_raw %>%
  select(!c(var12, var14, var16)) %>%
  select(!starts_with("crimeVar")) %>%
  drop_na() %>%
  mutate(across(3:11, ~as.factor(.))) %>%
  mutate(dummy = as.factor(dummy))

# Missing values
for (col in colnames(df_raw)) {
  missing <- df_raw[col] %>%
    is.na() %>%
    sum()
  paste(col, ": ", missing, "\n", sep = "") %>%
    cat()
}

# Top 10 worst columns:
# 1. var16: 361693
# 2. var12: 355042
# 3. var14: 290466
# 4. crimeVar7: 117363
# 5. crimeVar2: 114553
# 6. crimeVar4: 112798
# 7. crimeVar5: 110655
# 8. crimeVar1, crimeVar3, crimeVar6, crimeVar8, crimeVar9: 109988

# Correlation matrices:
df %>%
  select(target, 12:16) %>%
  cor() %>%
  corrplot()

df %>%
  select(target, starts_with("geodemVar")) %>%
  cor() %>%
  corrplot()
# Near perfect correlation:
# - geodemVar1
# - geodemVar6
# - geodemVar8
# - geodemVar10
# - geodemVar11
# - geodemVar13
# - geodemVar15
# - geodemVar17
# - geodemVar20
# - geodemVar21
# - geodemVar26
# - geodemVar27
# - geodemVar28
# - geodemVar33
# - geodemVar35
# - geodemVar36

df %>%
  select(target, starts_with("weatherVar")) %>%
  cor() %>%
  (function (m) m > 0.95) (.) %>%
  as.data.frame() %>%
  for (item in .) {
    column <- item[item,]
    row <- rownames(column)
    col <- colnames(column)
    paste(row, col, sep = ", ") %>%
      cat()
  }

# Near perfect correlation:
# - weatherVar


# Fill NAs
missing_weather <- tibble(column = character(), group = integer())
tmp <- missing_weather
for (col in colnames(df_raw[67:302])) {
  missing <- df_raw[col] %>%
    is.na() %>%
    sum()
  tmp[1, 1] <- col
  tmp[1, 2] <- missing
  missing_weather <- rbind(missing_weather, tmp)
}
missing_weather <- missing_weather %>%
  mutate(group = as.factor(group)) %>%
  arrange(group)

groups <- levels(missing_weather$group)
output_df <- df_raw %>%
  select(67:302)
for (group in groups) {
  tmp_df <- output_df
  cols <- missing_weather[missing_weather$group == group, ]$column
  for (col in cols) {
  }
}

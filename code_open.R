library(readxl)
charls <- read_excel("CHARLS.xls")
data <- charls

library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(pROC)
library(mice)

summary(data)
str(data)
library(dplyr)

is_char_or_factor <- function(x) {
  is.character(x) | is.factor(x)
}

data <- data %>%
  mutate(across(where(is_char_or_factor), ~na_if(.x, "NA")))
sum(data == "NA", na.rm = TRUE)

replace_na_in_numeric <- function(x) {
  x <- as.character(x)
  x <- na_if(x, "NA")
  x <- as.numeric(x)
  return(x)
}

data <- data %>%
  mutate(across(where(is.numeric), replace_na_in_numeric))
sum(data == "NA", na.rm = TRUE)

numeric_cols <- c("sleep_night", "dressa", "batha", "eata", "beda", "toilta", "urina", "ADL",
                  "housewka", "mealsa", "shopa", "phonea", "medsa", "moneya", "cesd10", "tr20",
                  "mo", "dy", "yr", "dw", "ds", "draw", "ser7", "age", "bmi", "mwaist", "income_total",
                  "act_1", "act_2", "act_3", "act_4", "act_5", "act_6", "act_7", "act_8",
                  "social1", "social2", "social3", "social4", "social5", "social6", "social7",
                  "social8", "social9", "social10", "social11",
                  paste0("da042s", 1:15))
data <- data %>%
  mutate(across(all_of(numeric_cols), as.numeric))

data$ID <- as.character(data$ID)
data$iwy <- as.character(data$iwy)
factor_cols <- c("ragender", "hrural", "hibpe", "dyslipe", "diabe", "cancre", "lunge", "livere",
                 "hearte", "stroke", "kidneye", "digeste", "psyche", "memrye", "arthre", "asthmae",
                 "vgact_c", "mdact_c", "ltact_c", "drinkl", "smoken", "socwk", "satlife", "raeduc_c",
                 "marry", "phy_acta", "hobby", "work", "retire")
data <- data %>%
  mutate(across(all_of(factor_cols), as.factor))

data <- data %>%
  mutate(pain_count = rowSums(across(starts_with("da042s"), ~ifelse(. == 0, 0, ifelse(is.na(.), 0, 1)))))

data <- data %>%
  mutate(pain_level = cut(pain_count, 
                          breaks = c(-Inf, 0, 3, 7, Inf), 
                          labels = c("1", "2", "3", "4"),
                          include.lowest = TRUE)) %>%
  mutate(pain_level = factor(pain_level, 
                             levels = c("1", "2", "3", "4")))

data <- data %>%
  select(-matches("^da042s"))

data <- data %>%
  mutate(act_total = rowSums(across(starts_with("act")))) %>%
  mutate(act = case_when(
    act_total > 0 ~ "1",
    TRUE ~ "2"
  )) %>%
  mutate(act = factor(act, levels = c("1", "2")))

data <- data %>%
  select(-matches("^act"))

data <- data %>%
  mutate(adl_total = rowSums(across(c(dressa, batha, eata, beda, toilta, urina)))) %>%
  mutate(adl_level = cut(adl_total, 
                         breaks = c(-Inf, 0, 5, Inf), 
                         labels = c("1", "2", "3"),
                         include.lowest = TRUE)) %>%
  mutate(adl_level = factor(adl_level, levels = c("1", "2", "3")))

data <- data %>%
  select(-c(dressa, batha, eata, beda, toilta, urina))

data <- data %>%
  mutate(iadl_total = rowSums(across(c(housewka, mealsa, shopa, phonea, medsa, moneya)))) %>%
  mutate(iadl_level = case_when(
    iadl_total >= 6 ~ "Independence, good social function",
    iadl_total >= 4 & iadl_total <= 5 ~ "Mild dependence, need partial assistance",
    iadl_total <= 3 ~ "Obvious dependence, need more care",
    TRUE ~ NA_character_
  )) %>%
  mutate(iadl_level = factor(iadl_level, levels = c("Independence, good social function", "Mild dependence, need partial assistance", "Obvious dependence, need more care")))

data <- data %>%
  select(-c(housewka, mealsa, shopa, phonea, medsa, moneya))

data <- data %>%
  mutate(cognition_total = rowSums(across(c(tr20, mo, dy, yr, dw, ds, draw, ser7))))

data <- data %>%
  mutate(social_total = rowSums(across(starts_with("social")))) %>%
  mutate(social = case_when(
    social_total > 0 ~ "Has social activity",
    TRUE ~ "No social activity"
  )) %>%
  mutate(social = factor(social, levels = c("Has social activity", "No social activity")))

data <- data %>%
  select(-matches("^social"))

data$age_group <- cut(data$age, 
                      breaks = seq(40, 85, by = 5), 
                      labels = paste0(seq(40, 80, by = 5), "-", seq(45, 85, by = 5)),
                      right = FALSE)
summary(data$age_group)

age_group_stats <- data %>%
  group_by(age_group) %>%
  summarise(mean_cognition = mean(cognition_total, na.rm = TRUE),
            sd_cognition = sd(cognition_total, na.rm = TRUE))

age_group_stats <- age_group_stats %>%
  mutate(cognition_threshold = mean_cognition - sd_cognition)

data_with_threshold <- data %>%
  left_join(age_group_stats, by = "age_group")

data_with_threshold <- data_with_threshold %>%
  mutate(cognitive_decline = factor(case_when(
    cognition_total < cognition_threshold ~ "Yes",
    TRUE ~ "No"
  ), levels = c("No", "Yes")))

summary(data_with_threshold$cognitive_decline)

head(data)
str(data_with_threshold)

data_with_threshold <- data_with_threshold %>%
  select(-c(tr20, mo, dy, yr, dw, ds, draw, ser7))

data_cleaned <- data_with_threshold %>%
  select(-iadl_total, -adl_total, -matches("^tr"), -pain_count, -mean_cognition, -sd_cognition, -cognition_threshold)

str(data_cleaned)

library(tidyverse)

data_cleaned$iwy <- as.numeric(as.character(data_cleaned$iwy))

df_wide <- data_cleaned %>%
  pivot_wider(
    id_cols = c(ID, householdID),
    names_from = iwy,
    values_from = -c(ID, householdID, iwy)
  )

head(df_wide)

missing_values_count <- df_wide %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_count")

print(missing_values_count[order(-missing_values_count$missing_count), ])
summary(df_wide)

missing_values_count <- sapply(df_wide, function(x) sum(is.na(x)))
missing_values_prop <- missing_values_count / nrow(df_wide)

missing_data_summary <- data.frame(
  Variable = names(missing_values_count),
  Missing_Count = missing_values_count,
  Missing_Prop = missing_values_prop
)

print(missing_data_summary[order(-missing_data_summary$Missing_Count), ])

df_wide <- df_wide[, !grepl("_2012$|_2014$|_2016$", names(df_wide))]

library(naniar)
library(mice)
library(VIM)

gg_miss_case(df_wide, order_cases = TRUE)
gg_miss_var(df_wide)

set.seed(123)
df_sample <- slice_sample(df_wide, n = 2000)
vis_miss(df_sample, sort_miss = TRUE)

df_wide <- df_wide[ , !grepl(
  "^(bmi_|mwaist_|stroke_2018|iadl_level_2011)$",
  names(df_wide)
)]

missing_check <- sapply(df_wide, function(x) sum(is.na(x)))
missing_check[missing_check > 0]

df_wide <- df_wide %>%
  mutate(
    age_2015 = ifelse(is.na(age_2015), age_2013 + 2, age_2015),
    age_2018 = ifelse(is.na(age_2018), age_2015 + 3, age_2018),
    age_2020 = ifelse(is.na(age_2020), age_2018 + 2, age_2020)
  ) %>%
  mutate(
    ragender_2015 = ifelse(is.na(ragender_2015), coalesce(ragender_2013, ragender_2011), ragender_2015),
    ragender_2018 = ifelse(is.na(ragender_2018), coalesce(ragender_2013, ragender_2011), ragender_2018),
    ragender_2020 = ifelse(is.na(ragender_2020), coalesce(ragender_2013, ragender_2011), ragender_2020),
    
    raeduc_c_2015 = ifelse(is.na(raeduc_c_2015), coalesce(raeduc_c_2013, raeduc_c_2011), raeduc_c_2015),
    raeduc_c_2018 = ifelse(is.na(raeduc_c_2018), coalesce(raeduc_c_2013, raeduc_c_2011), raeduc_c_2018),
    raeduc_c_2020 = ifelse(is.na(raeduc_c_2020), coalesce(raeduc_c_2013, raeduc_c_2011), raeduc_c_2020),
    
    marry_2015 = ifelse(is.na(marry_2015), coalesce(marry_2013, marry_2011), marry_2015),
    marry_2018 = ifelse(is.na(marry_2018), coalesce(marry_2013, marry_2011), marry_2018),
    marry_2020 = ifelse(is.na(marry_2020), coalesce(marry_2013, marry_2011), marry_2020)
  )

summary(df_wide[, c("age_2015", "ragender_2015", "raeduc_c_2015", "marry_2015")])

df_wide <- df_wide %>%
  mutate(
    age_2015 = ifelse(is.na(age_2015),
                      coalesce(age_2013 + 2,
                               age_2011 + 4,
                               age_2018 - 3,
                               age_2020 - 5),
                      age_2015),
    
    ragender_2015 = {
      x <- as.character(ragender_2015)
      x[is.na(x)] <- coalesce(as.character(ragender_2013),
                              as.character(ragender_2011),
                              as.character(ragender_2018),
                              as.character(ragender_2020))[is.na(x)]
      factor(x, levels = union(levels(ragender_2013),
                               levels(ragender_2011)))
    },
    
    raeduc_c_2015 = {
      x <- as.character(raeduc_c_2015)
      x[is.na(x)] <- coalesce(as.character(raeduc_c_2013),
                              as.character(raeduc_c_2011),
                              as.character(raeduc_c_2018),
                              as.character(raeduc_c_2020))[is.na(x)]
      factor(x, levels = union(levels(raeduc_c_2013),
                               levels(raeduc_c_2011)))
    },
    
    marry_2015 = {
      x <- as.character(marry_2015)
      x[is.na(x)] <- coalesce(as.character(marry_2013),
                              as.character(marry_2011),
                              as.character(marry_2018),
                              as.character(marry_2020))[is.na(x)]
      factor(x, levels = union(levels(marry_2013),
                               levels(marry_2011)))
    }
  )

summary(df_wide[, c("age_2015", "ragender_2015", "raeduc_c_2015", "marry_2015")])

grep("_2011$|_2013$|_2015$|_2018$|_2020$", names(df_wide), value = TRUE)

fill_any_year <- function(df, stub) {
  cols <- grep(paste0("^", stub, "_\\d{4}$"), names(df), value = TRUE)
  if (length(cols) == 0) return(df)
  
  chr_mat <- as.matrix(df[cols]) |> as.data.frame() |> lapply(as.character) |> as.data.frame()
  
  fill_vec <- apply(chr_mat, 1, function(x) x[!is.na(x)][1])
  
  for (col in cols) {
    old_vec <- as.character(df[[col]])
    new_vec <- ifelse(is.na(old_vec), fill_vec, old_vec)
    df[[col]] <- factor(new_vec, levels = unique(na.omit(new_vec)))
  }
  df
}

var_stubs <- c("raeduc_c", "hrural", "marry")

for (v in var_stubs) {
  df_wide <- fill_any_year(df_wide, v)
}

summary(df_wide[, c("raeduc_c_2011", "raeduc_c_2013", "raeduc_c_2015")])
summary(df_wide[, c("hrural_2011", "hrural_2013", "hrural_2015")])
summary(df_wide[, c("marry_2011", "marry_2013", "marry_2015")])

library(stringr)

cols <- setdiff(names(df_wide), c("ID", "householdID"))

tmp <- tibble(
  base_var = str_remove(cols, "_[0-9]{4}$"),
  year     = str_extract(cols, "[0-9]{4}$")
) %>%
  filter(!is.na(year))

presence_tbl <- tmp %>%
  mutate(flag = TRUE) %>%
  pivot_wider(names_from = year, values_from = flag, values_fill = FALSE) %>%
  arrange(base_var)

print(presence_tbl)

vars_by_year <- names(df_wide) %>%
  str_remove("_[0-9]{4}$") %>%
  str_subset("^[0-9]{4}$", negate = TRUE) %>%
  tibble(col = ., yr = str_extract(names(df_wide), "[0-9]{4}$")) %>%
  filter(!is.na(yr)) %>%
  group_by(yr) %>%
  summarise(vars_this_year = list(unique(col)), .groups = "drop")

print(vars_by_year)

vars_flat <- vars_by_year %>%
  unnest(cols = vars_this_year)

print(vars_flat)

library(dplyr)
library(stringr)

target_triplet4 <- c("2011","2013","2015","2018")

base_vars_4yr <- names(df_wide) %>%
  str_remove("_[0-9]{4}$") %>%
  tibble(base_var = ., yr = str_extract(names(df_wide), "[0-9]{4}$")) %>%
  group_by(base_var) %>%
  filter(yr %in% target_triplet4) %>%
  summarise(present_all = n_distinct(yr) == length(target_triplet4), .groups = "drop") %>%
  filter(present_all) %>%
  pull(base_var)

keep_cols_4yr <- c("ID","householdID",
                   outer(base_vars_4yr, target_triplet4, paste, sep = "_") %>% as.vector())

df_4yr <- df_wide %>% select(all_of(keep_cols_4yr[keep_cols_4yr %in% names(df_wide)]))
glimpse(df_4yr)
summary(df_4yr)

library(dplyr)

yrs      <- c(2011, 2013, 2015, 2018)
age_cols <- paste0("age_", yrs)

df_4yr <- df_4yr %>%
  group_by(ID) %>%
  mutate(
    age_2011 = ifelse(is.na(age_2011), age_2013 - 2, age_2011),
    age_2013 = ifelse(is.na(age_2013),
                      coalesce(age_2011 + 2, age_2015 - 2),
                      age_2013),
    age_2015 = ifelse(is.na(age_2015),
                      coalesce(age_2013 + 2, age_2018 - 3),
                      age_2015),
    age_2018 = ifelse(is.na(age_2018), age_2015 + 3, age_2018)
  ) %>%
  ungroup()

sapply(df_4yr[age_cols], function(x) sum(is.na(x)))

df_4yr <- df_4yr %>%
  group_by(ID) %>%
  mutate(
    age_2011 = coalesce(age_2011,
                        age_2013 - 2,
                        age_2015 - 4,
                        age_2018 - 7)
  ) %>%
  ungroup()

missing_report <- df_4yr %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  mutate(Missing_Prop = Missing_Count / nrow(df_4yr)) %>%
  arrange(desc(Missing_Prop))

print(missing_report, n = Inf)

df_4yr <- df_4yr %>%
  mutate(
    marry_cat_2011 = case_when(
      marry_2011 %in% c(1, 4, 6) ~ "Married",
      TRUE ~ "Not Married"
    ),
    marry_cat_2013 = case_when(
      marry_2013 %in% c(1, 4, 6) ~ "Married",
      TRUE ~ "Not Married"
    ),
    marry_cat_2015 = case_when(
      marry_2015 %in% c(1, 4, 6) ~ "Married",
      TRUE ~ "Not Married"
    ),
    marry_cat_2018 = case_when(
      marry_2018 %in% c(1, 4, 6) ~ "Married",
      TRUE ~ "Not Married"
    ),
  )

head(df_4yr[, c("marry_2011", "marry_2013", "marry_2015","marry_2018", "marry_cat_2018",  "marry_cat_2011", "marry_cat_2013", "marry_cat_2015")])

df_4yr_clean <- df_4yr %>%
  select(-grep("^mwaist_", names(.)), -grep("^bmi_", names(.)))

df_4yr_clean <- df_4yr_clean[, !names(df_4yr_clean) %in% c("marry_2011", "marry_2013", "marry_2015", "marry_2018")]

df_4yr_clean <- df_4yr_clean %>%
  mutate(
    marry_cat_2011 = factor(marry_cat_2011),
    marry_cat_2013 = factor(marry_cat_2013),
    marry_cat_2015 = factor(marry_cat_2015),
    marry_cat_2018 = factor(marry_cat_2018)
  )

str(df_4yr_clean)
summary(df_4yr_clean)

mid_miss_vars <- missing_report %>% 
  filter(Missing_Prop > 0.3 & Missing_Prop <= 0.8) %>% 
  pull(Variable)

rowNAA <- df_4yr_clean %>%
  mutate(
    Total_N = ncol(.) - 2,
    Missing_Count = rowSums(is.na(select(., -ID, -householdID))),
    Missing_Prop = Missing_Count / Total_N
  ) %>%
  filter(Missing_Prop > 0)

head(rowNAA)

high_missing_ids <- rowNAA %>%
  filter(Missing_Prop > 0.20) %>%
  select(ID, householdID)

data_final <- df_4yr_clean %>%
  anti_join(high_missing_ids, by = c("ID", "householdID"))

data_final <- data_final[, !grepl("^bmi|^mwaist", names(data_final))]
data_final <- data_final[, !grepl("^ADL", names(data_final))]

age_cols <- grep("^age", names(data_final), value = TRUE)
data_final <- data_final[complete.cases(data_final[age_cols]), ]

cols_to_keep <- names(data_final)[!grepl("^marry_[0-9]{4}$", names(data_final))]
data_final <- data_final[, cols_to_keep]

gender_cols <- c("ragender_2011", "ragender_2013", "ragender_2015", "ragender_2018")

data_final <- data_final %>%
  mutate(across(all_of(gender_cols), as.character))

data_final <- data_final %>%
  rowwise() %>%
  mutate(across(all_of(gender_cols),
                ~ coalesce(.x,
                           na.omit(c_across(all_of(gender_cols)))[1]))) %>%
  ungroup()

data_final <- data_final %>%
  mutate(across(all_of(gender_cols),
                ~ factor(.x, levels = c("1", "2"))))

names(data_final)
str(data_final)

library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

missing_summary <- data_final %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_rate") %>%
  arrange(desc(missing_rate))

print(head(missing_summary, 30), n = 30)



missing_by_year <- missing_summary %>%
  mutate(
    year = case_when(
      grepl("_2011$", variable) ~ "2011",
      grepl("_2013$", variable) ~ "2013", 
      grepl("_2015$", variable) ~ "2015",
      grepl("_2018$", variable) ~ "2018",
      TRUE ~ "Other/Cross-wave"
    ),
    variable_type = case_when(
      grepl("^cognition", variable) ~ "Cognitive function",
      grepl("^cesd", variable) ~ "Depressive symptoms",
      grepl("^income", variable) ~ "Income",
      grepl("^adl_", variable) ~ "ADL",
      grepl("^iadl_", variable) ~ "IADL",
      grepl("^(ltact|mdact|vgact|phy_act)", variable) ~ "Physical activity",
      grepl("^(arthre|asthma|cancre|diabe|digeste|dyslipe|hearte|hibpe|kidneye|livere|lunge|psyche)", variable) ~ "Chronic diseases",
      grepl("^(ragender|raeduc|age|hrural|retire|work|marry)", variable) ~ "Demographics",
      grepl("^(sleep|smoken|drinkl|hobby|socwk|satlife|pain)", variable) ~ "Lifestyle",
      grepl("^(memrye)", variable) ~ "Memory self-report",
      TRUE ~ "Other"
    )
  )

year_summary <- missing_by_year %>%
  filter(year != "Other/Cross-wave") %>%
  group_by(year) %>%
  summarise(
    avg_missing_rate = mean(missing_rate),
    median_missing_rate = median(missing_rate),
    high_missing_vars = sum(missing_rate > 0.3),
    .groups = 'drop'
  )

print(year_summary)

type_summary <- missing_by_year %>%
  group_by(variable_type) %>%
  summarise(
    n_vars = n(),
    avg_missing_rate = mean(missing_rate),
    max_missing_rate = max(missing_rate),
    high_missing_vars = sum(missing_rate > 0.3),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_missing_rate))

print(type_summary, n = 20)

ggplot(missing_summary, aes(x = missing_rate)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0.3, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0.5, color = "darkred", linetype = "dashed") +
  labs(title = "Distribution of variable missing rates",
       x = "Missing proportion", 
       y = "Number of variables") +
  theme_minimal()

key_vars <- c("cognition_total_2011", "cognition_total_2013", "cognition_total_2015", "cognition_total_2018",
              "cesd10_2011", "cesd10_2013", "cesd10_2015", "cesd10_2018",
              "income_total_2011", "income_total_2013", "income_total_2015", "income_total_2018",
              "ragender_2011", "ragender_2013", "ragender_2015", "ragender_2018",
              "ltact_c_2011", "ltact_c_2013", "ltact_c_2015", "ltact_c_2018")

key_missing <- missing_summary %>%
  filter(variable %in% key_vars) %>%
  mutate(
    year = substr(variable, nchar(variable)-3, nchar(variable)),
    var_core = gsub("_2011|_2013|_2015|_2018", "", variable)
  )

ggplot(key_missing, aes(x = year, y = var_core, fill = missing_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.3) +
  labs(title = "Missing rate heatmap for key variables",
       x = "Survey year",
       y = "Variable",
       fill = "Missing proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0))


cognition_vars <- missing_summary %>% 
  filter(grepl("cognition", variable)) %>%
  arrange(desc(missing_rate))
print(cognition_vars, n = 20)

physical_vars <- missing_summary %>% 
  filter(grepl("ltact|mdact|vgact|phy_act", variable)) %>%
  arrange(desc(missing_rate))
print(physical_vars, n = 20)

income_vars <- missing_summary %>% 
  filter(grepl("income", variable)) %>%
  arrange(desc(missing_rate))
print(income_vars, n = 10)

demographic_vars <- missing_summary %>% 
  filter(grepl("ragender|raeduc|age$|hrural", variable)) %>%
  arrange(desc(missing_rate))
print(demographic_vars, n = 15)


missing_summary <- missing_summary %>%
  mutate(
    recommendation = case_when(
      missing_rate == 0 ~ "No missing, keep",
      missing_rate <= 0.05 ~ "Very low missing, simple imputation",
      missing_rate > 0.05 & missing_rate <= 0.2 ~ "Low missing, multiple imputation",
      missing_rate > 0.2 & missing_rate <= 0.5 ~ "Moderate missing, careful handling",
      missing_rate > 0.5 & missing_rate <= 0.7 ~ "High missing, consider deletion or special treatment",
      missing_rate > 0.7 ~ "Very high missing, recommend deletion"
    )
  )

rec_summary <- missing_summary %>%
  group_by(recommendation) %>%
  summarise(n_vars = n(), .groups = 'drop') %>%
  arrange(desc(n_vars))

print(rec_summary)

write.csv(missing_summary, "missing_analysis_summary.csv", row.names = FALSE)

cognitive_decline_vars <- missing_summary %>% 
  filter(grepl("cognitive_decline", variable))
print(cognitive_decline_vars)


gender_vars <- c("ragender_2011", "ragender_2013", "ragender_2015", "ragender_2018")
gender_missing <- sapply(data_final[gender_vars], function(x) sum(is.na(x)))
gender_missing_rate <- sapply(data_final[gender_vars], function(x) mean(is.na(x)))

for(i in seq_along(gender_vars)){
}


gender_consistency <- data_final %>%
  select(ID, ragender_2011, ragender_2013, ragender_2015, ragender_2018) %>%
  mutate(
    has_any_gender = !(is.na(ragender_2011) & is.na(ragender_2013) & 
                         is.na(ragender_2015) & is.na(ragender_2018)),
    all_genders = paste(
      ifelse(!is.na(ragender_2011), as.character(ragender_2011), ""),
      ifelse(!is.na(ragender_2013), as.character(ragender_2013), ""),
      ifelse(!is.na(ragender_2015), as.character(ragender_2015), ""),
      ifelse(!is.na(ragender_2018), as.character(ragender_2018), ""),
      sep = "|"
    )
  ) %>%
  filter(has_any_gender) %>%
  rowwise() %>%
  mutate(
    unique_genders = paste(sort(unique(unlist(strsplit(all_genders, "\\|"))[unlist(strsplit(all_genders, "\\|")) != ""])), collapse = ",")
  ) %>%
  ungroup()

consistency_summary <- gender_consistency %>%
  group_by(unique_genders) %>%
  summarise(
    n_ids = n(),
    proportion = n()/nrow(gender_consistency)
  ) %>%
  arrange(desc(n_ids))

print(consistency_summary)


data_filled <- data_final

fill_gender <- function(row) {
  if(!is.na(row["ragender_2015"])) {
    return(row["ragender_2015"])
  }
  else if(!is.na(row["ragender_2018"])) {
    return(row["ragender_2018"])
  }
  else {
    return(NA)
  }
}

for(i in 1:nrow(data_filled)) {
  if(is.na(data_filled$ragender_2011[i])) {
    data_filled$ragender_2011[i] <- fill_gender(data_filled[i, ])
  }
  
  if(is.na(data_filled$ragender_2013[i])) {
    data_filled$ragender_2013[i] <- fill_gender(data_filled[i, ])
  }
}

gender_missing_after <- sapply(data_filled[gender_vars], function(x) sum(is.na(x)))
gender_missing_rate_after <- sapply(data_filled[gender_vars], function(x) mean(is.na(x)))

for(i in seq_along(gender_vars)){
}


still_missing_gender <- data_filled %>%
  filter(is.na(ragender_2011) | is.na(ragender_2013)) %>%
  nrow()


if(still_missing_gender > 0) {
  missing_gender_info <- data_filled %>%
    filter(is.na(ragender_2011) | is.na(ragender_2013)) %>%
    select(ID, ragender_2011, ragender_2013, ragender_2015, ragender_2018) %>%
    mutate(
      missing_2011 = is.na(ragender_2011),
      missing_2013 = is.na(ragender_2013),
      missing_2015 = is.na(ragender_2015),
      missing_2018 = is.na(ragender_2018)
    )
  
  print(table(
    `2011 missing` = missing_gender_info$missing_2011,
    `2013 missing` = missing_gender_info$missing_2013,
    `2015 missing` = missing_gender_info$missing_2015,
    `2018 missing` = missing_gender_info$missing_2018
  ))
  
  age_marry_pattern <- data_filled %>%
    filter(!is.na(ragender_2011)) %>%
    group_by(ragender_2011) %>%
    summarise(
      avg_age = mean(age_2011, na.rm = TRUE),
      married_prop = mean(marry_cat_2011 == "Married", na.rm = TRUE),
      n = n()
    )
  
  print(age_marry_pattern)
}

data_filled <- data_filled %>%
  mutate(
    final_gender = case_when(
      !is.na(ragender_2018) ~ ragender_2018,
      !is.na(ragender_2015) ~ ragender_2015,
      !is.na(ragender_2013) ~ ragender_2013,
      !is.na(ragender_2011) ~ ragender_2011,
      TRUE ~ NA
    ),
    ragender_2011 = ifelse(is.na(ragender_2011), final_gender, ragender_2011),
    ragender_2013 = ifelse(is.na(ragender_2013), final_gender, ragender_2013),
    ragender_2015 = ifelse(is.na(ragender_2015), final_gender, ragender_2015),
    ragender_2018 = ifelse(is.na(ragender_2018), final_gender, ragender_2018)
  ) %>%
  select(-final_gender)

final_gender_missing <- sapply(data_filled[gender_vars], function(x) sum(is.na(x)))
print(final_gender_missing)


if(sum(apply(data_filled[gender_vars], 1, function(x) all(is.na(x)))) > 0) {
  data_filled <- data_filled %>%
    mutate(
      gender_missing_flag = ifelse(
        is.na(ragender_2011) & is.na(ragender_2013) & 
          is.na(ragender_2015) & is.na(ragender_2018),
        "Completely missing",
        "Has gender data"
      )
    )
  
  print(table(data_filled$gender_missing_flag))
}

data_final <- data_filled

saveRDS(data_final, "data_final_gender_filled.rds")

new_missing <- data_final %>%
  select(ragender_2011, ragender_2013, ragender_2015, ragender_2018,
         cognition_total_2011, cesd10_2011, income_total_2011) %>%
  summarise(across(everything(), ~mean(is.na(.))))

print(new_missing)

missing_summary_updated <- data_final %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_rate") %>%
  arrange(desc(missing_rate))


print(head(missing_summary_updated, 50), n = 50)


missing_summary_with_type <- missing_summary_updated %>%
  mutate(
    variable_type = case_when(
      grepl("^cognition", variable) ~ "Cognitive function",
      grepl("^cesd", variable) ~ "Depressive symptoms",
      grepl("^income", variable) ~ "Income",
      grepl("^adl_", variable) ~ "ADL",
      grepl("^iadl_", variable) ~ "IADL",
      grepl("^(ltact|mdact|vgact|phy_act)", variable) ~ "Physical activity",
      grepl("^(arthre|asthma|cancre|diabe|digeste|dyslipe|hearte|hibpe|kidneye|livere|lunge|psyche)", variable) ~ "Chronic diseases",
      grepl("^(ragender|raeduc|age$|hrural|retire|work|marry)", variable) ~ "Demographics",
      grepl("^(sleep|smoken|drinkl|hobby|socwk|satlife|pain)", variable) ~ "Lifestyle",
      grepl("^(memrye)", variable) ~ "Memory self-report",
      TRUE ~ "Other"
    ),
    year = case_when(
      grepl("_2011$", variable) ~ "2011",
      grepl("_2013$", variable) ~ "2013", 
      grepl("_2015$", variable) ~ "2015",
      grepl("_2018$", variable) ~ "2018",
      TRUE ~ "Other/Cross-wave"
    )
  )

type_summary <- missing_summary_with_type %>%
  group_by(variable_type) %>%
  summarise(
    n_vars = n(),
    avg_missing_rate = mean(missing_rate),
    median_missing_rate = median(missing_rate),
    max_missing_rate = max(missing_rate),
    high_missing_vars = sum(missing_rate > 0.3),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_missing_rate))

print(type_summary, n = 20)


cognition_vars <- missing_summary_updated %>% 
  filter(grepl("cognition", variable)) %>%
  arrange(desc(missing_rate))
print(cognition_vars, n = 20)

physical_vars <- missing_summary_updated %>% 
  filter(grepl("ltact|mdact|vgact|phy_act", variable)) %>%
  arrange(desc(missing_rate))
print(physical_vars, n = 20)

income_vars <- missing_summary_updated %>% 
  filter(grepl("income", variable)) %>%
  arrange(desc(missing_rate))
print(income_vars, n = 10)

gender_vars <- missing_summary_updated %>% 
  filter(grepl("ragender", variable)) %>%
  arrange(desc(missing_rate))
print(gender_vars, n = 10)

cesd_vars <- missing_summary_updated %>% 
  filter(grepl("cesd", variable)) %>%
  arrange(desc(missing_rate))
print(cesd_vars, n = 10)

satlife_vars <- missing_summary_updated %>% 
  filter(grepl("satlife", variable)) %>%
  arrange(desc(missing_rate))
print(satlife_vars, n = 10)


year_summary <- missing_summary_with_type %>%
  filter(year != "Other/Cross-wave") %>%
  group_by(year) %>%
  summarise(
    avg_missing_rate = mean(missing_rate),
    median_missing_rate = median(missing_rate),
    high_missing_vars = sum(missing_rate > 0.3),
    .groups = 'drop'
  )

print(year_summary)


for (yr in c("2011", "2013", "2015", "2018")) {
  year_vars <- missing_summary_with_type %>%
    filter(year == yr) %>%
    arrange(desc(missing_rate)) %>%
    head(5)
  print(year_vars)
}

library(ggplot2)

p1 <- ggplot(missing_summary_updated, aes(x = missing_rate)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = c(0.1, 0.3, 0.5), color = c("orange", "red", "darkred"), 
             linetype = "dashed", alpha = 0.7) +
  annotate("text", x = 0.1, y = 0, label = "10%", vjust = -1, color = "orange") +
  annotate("text", x = 0.3, y = 0, label = "30%", vjust = -1, color = "red") +
  annotate("text", x = 0.5, y = 0, label = "50%", vjust = -1, color = "darkred") +
  labs(title = "Distribution of variable missing rates",
       subtitle = "Dashed lines: 10%, 30%, 50% thresholds",
       x = "Missing proportion", 
       y = "Number of variables") +
  theme_minimal()

print(p1)

key_vars_list <- c("cognition_total_2011", "cognition_total_2013", "cognition_total_2015", "cognition_total_2018",
                   "cesd10_2011", "cesd10_2013", "cesd10_2015", "cesd10_2018",
                   "income_total_2011", "income_total_2013", "income_total_2015", "income_total_2018",
                   "ragender_2011", "ragender_2013", "ragender_2015", "ragender_2018",
                   "age_2011", "age_2013", "age_2015", "age_2018")

key_vars_data <- missing_summary_updated %>%
  filter(variable %in% key_vars_list) %>%
  mutate(
    year = substr(variable, nchar(variable)-3, nchar(variable)),
    var_core = gsub("_2011|_2013|_2015|_2018", "", variable),
    var_group = case_when(
      grepl("cognition", var_core) ~ "Cognitive function",
      grepl("cesd", var_core) ~ "Depressive symptoms",
      grepl("income", var_core) ~ "Income",
      grepl("ragender", var_core) ~ "Gender",
      grepl("age", var_core) ~ "Age"
    )
  )

p2 <- ggplot(key_vars_data, aes(x = reorder(variable, missing_rate), y = missing_rate, fill = var_group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Missing rates for key variables",
       x = "Variable",
       y = "Missing proportion",
       fill = "Variable type") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)

report_data <- missing_summary_with_type %>%
  mutate(
    recommendation = case_when(
      missing_rate == 0 ~ "No missing, keep",
      missing_rate <= 0.05 ~ "Very low missing, simple imputation",
      missing_rate > 0.05 & missing_rate <= 0.2 ~ "Low missing, multiple imputation",
      missing_rate > 0.2 & missing_rate <= 0.5 ~ "Moderate missing, careful handling",
      missing_rate > 0.5 & missing_rate <= 0.7 ~ "High missing, consider deletion or special treatment",
      missing_rate > 0.7 ~ "Very high missing, recommend deletion"
    )
  )

write.csv(report_data, "missing_analysis_detailed_report.csv", row.names = FALSE)

rec_summary <- report_data %>%
  group_by(recommendation) %>%
  summarise(n_vars = n(), .groups = 'drop') %>%
  arrange(desc(n_vars))

print(rec_summary)

vars_to_consider_removing <- report_data %>%
  filter(missing_rate > 0.5) %>%
  select(variable, missing_rate, variable_type, year, recommendation)

print(vars_to_consider_removing, n = 30)


physical_details <- report_data %>%
  filter(grepl("Physical activity", variable_type)) %>%
  select(variable, missing_rate, year, recommendation) %>%
  arrange(desc(missing_rate))

print(physical_details, n = 20)



missing_summary <- data_final %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_rate") %>%
  arrange(desc(missing_rate))

high_missing_vars <- missing_summary %>%
  filter(missing_rate > 0.5) %>%
  pull(variable)

print(high_missing_vars)

get_core_variable_name <- function(var_name) {
  core_name <- sub("_(2011|2013|2015|2018)$", "", var_name)
  return(core_name)
}

high_missing_cores <- unique(sapply(high_missing_vars, get_core_variable_name))

print(high_missing_cores)

vars_to_remove <- character(0)

for (core in high_missing_cores) {
  pattern <- paste0("^", core, "_(2011|2013|2015|2018)$")
  matching_vars <- grep(pattern, names(data_final), value = TRUE, perl = TRUE)
  
  if (length(matching_vars) > 0) {
    vars_to_remove <- c(vars_to_remove, matching_vars)
  } else {
    vars_to_remove <- c(vars_to_remove, core)
  }
}

vars_to_remove <- unique(vars_to_remove)

print(vars_to_remove)

existing_vars_to_remove <- intersect(vars_to_remove, names(data_final))
non_existing_vars <- setdiff(vars_to_remove, names(data_final))

if (length(non_existing_vars) > 0) {
  print(non_existing_vars)
}



data_cleaned <- data_final %>%
  select(-all_of(existing_vars_to_remove))


missing_summary_cleaned <- data_cleaned %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_rate") %>%
  arrange(desc(missing_rate))

remaining_high_missing <- missing_summary_cleaned %>%
  filter(missing_rate > 0.5)

if (nrow(remaining_high_missing) > 0) {
  print(remaining_high_missing, n = 20)
} else {
}

print(head(missing_summary_cleaned, 20), n = 20)


deleted_vars_info <- data.frame(
  variable = existing_vars_to_remove,
  stringsAsFactors = FALSE
) %>%
  mutate(
    core_variable = sapply(variable, get_core_variable_name),
    year = str_extract(variable, "2011|2013|2015|2018$"),
    var_type = case_when(
      grepl("ltact|mdact|vgact|phy_act", core_variable) ~ "Physical activity",
      grepl("income", core_variable) ~ "Income",
      grepl("cognition", core_variable) ~ "Cognitive function",
      grepl("ragender", core_variable) ~ "Gender",
      TRUE ~ "Other"
    )
  )

deleted_summary <- deleted_vars_info %>%
  group_by(var_type) %>%
  summarise(
    n_vars = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_vars))

print(deleted_summary)

key_vars_to_check <- c(
  "cognition_total_2011", "cognition_total_2013", "cognition_total_2015", "cognition_total_2018",
  "cesd10_2011", "cesd10_2013", "cesd10_2015", "cesd10_2018",
  "age_2011", "age_2013", "age_2015", "age_2018",
  "ragender_2011", "ragender_2013", "ragender_2015", "ragender_2018",
  "raeduc_c_2011", "raeduc_c_2013", "raeduc_c_2015", "raeduc_c_2018"
)

missing_key_vars <- setdiff(key_vars_to_check, names(data_cleaned))

if (length(missing_key_vars) > 0) {
  print(missing_key_vars)
} else {
}

data_final <- data_cleaned

saveRDS(data_final, "data_final_cleaned.rds")
write.csv(data_final, "data_final_cleaned.csv", row.names = FALSE)


str(data_final[, 1:min(20, ncol(data_final))])


all_vars <- names(data_final)
core_vars_all <- unique(sapply(all_vars, function(x) {
  if (grepl("_(2011|2013|2015|2018)$", x)) {
    return(sub("_(2011|2013|2015|2018)$", "", x))
  } else {
    return(x)
  }
}))

time_varying_cores <- core_vars_all[sapply(core_vars_all, function(core) {
  years <- c("2011", "2013", "2015", "2018")
  present_years <- sapply(years, function(yr) {
    var_name <- paste0(core, "_", yr)
    return(var_name %in% all_vars)
  })
  return(sum(present_years) > 1)
})]

if (length(time_varying_cores) > 0) {
}

if (length(time_varying_cores) > 0) {
  static_vars <- all_vars[!grepl("_(2011|2013|2015|2018)$", all_vars)]
  
  data_long <- data_final %>%
    pivot_longer(
      cols = matches("_(2011|2013|2015|2018)$"),
      names_to = c(".value", "year"),
      names_pattern = "(.*)_(2011|2013|2015|2018)$"
    ) %>%
    arrange(ID, year)
  
  
  saveRDS(data_long, "data_long_format.rds")
}


all_columns <- sort(names(data_final))

for(i in seq(1, length(all_columns), by = 5)) {
  end_idx <- min(i+4, length(all_columns))
}

library(mice)
library(dplyr)
library(purrr)
library(ggplot2)
library(nlme)




data_long <- data_long %>%
  mutate(
    year = as.numeric(year),
    time = case_when(
      year == 2011 ~ 0,
      year == 2013 ~ 2,
      year == 2015 ~ 4,
      year == 2018 ~ 7,
      TRUE ~ NA_real_
    ),
    ragender = factor(ragender, levels = c(1, 2), 
                      labels = c("Male", "Female"))
  )



missing_long <- data_long %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_rate") %>%
  arrange(desc(missing_rate))

print(head(missing_long, 15), n = 15)


vars_to_exclude <- c("householdID", "cognitive_decline")

data_for_imp <- data_long %>%
  select(-all_of(vars_to_exclude))

data_for_imp$ID_imp <- data_long$ID
data_for_imp$time_imp <- data_long$time



var_types <- sapply(data_for_imp, class)
print(table(var_types))


set.seed(123)

init_imp <- mice(data_for_imp, 
                 maxit = 0, 
                 printFlag = FALSE,
                 defaultMethod = c("pmm", "logreg", "polyreg", "polr"))

method_imp <- init_imp$method
pred_matrix <- init_imp$predictorMatrix


pred_matrix <- quickpred(data_for_imp, 
                         mincor = 0.1,
                         minpuc = 0.05,
                         include = c("ID_imp", "time_imp"))

method_imp["ID_imp"] <- ""
method_imp["time_imp"] <- ""
pred_matrix["ID_imp", ] <- 0
pred_matrix[, "ID_imp"] <- 1
pred_matrix["time_imp", ] <- 0
pred_matrix[, "time_imp"] <- 1


numeric_vars <- names(data_for_imp)[sapply(data_for_imp, is.numeric)]
numeric_vars <- numeric_vars[!numeric_vars %in% c("ID_imp", "time_imp", "year")]
for(var in numeric_vars) {
  method_imp[var] <- "pmm"
}

factor_vars <- names(data_for_imp)[sapply(data_for_imp, is.factor)]
binary_vars <- factor_vars[sapply(data_for_imp[factor_vars], function(x) nlevels(x) == 2)]
for(var in binary_vars) {
  if(var %in% names(method_imp)) {
    method_imp[var] <- "logreg"
  }
}

multi_level_vars <- factor_vars[sapply(data_for_imp[factor_vars], function(x) nlevels(x) > 2)]
for(var in multi_level_vars) {
  if(var %in% names(method_imp)) {
    method_imp[var] <- "polyreg"
  }
}


tryCatch({
  start_time <- Sys.time()
  
  imp_test <- mice(data_for_imp,
                   m = 2,
                   maxit = 3,
                   method = method_imp,
                   predictorMatrix = pred_matrix,
                   printFlag = TRUE,
                   seed = 123)
  
  end_time <- Sys.time()
  
  
  plot(imp_test)
  
  complete_test_1 <- complete(imp_test, 1)
  
  key_vars <- c("cognition_total", "cesd10", "age", "ragender", "sleep_night")
  
  for(var in key_vars) {
    if(var %in% names(data_for_imp)) {
      orig <- data_for_imp[[var]]
      impd <- complete_test_1[[var]]
      
      if(is.numeric(orig)) {
      } else if(is.factor(orig)) {
      }
    }
  }
  
  
  start_time_full <- Sys.time()
  
  imp_final <- mice(data_for_imp,
                    m = 5,
                    maxit = 10,
                    method = method_imp,
                    predictorMatrix = pred_matrix,
                    printFlag = TRUE,
                    seed = 123)
  
  end_time_full <- Sys.time()
  
  save(imp_final, file = "longitudinal_imputation_final.RData")
  
  
  complete_data_1 <- complete(imp_final, 1)
  
  complete_data_1 <- complete_data_1 %>%
    select(-ID_imp, -time_imp)
  
  final_imputed_long <- complete_data_1 %>%
    left_join(
      data_long %>% select(ID, year, cognitive_decline),
      by = c("ID", "year")
    ) %>%
    select(ID, year, everything())
  
  if(!"time" %in% names(final_imputed_long)) {
    final_imputed_long <- final_imputed_long %>%
      mutate(
        time = case_when(
          year == 2011 ~ 0,
          year == 2013 ~ 2,
          year == 2015 ~ 4,
          year == 2018 ~ 7
        )
      ) %>%
      select(ID, year, time, everything())
  }
  
  
  age_group_stats <- final_imputed_long %>%
    group_by(year, age_group) %>%
    summarise(
      mean_cognition = mean(cognition_total, na.rm = TRUE),
      sd_cognition = sd(cognition_total, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(cognition_threshold = mean_cognition - sd_cognition)
  
  final_imputed_long <- final_imputed_long %>%
    left_join(age_group_stats, by = c("year", "age_group"))
  
  final_imputed_long <- final_imputed_long %>%
    mutate(
      cognitive_decline_new = factor(
        case_when(
          cognition_total < cognition_threshold ~ "Yes",
          TRUE ~ "No"
        ),
        levels = c("No", "Yes")
      )
    )
  
  if("cognitive_decline" %in% names(final_imputed_long)) {
    comparison <- table(
      Original = final_imputed_long$cognitive_decline,
      New = final_imputed_long$cognitive_decline_new,
      useNA = "always"
    )
    
    print(comparison)
    
    final_imputed_long$cognitive_decline <- final_imputed_long$cognitive_decline_new
  }
  
  final_imputed_long <- final_imputed_long %>%
    select(-cognition_threshold, -mean_cognition, -sd_cognition, -cognitive_decline_new)
  
  
  saveRDS(final_imputed_long, "final_long_imputed.rds")
  write.csv(final_imputed_long, "final_long_imputed.csv", row.names = FALSE)
  
  
  
  final_wide_imputed <- final_imputed_long %>%
    pivot_wider(
      id_cols = ID,
      names_from = year,
      values_from = -c(ID, year, time),
      names_sep = "_"
    )
  
  
  saveRDS(final_wide_imputed, "final_wide_imputed.rds")
  write.csv(final_wide_imputed, "final_wide_imputed.csv", row.names = FALSE)
  
  
  
  missing_before <- data_long %>%
    select(-ID, -year, -cognitive_decline) %>%
    summarise_all(~mean(is.na(.))) %>%
    unlist() %>%
    mean()
  
  missing_after <- final_imputed_long %>%
    select(-ID, -year, -time, -cognitive_decline) %>%
    summarise_all(~mean(is.na(.))) %>%
    unlist() %>%
    mean()
  
  
  key_vars_check <- c("cognition_total", "cesd10", "age", "ragender", 
                      "raeduc_c", "sleep_night", "satlife")
  
  for(var in key_vars_check) {
    if(var %in% names(data_long) && var %in% names(final_imputed_long)) {
      before <- mean(is.na(data_long[[var]]))
      after <- mean(is.na(final_imputed_long[[var]]))
      
      if(before > 0) {
      }
    }
  }
  
  
  cont_vars <- c("cognition_total", "cesd10", "age", "sleep_night")
  for(var in cont_vars) {
    if(var %in% names(final_imputed_long)) {
      values <- final_imputed_long[[var]]
      stats <- data.frame(
        Variable = var,
        Min = min(values, na.rm = TRUE),
        Max = max(values, na.rm = TRUE),
        Mean = mean(values, na.rm = TRUE),
        SD = sd(values, na.rm = TRUE),
        Missing = sum(is.na(values))
      )
      print(stats)
    }
  }
  
  cat_vars <- c("cognitive_decline", "ragender", "raeduc_c", "hrural")
  for(var in cat_vars) {
    if(var %in% names(final_imputed_long)) {
      print(table(final_imputed_long[[var]], useNA = "always"))
    }
  }
  
  
}, error = function(e) {
  
  simple_imputed <- data_for_imp
  
  for(var in names(simple_imputed)) {
    if(is.numeric(simple_imputed[[var]])) {
      median_val <- median(simple_imputed[[var]], na.rm = TRUE)
      simple_imputed[[var]][is.na(simple_imputed[[var]])] <- median_val
    }
    else if(is.factor(simple_imputed[[var]])) {
      freq_table <- table(simple_imputed[[var]])
      if(length(freq_table) > 0) {
        mode_val <- names(freq_table)[which.max(freq_table)]
        simple_imputed[[var]][is.na(simple_imputed[[var]])] <- mode_val
      }
    }
  }
  
  
  simple_final <- bind_cols(
    data_long %>% select(ID, year, cognitive_decline),
    simple_imputed %>% select(-ID_imp, -time_imp)
  )
  
  saveRDS(simple_final, "simple_imputed_long.rds")
})

final_data <- readRDS("final_long_imputed.rds")



gender_consistency <- final_data %>%
  select(ID, year, ragender) %>%
  pivot_wider(
    id_cols = ID,
    names_from = year,
    values_from = ragender,
    names_prefix = "gender_"
  )

gender_consistency <- gender_consistency %>%
  mutate(
    has_gender = !(is.na(gender_2011) & is.na(gender_2013) & 
                     is.na(gender_2015) & is.na(gender_2018)),
    
    all_same = case_when(
      has_gender == FALSE ~ NA,
      TRUE ~ (
        (is.na(gender_2011) | is.na(gender_2013) | gender_2011 == gender_2013) &
          (is.na(gender_2013) | is.na(gender_2015) | gender_2013 == gender_2015) &
          (is.na(gender_2015) | is.na(gender_2018) | gender_2015 == gender_2018)
      )
    ),
    
    inconsistency_notes = case_when(
      has_gender == FALSE ~ "Missing all years",
      all_same == TRUE ~ "Consistent",
      TRUE ~ "Inconsistent"
    )
  )

consistency_summary <- table(gender_consistency$inconsistency_notes, useNA = "always")
print(consistency_summary)

inconsistent_ids <- gender_consistency %>%
  filter(inconsistency_notes == "Inconsistent") %>%
  pull(ID)


if(length(inconsistent_ids) > 0) {
  
  for(i in 1:min(10, length(inconsistent_ids))) {
    id <- inconsistent_ids[i]
    
    id_data <- final_data %>%
      filter(ID == id) %>%
      select(year, ragender) %>%
      arrange(year)
    
    print(id_data)
  }
}


gender_reference <- final_data %>%
  select(ID, year, ragender) %>%
  pivot_wider(
    id_cols = ID,
    names_from = year,
    values_from = ragender
  ) %>%
  mutate(
    final_gender = case_when(
      !is.na(`2018`) ~ `2018`,
      !is.na(`2015`) ~ `2015`,
      !is.na(`2013`) ~ `2013`,
      !is.na(`2011`) ~ `2011`,
      TRUE ~ NA
    ),
    
    gender_source = case_when(
      !is.na(`2018`) ~ "2018",
      !is.na(`2015`) ~ "2015",
      !is.na(`2013`) ~ "2013",
      !is.na(`2011`) ~ "2011",
      TRUE ~ "Missing"
    )
  ) %>%
  select(ID, final_gender, gender_source)

source_summary <- table(gender_reference$gender_source, useNA = "always")
print(source_summary)


final_data <- final_data %>%
  mutate(ragender_original = ragender)

final_data <- final_data %>%
  left_join(gender_reference %>% select(ID, final_gender), by = "ID") %>%
  mutate(
    ragender = final_gender,
    ragender = factor(ragender, levels = c("Male", "Female"))
  ) %>%
  select(-final_gender)


gender_consistency_after <- final_data %>%
  select(ID, year, ragender) %>%
  pivot_wider(
    id_cols = ID,
    names_from = year,
    values_from = ragender,
    names_prefix = "gender_"
  ) %>%
  mutate(
    has_gender = !(is.na(gender_2011) & is.na(gender_2013) & 
                     is.na(gender_2015) & is.na(gender_2018)),
    all_same = case_when(
      has_gender == FALSE ~ NA,
      TRUE ~ (
        (is.na(gender_2011) | is.na(gender_2013) | gender_2011 == gender_2013) &
          (is.na(gender_2013) | is.na(gender_2015) | gender_2013 == gender_2015) &
          (is.na(gender_2015) | is.na(gender_2018) | gender_2015 == gender_2018)
      )
    ),
    inconsistency_notes = case_when(
      has_gender == FALSE ~ "Missing all years",
      all_same == TRUE ~ "Consistent",
      TRUE ~ "Inconsistent"
    )
  )

final_data <- final_data %>% 
  mutate(raeduc_c = str_squish(as.character(raeduc_c)) %>%
           factor())

saveRDS(final_data, "final_long_imputed_gender_corrected.rds")
write.csv(final_data, "final_long_imputed_gender_corrected.csv", row.names = FALSE)

final_wide_corrected <- final_data %>%
  pivot_wider(
    id_cols = ID,
    names_from = year,
    values_from = -c(ID, year, time),
    names_sep = "_"
  )

saveRDS(final_wide_corrected, "final_wide_imputed_gender_corrected.rds")
write.csv(final_wide_corrected, "final_wide_imputed_gender_corrected.csv", row.names = FALSE)


final_data <- final_data %>% select(-ragender_original)
final_data <- final_data %>% select(-cognition_total)
final_data <- final_data %>% select(-age_group)
final_data <- final_data %>% select(-time)

library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(caret)
library(Boruta)
library(randomForest)
library(xgboost)
library(pROC)
library(ROSE)
library(smotefamily)
library(glmnet)
library(kernlab)
library(nnet)
library(ggplot2)
library(patchwork)
library(tableone)
library(gtsummary)
library(flextable)
library(kableExtra)

final_real <- final_data %>%
  arrange(ID, year) %>%
  mutate(
    pain_binary = factor(
      ifelse(as.numeric(as.character(pain_level)) > 1, 1, 0),
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    
    cesd_binary = factor(
      ifelse(cesd10 < 10, 0, 1),
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    
    across(where(is.character), as.factor)
  )


pain_data_wide <- final_real %>%
  filter(year %in% c(2011, 2013, 2015)) %>%
  select(ID, year, pain_binary) %>%
  pivot_wider(
    id_cols = ID,
    names_from = year,
    values_from = pain_binary,
    names_sep = "_"
  ) %>%
  rename(
    pain_2011 = `2011`,
    pain_2013 = `2013`,
    pain_2015 = `2015`
  )

chronic_pain_vars <- pain_data_wide %>%
  mutate(
    pain_2011_bin = ifelse(pain_2011 == "Yes", 1, 0),
    pain_2013_bin = ifelse(pain_2013 == "Yes", 1, 0),
    pain_2015_bin = ifelse(pain_2015 == "Yes", 1, 0),
    
    chronic_pain_2yr = ifelse(
      (pain_2011_bin == 1 & pain_2013_bin == 1) |
        (pain_2013_bin == 1 & pain_2015_bin == 1), 1, 0
    )
  ) %>%
  select(ID, chronic_pain_2yr)

cognitive_vars <- final_real %>%
  filter(year %in% c(2011, 2013, 2015)) %>%
  group_by(ID) %>%
  summarise(
    cognitive_decline_ever = ifelse(
      any(cognitive_decline == "Yes", na.rm = TRUE), 1, 0
    ),
    .groups = 'drop'
  )

comorbid_period_vars <- chronic_pain_vars %>%
  left_join(cognitive_vars, by = "ID") %>%
  mutate(
    comorbid_period = ifelse(
      chronic_pain_2yr == 1 & cognitive_decline_ever == 1, 1, 0
    )
  ) %>%
  select(ID, comorbid_period)

comorbid_2018 <- final_real %>%
  filter(year == 2018) %>%
  mutate(
    comorbid_2018 = ifelse(
      cognitive_decline == "Yes" & pain_binary == "Yes", 1, 0
    )
  ) %>%
  select(ID, comorbid_2018)

selected_chronic <- c("arthre", "diabe", "hibpe", "dyslipe")

behavioral_vars <- c("hobby", "retire", "socwk", "work", "satlife", 
                     "drinkl", "smoken")

baseline_2011 <- final_real %>%
  filter(year == 2011) %>%
  select(
    ID,
    age_2011 = age,
    gender = ragender,
    education_raw = raeduc_c,
    marital = marry_cat,
    rural = hrural,
    
    cesd_binary_2011 = cesd_binary,
    sleep_2011 = sleep_night,
    adl_2011 = adl_level,
    
    all_of(selected_chronic),
    
    all_of(behavioral_vars)
  ) %>%
  mutate(
    education_ord = factor(education_raw,
                           levels = c("1", "2", "3", "4"),
                           labels = c("Illiterate", "Primary", "Junior high", "Technical secondary or high school and above"),
                           ordered = TRUE),
    
    n_chronic_2011 = rowSums(
      select(., arthre, diabe, hibpe, dyslipe) == "1",
      na.rm = TRUE
    ),
    
    has_arthritis = ifelse(arthre == "1", 1, 0),
    has_diabetes = ifelse(diabe == "1", 1, 0),
    has_hypertension = ifelse(hibpe == "1", 1, 0),
    has_dyslipidemia = ifelse(dyslipe == "1", 1, 0),
    
    social_engagement = ifelse(hobby == "1" | socwk == "1", 1, 0),
    
    high_satlife = ifelse(as.numeric(as.character(satlife)) >= 4, 1, 0),
    
    retired = ifelse(retire == "1", 1, 0),
    working = ifelse(work == "1", 1, 0)
  )

features_2015 <- final_real %>%
  filter(year == 2015) %>%
  select(
    ID,
    age_2015 = age,
    cesd_binary_2015 = cesd_binary,
    sleep_2015 = sleep_night,
    diabe_2015 = diabe,
    hibpe_2015 = hibpe,
    arthre_2015 = arthre,
    hobby_2015 = hobby,
    socwk_2015 = socwk
  )

longitudinal_features <- final_real %>%
  filter(year %in% c(2011, 2013, 2015)) %>%
  select(ID, year, cesd10, sleep_night) %>%
  arrange(ID, year) %>%
  group_by(ID) %>%
  summarise(
    cesd_slope = if(sum(!is.na(cesd10)) >= 2) {
      model <- lm(cesd10 ~ year, na.action = na.exclude)
      if(length(coef(model)) >= 2) coef(model)[2] else NA
    } else NA,
    
    sleep_slope = if(sum(!is.na(sleep_night)) >= 2) {
      model <- lm(sleep_night ~ year, na.action = na.exclude)
      if(length(coef(model)) >= 2) coef(model)[2] else NA
    } else NA,
    
    cesd_change = if(!is.na(first(cesd10)) & !is.na(last(cesd10))) {
      last(cesd10) - first(cesd10)
    } else NA,
    
    sleep_change = if(!is.na(first(sleep_night)) & !is.na(last(sleep_night))) {
      last(sleep_night) - first(sleep_night)
    } else NA,
    .groups = 'drop'
  )

track_a_data <- baseline_2011 %>%
  left_join(features_2015, by = "ID") %>%
  left_join(longitudinal_features, by = "ID") %>%
  left_join(comorbid_period_vars, by = "ID") %>%
  mutate(
    cesd_change_11_15 = ifelse(!is.na(cesd_binary_2011) & !is.na(cesd_binary_2015),
                               as.numeric(cesd_binary_2015) - as.numeric(cesd_binary_2011), NA),
    
    sleep_change_11_15 = sleep_2015 - sleep_2011,
    
    age_change_11_15 = age_2015 - age_2011,
    
    new_diabetes_11_15 = ifelse(
      !is.na(diabe) & !is.na(diabe_2015) & diabe == "0" & diabe_2015 == "1", 1, 0
    ),
    new_hypertension_11_15 = ifelse(
      !is.na(hibpe) & !is.na(hibpe_2015) & hibpe == "0" & hibpe_2015 == "1", 1, 0
    ),
    new_arthritis_11_15 = ifelse(
      !is.na(arthre) & !is.na(arthre_2015) & arthre == "0" & arthre_2015 == "1", 1, 0
    ),
    
    hobby_lost_11_15 = ifelse(
      !is.na(hobby) & !is.na(hobby_2015) & hobby == "1" & hobby_2015 == "0", 1, 0
    ),
    social_decline_11_15 = ifelse(
      !is.na(socwk) & !is.na(socwk_2015) & socwk == "1" & socwk_2015 == "0", 1, 0
    ),
    
    outcome = factor(comorbid_period,
                     levels = c(0, 1),
                     labels = c("No", "Yes"))
  ) %>%
  select(-education_raw)

print(table(track_a_data$outcome, useNA = "ifany"))

track_b_data <- baseline_2011 %>%
  left_join(comorbid_2018, by = "ID") %>%
  mutate(
    age = age_2011,
    gender = factor(gender),
    education = education_ord,
    depression = cesd_binary_2011,
    sleep = sleep_2011,
    has_arthritis = factor(has_arthritis, levels = c(0, 1), labels = c("No", "Yes")),
    has_diabetes = factor(has_diabetes, levels = c(0, 1), labels = c("No", "Yes")),
    has_hypertension = factor(has_hypertension, levels = c(0, 1), labels = c("No", "Yes")),
    has_dyslipidemia = factor(has_dyslipidemia, levels = c(0, 1), labels = c("No", "Yes")),
    chronic_count = n_chronic_2011,
    social_engagement = factor(social_engagement, levels = c(0, 1), labels = c("No", "Yes")),
    high_satlife = factor(high_satlife, levels = c(0, 1), labels = c("No", "Yes")),
    smoker = factor(smoken, levels = c("0", "1"), labels = c("No", "Yes")),
    drinker = factor(drinkl, levels = c("0", "1"), labels = c("No", "Yes")),
    outcome_2018 = factor(comorbid_2018,
                          levels = c(0, 1),
                          labels = c("No", "Yes"))
  ) %>%
  select(
    ID,
    outcome = outcome_2018,
    age, gender, education, depression, sleep,
    has_arthritis, has_diabetes, has_hypertension, has_dyslipidemia,
    chronic_count, social_engagement, high_satlife, smoker, drinker
  ) %>%
  na.omit()

tableone_data <- baseline_2011 %>%
  left_join(comorbid_period_vars, by = "ID") %>%
  mutate(
    comorbid_group = factor(comorbid_period,
                            levels = c(0, 1),
                            labels = c("Non-comorbid", "Comorbid")),
    
    age = age_2011,
    gender = factor(gender),
    education = education_ord,
    depression = cesd_binary_2011,
    sleep = cut(sleep_2011,
                breaks = c(0, 5, 7, 9, 24),
                labels = c("<5h", "5-7h", "7-9h", ">9h")),
    adl = factor(adl_2011),
    has_arthritis = factor(has_arthritis, levels = c(0, 1), labels = c("No", "Yes")),
    has_diabetes = factor(has_diabetes, levels = c(0, 1), labels = c("No", "Yes")),
    has_hypertension = factor(has_hypertension, levels = c(0, 1), labels = c("No", "Yes")),
    has_dyslipidemia = factor(has_dyslipidemia, levels = c(0, 1), labels = c("No", "Yes")),
    chronic_count_group = cut(n_chronic_2011,
                              breaks = c(-1, 0, 1, 2, 4),
                              labels = c("0", "1", "2", "3-4")),
    social_engagement = factor(social_engagement, levels = c(0, 1), labels = c("No", "Yes")),
    high_satlife = factor(high_satlife, levels = c(0, 1), labels = c("No", "Yes")),
    smoker = factor(smoken, levels = c("0", "1"), labels = c("No", "Yes")),
    drinker = factor(drinkl, levels = c("0", "1"), labels = c("No", "Yes")),
    retired = factor(retired, levels = c(0, 1), labels = c("No", "Yes")),
    working = factor(working, levels = c(0, 1), labels = c("No", "Yes")),
    marital_status = factor(marital, levels = c("Married", "Not Married")),
    rural_area = factor(rural, levels = c("1", "0"), labels = c("Rural", "Urban"))
  ) %>%
  select(
    comorbid_group,
    age, gender, education, marital_status, rural_area,
    depression, sleep, adl,
    has_arthritis, has_diabetes, has_hypertension, has_dyslipidemia,
    chronic_count_group, social_engagement, high_satlife,
    smoker, drinker, retired, working
  ) %>%
  na.omit()

if (!dir.exists("ree")) dir.create("ree")
saveRDS(track_a_data, "ree/track_a_data.rds")
saveRDS(track_b_data, "ree/track_b_data.rds")
saveRDS(tableone_data, "ree/tableone_data.rds")
write.csv(track_a_data, "ree/track_a_data.csv", row.names = FALSE)
write.csv(track_b_data, "ree/track_b_data.csv", row.names = FALSE)
write.csv(tableone_data, "ree/tableone_data.csv", row.names = FALSE)

model_data_a_optimized <- track_a_data %>%
  select(
    ID,
    outcome,
    age_2011, gender, education_ord, marital, rural,
    cesd_binary_2011, sleep_2011, adl_2011,
    n_chronic_2011,
    hobby, socwk, work, satlife, drinkl, smoken, retire,
    social_engagement, high_satlife,
    cesd_slope, sleep_slope,
    cesd_change, sleep_change,
    new_diabetes_11_15, new_hypertension_11_15, new_arthritis_11_15,
    hobby_lost_11_15, social_decline_11_15
  ) %>%
  mutate(
    education_ord = factor(education_ord, 
                           levels = c("Illiterate", "Primary", "Junior high", "Technical secondary or high school and above"),
                           ordered = TRUE),
    adl_2011 = factor(adl_2011, levels = c("1", "2", "3"), ordered = TRUE),
    satlife_binary = factor(
      ifelse(as.numeric(as.character(satlife)) >= 4, "High", "Low"),
      levels = c("Low", "High")
    ),
    across(c(gender, marital, rural, cesd_binary_2011,
             hobby, socwk, work, drinkl, smoken, retire,
             social_engagement, high_satlife),
           as.factor),
    across(c(new_diabetes_11_15, new_hypertension_11_15, new_arthritis_11_15,
             hobby_lost_11_15, social_decline_11_15),
           ~factor(., levels = c(0, 1), labels = c("No", "Yes")))
  ) %>%
  select(-satlife) %>%
  na.omit()

set.seed(123)

id_level_data <- model_data_a_optimized %>%
  group_by(ID) %>%
  summarise(
    outcome = last(outcome),
    .groups = 'drop')

train_ids <- id_level_data %>%
  group_by(outcome) %>%
  sample_frac(0.7) %>%
  pull(ID)

test_ids <- setdiff(id_level_data$ID, train_ids)

train_data <- model_data_a_optimized[model_data_a_optimized$ID %in% train_ids, ]
test_data <- model_data_a_optimized[model_data_a_optimized$ID %in% test_ids, ]

train_data <- train_data %>% select(-ID)
test_data <- test_data %>% select(-ID)

dmy_optimized <- dummyVars(" ~ .", 
                           data = train_data[, -which(names(train_data) == "outcome")],
                           fullRank = TRUE)

train_features_opt <- predict(dmy_optimized, newdata = train_data[, -which(names(train_data) == "outcome")])
train_features_df_opt <- as.data.frame(lapply(as.data.frame(train_features_opt), as.numeric))
train_outcome_opt <- train_data$outcome

test_features_opt <- predict(dmy_optimized, newdata = test_data[, -which(names(test_data) == "outcome")])
test_features_df_opt <- as.data.frame(lapply(as.data.frame(test_features_opt), as.numeric))
test_outcome_opt <- test_data$outcome

train_data_opt <- data.frame(outcome = train_outcome_opt, train_features_df_opt)
test_data_opt <- data.frame(outcome = test_outcome_opt, test_features_df_opt)

colnames(train_data_opt) <- make.names(colnames(train_data_opt))
colnames(test_data_opt) <- make.names(colnames(test_data_opt))

if(sum(train_data_opt$outcome == "Yes") >= 10) {
  X_train_opt <- train_data_opt[, -which(names(train_data_opt) == "outcome")]
  y_train_opt <- ifelse(train_data_opt$outcome == "Yes", 1, 0)
  
  tryCatch({
    smote_result_opt <- SMOTE(X = X_train_opt, target = y_train_opt, K = 5, dup_size = 0)
    smote_data_opt <- smote_result_opt$data
    colnames(smote_data_opt)[ncol(smote_data_opt)] <- "outcome"
    smote_data_opt$outcome <- factor(smote_data_opt$outcome, levels = c(0, 1), labels = c("No", "Yes"))
    train_data_balanced_opt <- smote_data_opt
  }, error = function(e) {
    train_data_balanced_opt <- ovun.sample(outcome ~ ., data = train_data_opt, 
                                           method = "over", N = 2 * sum(train_data_opt$outcome == "No"))$data
  })
} else {
  train_data_balanced_opt <- ovun.sample(outcome ~ ., data = train_data_opt, 
                                         method = "over", N = 2 * sum(train_data_opt$outcome == "No"))$data
}

set.seed(123)
boruta_opt <- Boruta(outcome ~ ., data = train_data_balanced_opt, 
                     doTrace = 2, maxRuns = 150, ntree = 300)

confirmed_features_opt <- getSelectedAttributes(boruta_opt, withTentative = FALSE)

if(length(confirmed_features_opt) == 0) {
  importance_df_opt <- attStats(boruta_opt)
  importance_sorted_opt <- importance_df_opt[order(-importance_df_opt$meanImp), ]
  confirmed_features_opt <- rownames(head(importance_sorted_opt, 20))
}

exclude_patterns <- c("pain", "chronic_pain", "cognitive_decline")
confirmed_features_opt <- confirmed_features_opt[!grepl(paste(exclude_patterns, collapse = "|"), confirmed_features_opt)]
print(confirmed_features_opt)
importance_stats <- attStats(boruta_opt)
importance_stats <- importance_stats[order(-importance_stats$meanImp), ]
head(importance_stats)
write.csv(importance_stats, "ree/boruta_feature_importance.csv", row.names = TRUE)

train_final_opt <- train_data_balanced_opt[, c("outcome", confirmed_features_opt)]
test_final_opt <- test_data_opt[, c("outcome", confirmed_features_opt)]

missing_in_test_opt <- setdiff(confirmed_features_opt, colnames(test_final_opt))
if(length(missing_in_test_opt) > 0) {
  for(feature in missing_in_test_opt) {
    test_final_opt[[feature]] <- 0
  }
  test_final_opt <- test_final_opt[, c("outcome", confirmed_features_opt)]
}

train_final_opt$outcome <- factor(
  ifelse(as.character(train_final_opt$outcome) %in% c("0", "No"), "No", "Yes"),
  levels = c("No", "Yes")
)

test_final_opt$outcome <- factor(
  ifelse(as.character(test_final_opt$outcome) %in% c("0", "No"), "No", "Yes"),
  levels = c("No", "Yes")
)

vif_data <- train_final_opt

vif_data_num <- vif_data[, -which(names(vif_data) == "outcome")]

nzv <- nearZeroVar(vif_data_num, saveMetrics = TRUE)
if(any(nzv$zeroVar)) {
  zero_var_vars <- rownames(nzv)[nzv$zeroVar]
  vif_data_num <- vif_data_num[, !nzv$zeroVar]
}

set.seed(123)
vif_lm <- lm(rnorm(nrow(vif_data_num)) ~ ., data = vif_data_num)

alias_info <- alias(vif_lm)
aliased <- rownames(alias_info$Complete)
if(length(aliased) > 0) {
  vif_data_num <- vif_data_num[, !names(vif_data_num) %in% aliased]
  vif_lm <- lm(rnorm(nrow(vif_data_num)) ~ ., data = vif_data_num)
}

vif_values <- car::vif(vif_lm)
vif_df <- data.frame(Feature = names(vif_values), VIF = as.numeric(vif_values)) %>%
  arrange(desc(VIF))

print(vif_df)

if(!dir.exists("ree/results")) dir.create("ree/results", recursive = TRUE)
write.csv(vif_df, "ree/results/vif_after_feature_selection.csv", row.names = FALSE)

p_vif <- ggplot(vif_df, aes(x = reorder(Feature, VIF), y = VIF)) +
  geom_col(aes(fill = VIF > 5), width = 0.7) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "orange") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("TRUE" = "#E41A1C", "FALSE" = "#4DAF4A"),
                    labels = c("VIF<=5", "VIF>5"), name = "Collinearity") +
  coord_flip() +
  labs(title = "VIF Multicollinearity Check After Feature Selection",
       x = "Feature", y = "VIF Value") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("ree/results/vif_after_feature_selection.png", p_vif, width = 10, height = 8, dpi = 300)
print(p_vif)

vif_remove <- c("social_engagement.1")
train_final_opt <- train_final_opt[, !names(train_final_opt) %in% vif_remove]
test_final_opt <- test_final_opt[, !names(test_final_opt) %in% vif_remove]


ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  selectionFunction = "best",
  allowParallel = TRUE
)

models <- list()
model_predictions <- list()
model_probs <- list()
performance_metrics <- list()

set.seed(123)
tryCatch({
  glmnet_grid <- expand.grid(
    alpha = seq(0, 1, 0.25),
    lambda = 10^seq(-3, 1, length = 20)
  )
  
  models$glmnet <- train(
    outcome ~ .,
    data = train_final_opt,
    method = "glmnet",
    family = "binomial",
    tuneGrid = glmnet_grid,
    metric = "ROC",
    trControl = ctrl,
    preProc = c("center", "scale"),
    importance = TRUE
  )
}, error = function(e) {
})

set.seed(123)
tryCatch({
  n_features <- ncol(train_final_opt) - 1
  rf_grid <- expand.grid(
    mtry = unique(round(seq(2, min(30, n_features), length = 8)))
  )
  
  models$rf <- train(
    outcome ~ .,
    data = train_final_opt,
    method = "rf",
    tuneGrid = rf_grid,
    metric = "ROC",
    trControl = ctrl,
    ntree = 500,
    importance = TRUE,
    verbose = FALSE
  )
}, error = function(e) {
})

set.seed(123)
tryCatch({
  xgb_grid <- expand.grid(
    nrounds = c(50, 100, 150),
    max_depth = c(3, 5, 7),
    eta = c(0.01, 0.05, 0.1),
    gamma = c(0, 0.1, 0.2),
    colsample_bytree = c(0.6, 0.8, 1.0),
    min_child_weight = c(1, 3, 5),
    subsample = c(0.6, 0.8, 1.0)
  )
  
  models$xgb <- train(
    outcome ~ .,
    data = train_final_opt,
    method = "xgbTree",
    tuneGrid = xgb_grid,
    metric = "ROC",
    trControl = ctrl,
    verbosity = 0,
    nthread = 4
  )
  print(models$xgb$bestTune)
}, error = function(e) {
})

set.seed(123)
tryCatch({
  svm_grid <- expand.grid(
    sigma = c(0.001, 0.01, 0.1, 1),
    C = c(0.1, 1, 10, 100)
  )
  
  models$svm <- train(
    outcome ~ .,
    data = train_final_opt,
    method = "svmRadial",
    tuneGrid = svm_grid,
    metric = "ROC",
    trControl = ctrl,
    preProc = c("center", "scale"),
    probability = TRUE
  )
}, error = function(e) {
})

set.seed(123)
tryCatch({
  nnet_grid <- expand.grid(
    size = c(5, 10, 15, 20),
    decay = c(0.0001, 0.001, 0.01, 0.1)
  )
  
  models$nnet <- train(
    outcome ~ .,
    data = train_final_opt,
    method = "nnet",
    tuneGrid = nnet_grid,
    metric = "ROC",
    trControl = ctrl,
    preProc = c("center", "scale"),
    trace = FALSE,
    MaxNWts = 5000,
    maxit = 500,
    linout = FALSE
  )
}, error = function(e) {
})

set.seed(123)
tryCatch({
  nb_grid <- expand.grid(
    usekernel = c(TRUE, FALSE),
    fL = c(0, 0.5, 1),
    adjust = c(0.5, 1, 1.5)
  )
  
  models$nb <- train(
    outcome ~ .,
    data = train_final_opt,
    method = "nb",
    tuneGrid = nb_grid,
    metric = "ROC",
    trControl = ctrl,
    preProc = c("center", "scale")
  )
  print(models$nb$bestTune)
}, error = function(e) {
})

models <- models[!sapply(models, is.null)]


all_results <- data.frame()
all_roc_curves <- list()
all_predictions <- list()

for(model_name in names(models)) {
  
  tryCatch({
    model <- models[[model_name]]
    
    pred_prob <- predict(model, test_final_opt, type = "prob")[, "Yes"]
    pred_class <- predict(model, test_final_opt)
    
    all_predictions[[model_name]] <- list(
      probs = pred_prob,
      classes = pred_class
    )
    
    roc_obj <- roc(test_final_opt$outcome, pred_prob, levels = c("No", "Yes"), direction = "<")
    all_roc_curves[[model_name]] <- roc_obj
    auc_val <- auc(roc_obj)
    
    cm <- confusionMatrix(pred_class, test_final_opt$outcome, positive = "Yes")
    
    accuracy <- cm$overall["Accuracy"]
    sensitivity <- cm$byClass["Sensitivity"]
    specificity <- cm$byClass["Specificity"]
    ppv <- cm$byClass["Pos Pred Value"]
    npv <- cm$byClass["Neg Pred Value"]
    
    f1 <- 2 * (ppv * sensitivity) / (ppv + sensitivity)
    
    pr_auc <- NA
    if(requireNamespace("PRROC", quietly = TRUE)) {
      pr_obj <- PRROC::pr.curve(
        scores.class0 = pred_prob[test_final_opt$outcome == "Yes"],
        scores.class1 = pred_prob[test_final_opt$outcome == "No"],
        curve = TRUE
      )
      pr_auc <- pr_obj$auc.integral
    }
    
    model_results <- data.frame(
      Model = model_name,
      AUC = round(auc_val, 3),
      PR_AUC = ifelse(is.na(pr_auc), NA, round(pr_auc, 3)),
      Accuracy = round(accuracy, 3),
      Sensitivity = round(sensitivity, 3),
      Specificity = round(specificity, 3),
      PPV = round(ppv, 3),
      NPV = round(npv, 3),
      F1 = round(f1, 3),
      CV_AUC = round(max(model$results$ROC, na.rm = TRUE), 3),
      N_Features = length(confirmed_features_opt)
    )
    
    all_results <- rbind(all_results, model_results)
    
    
  }, error = function(e) {
  })
}

all_results <- all_results[order(-all_results$AUC), ]
rownames(all_results) <- NULL

print(all_results)

delong_results <- data.frame()
model_names <- all_results$Model

if(length(model_names) >= 2) {
  for(i in 1:(length(model_names)-1)) {
    for(j in (i+1):length(model_names)) {
      model_i <- model_names[i]
      model_j <- model_names[j]
      
      
      probs_i <- all_predictions[[model_i]]$probs
      probs_j <- all_predictions[[model_j]]$probs
      
      tryCatch({
        roc_i <- roc(test_final_opt$outcome, probs_i, levels = c("No", "Yes"), direction = "<", quiet = TRUE)
        roc_j <- roc(test_final_opt$outcome, probs_j, levels = c("No", "Yes"), direction = "<", quiet = TRUE)
        
        test_res <- roc.test(roc_i, roc_j, method = "delong")
        
        p_display <- ifelse(test_res$p.value < 0.0001, "< 0.0001", 
                            format(round(test_res$p.value, 4), nsmall = 4))
        
        delong_results <- rbind(delong_results, data.frame(
          Model1 = model_i,
          Model2 = model_j,
          AUC1 = round(as.numeric(auc(roc_i)), 3),
          AUC2 = round(as.numeric(auc(roc_j)), 3),
          p_value = test_res$p.value,
          p_display = p_display,
          Significant = ifelse(test_res$p.value < 0.05, "Yes", "No"),
          stringsAsFactors = FALSE
        ))
        
        
      }, error = function(e) {
      })
    }
  }
  
  if(nrow(delong_results) > 0) {
    write.csv(delong_results, "ree/results/delong_test_models.csv", row.names = FALSE)
    print(delong_results[, c("Model1", "Model2", "AUC1", "AUC2", "p_display", "Significant")])
    
    equiv_pairs <- delong_results[delong_results$p_value > 0.05, ]
    
    if(nrow(equiv_pairs) > 0) {
      for(k in 1:nrow(equiv_pairs)) {
      }
    } else {
    }
  }
} else {
}


dca_models <- unique(c(all_results$Model[1], equiv_pairs$Model1, equiv_pairs$Model2))
dca_models <- intersect(dca_models, names(all_predictions))


if(length(dca_models) >= 1) {
  dca_data <- data.frame(
    outcome = ifelse(test_final_opt$outcome == "Yes", 1, 0)
  )
  
  for(model_name in dca_models) {
    dca_data[[model_name]] <- all_predictions[[model_name]]$probs
  }
  
  dca_formula <- as.formula(paste("outcome ~", paste(dca_models, collapse = " + ")))
  
  dca_result <- tryCatch({
    decision_curve(
      dca_formula, 
      data = dca_data,
      thresholds = seq(0, 0.5, by = 0.01),
      bootstraps = 100
    )
  }, error = function(e) {
    NULL
  })
  
  if(!is.null(dca_result)) {
    dca_df <- as.data.frame(dca_result$derived.data)
    
    if("thresholds" %in% names(dca_df)) dca_df$threshold <- dca_df$thresholds
    if("NB" %in% names(dca_df) && !"net_benefit" %in% names(dca_df)) {
      dca_df$net_benefit <- dca_df$NB
    }
    
    if(all(c("threshold", "net_benefit", "model") %in% names(dca_df))) {
      model_colors_dca <- c(
        "glmnet" = "#E41A1C",
        "rf" = "#377EB8", 
        "xgb" = "#4DAF4A",
        "svm" = "#984EA3",
        "nnet" = "#FF7F00",
        "nb" = "#A65628",
        "Treat All" = "darkgray",
        "Treat None" = "gray50"
      )
      
      p_dca <- ggplot(dca_df, aes(x = threshold, y = net_benefit, color = model)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = model_colors_dca[intersect(names(model_colors_dca), unique(dca_df$model))]) +
        labs(
          title = "Decision Curve Analysis (DCA) - Statistically Equivalent Model Group",
          subtitle = paste("Models included:", paste(dca_models, collapse = ", ")),
          x = "Threshold Probability", 
          y = "Net Benefit",
          color = "Model"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = c(0.8, 0.3),
          legend.background = element_rect(fill = "white", color = "gray80"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          panel.grid.minor = element_blank()
        ) +
        coord_cartesian(xlim = c(0, 0.5), ylim = c(-0.1, max(dca_df$net_benefit, na.rm = TRUE) * 1.1))
      
      ggsave("ree/results/dca_equivalent_models.png", p_dca, width = 10, height = 8, dpi = 300)
      print(p_dca)
      
      saveRDS(dca_result, "ree/results/dca_equivalent_models.rds")
    }
  }
} else {
}


if(length(all_roc_curves) > 0) {
  model_colors <- c(
    "glmnet" = "#E41A1C",
    "rf" = "#377EB8",
    "xgb" = "#4DAF4A",
    "svm" = "#984EA3",
    "nnet" = "#FF7F00",
    "nb" = "#A65628"
  )
  
  valid_models <- intersect(names(model_colors), names(all_roc_curves))
  
  roc_data <- data.frame()
  for(model_name in valid_models) {
    roc_df <- data.frame(
      Sensitivity = all_roc_curves[[model_name]]$sensitivities,
      Specificity = all_roc_curves[[model_name]]$specificities,
      Model = model_name,
      AUC = paste0(model_name, " (AUC = ", round(auc(all_roc_curves[[model_name]]), 3), ")")
    )
    roc_data <- rbind(roc_data, roc_df)
  }
  
  p_roc <- ggplot(roc_data, aes(x = 1 - Specificity, y = Sensitivity, color = Model)) +
    geom_line(linewidth = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    scale_color_manual(values = model_colors[valid_models]) +
    labs(
      title = "ROC Curve Comparison - Prediction of Comorbidity with Cognitive Decline and Pain",
      x = "1 - Specificity (False Positive Rate)",
      y = "Sensitivity (True Positive Rate)",
      color = "Model"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = c(0.8, 0.2),
      legend.background = element_rect(fill = "white", color = "gray80"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.2)
    ) +
    coord_equal()
  
  ggsave("ree/results/roc_curves_comparison.png", p_roc, width = 10, height = 8, dpi = 300)
  print(p_roc)
}


metrics_long <- all_results %>%
  select(Model, AUC, Accuracy, Sensitivity, Specificity, F1) %>%
  pivot_longer(
    cols = -Model,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = factor(Metric, 
                    levels = c("AUC", "Accuracy", "Sensitivity", "Specificity", "F1"),
                    labels = c("AUC", "Accuracy", "Sensitivity", "Specificity", "F1 Score"))
  )

metric_colors <- c(
  "AUC" = "#1f77b4",
  "Accuracy" = "#ff7f0e",
  "Sensitivity" = "#2ca02c",
  "Specificity" = "#d62728",
  "F1 Score" = "#9467bd"
)

p_metrics <- ggplot(metrics_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = metric_colors) +
  labs(
    title = "Model Performance Metrics Comparison",
    x = "Model",
    y = "Score",
    fill = "Metric"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", alpha = 0.5, linewidth = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50", alpha = 0.5, linewidth = 0.5) +
  annotate("text", x = 0.5, y = 0.82, label = "Good threshold", color = "red", hjust = 0, size = 3)

ggsave("ree/results/model_metrics_comparison.png", p_metrics, width = 12, height = 8, dpi = 300)
print(p_metrics)


best_overall <- all_results[all_results$Model == "xgb", ]


if(exists("equiv_pairs") && nrow(equiv_pairs) > 0) {
  for(k in 1:nrow(equiv_pairs)) {
  }
}

best_model <- models$xgb
saveRDS(best_model, file = "ree/results/best_model_xgb_final.rds")

write.csv(all_results, "ree/results/model_performance_comparison.csv", row.names = FALSE)


set.seed(789)
n_total <- nrow(train_final_opt)
frac_seq <- c(0.2, 0.4, 0.6, 0.8, 1.0)
n_reps <- 3

all_learning_curves <- list()

train_model_simple <- function(model_name, train_data, frac) {
  ctrl_simple <- trainControl(
    method = "cv",
    number = 3,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    verboseIter = FALSE
  )
  
  tryCatch({
    if(model_name == "glmnet") {
      glmnet_grid <- expand.grid(alpha = c(0, 0.5, 1), lambda = 10^seq(-3, 0, length = 5))
      train(outcome ~ ., data = train_data, method = "glmnet", family = "binomial",
            tuneGrid = glmnet_grid, metric = "ROC", trControl = ctrl_simple, 
            preProc = c("center", "scale"), verbose = FALSE)
    } else if(model_name == "rf") {
      n_features <- ncol(train_data) - 1
      rf_grid <- expand.grid(mtry = unique(round(seq(2, min(20, n_features), length = 4))))
      train(outcome ~ ., data = train_data, method = "rf", tuneGrid = rf_grid,
            metric = "ROC", trControl = ctrl_simple, ntree = 200, importance = FALSE, verbose = FALSE)
    } else if(model_name == "xgb") {
      xgb_grid <- expand.grid(nrounds = c(50, 100), max_depth = c(3, 5), eta = c(0.05, 0.1),
                              gamma = 0, colsample_bytree = 0.8, min_child_weight = 3, subsample = 0.8)
      train(outcome ~ ., data = train_data, method = "xgbTree", tuneGrid = xgb_grid,
            metric = "ROC", trControl = ctrl_simple, verbosity = 0, nthread = 2)
    } else if(model_name == "svm") {
      svm_grid <- expand.grid(sigma = c(0.01, 0.1), C = c(1, 10))
      train(outcome ~ ., data = train_data, method = "svmRadial", tuneGrid = svm_grid,
            metric = "ROC", trControl = ctrl_simple, preProc = c("center", "scale"), probability = TRUE)
    } else if(model_name == "nnet") {
      nnet_grid <- expand.grid(size = c(5, 10), decay = c(0.001, 0.01))
      train(outcome ~ ., data = train_data, method = "nnet", tuneGrid = nnet_grid,
            metric = "ROC", trControl = ctrl_simple, preProc = c("center", "scale"), 
            trace = FALSE, MaxNWts = 3000, maxit = 300, linout = FALSE)
    } else if(model_name == "nb") {
      nb_grid <- expand.grid(usekernel = TRUE, fL = 0, adjust = 1)
      train(outcome ~ ., data = train_data, method = "nb", tuneGrid = nb_grid,
            metric = "ROC", trControl = ctrl_simple, preProc = c("center", "scale"))
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })
}

for(model_name in names(models)) {
  
  lc_results <- data.frame()
  
  for(frac in frac_seq) {
    
    for(rep in 1:n_reps) {
      train_idx <- sample(1:n_total, size = floor(n_total * frac))
      val_idx <- setdiff(1:n_total, train_idx)
      
      if(length(val_idx) < 10) next
      
      train_sub <- train_final_opt[train_idx, ]
      val_sub <- train_final_opt[val_idx, ]
      
      if(length(unique(train_sub$outcome)) < 2 || length(unique(val_sub$outcome)) < 2) next
      
      train_smote <- tryCatch({
        ovun.sample(outcome ~ ., data = train_sub, method = "over",
                    N = 2 * sum(train_sub$outcome == "No"))$data
      }, error = function(e) train_sub)
      
      model_lc <- train_model_simple(model_name, train_smote, frac)
      
      if(!is.null(model_lc)) {
        pred_val <- tryCatch({
          predict(model_lc, val_sub, type = "prob")[, "Yes"]
        }, error = function(e) NULL)
        
        pred_train <- tryCatch({
          predict(model_lc, train_smote, type = "prob")[, "Yes"]
        }, error = function(e) NULL)
        
        if(!is.null(pred_val) && !is.null(pred_train)) {
          auc_val <- tryCatch({
            as.numeric(auc(roc(val_sub$outcome, pred_val, quiet = TRUE)))
          }, error = function(e) NA)
          
          auc_train <- tryCatch({
            as.numeric(auc(roc(train_smote$outcome, pred_train, quiet = TRUE)))
          }, error = function(e) NA)
          
          lc_results <- rbind(lc_results, data.frame(
            Model = model_name,
            Train_Fraction = frac,
            Rep = rep,
            Train_AUC = auc_train,
            Val_AUC = auc_val,
            Gap = auc_train - auc_val
          ))
        }
      }
    }
  }
  
  all_learning_curves[[model_name]] <- lc_results
  
  if(nrow(lc_results) > 0) {
    avg_gap <- mean(lc_results$Gap, na.rm = TRUE)
    max_gap <- max(lc_results$Gap, na.rm = TRUE)
  }
}


all_lc_data <- do.call(rbind, all_learning_curves)

if(nrow(all_lc_data) > 0) {
  lc_summary <- all_lc_data %>%
    group_by(Model, Train_Fraction) %>%
    summarise(
      Mean_Train_AUC = mean(Train_AUC, na.rm = TRUE),
      SD_Train_AUC = sd(Train_AUC, na.rm = TRUE),
      Mean_Val_AUC = mean(Val_AUC, na.rm = TRUE),
      SD_Val_AUC = sd(Val_AUC, na.rm = TRUE),
      Mean_Gap = mean(Gap, na.rm = TRUE),
      .groups = 'drop'
    )
  
  p_lc_comparison <- ggplot(lc_summary, aes(x = Train_Fraction, y = Mean_Val_AUC, color = Model)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_ribbon(aes(ymin = Mean_Val_AUC - SD_Val_AUC, ymax = Mean_Val_AUC + SD_Val_AUC, fill = Model),
                alpha = 0.2, color = NA) +
    scale_color_brewer(palette = "Set2") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Learning Curves Comparison Across Models (Validation AUC)",
         subtitle = "Shaded area represents ±1 standard deviation",
         x = "Training Data Fraction", y = "Validation AUC", color = "Model", fill = "Model") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom") +
    ylim(0.5, 1)
  
  ggsave("ree/results/learning_curves_all_models_comparison.png", p_lc_comparison, width = 12, height = 8, dpi = 300)
  print(p_lc_comparison)
  
  p_lc_individual <- ggplot(all_lc_data, aes(x = Train_Fraction)) +
    geom_smooth(aes(y = Train_AUC, color = "Training"), method = "loess", se = FALSE, linewidth = 1) +
    geom_smooth(aes(y = Val_AUC, color = "Validation"), method = "loess", se = FALSE, linewidth = 1) +
    geom_point(aes(y = Train_AUC, color = "Training"), alpha = 0.3, size = 2) +
    geom_point(aes(y = Val_AUC, color = "Validation"), alpha = 0.3, size = 2) +
    scale_color_manual(values = c("Training" = "blue", "Validation" = "red")) +
    facet_wrap(~Model, ncol = 3) +
    labs(title = "Individual Model Learning Curves (Training vs Validation)",
         subtitle = "Blue = Training AUC, Red = Validation AUC",
         x = "Training Data Fraction", y = "AUC", color = "Dataset") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "bottom", strip.text = element_text(face = "bold")) +
    ylim(0.5, 1)
  
  ggsave("ree/results/learning_curves_individual_models.png", p_lc_individual, width = 14, height = 10, dpi = 300)
  print(p_lc_individual)
  
  gap_summary <- all_lc_data %>%
    group_by(Model) %>%
    summarise(Mean_Gap = mean(Gap, na.rm = TRUE), Max_Gap = max(Gap, na.rm = TRUE),
              SD_Gap = sd(Gap, na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(Mean_Gap))
  
  p_gap_comparison <- ggplot(gap_summary, aes(x = reorder(Model, Mean_Gap), y = Mean_Gap)) +
    geom_col(aes(fill = Mean_Gap > 0.1), width = 0.7) +
    geom_errorbar(aes(ymin = Mean_Gap - SD_Gap, ymax = Mean_Gap + SD_Gap), width = 0.2) +
    geom_hline(yintercept = 0.1, linetype = "dashed", color = "red", linewidth = 1) +
    annotate("text", x = 0.5, y = 0.12, label = "Overfitting threshold", color = "red", hjust = 0) +
    scale_fill_manual(values = c("TRUE" = "#E41A1C", "FALSE" = "#4DAF4A"),
                      labels = c("TRUE" = "Overfitting", "FALSE" = "Normal"), name = "Status") +
    labs(title = "Overfitting Comparison Across Models",
         subtitle = "Mean gap (Train AUC - Val AUC)",
         x = "Model", y = "Mean AUC Gap (Train - Val)") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()
  
  ggsave("ree/results/overfitting_gap_comparison.png", p_gap_comparison, width = 10, height = 6, dpi = 300)
  print(p_gap_comparison)
  
  write.csv(lc_summary, "ree/results/learning_curves_summary.csv", row.names = FALSE)
  write.csv(gap_summary, "ree/results/overfitting_gap_summary.csv", row.names = FALSE)
  
  print(gap_summary)
  
  overfitted_models <- gap_summary$Model[gap_summary$Mean_Gap > 0.1]
  if(length(overfitted_models) > 0) {
  } else {
  }
}



if(!requireNamespace("shapviz", quietly = TRUE)) install.packages("shapviz")
library(shapviz)

best_model_xgb <- models$xgb
final_features <- setdiff(names(train_final_opt), "outcome")
train_matrix <- as.matrix(train_final_opt[, final_features])
test_matrix <- as.matrix(test_final_opt[, final_features])
dtrain_xgb <- xgb.DMatrix(data = train_matrix, label = as.numeric(train_final_opt$outcome) - 1)
dtest_xgb <- xgb.DMatrix(data = test_matrix, label = as.numeric(test_final_opt$outcome) - 1)

best_params <- best_model_xgb$bestTune

xgb_model_native <- xgboost(
  data = dtrain_xgb,
  nrounds = best_params$nrounds,
  max_depth = best_params$max_depth,
  eta = best_params$eta,
  gamma = best_params$gamma,
  colsample_bytree = best_params$colsample_bytree,
  min_child_weight = best_params$min_child_weight,
  subsample = best_params$subsample,
  objective = "binary:logistic",
  eval_metric = "auc",
  verbose = 0
)


shap_values <- shapviz(xgb_model_native, X_pred = test_matrix, X = test_final_opt[, final_features])

p_shap_beeswarm <- sv_importance(shap_values, kind = "beeswarm", show_numbers = TRUE) +
  ggtitle("XGBoost Feature Importance - SHAP Beeswarm Plot") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10))
ggsave("ree/results/xgb_shap_beeswarm.png", p_shap_beeswarm, width = 10, height = 8, dpi = 300)
print(p_shap_beeswarm)

p_shap_bar <- sv_importance(shap_values, kind = "bar", show_numbers = TRUE) +
  ggtitle("XGBoost Feature Importance - Mean |SHAP| Values") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10))
ggsave("ree/results/xgb_shap_importance_bar.png", p_shap_bar, width = 10, height = 8, dpi = 300)
print(p_shap_bar)

yes_idx <- which(test_final_opt$outcome == "Yes")[1]
no_idx <- which(test_final_opt$outcome == "No")[1]

p_shap_yes <- sv_waterfall(shap_values, row_id = yes_idx) +
  ggtitle(paste("Positive Sample Prediction Explanation (Sample", yes_idx, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("ree/results/xgb_shap_waterfall_yes.png", p_shap_yes, width = 10, height = 6, dpi = 300)
print(p_shap_yes)

p_shap_no <- sv_waterfall(shap_values, row_id = no_idx) +
  ggtitle(paste("Negative Sample Prediction Explanation (Sample", no_idx, ")")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggsave("ree/results/xgb_shap_waterfall_no.png", p_shap_no, width = 10, height = 6, dpi = 300)
print(p_shap_no)

top6_features <- names(sort(colMeans(abs(shap_values$S)), decreasing = TRUE))[1:6]
p_dependence_list <- list()

for(i in seq_along(top6_features)) {
  feature <- top6_features[i]
  p_dep <- sv_dependence(shap_values, v = feature, color_var = "auto") +
    ggtitle(paste("SHAP Dependence Plot:", feature)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.text = element_text(size = 9))
  p_dependence_list[[i]] <- p_dep
}

if(requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)
  p_dependence_combined <- wrap_plots(p_dependence_list, ncol = 2) +
    plot_annotation(title = "XGBoost SHAP Dependence Plots for Top 6 Features",
                    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)))
  ggsave("ree/results/xgb_shap_dependence_top6.png", p_dependence_combined, width = 14, height = 10, dpi = 300)
  print(p_dependence_combined)
} else {
  for(i in seq_along(top6_features)) {
    ggsave(paste0("ree/results/xgb_shap_dependence_", top6_features[i], ".png"), 
           p_dependence_list[[i]], width = 8, height = 6, dpi = 300)
  }
}


shap_data_export <- data.frame(ID = 1:nrow(test_matrix), outcome = test_final_opt$outcome,
                               prediction = predict(xgb_model_native, dtest_xgb), shap_values$S)
write.csv(shap_data_export, "ree/results/xgb_shap_values_testset.csv", row.names = FALSE)

shap_importance <- data.frame(
  feature = names(sort(colMeans(abs(shap_values$S)), decreasing = TRUE)),
  mean_abs_shap = sort(colMeans(abs(shap_values$S)), decreasing = TRUE),
  rank = 1:length(final_features)
)
write.csv(shap_importance, "ree/results/xgb_shap_feature_importance.csv", row.names = FALSE)




safe_smote <- function(train_data, outcome_col = "outcome", target_ratio = 0.46) {
  require(smotefamily)
  train_data <- as.data.frame(train_data)
  X <- train_data %>% select(-all_of(outcome_col))
  y <- train_data[[outcome_col]]
  y_num <- ifelse(as.character(y) == "Yes" | y == 1, 1, 0)
  n_pos <- sum(y_num == 1)
  n_neg <- sum(y_num == 0)
  if (n_pos < 5) return(train_data)
  if (n_pos/(n_pos+n_neg) >= target_ratio) {
    return(train_data)
  }
  desired_total_pos <- round(target_ratio * n_neg / (1 - target_ratio))
  dup_size <- max(1, min(20, round((desired_total_pos - n_pos) / n_pos)))
  smote_result <- tryCatch({
    SMOTE(X = X, target = y_num, K = min(5, n_pos - 1), dup_size = dup_size)
  }, error = function(e) NULL)
  if (is.null(smote_result)) return(train_data)
  balanced <- smote_result$data
  target_col <- ncol(balanced)
  result <- as.data.frame(balanced[, -target_col])
  result[[outcome_col]] <- factor(ifelse(balanced[, target_col] == 1, "Yes", "No"),
                                  levels = c("No", "Yes"))
  final_pos <- sum(result[[outcome_col]] == "Yes")
  return(result)
}

required_packages <- c("caret", "glmnet", "pROC", "car", "ggplot2", "dplyr", "rmda", "smotefamily")
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


baseline_2011 <- final_data %>%
  filter(year == 2011) %>%
  select(
    ID, 
    raeduc_c,
    cesd10,
    ragender,
    adl_level,
    sleep_night,
    socwk,
    hrural,
    marry_cat,
    arthre, asthmae, cancre, diabe, digeste, 
    dyslipe, hearte, hibpe, kidneye, livere, lunge, memrye, psyche
  ) %>%
  mutate(
    education_num = as.numeric(as.character(raeduc_c)) - 1,
    
    depression = ifelse(cesd10 >= 10, 1, 0),
    
    gender_female = ifelse(ragender == "Female", 1, 0),
    
    adl_limit = ifelse(as.numeric(as.character(adl_level)) >= 2, 1, 0),
    
    sleep_hours = as.numeric(sleep_night),
    
    social = ifelse(socwk == "1", 1, 0),
    
    is_rural = ifelse(hrural == "1", 1, 0),
    
    unmarried = ifelse(marry_cat != "Married", 1, 0),
    
    n_chronic = (as.numeric(arthre)-1) + (as.numeric(asthmae)-1) + 
      (as.numeric(cancre)-1) + (as.numeric(diabe)-1) + 
      (as.numeric(digeste)-1) + (as.numeric(dyslipe)-1) + 
      (as.numeric(hearte)-1) + (as.numeric(hibpe)-1) + 
      (as.numeric(kidneye)-1) + (as.numeric(livere)-1) + 
      (as.numeric(lunge)-1) + (as.numeric(memrye)-1) + 
      (as.numeric(psyche)-1)
  ) %>%
  select(ID, education_num, depression, n_chronic, gender_female,
         adl_limit, sleep_hours, social, is_rural, unmarried)

outcome_2018 <- final_data %>%
  filter(year == 2018) %>%
  select(ID, cognitive_decline, pain_level) %>%
  mutate(
    comorbid_2018 = ifelse(
      cognitive_decline == "Yes" & as.numeric(as.character(pain_level)) %in% c(3, 4),
      1, 0
    )
  ) %>%
  select(ID, outcome = comorbid_2018)

track_b_data <- baseline_2011 %>%
  inner_join(outcome_2018, by = "ID") %>%
  na.omit()

print(table(track_b_data$outcome))

set.seed(456)

id_level <- track_b_data %>%
  group_by(ID) %>%
  summarise(outcome = first(outcome), .groups = 'drop')

train_ids <- id_level %>%
  group_by(outcome) %>%
  sample_frac(0.7) %>%
  pull(ID)

test_ids <- setdiff(id_level$ID, train_ids)

train_data <- track_b_data[track_b_data$ID %in% train_ids, ] %>% select(-ID)
test_data <- track_b_data[track_b_data$ID %in% test_ids, ] %>% select(-ID)



predictor_vars <- c("education_num", "depression", "n_chronic", "gender_female",
                    "adl_limit", "sleep_hours", "social", "is_rural", "unmarried")

corr_matrix <- cor(train_data[, predictor_vars])
print(round(corr_matrix, 2))

vif_model <- lm(outcome ~ ., data = train_data[, c("outcome", predictor_vars)])
vif_values <- car::vif(vif_model)

vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = as.numeric(vif_values),
  Tolerance = 1/as.numeric(vif_values)
) %>% arrange(desc(VIF))

print(vif_df)
write.csv(vif_df, "ree/results/trackB_vif.csv", row.names = FALSE)


X_train <- as.matrix(train_data[, predictor_vars])
y_train <- train_data$outcome

set.seed(123)
cv_lasso <- cv.glmnet(X_train, y_train, family = "binomial", 
                      alpha = 1, nfolds = 5, type.measure = "auc")

lasso_coef <- coef(cv_lasso, s = "lambda.min")
coef_df <- data.frame(
  Feature = rownames(lasso_coef)[-1],
  Coefficient = as.numeric(lasso_coef)[-1]
) %>% arrange(desc(abs(Coefficient)))

print(coef_df)

selected_features <- coef_df$Feature[coef_df$Coefficient != 0]
if(length(selected_features) < 3) {
  lasso_coef_1se <- coef(cv_lasso, s = "lambda.1se")
  selected_features <- rownames(lasso_coef_1se)[which(lasso_coef_1se[-1,1] != 0)]
} else {
}


train_selected <- train_data[, c("outcome", selected_features)]
test_selected <- test_data[, c("outcome", selected_features)]

if(!dir.exists("ree/results")) dir.create("ree/results", recursive = TRUE)
png("ree/results/trackB_lasso_cv.png", width = 1200, height = 800, res = 300)
plot(cv_lasso, main = "LASSO Cross-Validation")
abline(v = log(cv_lasso$lambda.min), col = "red", lty = 2, lwd = 2)
abline(v = log(cv_lasso$lambda.1se), col = "blue", lty = 2, lwd = 2)
legend("topright", legend = c("lambda.min", "lambda.1se"), 
       col = c("red", "blue"), lty = 2, lwd = 2)
dev.off()


train_smote <- train_selected
train_smote$outcome <- factor(ifelse(train_smote$outcome == 1, "Yes", "No"))


train_balanced <- safe_smote(train_smote, target_ratio = 0.46)
train_balanced$outcome <- ifelse(train_balanced$outcome == "Yes", 1, 0)



models <- list()
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                     summaryFunction = twoClassSummary, savePredictions = "final")

train_caret <- train_balanced
train_caret$outcome <- factor(ifelse(train_caret$outcome == 1, "Yes", "No"))

set.seed(123)
models$logistic <- train(outcome ~ ., data = train_caret, method = "glm",
                         family = "binomial", metric = "ROC", trControl = ctrl,
                         preProc = c("center", "scale"))

set.seed(123)
enet_grid <- expand.grid(alpha = c(0, 0.5, 1), lambda = 10^seq(-4, 0, length = 20))
models$glmnet <- train(outcome ~ ., data = train_caret, method = "glmnet",
                       family = "binomial", tuneGrid = enet_grid, metric = "ROC",
                       trControl = ctrl, preProc = c("center", "scale"))



results <- data.frame()
predictions <- list()
best_thresholds <- list()

for(model_name in names(models)) {
  
  model <- models[[model_name]]
  pred_prob <- predict(model, test_selected, type = "prob")[, "Yes"]
  predictions[[model_name]] <- pred_prob
  
  roc_obj <- roc(test_selected$outcome, pred_prob, quiet = TRUE)
  auc_val <- auc(roc_obj)
  
  youden <- roc_obj$sensitivities + roc_obj$specificities - 1
  best_thresh <- roc_obj$thresholds[which.max(youden)]
  best_thresholds[[model_name]] <- best_thresh
  
  pred_class <- ifelse(pred_prob > best_thresh, 1, 0)
  cm <- confusionMatrix(factor(pred_class, levels = c(0,1), labels = c("No","Yes")),
                        factor(test_selected$outcome, levels = c(0,1), labels = c("No","Yes")))
  
  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  ppv <- cm$byClass["Pos Pred Value"]
  npv <- cm$byClass["Neg Pred Value"]
  f1 <- 2 * (ppv * sens) / (ppv + sens)
  bal_acc <- (sens + spec) / 2
  
  results <- rbind(results, data.frame(
    Model = model_name, AUC = round(auc_val, 3),
    Threshold = round(best_thresh, 3), Accuracy = round(cm$overall["Accuracy"], 3),
    Sensitivity = round(sens, 3), Specificity = round(spec, 3),
    PPV = round(ppv, 3), NPV = round(npv, 3), F1 = round(f1, 3),
    Balanced_Acc = round(bal_acc, 3), CV_AUC = round(max(model$results$ROC), 3)
  ))
  
}

print(results)

best_idx <- which.max(results$Balanced_Acc)
best_model_name <- results$Model[best_idx]
best_model <- models[[best_model_name]]
best_thresh <- best_thresholds[[best_model_name]]



calibrate_model <- function(pred_prob, true_y) {
  calib_data <- data.frame(y = true_y, p = pred_prob)
  calib_model <- glm(y ~ p, data = calib_data, family = binomial())
  calib_prob <- predict(calib_model, newdata = data.frame(p = pred_prob), type = "response")
  return(calib_prob)
}

for(model_name in names(predictions)) {
  calib_prob <- calibrate_model(predictions[[model_name]], test_selected$outcome)
  roc_before <- roc(test_selected$outcome, predictions[[model_name]], quiet = TRUE)
  roc_after <- roc(test_selected$outcome, calib_prob, quiet = TRUE)
}


delong_results <- data.frame()
model_names <- results$Model

for(i in 1:(length(model_names)-1)) {
  for(j in (i+1):length(model_names)) {
    mi <- model_names[i]
    mj <- model_names[j]
    
    roc_i <- roc(test_selected$outcome, predictions[[mi]], quiet = TRUE)
    roc_j <- roc(test_selected$outcome, predictions[[mj]], quiet = TRUE)
    test_res <- roc.test(roc_i, roc_j, method = "delong")
    
    p_disp <- ifelse(test_res$p.value < 0.0001, "< 0.0001", 
                     format(round(test_res$p.value, 4), nsmall = 4))
    
    delong_results <- rbind(delong_results, data.frame(
      Model1 = mi, Model2 = mj, AUC1 = round(auc(roc_i), 3),
      AUC2 = round(auc(roc_j), 3), p_value = test_res$p.value,
      p_display = p_disp, Significant = ifelse(test_res$p.value < 0.05, "Yes", "No")
    ))
  }
}

write.csv(delong_results, "ree/results/trackB_delong.csv", row.names = FALSE)
print(delong_results)


if(length(predictions) >= 1) {
  dca_data <- data.frame(outcome = test_selected$outcome)
  for(mn in names(predictions)) dca_data[[mn]] <- predictions[[mn]]
  
  dca_formula <- as.formula(paste("outcome ~", paste(names(predictions), collapse = " + ")))
  dca_result <- tryCatch({
    decision_curve(dca_formula, data = dca_data, thresholds = seq(0, 0.5, by = 0.01), bootstraps = 100)
  }, error = function(e) NULL)
  
  if(!is.null(dca_result)) {
    dca_df <- as.data.frame(dca_result$derived.data)
    if("thresholds" %in% names(dca_df)) dca_df$threshold <- dca_df$thresholds
    if("NB" %in% names(dca_df)) dca_df$net_benefit <- dca_df$NB
    
    model_colors <- c("logistic" = "#E41A1C", "glmnet" = "#377EB8",
                      "Treat All" = "gray60", "Treat None" = "gray40")
    
    p_dca <- ggplot(dca_df, aes(x = threshold, y = net_benefit, color = model)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = model_colors) +
      labs(title = "Decision Curve Analysis (DCA)",
           subtitle = "Track B: Logistic Regression vs Elastic Net",
           x = "Threshold Probability", y = "Net Benefit", color = "Model") +
      theme_minimal(base_size = 12) +
      theme(legend.position = c(0.8, 0.3), 
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
    ggsave("ree/results/trackB_dca.png", p_dca, width = 10, height = 8, dpi = 300)
    print(p_dca)
    saveRDS(dca_result, "ree/results/trackB_dca.rds")
  } else {
  }
} else {
}


train_simple <- function(mn, train_data, frac) {
  ctrl_s <- trainControl(method = "cv", number = 3, classProbs = TRUE,
                         summaryFunction = twoClassSummary, verboseIter = FALSE)
  tryCatch({
    if(mn == "logistic") {
      train(make.names(outcome) ~ ., data = train_data, method = "glm", family = "binomial",
            metric = "ROC", trControl = ctrl_s, preProc = c("center", "scale"))
    } else if(mn == "glmnet") {
      gr <- expand.grid(alpha = c(0, 0.5, 1), lambda = 10^seq(-3, 0, length = 5))
      train(make.names(outcome) ~ ., data = train_data, method = "glmnet", family = "binomial",
            tuneGrid = gr, metric = "ROC", trControl = ctrl_s, preProc = c("center", "scale"))
    } else NULL
  }, error = function(e) NULL)
}

set.seed(789)
n_total <- nrow(train_balanced)
frac_seq <- c(0.2, 0.4, 0.6, 0.8, 1.0)
n_reps <- 3
all_lc <- list()

for(mn in names(models)) {
  lc_res <- data.frame()
  for(frac in frac_seq) {
    for(rep in 1:n_reps) {
      tr_idx <- sample(1:n_total, floor(n_total * frac))
      vl_idx <- setdiff(1:n_total, tr_idx)
      if(length(vl_idx) < 10) next
      
      tr_sub <- train_balanced[tr_idx, ]
      vl_sub <- train_balanced[vl_idx, ]
      if(length(unique(tr_sub$outcome)) < 2) next
      
      tr_sub$outcome <- factor(tr_sub$outcome, levels = c(0,1), labels = c("No","Yes"))
      vl_sub$outcome <- factor(vl_sub$outcome, levels = c(0,1), labels = c("No","Yes"))
      
      mdl <- train_simple(mn, tr_sub, frac)
      if(!is.null(mdl)) {
        p_vl <- tryCatch(predict(mdl, vl_sub, type = "prob")[, "Yes"], error = function(e) NULL)
        p_tr <- tryCatch(predict(mdl, tr_sub, type = "prob")[, "Yes"], error = function(e) NULL)
        if(!is.null(p_vl) && !is.null(p_tr)) {
          a_vl <- tryCatch(as.numeric(auc(roc(vl_sub$outcome, p_vl, quiet = TRUE))), error = function(e) NA)
          a_tr <- tryCatch(as.numeric(auc(roc(tr_sub$outcome, p_tr, quiet = TRUE))), error = function(e) NA)
          lc_res <- rbind(lc_res, data.frame(Model = mn, Train_Fraction = frac, Rep = rep,
                                             Train_AUC = a_tr, Val_AUC = a_vl, Gap = a_tr - a_vl))
        }
      }
    }
  }
  all_lc[[mn]] <- lc_res
  if(nrow(lc_res) > 0) {
  }
}

all_lc_data <- do.call(rbind, all_lc)
if(nrow(all_lc_data) > 0) {
  lc_sum <- all_lc_data %>% group_by(Model, Train_Fraction) %>%
    summarise(Mean_Val_AUC = mean(Val_AUC, na.rm = TRUE), SD_Val_AUC = sd(Val_AUC, na.rm = TRUE), .groups = 'drop')
  
  p_lc <- ggplot(lc_sum, aes(x = Train_Fraction, y = Mean_Val_AUC, color = Model)) +
    geom_line(linewidth = 1.2) + geom_point(size = 3) +
    geom_ribbon(aes(ymin = Mean_Val_AUC - SD_Val_AUC, ymax = Mean_Val_AUC + SD_Val_AUC, fill = Model), alpha = 0.2) +
    scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
    labs(title = "Track B Learning Curves (Validation AUC)",
         subtitle = "Based on balanced training set, shaded area ±1 SD",
         x = "Training Data Fraction", y = "Validation AUC") +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom",
                                          plot.title = element_text(hjust = 0.5, face = "bold")) +
    ylim(0.5, 1)
  ggsave("ree/results/trackB_learning_curves.png", p_lc, width = 10, height = 8, dpi = 300)
  
  gap_sum <- all_lc_data %>% group_by(Model) %>%
    summarise(Mean_Gap = mean(Gap, na.rm = TRUE), Max_Gap = max(Gap, na.rm = TRUE), .groups = 'drop')
  
  p_gap <- ggplot(gap_sum, aes(x = reorder(Model, Mean_Gap), y = Mean_Gap)) +
    geom_col(aes(fill = Mean_Gap > 0.1), width = 0.7) +
    geom_hline(yintercept = 0.1, linetype = "dashed", color = "red") +
    scale_fill_manual(values = c("TRUE" = "#E41A1C", "FALSE" = "#4DAF4A")) +
    coord_flip() + labs(title = "Track B Overfitting Test", x = "Model", y = "Average Gap (Train - Val)") +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  ggsave("ree/results/trackB_overfitting.png", p_gap, width = 8, height = 6, dpi = 300)
  write.csv(gap_sum, "ree/results/trackB_gap_summary.csv", row.names = FALSE)
  
  print(gap_sum)
} else {
}


internal_results <- data.frame()

for(model_name in names(models)) {
  model <- models[[model_name]]
  pred_prob <- predict(model, test_selected, type = "prob")[, "Yes"]
  thresh <- best_thresholds[[model_name]]
  pred_class <- ifelse(pred_prob > thresh, 1, 0)
  
  roc_int <- roc(test_selected$outcome, pred_prob, quiet = TRUE)
  cm_int <- confusionMatrix(factor(pred_class, levels = c(0,1), labels = c("No","Yes")),
                            factor(test_selected$outcome, levels = c(0,1), labels = c("No","Yes")))
  
  internal_results <- rbind(internal_results, data.frame(
    Model = model_name, Internal_AUC = round(auc(roc_int), 3),
    Sensitivity = round(cm_int$byClass["Sensitivity"], 3),
    Specificity = round(cm_int$byClass["Specificity"], 3),
    Balanced_Acc = round((cm_int$byClass["Sensitivity"] + cm_int$byClass["Specificity"])/2, 3),
    Threshold = round(thresh, 3)
  ))
}

print(internal_results)


thresh_analysis <- function(pred_prob, actual, thresholds = seq(0.1, 0.9, 0.05)) {
  res <- data.frame()
  for(th in thresholds) {
    pred <- ifelse(pred_prob > th, 1, 0)
    cm <- confusionMatrix(factor(pred, levels = c(0,1), labels = c("No","Yes")),
                          factor(actual, levels = c(0,1), labels = c("No","Yes")))
    prev <- mean(actual)
    net_ben <- (cm$byClass["Sensitivity"] * prev) - 
      ((1 - cm$byClass["Specificity"]) * (1 - prev) * (th/(1-th)))
    res <- rbind(res, data.frame(
      Threshold = th, Sensitivity = cm$byClass["Sensitivity"],
      Specificity = cm$byClass["Specificity"], Net_Benefit = net_ben
    ))
  }
  return(res)
}

thresh_res <- thresh_analysis(predictions[[best_model_name]], test_selected$outcome)

p_thresh <- ggplot(thresh_res, aes(x = Threshold)) +
  geom_line(aes(y = Sensitivity, color = "Sensitivity"), linewidth = 1.2) +
  geom_line(aes(y = Specificity, color = "Specificity"), linewidth = 1.2) +
  geom_line(aes(y = Net_Benefit, color = "Net Benefit"), linewidth = 1.2) +
  geom_vline(xintercept = best_thresh, linetype = "dashed", color = "red") +
  annotate("text", x = best_thresh, y = 0.95, 
           label = paste("Optimal:", round(best_thresh, 2)), 
           color = "red", hjust = -0.1) +
  scale_color_manual(values = c("Sensitivity" = "#2E86AB", 
                                "Specificity" = "#A23B72", 
                                "Net Benefit" = "#F18F01")) +
  labs(title = "Threshold Sensitivity Analysis", 
       subtitle = paste("Model:", best_model_name),
       x = "Threshold", y = "Performance Metric") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave("ree/results/trackB_threshold_analysis.png", p_thresh, width = 10, height = 8, dpi = 300)
print(p_thresh)


if(best_model_name == "logistic") {
  coef_vals <- coef(best_model$finalModel)
  feature_names <- names(coef_vals)[-1]
  importance_df <- data.frame(
    Feature = feature_names,
    Coefficient = as.numeric(coef_vals)[-1],
    Importance = abs(as.numeric(coef_vals)[-1]),
    OR = exp(as.numeric(coef_vals)[-1])
  )
} else if(best_model_name == "glmnet") {
  coef_mat <- coef(best_model$finalModel, s = best_model$bestTune$lambda)
  coef_vals <- as.numeric(coef_mat)[-1]
  feature_names <- rownames(coef_mat)[-1]
  importance_df <- data.frame(
    Feature = feature_names,
    Coefficient = coef_vals,
    Importance = abs(coef_vals),
    OR = exp(coef_vals)
  )
}

importance_df <- importance_df[order(-importance_df$Importance), ]
importance_df$Direction <- ifelse(importance_df$Coefficient > 0, "Risk factor", "Protective factor")

print(head(importance_df, 10))

write.csv(importance_df, "ree/results/trackB_feature_importance.csv", row.names = FALSE)

top_n <- min(10, nrow(importance_df))
plot_data <- head(importance_df, top_n)
plot_data$Feature <- factor(plot_data$Feature, levels = rev(plot_data$Feature))

p_imp <- ggplot(plot_data, aes(x = Feature, y = Importance, fill = Direction)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Risk factor" = "#E63946", "Protective factor" = "#2A9D8F")) +
  coord_flip() +
  labs(title = "Feature Importance (Absolute Coefficient)",
       subtitle = best_model_name,
       x = "Feature", y = "Absolute Coefficient") +
  theme_minimal() + theme(legend.position = "bottom")

ggsave("ree/results/trackB_feature_importance.png", p_imp, width = 8, height = 6, dpi = 300)


saveRDS(best_model, "ree/results/trackB_best_model.rds")
write.csv(results, "ree/results/trackB_performance.csv", row.names = FALSE)
write.csv(internal_results, "ree/results/trackB_internal_validation.csv", row.names = FALSE)
write.csv(thresh_res, "ree/results/trackB_threshold_analysis.csv", row.names = FALSE)

final_summary <- data.frame(
  Metric = c("Best_Model", "AUC", "Threshold", "Sensitivity", "Specificity", 
             "Balanced_Acc", "Features", "Train_N", "Test_N"),
  Value = c(best_model_name, results$AUC[best_idx], best_thresh,
            results$Sensitivity[best_idx], results$Specificity[best_idx],
            results$Balanced_Acc[best_idx], length(selected_features),
            nrow(train_balanced), nrow(test_selected))
)
write.csv(final_summary, "ree/results/trackB_summary.csv", row.names = FALSE)
saveRDS(models$logistic, "ree/results/trackB_logistic_final.rds")
save(track_b_data, train_selected, test_selected, train_balanced,
     models, best_model, results, internal_results, importance_df,
     file = "ree/results/trackB_complete.RData")

logistic_model <- models$logistic
coef_log <- coef(summary(logistic_model$finalModel))

coef_table <- data.frame(
  Variable = rownames(coef_log),
  Coefficient = coef_log[, "Estimate"],
  Std_Error = coef_log[, "Std. Error"],
  z_value = coef_log[, "z value"],
  p_value = coef_log[, "Pr(>|z|)"],
  OR = exp(coef_log[, "Estimate"]),
  OR_CI_lower = exp(coef_log[, "Estimate"] - 1.96 * coef_log[, "Std. Error"]),
  OR_CI_upper = exp(coef_log[, "Estimate"] + 1.96 * coef_log[, "Std. Error"])
)

coef_table <- coef_table[coef_table$Variable != "(Intercept)", ]
coef_table <- coef_table[order(-abs(coef_table$Coefficient)), ]

print(coef_table)
write.csv(coef_table, "ree/results/trackB_logistic_coefficients_with_CI.csv", row.names = FALSE)

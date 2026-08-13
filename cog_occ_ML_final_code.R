library(tidyverse)
library(pacman)
library(tidymodels)
library(purrr)
library(glmnet)
library(dplyr)
library(stringr)
library(readr)
library(estimatr)
library(fixest)
library(knitr)
library(ranger)
library(parsnip)
library(xgboost)
library(themis)
library(kableExtra)
library(scales)
library(tidyr)

specific_decimal <- function(x, k=3) trimws(format(round(x, k), nsmall=k))

theme_ireland <- theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom", #c(0.25, 0.9),
    legend.text = element_text(size=12),
    axis.line.x = element_line(linewidth=.3),
    axis.line.y = element_line(linewidth=.3)
  )

ireland_palette <- c("deepskyblue2","deeppink2","lightcoral", "maroon", "maroon4", "peachpuff", "palevioletred","lightblue","lemonchiffon2","deepskyblue4")


opts_chunk$set(
  commment = "#>",
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.width = 9,
  fig.height = 6,
  fig.align = "center"
)



# Load IPUMS data
ipums <- read_csv("IPUMS2019.csv.gz")

# Load O*NET files
skills <- read_csv("Skills.csv")
work_act <- read_csv("Work Activities.csv")



# O*NET cognitive index construction; filter skills to IM (important) scale and cognitive items only, select relevant cols (ditching 'level' scale)

# Define cognitive skill items

cognitive_skills <- c(
  "Critical Thinking",
  "Complex Problem Solving", 
  "Active Learning",
  "Reading Comprehension",
  "Writing",
  "Mathematics"
)

cog_index <- skills %>%
  filter(`Scale ID` == "IM",
         `Element Name` %in% cognitive_skills) %>% 
  select(`O*NET-SOC Code`, `Element Name`, `Data Value`) %>% 
  pivot_wider(names_from = `Element Name`, values_from = `Data Value`) %>% 
  # Create average cognitive score across items
  mutate(cog_score = rowMeans(across(all_of(cognitive_skills)), na.rm = TRUE))

## Thresholding and SOC prep; thresholding the score into an outcome variable and prepping the SOC codes for merging with IPUMS


# Threshold into binary outcome at the median
cog_index <- cog_index %>% 
  mutate(high_cog = as.integer(cog_score >= median(cog_score)))

# Now prepping SOC code for merging
# O*NET uses 8-digit codes like "11-1011.00"; need to strip to 6-digit like "111011" to match IPUMS OCCSOC format
cog_index <- cog_index %>% 
  mutate(soc6 = str_remove_all(`O*NET-SOC Code`, "[-.]") %>%  str_sub(1, 6))

cog_index_6dig <- cog_index %>% 
  group_by(soc6) %>% 
  summarise(cog_score = mean(cog_score),
            high_cog = as.integer(mean(high_cog) >= 0.5)) %>% 
  ungroup()


# Initial IPUMS merge 

# merge onto IPUMS and do the sample restrictions at the same time 

# Cleaning OCCSOC in IPUMS to match - removing any non-alphanumeric characters; filter out XX codes before merging
ipums_clean <- ipums %>% 
  filter(UHRSWORK >= 35) %>% 
  filter(!str_detect(OCCSOC, "XX")) %>%   # dropping unidentified occupations
  left_join(cog_index_6dig, by = c("OCCSOC" = "soc6"))

# Checking merge quality
cat("Total rows after filters:", nrow(ipums_clean), "\n")
cat("Rows with matched cog_score:", sum(!is.na(ipums_clean$cog_score)), "\n")
cat("Rows without match:", sum(is.na(ipums_clean$cog_score)), "\n")


#4-digit fallback matching and final sample

# Build a 4-digit level lookup from cog_index_6dig
cog_4dig <- cog_index_6dig %>% 
  mutate(soc4 = str_sub(soc6, 1, 4)) %>% 
  group_by(soc4) %>% 
  summarise(cog_score_4dig = mean(cog_score),
            high_cog_4dig = as.integer(mean(high_cog) >= 0.5)) %>% 
  ungroup()

# Add a soc4 column to ipums_clean and attempt secondary match
ipums_clean <- ipums_clean %>% 
  mutate(soc4 = str_sub(OCCSOC, 1, 4)) %>% 
  left_join(cog_4dig, by = "soc4") %>% 
  # Fill in unmatched rows using 4-digit match
  mutate(
    cog_score_final = coalesce(cog_score, cog_score_4dig),
    high_cog_final = coalesce(high_cog, high_cog_4dig)
  )

# Now drop anything still unmatched and drop letter-coded occupations
ipums_final <- ipums_clean %>% 
  filter(!str_detect(OCCSOC, "[A-Za-z]")) %>% 
  filter(!is.na(high_cog_final))

cat("Final sample size:", nrow(ipums_final), "\n")
cat("Remaining unmatched:", sum(is.na(ipums_final$high_cog_final)), "\n")

table(ipums_final$high_cog_final)

#Feature engineering

# 5 in GD 

ipums_model <- ipums_final %>%
  mutate(
    educ_cat = case_when(
      EDUCD %in% c(2, 11, 12, 14, 15, 16, 17, 22, 23, 25, 26, 
                   30, 40, 50, 61) ~ "less_than_hs",
      EDUCD == 63 ~ "hs_diploma",
      EDUCD == 64 ~ "ged",
      EDUCD == 65 ~ "some_college",
      EDUCD == 71 ~ "associates",
      EDUCD == 81 ~ "bachelors",
      EDUCD %in% c(101, 114) ~ "masters",
      EDUCD == 115 ~ "professional",
      EDUCD == 116 ~ "doctoral",
      TRUE ~ "other"
    ) %>% factor(levels = c("less_than_hs", "ged", "hs_diploma", 
                            "some_college", "associates", "bachelors",
                            "masters", "professional", "doctoral", "other")),
    
    # fixing citizen
    citizen = factor(case_when(
      CITIZEN %in% c(0, 1) ~ "born_citizen",
      CITIZEN == 2 ~ "naturalized",
      CITIZEN == 3 ~ "not_citizen"
    )),
    
    # fixing english
    english = factor(case_when(
      SPEAKENG %in% c(1, 6) ~ "english_only",
      SPEAKENG == 2 ~ "very_well",
      SPEAKENG == 3 ~ "well",
      SPEAKENG == 4 ~ "not_well",
      SPEAKENG == 5 ~ "not_at_all"
    )),
    
    sex = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
    age = AGE,
    race_cat = case_when(
      RACED == 100 ~ "white",
      RACED == 200 ~ "black",
      RACED %in% c(400, 410, 420, 430, 440, 450, 460,
                   470, 480, 490, 500, 510) ~ "asian",
      TRUE ~ "other"
    ) %>% factor(),
    hispanic = factor(if_else(HISPAND == 0, "not_hispanic", "hispanic")),
    nativity = factor(if_else(BPL <= 120, "us_born", "foreign_born")),
    degfield_cat = case_when(
      DEGFIELD == 0 ~ "no_degree",
      DEGFIELD %in% c(11, 13) ~ "stem_engineering",
      DEGFIELD %in% c(21, 22, 23, 24, 25, 26) ~ "stem_science",
      DEGFIELD == 37 ~ "stem_math_cs",
      DEGFIELD %in% c(32, 33, 34, 35, 36) ~ "social_science",
      DEGFIELD %in% c(51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61) ~ "humanities",
      DEGFIELD %in% c(62, 63, 64) ~ "education",
      DEGFIELD %in% c(40, 41, 42, 43, 44, 45, 46, 47, 48) ~ "business",
      TRUE ~ "other"
    ) %>% factor(),
    ind_cat = case_when(
      IND %in% 170:290   ~ "agriculture",
      IND %in% 370:490   ~ "mining_construction",
      IND %in% 1070:3990 ~ "manufacturing",
      IND %in% 4070:4590 ~ "wholesale_trade",
      IND %in% 4670:5790 ~ "retail_trade",
      IND %in% 6070:6390 ~ "transportation",
      IND %in% 570:690   ~ "utilities",
      IND %in% 6470:6780 ~ "information",
      IND %in% 6870:7190 ~ "finance",
      IND %in% 7270:7790 ~ "professional_services",
      IND %in% 7860:8470 ~ "education_health",
      IND %in% 8560:8690 ~ "arts_entertainment",
      IND %in% 8770:9290 ~ "other_services",
      IND %in% 9370:9590 ~ "public_admin",
      TRUE ~ "other"
    ) %>% factor(),
    metro = factor(PWMETSTAT),
    state = factor(STATEFIP),
    high_cog = factor(high_cog_final, levels = c(0, 1), 
                      labels = c("low", "high"))
  ) %>%
  select(high_cog, sex, age, educ_cat, race_cat, hispanic, nativity,
         citizen, english, degfield_cat, ind_cat, metro, state,
         cog_score_final)


# Train/test split

# Split: 80/20

set.seed(8489)

data_split <- initial_split(ipums_model, prop = 0.8, strata = high_cog)

train_data <- training(data_split)
test_data <- testing(data_split)


# Recipe

model_recipe <- recipe(high_cog ~ ., data = train_data) %>%
  step_rm(cog_score_final) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())


# Model specifications and workflows 

logistic_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

xgb_spec_fixed <- boost_tree(
  trees = 200, mtry = 6, learn_rate = 0.1, tree_depth = 4
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")


logistic_wf <- workflow() %>% add_recipe(model_recipe) %>% add_model(logistic_spec)

lasso_wf <- workflow() %>% add_recipe(model_recipe) %>% add_model(lasso_spec)

xgb_wf_fixed <- workflow() %>% add_recipe(model_recipe) %>% add_model(xgb_spec_fixed)


#CV folds and full model fitting

set.seed(8489)
cv_folds_full <- vfold_cv(train_data, v = 5, strata = high_cog)

lasso_grid <- grid_regular(penalty(), levels = 10)

logistic_fit_full <- logistic_wf %>%
  fit_resamples(
    resamples = cv_folds_full,
    metrics = metric_set(accuracy, roc_auc),
    control = control_resamples(save_pred = TRUE)
  )

lasso_fit_full <- lasso_wf %>%
  tune_grid(
    resamples = cv_folds_full,
    grid = lasso_grid,
    metrics = metric_set(accuracy, roc_auc),
    control = control_grid(save_pred = TRUE)
  )

xgb_fit_full <- xgb_wf_fixed %>%
  fit_resamples(
    resamples = cv_folds_full,
    metrics = metric_set(accuracy, roc_auc),
    control = control_resamples(save_pred = TRUE)
  )


# Final test set evaluation 

best_lasso_penalty <- lasso_fit_full %>% select_best(metric = "roc_auc")

final_logistic_wf <- logistic_wf
final_lasso_wf <- lasso_wf %>% finalize_workflow(best_lasso_penalty)
final_xgb_wf <- xgb_wf_fixed

logistic_final <- final_logistic_wf %>% last_fit(data_split)
lasso_final <- final_lasso_wf %>% last_fit(data_split)
xgb_final <- final_xgb_wf %>% last_fit(data_split)

collect_metrics(logistic_final)
collect_metrics(lasso_final)
collect_metrics(xgb_final)

## data distribution plots ----

### cog score dist. ( w tHold line) ----

ggplot(ipums_final, aes(x = cog_score_final)) +
  geom_histogram(bins = 40, fill = "violetred4", color = "white") +
  geom_vline(xintercept = median(ipums_final$cog_score_final),
             lty = 2, color = "gray40", linewidth = 1) +
  annotate("text", 
           x = median(ipums_final$cog_score_final) - 0.09, 
           y = 62500, 
           label = "Median threshold", 
           color = "gray40",
           angle = 90,
           hjust = 0) +
  labs(title = "Distribution of O*NET Cognitive Skill Scores",
       subtitle = "Dashed line indicates median threshold for high/low classification",
       x = "Cognitive Skill Score",
       y = "Count") +
  theme_ireland

#### Education distribution by outcome ----

ipums_model %>%
  filter(educ_cat != "other") %>%
  ggplot(aes(x = educ_cat, fill = high_cog)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("violetred4", "hotpink2"),
                    labels = c("Low Cognitive", "High Cognitive"),
                    name = "Occupation Type") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(
    "less_than_hs" = "< High School",
    "ged" = "GED",
    "hs_diploma" = "HS Diploma",
    "some_college" = "Some College",
    "associates" = "Associate's",
    "bachelors" = "Bachelor's",
    "masters" = "Master's",
    "professional" = "Professional",
    "doctoral" = "Doctoral"
  )) +
  labs(title = "Cognitive Occupation Placement by Education Level",
       subtitle = "Share of workers in high vs. low cognitive occupations",
       x = "Education Level",
       y = "Share") +
  theme_ireland +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Race distribution by outcome ----

ipums_model %>%
  ggplot(aes(x = race_cat, fill = high_cog)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("violetred4", "hotpink2"),
                    labels = c("Low Cognitive", "High Cognitive"),
                    name = "Occupation Type") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c(
    "white" = "White",
    "black" = "Black",
    "asian" = "Asian",
    "other" = "Other"
  )) +
  labs(title = "Cognitive Occupation Placement by Race",
       subtitle = "Share of workers in high vs. low cognitive occupations",
       x = "Race",
       y = "Share") +
  theme_ireland


#### Industry distribution by outcome ----

ipums_model %>%
  mutate(ind_cat = recode(ind_cat,
                          "agriculture" = "Agriculture",
                          "mining_construction" = "Mining/Construction",
                          "manufacturing" = "Manufacturing",
                          "wholesale_trade" = "Wholesale Trade",
                          "retail_trade" = "Retail Trade",
                          "transportation" = "Transportation",
                          "utilities" = "Utilities",
                          "information" = "Information",
                          "finance" = "Finance",
                          "professional_services" = "Professional Services",
                          "education_health" = "Education & Health",
                          "arts_entertainment" = "Arts & Entertainment",
                          "other_services" = "Other Services",
                          "public_admin" = "Public Admin",
                          "other" = "Other"
  )) %>%
  group_by(ind_cat) %>%
  mutate(pct_high = mean(high_cog == "high")) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(ind_cat, pct_high), fill = high_cog)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("violetred4", "hotpink2"),
                    labels = c("Low Cognitive", "High Cognitive"),
                    name = "Occupation Type") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Cognitive Occupation Placement by Industry",
       subtitle = "Share of workers in high vs. low cognitive occupations",
       x = NULL,
       y = "Share") +
  theme_ireland +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Degree Field distribution by outcome ----

ipums_model %>%
  mutate(degfield_cat = recode(degfield_cat,
                               "no_degree" = "No Degree",
                               "stem_engineering" = "STEM Engineering",
                               "stem_science" = "STEM Science",
                               "stem_math_cs" = "STEM Math/CS",
                               "social_science" = "Social Science",
                               "humanities" = "Humanities",
                               "education" = "Education",
                               "business" = "Business",
                               "other" = "Other"
  )) %>%
  group_by(degfield_cat) %>%
  mutate(pct_high = mean(high_cog == "high")) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(degfield_cat, pct_high), fill = high_cog)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("violetred4", "hotpink2"),
                    labels = c("Low Cognitive", "High Cognitive"),
                    name = "Occupation Type") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Cognitive Occupation Placement by Degree Field",
       subtitle = "Share of workers in high vs. low cognitive occupations",
       x = NULL,
       y = "Share") +
  theme_ireland +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Cross-validation performance ----

tibble(
  Model = c("Logistic Regression", "LASSO", "XGBoost"),
  Accuracy = c(0.781, 0.781, 0.782),
  AUC = c(0.857, 0.857, 0.859)
) %>%
  kable(align = "lcc") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, bold = TRUE)

## Penalty Tuning Plots ----


### LASSO ----

lasso_fit_full %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_line(color = "violetred4", linewidth = 1) +
  geom_point(color = "violetred4", size = 2) +
  geom_vline(xintercept = best_lasso_penalty$penalty,
             lty = 2, color = "gray40") +
  scale_x_log10() +
  labs(title = "Penalty Tuning: Lasso",
       subtitle = "Cross-validated AUC across penalty values",
       x = "Penalty (log scale)",
       y = "Mean AUC (5-fold CV)") +
  theme_ireland


## Test Set Metrics ----
test_metrics <- metric_set(accuracy, roc_auc, sensitivity, 
                           precision, brier_class)

logistic_metrics <- logistic_final %>%
  collect_predictions() %>%
  test_metrics(truth = high_cog, 
               estimate = .pred_class,
               .pred_high,
               event_level = "second") %>%
  mutate(model = "Logistic Regression")

lasso_metrics <- lasso_final %>%
  collect_predictions() %>%
  test_metrics(truth = high_cog,
               estimate = .pred_class,
               .pred_high,
               event_level = "second") %>%
  mutate(model = "LASSO")

xgb_metrics <- xgb_final %>%
  collect_predictions() %>%
  test_metrics(truth = high_cog,
               estimate = .pred_class,
               .pred_high,
               event_level = "second") %>%
  mutate(model = "XGBoost")

#correcting for right brier scores 

# Pull correct brier scores from last_fit objects
brier_scores <- bind_rows(
  collect_metrics(logistic_final) %>% mutate(model = "Logistic Regression"),
  collect_metrics(lasso_final) %>% mutate(model = "LASSO"),
  collect_metrics(xgb_final) %>% mutate(model = "XGBoost")
) %>%
  filter(.metric == "brier_class") %>%
  select(model, brier_class = .estimate)

# Build final metrics table with correct brier scores
final_metrics_table <- bind_rows(logistic_metrics, lasso_metrics, xgb_metrics) %>%
  select(model, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  select(-brier_class) %>%  # drop the wrong brier scores
  left_join(brier_scores, by = "model") %>%
  mutate(across(where(is.numeric), ~round(., 3)))



final_metrics_table %>%
  rename(
    Model = model,
    Accuracy = accuracy,
    Sensitivity = sensitivity,
    Precision = precision,
    AUC = roc_auc,
    `Brier Score` = brier_class
  ) %>%
  kable(align = "lccccc") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE) %>%
  column_spec(1, bold = TRUE)



# ROC curves ----

roc_logistic <- logistic_final %>%
  collect_predictions() %>%
  roc_curve(truth = high_cog, .pred_high,
            event_level = "second") %>%
  mutate(model = "Logistic")

roc_lasso <- lasso_final %>%
  collect_predictions() %>%
  roc_curve(truth = high_cog, .pred_high,
            event_level = "second") %>%
  mutate(model = "LASSO")

roc_xgb <- xgb_final %>%
  collect_predictions() %>%
  roc_curve(truth = high_cog, .pred_high,
            event_level = "second") %>%
  mutate(model = "XGBoost")

## ROC curve plot ----

bind_rows(roc_logistic, roc_lasso, roc_xgb) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line(linewidth = 1, alpha = .5) +
  geom_abline(lty = 2, color = "gray50") +
  scale_color_manual(values = c("violetred4", "hotpink2", "lightcoral")) +
  labs(title = "ROC Curves — Test Set Performance",
       subtitle = "All Three Models",
       x = "1 - Specificity (False Positive Rate)",
       y = "Sensitivity (True Positive Rate)",
       color = "Model") +
  theme_ireland



# Confusion Matrix ----

plot_conf_mat <- function(last_fit_obj, title) {
  last_fit_obj %>%
    collect_predictions() %>%
    conf_mat(truth = high_cog, estimate = .pred_class) %>%
    autoplot(type = "heatmap") +
    scale_fill_gradient(low = "#e8f4f8", high = "#8B2252",
                        labels = scales::comma) +
    labs(title = title,
         x = "Truth",
         y = "Prediction",
         fill = "Count") +
    theme_ireland +
    theme(legend.position = "right")
  

## confusion matrix plots ----
    
  plot_conf_mat(logistic_final, "Confusion Matrix: Logistic Regression")
  plot_conf_mat(lasso_final, "Confusion Matrix: LASSO")
  plot_conf_mat(xgb_final, "Confusion Matrix: XGBoost")
  
  

#Variable Importance ---- 
  
## LASSO coefficients and importance ----
  # Extract LASSO coefficients
  lasso_coefficients <- lasso_final %>%
    extract_fit_parsnip() %>%
    tidy() %>%
    filter(term != "(Intercept)",
           estimate != 0) %>%  # only non-zeroed coefficients
    arrange(desc(abs(estimate)))
  
  
  lasso_importance <- lasso_coefficients %>%
    filter(!str_detect(term, "state_"),
           !str_detect(term, "metro_")) %>%
    mutate(term = recode(term,
                         "degfield_cat_no_degree" = "No College Degree",
                         "ind_cat_retail_trade" = "Industry: Retail Trade",
                         "educ_cat_bachelors" = "Bachelor's Degree",
                         "english_well" = "Speaks English Well",
                         "ind_cat_transportation" = "Industry: Transportation",
                         "educ_cat_associates" = "Associate's Degree",
                         "ind_cat_other" = "Industry: Other",
                         "english_not_well" = "Speaks English Not Well",
                         "ind_cat_manufacturing" = "Industry: Manufacturing",
                         "race_cat_black" = "Race: Black",
                         "degfield_cat_stem_science" = "Degree: STEM Science",
                         "educ_cat_doctoral" = "Doctoral Degree",
                         "ind_cat_arts_entertainment" = "Industry: Arts & Entertainment",
                         "educ_cat_some_college" = "Some College",
                         "ind_cat_other_services" = "Industry: Other Services",
                         "ind_cat_utilities" = "Industry: Utilities",
                         "educ_cat_professional" = "Professional Degree",
                         "educ_cat_hs_diploma" = "High School Diploma",
                         "race_cat_other" = "Race: Other",
                         "race_cat_white" = "Race: White",
                         "ind_cat_education_health" = "Industry: Education & Health",
                         "ind_cat_finance" = "Industry: Finance",
                         "ind_cat_mining_construction" = "Industry: Mining & Construction",
                         "hispanic_not_hispanic" = "Not Hispanic",
                         "english_not_at_all" = "Speaks No English",
                         "ind_cat_information" = "Industry: Information",
                         "ind_cat_public_admin" = "Industry: Public Admin",
                         "educ_cat_ged" = "GED",
                         "age" = "Age",
                         "degfield_cat_stem_math_cs" = "Degree: STEM Math/CS",
                         "degfield_cat_education" = "Degree: Education",
                         "degfield_cat_social_science" = "Degree: Social Science",
                         "degfield_cat_other" = "Degree: Other",
                         "degfield_cat_humanities" = "Degree: Humanities",
                         "nativity_us_born" = "US Born",
                         "degfield_cat_stem_engineering" = "Degree: STEM Engineering",
                         "ind_cat_wholesale_trade" = "Industry: Wholesale Trade",
                         "citizen_not_citizen" = "Not a Citizen",
                         "sex_Female" = "Female",
                         "ind_cat_professional_services" = "Industry: Professional Services"
    ))
  
##LASSO VIP plot ----

  lasso_importance %>%
    ggplot(aes(x = estimate,
               y = reorder(term, estimate),
               fill = estimate > 0)) +
    geom_col() +
    scale_fill_manual(values = c("violetred4", "hotpink2"),
                      labels = c("Negative", "Positive"),
                      name = "Direction") +
    labs(title = "LASSO Coefficients: Non-Geographic Predictors",
         subtitle = "Predictors of High Cognitive Occupation Placement that Survive LASSO Penalization",
         x = "Coefficient Estimate",
         y = NULL) +
    theme_ireland +
    theme(legend.position = "bottom")
  
## XGBOOST coefficients and importance ----
  
  xgb_importance <- xgb_final %>%
    extract_fit_parsnip() %>%
    vip::vi() %>%
    slice_head(n = 20) %>%
    mutate(Variable = recode(Variable,
                             "degfield_cat_no_degree" = "No College Degree",
                             "ind_cat_education_health" = "Industry: Education & Health",
                             "ind_cat_retail_trade" = "Industry: Retail Trade",
                             "educ_cat_masters" = "Master's Degree",
                             "degfield_cat_stem_science" = "Degree: STEM Science",
                             "educ_cat_hs_diploma" = "High School Diploma",
                             "degfield_cat_humanities" = "Degree: Humanities",
                             "ind_cat_finance" = "Industry: Finance",
                             "ind_cat_other" = "Industry: Other",
                             "degfield_cat_education" = "Degree: Education",
                             "ind_cat_transportation" = "Industry: Transportation",
                             "ind_cat_public_admin" = "Industry: Public Admin",
                             "ind_cat_professional_services" = "Industry: Professional Services",
                             "hispanic_not_hispanic" = "Not Hispanic",
                             "degfield_cat_social_science" = "Degree: Social Science",
                             "educ_cat_ged" = "GED",
                             "educ_cat_doctoral" = "Doctoral Degree",
                             "educ_cat_bachelors" = "Bachelor's Degree",
                             "ind_cat_arts_entertainment" = "Industry: Arts & Entertainment",
                             "race_cat_black" = "Race: Black"
    ))
  
## XBOOST VIP plot ----
  
  xgb_importance %>%
    ggplot(aes(x = Importance,
               y = reorder(Variable, Importance))) +
    geom_col(fill = "violetred4") +
    labs(title = "XGBoost Variable Importance",
         subtitle = "Top 20 Predictors of High Cognitive Occupation Placement",
         x = "Importance",
         y = NULL) +
    theme_ireland
# Predicting Cognitive Occupation Placement: A Machine Learning Approach

Classifies U.S. workers into high and low cognitive demand occupations using 2019 ACS microdata linked to O*NET occupational skill ratings, comparing the predictive performance of logistic regression, LASSO, and XGBoost across approximately 800,000 observations.

## Data
- 2019 American Community Survey (ACS), accessed via IPUMS USA
- O*NET Skills database (importance ratings for critical thinking, complex problem solving, active learning, reading comprehension, writing, and mathematics)
- Outcome variable constructed as a binary indicator from a continuous cognitive skill index, split at the median

## Methods
- Logistic regression (interpretable baseline)
- LASSO-penalized logistic regression (L1 regularization for variable selection)
- XGBoost (gradient boosted ensemble for nonlinear detection)
- 80/20 train/test split stratified on outcome; 5-fold cross-validation on training data
- Hyperparameter tuning via grid search on stratified 10% training subsample

## Key Findings
- All three models perform nearly identically: AUC of 0.857 (logistic, LASSO) and 0.859 (XGBoost), suggesting the prediction problem is largely linear in structure
- Absence of a college degree is the single strongest predictor of low cognitive occupation placement, with a LASSO coefficient more than twice the magnitude of any other variable
- Industry effects are the second most important predictor group; retail trade and transportation are strongly negative, finance and education & health strongly positive
- Racial gaps persist after conditioning on education, industry, English proficiency, degree field, and geography

## Files
- `cognitive occupation ML.html` — full analysis with figures, output and code embedded via "show" button 

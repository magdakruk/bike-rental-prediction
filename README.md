# Bike Rental Prediction

An individual university project focused on predicting the number of city bike rentals using statistical and machine learning methods in R.

## Project overview

The project includes exploratory data analysis, data visualisation, feature preparation and the comparison of predictive models.

The main models evaluated in the report were:

- baseline model,
- ridge regression,
- random forest.

The final predictions were prepared using an ensemble of several models, including random forest, gradient boosting, XGBoost, ridge regression and GAM.

## Results

Random forest achieved the best validation result among the models compared in the main report:

- baseline RMSE: approximately 1378.7,
- ridge regression RMSE: approximately 627.0,
- random forest RMSE: approximately 509.3.

## Technologies

- R
- tidyverse
- lubridate
- ranger
- glmnet
- rsample
- statistical analysis
- predictive modelling
- data visualisation

## Files

- `analysis.Rmd` – source report with R code
- `analysis.html` – generated project report
- `kaggle_model.R` – script used to prepare the final predictions
- `submission.csv` – final prediction file

## AI-assisted workflow

Generative AI tools were used extensively to support code creation, debugging, model selection and explanation of selected analytical methods.

I ran the code, reviewed the generated outputs, verified the results and adapted the proposed solutions to the requirements of the university assignment.



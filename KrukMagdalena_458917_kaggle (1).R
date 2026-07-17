packages <- c("data.table", "gbm", "ranger", "xgboost", "glmnet", "mgcv")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

suppressPackageStartupMessages({
  library(data.table)
  library(gbm)
  library(ranger)
  library(xgboost)
  library(glmnet)
  library(mgcv)
})

set.seed(458917)

train_path <- "bike_train.csv"
test_path <- "bike_test.csv"
sample_path <- "sample_submission.csv"
output_path <- "submission.csv"

train <- fread(train_path)
test <- fread(test_path)
sample_submission <- fread(sample_path)

clean_names <- function(dt) {
  bad <- names(dt)[grepl("^Unnamed", names(dt), ignore.case = TRUE)]
  if (length(bad) > 0) dt[, (bad) := NULL]
  setnames(dt, names(dt), trimws(names(dt)))
  dt
}

train <- clean_names(train)
test <- clean_names(test)
sample_submission <- clean_names(sample_submission)

if (!("date" %in% names(train))) stop("Brakuje kolumny date w pliku bike_train.csv")
if (!("date" %in% names(test))) stop("Brakuje kolumny date w pliku bike_test.csv")
if (!("count" %in% names(train))) stop("Brakuje kolumny count w pliku bike_train.csv")

train[, date := as.Date(date)]
test[, date := as.Date(date)]
sample_submission[, date := as.Date(date)]

observed_fixed_holiday <- function(year, month, day) {
  d <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
  w <- as.POSIXlt(d)$wday
  if (w == 6) return(d - 1)
  if (w == 0) return(d + 1)
  d
}

nth_weekday <- function(year, month, weekday_monday0, n) {
  d <- as.Date(sprintf("%04d-%02d-01", year, month))
  wd <- (as.POSIXlt(d)$wday + 6) %% 7
  d + ((weekday_monday0 - wd) %% 7) + 7 * (n - 1)
}

last_weekday <- function(year, month, weekday_monday0) {
  if (month == 12) {
    d <- as.Date(sprintf("%04d-01-01", year + 1)) - 1
  } else {
    d <- as.Date(sprintf("%04d-%02d-01", year, month + 1)) - 1
  }
  wd <- (as.POSIXlt(d)$wday + 6) %% 7
  d - ((wd - weekday_monday0) %% 7)
}

calendar_holidays <- function(year) {
  c(
    observed_fixed_holiday(year, 1, 1),
    nth_weekday(year, 1, 0, 3),
    nth_weekday(year, 2, 0, 3),
    last_weekday(year, 5, 0),
    observed_fixed_holiday(year, 6, 19),
    observed_fixed_holiday(year, 7, 4),
    nth_weekday(year, 9, 0, 1),
    nth_weekday(year, 10, 0, 2),
    observed_fixed_holiday(year, 11, 11),
    nth_weekday(year, 11, 3, 4),
    observed_fixed_holiday(year, 12, 25)
  )
}

is_holiday <- function(dates) {
  years <- unique(as.integer(format(dates, "%Y")))
  days <- as.Date(unlist(lapply(years, calendar_holidays)), origin = "1970-01-01")
  as.integer(dates %in% days)
}

is_black_friday <- function(dates) {
  years <- unique(as.integer(format(dates, "%Y")))
  days <- as.Date(unlist(lapply(years, function(y) nth_weekday(y, 11, 3, 4) + 1)), origin = "1970-01-01")
  as.integer(dates %in% days)
}

start_date <- min(train$date, na.rm = TRUE)

make_features <- function(dt) {
  dt <- copy(dt)
  dt[, month := as.integer(format(date, "%m"))]
  dt[, day := as.integer(format(date, "%d"))]
  dt[, yday := as.integer(format(date, "%j"))]
  dt[, quarter := ((month - 1) %/% 3) + 1]
  dt[, dow := ((as.POSIXlt(date)$wday + 6) %% 7) + 1]
  dt[, weekend := as.integer(dow %in% c(6, 7))]
  dt[, t := as.numeric(date - start_date) + 1]
  dt[, t_scaled := t / 365]
  dt[, t_scaled2 := t_scaled^2]
  
  for (k in 1:6) {
    dt[, paste0("sin_yday_", k) := sin(2 * pi * k * yday / 365)]
    dt[, paste0("cos_yday_", k) := cos(2 * pi * k * yday / 365)]
  }
  
  dt[, sin_dow := sin(2 * pi * dow / 7)]
  dt[, cos_dow := cos(2 * pi * dow / 7)]
  dt[, holiday := is_holiday(date)]
  dt[, workingday := as.integer(weekend == 0 & holiday == 0)]
  dt[, black_friday := is_black_friday(date)]
  dt[, xmas_period := as.integer(month == 12 & day >= 23 & day <= 27)]
  dt[, new_year_period := as.integer(month == 1 & day <= 3)]
  dt[, july4_period := as.integer(month == 7 & day >= 3 & day <= 5)]
  dt[, thanksgiving_period := 0L]
  
  years <- unique(as.integer(format(dt$date, "%Y")))
  for (yy in years) {
    tg <- nth_weekday(yy, 11, 3, 4)
    dt[date >= tg & date <= tg + 3, thanksgiving_period := 1L]
  }
  
  dt[, halloween_period := as.integer(month == 10 & day == 31)]
  dt[, year_end_period := as.integer(month == 12 & day >= 30)]
  dt[, early_march_bad_weather := as.integer(month == 3 & day <= 5 & wcond >= 2)]
  dt[, warm_humid_cloudy := as.integer(month %in% c(5, 6) & wcond == 2 & hum >= 70)]
  dt[, hot_summer_weekend := as.integer(month %in% c(7, 8) & weekend == 1 & temp >= 26)]
  dt[, early_april_long_weekend := as.integer(month == 4 & day <= 15 & dow %in% c(5, 6, 7))]
  dt[, temp2 := temp^2]
  dt[, atemp2 := atemp^2]
  dt[, hum2 := hum^2]
  dt[, wind2 := wind^2]
  dt[, feel_diff := atemp - temp]
  dt[, temp_atemp := temp * atemp]
  dt[, temp_hum := temp * hum]
  dt[, temp_wind := temp * wind]
  dt[, hum_wind := hum * wind]
  dt[, comfort := atemp - 0.15 * hum - 0.40 * wind]
  dt[, good_weather := as.integer(wcond == 1 & temp >= 8 & temp <= 28 & hum <= 78 & wind <= 20)]
  dt[, bad_weather := as.integer(wcond >= 3 | hum >= 86 | wind >= 26)]
  dt[, very_bad_weather := as.integer(wcond >= 3 & hum >= 80 & wind >= 20)]
  dt[, rain_cold := as.integer(wcond >= 2 & temp < 8)]
  dt[, cold_windy := as.integer(temp < 5 & wind > 20)]
  dt[, wcond_f := as.factor(wcond)]
  dt[, month_f := as.factor(month)]
  dt[, dow_f := as.factor(dow)]
  dt[, quarter_f := as.factor(quarter)]
  dt
}

train_f <- make_features(train)
test_f <- make_features(test)
train_f[, log_count := log1p(count)]

features <- setdiff(names(train_f), c("date", "count", "log_count"))
factors <- c("wcond_f", "month_f", "dow_f", "quarter_f")

for (col in factors) {
  train_f[[col]] <- as.factor(train_f[[col]])
  test_f[[col]] <- factor(test_f[[col]], levels = levels(train_f[[col]]))
}

train_df <- as.data.frame(train_f[, c("log_count", features), with = FALSE])
test_df <- as.data.frame(test_f[, features, with = FALSE])
formula_model <- as.formula(paste("log_count ~", paste(features, collapse = " + ")))

limit_pred <- function(x) {
  max_value <- as.numeric(quantile(train_f$count, 0.995, na.rm = TRUE)) * 1.18
  pmin(pmax(as.numeric(x), 0), max_value)
}

pred <- list()

set.seed(458917)
model_gbm_1 <- gbm(
  formula = formula_model,
  data = train_df,
  distribution = "gaussian",
  n.trees = 260,
  interaction.depth = 2,
  shrinkage = 0.040,
  n.minobsinnode = 4,
  bag.fraction = 0.85,
  train.fraction = 1,
  verbose = FALSE
)
pred$gbm_1 <- limit_pred(expm1(predict(model_gbm_1, newdata = test_df, n.trees = 260)))

set.seed(458918)
model_gbm_2 <- gbm(
  formula = formula_model,
  data = train_df,
  distribution = "gaussian",
  n.trees = 330,
  interaction.depth = 3,
  shrinkage = 0.025,
  n.minobsinnode = 5,
  bag.fraction = 0.85,
  train.fraction = 1,
  verbose = FALSE
)
pred$gbm_2 <- limit_pred(expm1(predict(model_gbm_2, newdata = test_df, n.trees = 330)))

set.seed(458919)
model_ranger_1 <- ranger(
  formula = formula_model,
  data = train_df,
  num.trees = 800,
  mtry = max(3, floor(sqrt(length(features)) * 2)),
  min.node.size = 2,
  splitrule = "extratrees",
  respect.unordered.factors = "order",
  seed = 458919
)
pred$ranger_1 <- limit_pred(expm1(predict(model_ranger_1, data = test_df)$predictions))

set.seed(458920)
model_ranger_2 <- ranger(
  formula = formula_model,
  data = train_df,
  num.trees = 800,
  mtry = max(3, floor(length(features) / 3)),
  min.node.size = 3,
  splitrule = "variance",
  respect.unordered.factors = "order",
  seed = 458920
)
pred$ranger_2 <- limit_pred(expm1(predict(model_ranger_2, data = test_df)$predictions))

matrix_formula <- as.formula(paste("~", paste(features, collapse = " + "), "- 1"))
x_train <- model.matrix(matrix_formula, data = train_df)
x_test <- model.matrix(matrix_formula, data = test_df)
y_train <- train_f$log_count

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test)

set.seed(458921)
model_xgb_1 <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    max_depth = 2,
    eta = 0.040,
    subsample = 0.88,
    colsample_bytree = 0.82,
    min_child_weight = 4,
    lambda = 2.0,
    alpha = 0.05
  ),
  data = dtrain,
  nrounds = 260,
  verbose = 0
)
pred$xgb_1 <- limit_pred(expm1(predict(model_xgb_1, dtest)))

set.seed(458922)
model_xgb_2 <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    max_depth = 3,
    eta = 0.025,
    subsample = 0.82,
    colsample_bytree = 0.78,
    min_child_weight = 5,
    lambda = 3.0,
    alpha = 0.10
  ),
  data = dtrain,
  nrounds = 360,
  verbose = 0
)
pred$xgb_2 <- limit_pred(expm1(predict(model_xgb_2, dtest)))

set.seed(458923)
model_ridge <- cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 0,
  nfolds = 10,
  standardize = TRUE,
  type.measure = "mse"
)
pred$ridge <- limit_pred(expm1(as.numeric(predict(model_ridge, newx = x_test, s = "lambda.min"))))

model_gam <- gam(
  log_count ~ s(yday, bs = "cc", k = 24) + s(temp, k = 8) + s(hum, k = 8) +
    s(wind, k = 8) + wcond_f + dow_f + weekend + holiday + xmas_period + new_year_period,
  data = as.data.frame(train_f),
  method = "REML"
)
pred$gam <- limit_pred(expm1(predict(model_gam, newdata = as.data.frame(test_f))))

weights <- c(
  gbm_1 = 0.13,
  gbm_2 = 0.13,
  ranger_1 = 0.17,
  ranger_2 = 0.12,
  xgb_1 = 0.17,
  xgb_2 = 0.17,
  ridge = 0.06,
  gam = 0.05
)

weights <- weights[names(weights) %in% names(pred)]
weights <- weights / sum(weights)
pred_matrix <- do.call(cbind, pred[names(weights)])
pred_final <- as.numeric(pred_matrix %*% weights)

season_factor <- c(1.006, 0.906, 1.046, 1.036, 0.922, 0.964, 0.993, 1.052, 1.070, 1.048, 0.994, 0.997)
pred_final <- pred_final * (1 + 0.85 * (season_factor[test_f$month] - 1))

pred_final[test_f$very_bad_weather == 1] <- pred_final[test_f$very_bad_weather == 1] * 0.20
pred_final[test_f$xmas_period == 1] <- pred_final[test_f$xmas_period == 1] * 0.50
pred_final[test_f$new_year_period == 1] <- pred_final[test_f$new_year_period == 1] * 0.70
pred_final[test_f$thanksgiving_period == 1] <- pred_final[test_f$thanksgiving_period == 1] * 0.60
pred_final[test_f$halloween_period == 1] <- pred_final[test_f$halloween_period == 1] * 0.22
pred_final[test_f$year_end_period == 1] <- pred_final[test_f$year_end_period == 1] * 0.55
pred_final[test_f$warm_humid_cloudy == 1] <- pred_final[test_f$warm_humid_cloudy == 1] * 0.88
pred_final[test_f$early_march_bad_weather == 1] <- pred_final[test_f$early_march_bad_weather == 1] * 0.80
pred_final[test_f$hot_summer_weekend == 1] <- pred_final[test_f$hot_summer_weekend == 1] * 0.88
pred_final[test_f$early_april_long_weekend == 1] <- pred_final[test_f$early_april_long_weekend == 1] * 1.10
pred_final <- limit_pred(pred_final)

prediction <- data.table(date = test_f$date, pred = pred_final)
submission <- merge(sample_submission[, .(date)], prediction, by = "date", all.x = TRUE, sort = FALSE)
submission <- submission[match(sample_submission$date, submission$date)]

if (any(is.na(submission$pred))) stop("Brakuje części predykcji. Sprawdź daty w plikach.")

submission[, date := as.character(date)]
submission[, pred := round(pred, 4)]
submission <- submission[, .(date, pred)]

fwrite(submission, output_path)
cat("Gotowe, zapisano submission.csv\n")

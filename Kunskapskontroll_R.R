# import packages
library("readxl")
library(MASS)
library(dplyr)
library(corrplot)
library(ggplot2)
library(tidymodels)
library(leaps)
library(car)
library(ggpubr)
library(caret)

# import the data
file_path <- "C:/Users/adria/OneDrive/Skrivbord/blocket_data.xlsx"
cars <- read_excel(file_path)
str(cars)
dim(cars)

# Remove variables
cars_updated <- cars %>%
  select(-Index, -Färg, -Märke)
str(cars_updated)
# calculate to days in trafic
calculate_days_in_traffic <- function(data_frame, date_column_name) {
  today <- Sys.Date() # Todays date
  base_date <- as.Date("1899-12-30") # Date for excel serie nummer

  data_frame %>%
    mutate(
      # Make serienummer to date
      Date_Converted = as.Date(as.numeric(!!rlang::sym(date_column_name)), origin = base_date),
      # Make new column
      Dagar_i_trafik = if_else(
        is.na(Date_Converted),
        0L,
        as.integer(today - Date_Converted) # Calculate difference
      )
    )
}

cars_updated <- calculate_days_in_traffic(cars_updated, "Datum_i_trafik")
str(cars_updated)
# remove previous columns
cars_updated <- cars_updated %>%
  select(-Datum_i_trafik, -Date_Converted)
str(cars_updated)
sum(is.na(cars_updated$Dagar_i_trafik))

# EDA
head(cars_updated)
str(cars_updated)
dim(cars_updated)

# convert "hästkrafter" to numerical
cars_updated$Hästkrafter <- gsub("[^0-9.]", "", cars_updated$Hästkrafter)
cars_updated$Hästkrafter <- as.numeric(cars_updated$Hästkrafter)
class(cars_updated$Hästkrafter)

# Handle växellåda column
unique(cars_updated$Växellåda)
cars_updated$Växellåda <- gsub("\r\n", "", cars_updated$Växellåda)
unique(cars_updated$Växellåda)
str(cars_updated)
# check for missing values
missing_values <- colSums(is.na(cars_updated))
print(missing_values)
unique(cars_updated$Motorstorlek)

# convert Motorstorlek to "liter"
cars_updated$Motorstorlek <- gsub("[^0-9.]", "", cars_updated$Motorstorlek)
cars_updated$Motorstorlek <- gsub(",", ".", cars_updated$Motorstorlek)
cars_updated$Motorstorlek <- as.numeric(as.character(cars_updated$Motorstorlek))
cars_updated$Motorstorlek <- ifelse(is.na(cars_updated$Motorstorlek), NA, round(cars_updated$Motorstorlek / 100) / 10)

# impute missing values for electric cars
cars_updated$Motorstorlek[is.na(cars_updated$Motorstorlek) & cars_updated$Bränsle == "El"] <- "Electric_engine"
str(cars_updated)
unique(cars_updated$Motorstorlek)

# check spread of values
ggplot(cars_updated, aes(x = Motorstorlek)) +
  geom_bar()
# replace engine values
cond_small <- c(1.5, 1.6, 1.8, 1.9)
cond_medium <- c(2)
cond_large <- c(2.3, 2.4, 2.5, 3, 3.2)
replace_small <- "Smaller_engine"
replace_medium <- "Medium_engine"
replace_large <- "Bigger_engine"
cars_updated$Motorstorlek <- replace(cars_updated$Motorstorlek, cars_updated$Motorstorlek %in% cond_small, replace_small)
cars_updated$Motorstorlek <- replace(cars_updated$Motorstorlek, cars_updated$Motorstorlek %in% cond_medium, replace_medium)
cars_updated$Motorstorlek <- replace(cars_updated$Motorstorlek, cars_updated$Motorstorlek %in% cond_large, replace_large)
unique(cars_updated$Motorstorlek)
# plot to see spread
ggplot(cars_updated, aes(x = Motorstorlek)) +
  geom_bar()

# check rows where "Biltyp" is missing
Biltyp_NA <- cars_updated %>%
  filter(is.na(Biltyp))
print(Biltyp_NA)
# impute rows where only Biltyp is missing
models_to_impute <- c("V70", "XC70", "V60")
cars_updated$Biltyp[cars_updated$Modell %in% models_to_impute] <- "Kombi"
Biltyp_missing <- cars_updated %>%
  filter(is.na(Biltyp))
print(Biltyp_missing)
# check spread of biltyp
ggplot(cars_updated, aes(x = Biltyp)) +
  geom_bar()
# get rows for Cab, coupe and familjebuss
car_types <- cars_updated %>%
  filter(Biltyp == "Familjebuss" | Biltyp == "Cab" | Biltyp == "Coupé")
print(car_types)
# remove since its small amount of data and not biased
cars_updated <- cars_updated %>%
  filter(!Biltyp %in% c("Familjebuss", "Cab", "Coupé"))
ggplot(cars_updated, aes(x = Biltyp)) +
  geom_bar()

# remove rest of missing values and also modell column
missing_values <- colSums(is.na(cars_updated))
print(sum(missing_values))
cars_updated <- na.omit(cars_updated)
sum(is.na(cars_updated))
dim(cars_updated)
cars_updated <- cars_updated %>%
  select(-Modell)

# dummy encoding
str(cars_updated)
data_recipe <- recipe(~., data = cars_updated) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  prep(training = cars_updated)
cars_processed <- bake(data_recipe, new_data = cars_updated)
str(cars_processed)

# make corrplot
correlation <- cor(cars_processed)
corrplot(correlation, method = "shade")

# perfect correlation between bränsle_El & motor_El
cor(cars_processed$Bränsle_El, cars_processed$Motorstorlek_Electric_engine)
# remove bränsle el
cars_processed <- cars_processed %>%
  select(-Bränsle_El)
str(cars_processed)

# scatter plots
Miltal_plot <- ggplot(cars_updated, aes(x = Försäljningspris, y = Miltal)) +
  geom_point() +
  scale_x_continuous(labels = comma)

# Hästkrafter
Häst_plot <- ggplot(cars_updated, aes(x = Försäljningspris, y = Hästkrafter)) +
  geom_point() +
  scale_x_continuous(labels = comma)

# Modellår
År_plot <- ggplot(cars_updated, aes(x = Försäljningspris, y = Modellår)) +
  geom_point() +
  scale_x_continuous(labels = comma)

# Dagar
trafik_plot <- ggplot(cars_updated, aes(x = Försäljningspris, y = Dagar_i_trafik)) +
  geom_point() +
  scale_x_continuous(labels = comma)

ggarrange(Miltal_plot, Häst_plot, År_plot, trafik_plot + rremove("x.text"),
  ncol = 2, nrow = 2
)

# boxplots
pris_box <- ggplot(cars_updated, aes(x = 1, y = Försäljningspris)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma)

miltal_box <- ggplot(cars_updated, aes(x = 1, y = Miltal)) +
  geom_boxplot()

hästkrafter_box <- ggplot(cars_updated, aes(x = 1, y = Hästkrafter)) +
  geom_boxplot()

dagar_box <- ggplot(cars_updated, aes(x = 1, y = Dagar_i_trafik)) +
  geom_boxplot()

ggarrange(pris_box, miltal_box, hästkrafter_box, dagar_box + rremove("x.text"),
  ncol = 2, nrow = 2
)

# find outlier in "Miltal"
outlier_mil <- cars_processed %>%
  filter(Miltal > 100000)
print(outlier_mil)
# typo, written in km instead of mil
index_outlier <- which(cars_processed$Miltal == 190724)
print(index_outlier)
cars_processed$Miltal[index_outlier] <- 19724
cars_processed[index_outlier, ]
# Outlier in Dagar_i_trafik
outlier_dagar <- cars_processed %>%
  filter(Dagar_i_trafik > 10000)
print(outlier_dagar)
# older cars so nothing wrong

# fit model with all variables
lm.fit <- lm(Försäljningspris ~ ., data = cars_processed)
summary(lm.fit)
# check correlance
vif(lm.fit)
# high corr kombi and suv
cars_prepped <- cars_processed %>%
  mutate(Biltyp_Big = as.integer(Biltyp_SUV == 1 | Biltyp_Kombi == 1), ) %>%
  select(-c(Biltyp_SUV, Biltyp_Kombi))
str(cars_prepped)

# lm_1: without Dagar_i_trafik
lm_1 <- lm(Försäljningspris ~ . - Dagar_i_trafik, data = cars_prepped)
summary(lm_1)
# check corr
vif(lm_1)

# lm_2: removed non significant values
lm_2 <- lm(
  Försäljningspris ~ . - Säljare_Privat - Bränsle_Diesel - Bränsle_Miljöbränsle.Hybrid - Växellåda_Manuell
    - Drivning_Tvåhjulsdriven - Motorstorlek_Smaller_engine - Dagar_i_trafik,
  data = cars_prepped
)
summary(lm_2)
vif(lm_2)

# using subset regression
set.seed(98)
sub.fit <- regsubsets(Försäljningspris ~ ., data = cars_prepped, nvmax = 16)
sub.fit_summary <- summary(sub.fit)
print(sub.fit_summary)
plot(sub.fit_summary$adjr2)
plot(sub.fit, scale = "adjr2")
coef(sub.fit, 7)

# model with 7 variables is the same as model 2
# third model using logic
str(cars_prepped)
lm_3 <- lm(Försäljningspris ~ Miltal + Modellår + Hästkrafter + Säljare_Privat + Bränsle_Miljöbränsle.Hybrid + Växellåda_Manuell
  + Motorstorlek_Medium_engine + Motorstorlek_Electric_engine, data = cars_prepped)
summary(lm_3)
vif(lm_3)
# use Cv to get RMSE
set.seed(99)
train_control <- trainControl(method = "cv", number = 5)

# lm_1
lm_1_cv <- train(Försäljningspris ~ . - Dagar_i_trafik, data = cars_prepped, method = "lm", trControl = train_control)
# lm_2
lm_2_cv <- train(
  Försäljningspris ~ . - Dagar_i_trafik - Säljare_Privat - Bränsle_Diesel - Bränsle_Miljöbränsle.Hybrid
    - Växellåda_Manuell - Drivning_Tvåhjulsdriven - Motorstorlek_Smaller_engine,
  data = cars_prepped, method = "lm", trControl = train_control
)
# lm_3
lm_3_cv <- train(
  Försäljningspris ~ Miltal + Modellår + Hästkrafter + Säljare_Privat + Bränsle_Miljöbränsle.Hybrid
    + Växellåda_Manuell + Motorstorlek_Medium_engine + Motorstorlek_Electric_engine,
  data = cars_prepped, method = "lm", trControl = train_control
)
# compare results
set.seed(101)
results <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3"),
  Adj_R_squared = c(
    summary(lm_1)$adj.r.squared,
    summary(lm_2)$adj.r.squared,
    summary(lm_3)$adj.r.squared
  ),
  BIC = c(BIC(lm_1), BIC(lm_2), BIC(lm_3)),
  AIC = c(AIC(lm_1), AIC(lm_2), AIC(lm_3)),
  RMSE = c(lm_1_cv$results$RMSE, lm_2_cv$results$RMSE, lm_3_cv$results$RMSE)
)
print(results)
# Model 2 got the lowest BIC and AIC but model 1 got the highest Adjr2 and lowest RMSE
par(mfrow = c(2, 2))
plot(lm_2)
summary(lm_2)

# add interaction terms
lm_interaction <- lm(
  Försäljningspris ~ Miltal + Modellår + Hästkrafter + Biltyp_Sedan + Motorstorlek_Electric_engine
    + Motorstorlek_Medium_engine + Biltyp_Big + Motorstorlek_Electric_engine * Hästkrafter,
  data = cars_prepped
)
summary(lm_interaction)
par(mfrow = c(2, 2))
plot(lm_interaction)
# adding interaction terms seems to reduce the pattern slightly

# sqrt variables
lm_sqrt <- lm(
  sqrt(Försäljningspris) ~ sqrt(Miltal) + sqrt(Modellår) + sqrt(Hästkrafter) + Biltyp_Sedan + Motorstorlek_Electric_engine
    + Motorstorlek_Medium_engine + Biltyp_Big + sqrt(Hästkrafter) * Motorstorlek_Electric_engine,
  data = cars_prepped
)
summary(lm_sqrt)
plot(lm_sqrt)

# find outlier
print(cars_prepped[5, ])
# typo obviously since price is very low compared to modellår and miltal
cars_prepped_2 <- cars_prepped[-5, ]
lm_sqrt_final <- lm(
  sqrt(Försäljningspris) ~ sqrt(Miltal) + sqrt(Modellår) + sqrt(Hästkrafter) + Biltyp_Sedan + Motorstorlek_Electric_engine
    + Motorstorlek_Medium_engine + Biltyp_Big + sqrt(Hästkrafter) * Motorstorlek_Electric_engine,
  data = cars_prepped_2
)
summary(lm_sqrt_final)
vif(lm_sqrt_final)
plot(lm_sqrt_final)
# use cross validation to get RMSE
train_control_sqrt <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final"
)
lm_sqrt_cv <- train(
  sqrt(Försäljningspris) ~ sqrt(Miltal) + sqrt(Modellår) + sqrt(Hästkrafter) + Biltyp_Sedan + Motorstorlek_Electric_engine
    + Motorstorlek_Medium_engine + Biltyp_Big + sqrt(Hästkrafter) * Motorstorlek_Electric_engine,
  data = cars_prepped_2, method = "lm", trControl = train_control_sqrt
)

print(lm_sqrt_cv$results)
predictions <- lm_sqrt_cv$pred$pred
observations <- lm_sqrt_cv$pred$obs
predictions_original_scale <- (predictions)^2
observations_original_scale <- (observations)^2

rmse_original <- sqrt(mean((predictions_original_scale - observations_original_scale)^2))


print(paste("RMSE on the original scale:", rmse_original))

# inference, trying model on a car on blocket with price =469900
new_data <- data.frame(
  Miltal = 4141,
  Modellår = 2021,
  Hästkrafter = 414,
  Biltyp_Sedan = 0,
  Motorstorlek_Electric_engine = 1,
  Motorstorlek_Medium_engine = 0,
  Biltyp_Big = 1
)
# Confidence intervall
KI <- predict(lm_sqrt_final, newdata = new_data, interval = "confidence", level = 0.95)
pred_price <- KI[, "fit"]^2
print(pred_price)
conf_low <- KI[, "lwr"]^2
conf_high <- KI[, "upr"]^2
print(c(
  "Predicted Price" = pred_price,
  "Lower Confidence Interval" = conf_low,
  "Upper Confidence Interval" = conf_high
))

# Prediction intervall
PI <- predict(lm_sqrt_final, newdata = new_data, interval = "prediction", level = 0.95)
pred_fit <- PI[, "fit"]^2
print(pred_price)
lwr <- PI[, "lwr"]^2
upr <- PI[, "upr"]^2
print(c(
  "Predicted Price" = pred_fit,
  "Lower prediction Interval" = lwr,
  "Upper prediction Interval" = upr
))


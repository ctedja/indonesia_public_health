# Load packages
library(tidyverse)
library(readr)
library(pROC)
library(broom)


# This is a World Bank public health microdataset of 8,305 individual cases
#   in Indonesia with 294 variables of an old Global Adult Tobacco Survey (GATS).
# Available at: https://extranet.who.int/ncdsmicrodata/index.php/catalog/260/datafile/F1
raw <- read_csv("WHO_Indonesia_Dataset.csv")

# Indonesia has one of the highest per capita smoking rates globally; the public
#   health toll is immense, and tobacco use remains high even as many other
#   countries have seen tobacco use decline. Our hypothesis is that there 
#   may be some variables which may be correlated with:
#     a) whether an individual does or does not smoke
#     b) smokers' willingness or ability to quit.
#   Such explanatory variables, as captured in this dataset, include exposure to 
#   public health messaging and warnings, and on the flipside, exposure to 
#   promotions/advertising for smoking, as well as knowledge, attitudes and
#   practices. Better understanding the impacts of these explanatory variables
#   may help to better design preventive interventions.

# Potential issues: 
#   1)  Smoking is known to be a "nonrational" behaviour problem, and even
#   2)  There's an obvious problem with the fact that questions on environmental 
#   exposure rely on recall, when it's known that advertising often
#   penetrates at a subconscious level.


# Data cleaning ----
data <- raw %>% 
  select(-c(5, 8:71, 74:77, 95:98, 99:126, 127:149, 152:166, 168:193, 195:226, 
            237:238, 263:268)) %>%
  # Let's first classify our variables. Broadly speaking, ones we are interested in
  #    are either:individual traits, smoking habits, exposure to warnings, 
  #   exposure to promos, and knowledge attitudes and practices (KAP)
  # First we have traits.
  rename(trait_caseid = CASEID,
         trait_age = age,
         trait_region = region,
         trait_urbanrural = residence,
         trait_gender = A01,
         trait_education = A04,
         trait_work_status = A05,
         trait_own_electricity = A06a,
         trait_own_flushtoilet = A06b,
         trait_own_fixedtelephone = A06c,
         trait_own_cellphone = A06d,
         trait_own_tv = A06e,
         trait_own_radio = A06f,
         trait_own_fridge = A06g,
         trait_own_car = A06h,
         trait_own_scooter = A06i,
         trait_own_washingmachine = A06j,
         trait_own_computer = A06k,
         trait_own_bicycle = A06l,
         trait_own_boat = A06m,
         trait_own_ac = A06n,
         #  Then we have outcomes
         outcome_quit_tried = D01,
         outcome_quit_length_attempt = D02a,
         outcome_quit_attitude = D08,
         outcome_smoking_current = B01,
         #  Exposure to warnings
         warn_newspapers_whitecig = G201a1,
         warn_newspapers_kretek = G201a2,
         warn_tv_whitecig = G201b1,
         warn_tv_kretek = G201b2,
         warn_radio_whitecig = G201c1,
         warn_radio_kretek = G201c2,
         warn_billboards_whitecig = G201d1,
         warn_billboards_kretek = G201d2,
         warn_somewhere_whitecig = G201e1,
         warn_somewhere_kretek = G201e2,
         warn_packet_whitecig = G202,
         warn_packet_kretek = G203,
         warn_impacts_quit = GG1,
         warn_impacts_health = GG2,
         #  Exposure to promos
         promo_stores_whitecig = G204a1,
         promo_stores_kretek = G204a2,
         promo_tv_whitecig = G204b1,
         promo_tv_kretek = G204b2,
         promo_radio_whitecig = G204c1,
         promo_radio_kretek = G204c2,
         promo_billboards_whitecig = G204d1,
         promo_billboards_kretek = G204d2,
         promo_posters_whitecig = G204e1,
         promo_posters_kretek = G204e2,
         promo_newspapers_whitecig = G204f1,
         promo_newspapers_kretek = G204f2,
         promo_cinemas_whitecig = G204g1,
         promo_cinemas_kretek = G204g2,
         promo_internet_whitecig = G204h1,
         promo_internet_kretek = G204h2,
         promo_publictransport_whitecig = G204i1,
         promo_publictransport_kretek = G204i2,
         promo_publicwalls_whitecig = G204j1,
         promo_publicwalls_kretek = G204j2,
         promo_rel_sports_whitecig = G205,
         promo_rel_sports_kretek = G205a,
         promo_rel_arts_whitecig = G205b,
         promo_rel_arts_kretek = G205c,
         promo_freesamples_whitecig = G206a1,
         promo_freesamples_kretek = G206a2,
         promo_sale_whitecig = G206b1,
         promo_sale_kretek = G206b2,
         promo_coupons_whitecig = G206c1,
         promo_coupons_kretek = G206c2,
         promo_discount_whitecig = G206d1,
         promo_discount_kretek = G206d2,
         promo_rel_cloth_whitecig = G206e1,
         promo_rel_cloth_kretek = G206e2,
         promo_mail_whitecig = G206f1,
         promo_mail_kretek = G206f2,
         #  Knowledge, attitudes and practices (KAP)
         kap_seriousillness = H01,
         kap_stroke = H02a,
         kap_heartattack = H02b,
         kap_lungcancer = H02c,
         kap_copd = H02d,
         kap_bladdercancer = H02e,
         kap_stomachcancer = H02f,
         kap_prematurebirth = H02g,
         kap_boneloss = H02h,
         kap_smokeless_seriousillness = H03,
         kap_passivesmoking = E17) %>%
  # And a quick reorder
  select(c(25:28, 1, 5:24, 30:89, 29, 2:4)) %>%
  # Before we fix our other variables up
  mutate(
    outcome_smoking_current =
      as.logical(
        case_when(outcome_smoking_current == 1 ~ 1,
                  outcome_smoking_current == 2 ~ 1,
                  outcome_smoking_current == 3 ~ 0)),
    outcome_quit_tried =
      as.logical(
        case_when(outcome_quit_tried == 1 ~ 1,
                  outcome_quit_tried == 2 ~ 0)),
    outcome_quit_length_attempt = 
      as.factor(
        case_when(outcome_quit_length_attempt == 1 ~ "months",
                  outcome_quit_length_attempt == 2 ~ "weeks",
                  outcome_quit_length_attempt == 3 ~ "days",
                  outcome_quit_length_attempt == 4 ~ "less_day",
                  outcome_quit_length_attempt == 7 ~ "dont_know")),
    outcome_quit_attitude =
      as.factor(
        case_when(outcome_quit_attitude == 1 ~ "within_month",
                  outcome_quit_attitude == 2 ~ "within_twelve_months",
                  outcome_quit_attitude == 3 ~ "someday_after_twelve_months",
                  outcome_quit_attitude == 4 ~ "not_interested",
                  outcome_quit_attitude == 7 ~ "dont_know")),
    trait_gender = 
      as.factor(
        case_when(trait_gender == 1 ~ "male",
                  trait_gender == 2 ~ "female")),
    trait_urbanrural = 
      as.factor(
        case_when(trait_urbanrural == 1 ~ "urban",
                  trait_urbanrural == 2 ~ "rural")),
    trait_education =
      as.factor(
        case_when(trait_education == 1 ~ "less_primary",
                  trait_education == 2 ~ "primary",
                  trait_education == 3 ~ "secondary",
                  trait_education == 4 ~ "highschool",
                  trait_education == 5 ~ "college",
                  trait_education == 6 ~ "postgrad")),
    trait_work_status =
      as.factor(
        case_when(trait_work_status == 1 ~ "govt",
                  trait_work_status == 2 ~ "nongovt",
                  trait_work_status == 3 ~ "selfemployed",
                  trait_work_status == 4 ~ "student",
                  trait_work_status == 5 ~ "homemaker",
                  trait_work_status == 6  ~ "retired",
                  trait_work_status == 7 ~ "unemployed_able_work",
                  trait_work_status == 8 ~ "unemployed_unable_work"))) %>%
  # And we can also need to deal with the way our explanatory variables are coded
  # In this case, 7 was coded in the original as 'don't know' but for our
  #   purposes, this 'don't know' can be (perhaps naively) assumed to mean 
  #   they cannot recall seeing it.
  mutate_at(vars(c(12:86)), function(x) {
    as.logical(case_when(x == 1 ~ 1,
                         x == 2 ~ 0,
                         x == 7 ~ 0,
                         x == 9 ~ 0,
                         is.na(x) ~ 0))
  }
  )



# Regression algorithms [WORK IN PROGRESS, still EDA] ----
# And now for regressions. With the sheer volume of variables here, we'll
#   see what automated feature selection, stepwise regression algorithm looks like:
# First we'll create a null model to "step up" from.
model_null <- glm(outcome_smoking_current ~ 1, data = data, family = "binomial")

# Now let's start with the variables on whether not respondents noticed warnings
#   Result: ROC AUC = 0.76. Model predicts correctly 81% of the time.
model_smoke_warn <- glm(as.formula(paste(colnames(data)[1], "~",
                                         paste(colnames(data)[c(26:39)], collapse = "+"),
                                         sep = "")), data = data, family = "binomial")

model_smoke_warn_stepwise <- step(model_null, 
                                  scope = list(lower = model_null, 
                                               upper = model_smoke_warn),
                                  direction = "forward")

pred_smoke_warn_stepwise <- predict(model_smoke_warn_stepwise, 
                                    data = data,
                                    type = "response")

ROC_smoke_warn <- roc(data$outcome_smoking_current, 
                      pred_smoke_warn_stepwise)

plot(ROC_smoke_warn, col = "green")
auc(ROC_smoke_warn)

mean(data$outcome_smoking_current == 
       ifelse(pred_smoke_warn_stepwise > mean(data$outcome_smoking_current), T, F))


# Then variables on whether or not respondents noticed or received promotions
#   ROC-AUC = 0.6239. Model predicts correctly 62% of the time.
model_smoke_promo <- glm(as.formula(paste(colnames(data)[1], "~",
                                          paste(colnames(data)[c(40:75)], collapse = "+"),
                                          sep = "")), data = data, family = "binomial")

model_smoke_promo_stepwise <- step(model_null, 
                                   scope = list(lower = model_null, 
                                                upper = model_smoke_promo),
                                   direction = "forward")

pred_smoke_promo_stepwise <- predict(model_smoke_promo_stepwise, 
                                     data = data,
                                     type = "response")

ROC_smoke_promo <- roc(data$outcome_smoking_current, 
                       pred_smoke_promo_stepwise)

plot(ROC_smoke_promo, col = "green")
auc(ROC_smoke_promo)

mean(data$outcome_smoking_current == 
       ifelse(pred_smoke_promo_stepwise > mean(data$outcome_smoking_current), T, F))


# Then variables on scores on knowledge, attitudes and practices (KAP)
#   ROC-AUC = 0.5779 It predicts it correctly 0.56% of the time.
model_smoke_kap <- glm(as.formula(paste(colnames(data)[1], "~",
                                        paste(colnames(data)[c(76:86)], collapse = "+"),
                                        sep = "")), data = data, family = "binomial")

model_smoke_kap_stepwise <- step(model_null, 
                                 scope = list(lower = model_null, 
                                              upper = model_smoke_kap),
                                 direction = "forward")

pred_smoke_kap_stepwise <- predict(model_smoke_kap_stepwise, 
                                   data = data,
                                   type = "response")

ROC_smoke_kap <- roc(data$outcome_smoking_current, 
                     pred_smoke_kap_stepwise)

plot(ROC_smoke_kap, col = "green")
auc(ROC_smoke_kap)

mean(data$outcome_smoking_current == 
       ifelse(pred_smoke_kap_stepwise > mean(data$outcome_smoking_current), T, F))


# Then finally seeing anything with traits. As expected.
#   Result: ROC AUC = 0.5667. Model predicts correctly only 56% of the time.
model_smoke_trait <- glm(as.formula(paste(colnames(data)[1], "~",
                                          paste(colnames(data)[c(12:25)], collapse = "+"),
                                          sep = "")), data = data, family = "binomial")

model_smoke_trait_stepwise <- step(model_null, 
                                   scope = list(lower = model_null, 
                                                upper = model_smoke_trait),
                                   direction = "forward")

pred_smoke_trait_stepwise <- predict(model_smoke_trait_stepwise, 
                                     data = data,
                                     type = "response")

ROC_smoke_trait <- roc(data$outcome_smoking_current, 
                       pred_smoke_trait_stepwise)

plot(ROC_smoke_trait, col = "green")
auc(ROC_smoke_trait)

mean(data$outcome_smoking_current == 
       ifelse(pred_smoke_trait_stepwise > mean(data$outcome_smoking_current), T, F))



# Then testing with a full dataset
#   Result: ROC AUC = 0.8531. Model predicts correctly 80% of the time. Pseudo-R^2 = 0.363
#   This is, blindly, a stronger model than the one with only warnings. But let's see.
#   Also, curiously, result without traits (c(12:76)): ROC AUC = 0.8484. Model predicts 79%. 
model_smoke_full <- glm(as.formula(paste(colnames(data)[1], "~",
                                         paste(colnames(data)[c(12:86)], collapse = "+"),
                                         sep = "")), data = data, family = "binomial")

model_smoke_full_stepwise <- step(model_null, 
                                  scope = list(lower = model_null, 
                                               upper = model_smoke_full),
                                  direction = "forward")

pred_smoke_full_stepwise <- predict(model_smoke_full_stepwise, 
                                    data = data,
                                    type = "response")

ROC_smoke_full <- roc(data$outcome_smoking_current, 
                      pred_smoke_full_stepwise)

plot(ROC_smoke_full, col = "green")
auc(ROC_smoke_full)
model_smoke_full_stepwise
mean(data$outcome_smoking_current == 
       ifelse(pred_smoke_full_stepwise > mean(data$outcome_smoking_current), T, F))

# A few additional tests to look at our model
glance(model_smoke_full_stepwise) %>%
  summarize(pR2 = 1 - deviance/null.deviance)

test <- data %>% mutate(predict = predict(model_smoke_full_stepwise,
                                          type = "response"))

glimpse(test)
library(WVPlots)
GainCurvePlot(test, "predict", "outcome_smoking_current", "GainCurvePlot")


# To-Do ----
# 1) Still need to work on our model, make sure it makes sense (still some issues)
# 2) Some multiple collinearity(?) with the way kretek and whitecig are disaggregated,
#     perhaps this would benefit from aggregating the variables
# 3) See differences for urban/rural

# Unexplored questions: they applied a weighting that we may need to consider.



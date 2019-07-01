library(MASS)
library(data.table)
library(tidyverse)
library(caret)
library(slam)
library(mgcv)
library(plyr)
library(ordinalNet)


# retrieve the working dataset and the stored lists of variables. All scores
# should already be stored in the working set

# root						<- "path"

root						<- "path"
dataDir         <- paste0(root,"data", collapse = "")
outDir          <- paste0(root,"outputs", collapse = "")
utDir           <- paste0(root,"utilities",	collapse = "")

rm("root")

setwd(dataDir)

wset 							<- fread("working_set.csv")
it_post_labels		<- fread("it_post_labels.csv") %>% unlist()
en_post_labels    <- fread("en_post_labels.csv") %>% unlist()
de_post_labels    <- fread("de_post_labels.csv") %>% unlist()

# create variable lists by groups (color, demo)

vars_color  		<- grep("\\bcolor_[^cluster]", colnames(wset), value = T)
vars_demo   		<- grep("\\bdemos_", colnames(wset), value = T)
vars_gender 		<- grep("\\bdemos_gender_", colnames(wset), value = T)
vars_age    		<- grep("\\bdemos_age_", colnames(wset), value = T)
vars_traits 		<- grep("\\bdemos_traits_", colnames(wset), value = T)
vars_pic_topics <- grep("\\bpic_topic_", colnames(wset), value = T)
vars_it_topics	<- grep("\\bpost_topic_it_", colnames(wset), value = T)
vars_en_topics  <- grep("\\bpost_topic_en_", colnames(wset), value = T)
vars_de_topics	<- grep("\\bpost_topic_de_", colnames(wset), value = T)

# if not done already, save the list of columns in a table where you can specify
# (off line) the type of each variable

# variables <- data.table(name = colnames(wset), type = "factor", use = 0)
# setwd(dataDir); fwrite(variables,"variables.csv")



setwd(dataDir); variables <- fread("variables.csv")
for(row in variables[type == "character",name]){wset[,eval(row) := as.character(get(row))]}
for(row in variables[type == "date", name]){wset[,eval(row) := as.Date(get(row))]}
for(row in variables[type == "time", name]){wset[,eval(row) := as.ITime(get(row))]}
for(row in variables[type == "numeric", name]){wset[, eval(row) := as.numeric(get(row))]}
for(row in variables[type == "factor", name]){wset[, eval(row) := as.factor(get(row))]}


for(col in it_post_labels){wset[is.na(get(col)), col] <- 0}
for(col in en_post_labels){wset[is.na(get(col)), col] <- 0}
for(col in de_post_labels){wset[is.na(get(col)), col] <- 0}

factor_columns <- variables[type == "factor", .(name)] %>% unlist()
numeric_groups <- c("age","gender","traits","color","pic_topics","it_topics","en_topics","de_topics")

rm("row","col")

wset[,day_of_week := factor(day_of_week, levels = c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"))]
wset[,month := factor(month, levels = c("january","february","march","april","may","june","july","august","september","october","november","december"))]
wset[,hour_of_day := factor(hour_of_day, levels = as.character(seq(0,23)))]

# create numeric versions of period variables
# wset[,num_month := as.numeric(mapvalues(month, from = levels(month), to = seq(1,12)))]
# wset[,num_dow := as.numeric(mapvalues(day_of_week, from = levels(day_of_week), to = seq(1,7)))]
# wset[,num_hod := as.numeric(mapvalues(hour_of_day, from = levels(hour_of_day), to = seq(0,23)))]
# wset <- wset[summary_color_dominant != ""]

# this section is designed to help the selection of features, in two parts: first the categorical variables, then the numeric ones

# wset[,resp := log1p(like)]
#
# sample_size <- length(wset[,id]) * .1
# wset_sample <- sample(wset[,id], sample_size)
# wset_sample <- wset[id %in% wset_sample]
# wset_part <- createDataPartition(wset_sample[,resp], times = 1, p = 0.8, groups = 10) %>%
# 	unlist() %>% as.character()
# train_set <- wset_sample[row.names(as.data.frame(id)) %in% wset_part]
# test_set <- wset_sample[!(row.names(as.data.frame(id)) %in% wset_part)]
#
# rm("wset_part")

# select the subset of features to use in modeling

# var <- c("facebook_category"); var_adj <- c(paste0(var,"_adj"))
#
# dependant <- "resp ~ "
# predictors <- var_adj
# model_formula <- paste(dependant, predictors) %>% as.formula()
# model <- glm(model_formula, data = train_set, family = "gaussian")
# model_summary <- summary(model)
# model_summary
#
# model_variance_expl <- paste0(round((1 - model$deviance / model$null.deviance) * 100),"%")
# print(model_variance_expl)
# plot model results

# ggplot() +
# 	stat_density(aes(model$residuals), fill = "darkcyan", alpha = 1) +
# 	stat_density(data = wset, mapping = aes(scale(resp)), fill = "darkred", alpha = .25) +
# 	coord_cartesian(xlim = c(-4,4)) +
# 	labs(x = "residuals") +
# 	annotate("text", x = 1, y = 0.3, hjust = 0, label = "log - likes", colour = "darkred") +
# 	annotate("text", x = -2, y = 0.15, hjust = 1, label = "model residuals", colour = "darkcyan") +
# 	geom_vline(xintercept = 0)

# ggplot(train_set, aes(model$fitted.values, resp, colour = category)) +
# 	geom_point() +
# 	geom_smooth(method = "lm") +
# 	labs(x = "predicted", y = "observed") +
# 	geom_abline(slope = 1) +
# 	coord_equal(xlim = c(0,12), ylim = c(0,12))

# plot(model)

# transform categorical variables to exclude unrelevant responses

# retained <- data.table(id = row.names(model_summary$coefficients), sign = model_summary$coefficients[,4])
# retained[,id := gsub(var_adj,"",id)]
# retained <- retained[id != "(Intercept)"]
# retained <- retained[order(sign)][sign < .1,id] %>% as.character()
# wset[get(var) %in% retained, eval(var_adj) := get(var)]
# wset[!(get(var) %in% retained), eval(var_adj) := "other"]
# wset[, eval(var_adj) := relevel(get(var_adj), ref = "other")]
#
# setwd(utDir); filename = paste0(var,"_levels.csv")
# fwrite(as.data.table(retained), filename)

# setwd(utDir);
# factor_predictors <- character()
# for (var in factor_columns){
# 	filename <- paste0(var,"_levels.csv")
# 	factor_levels <- fread(filename, sep = ",")
#  	factor_levels <- length(factor_levels[,retained])
#  	if(factor_levels){factor_predictors <- c(factor_predictors,paste0(var,"_adj"))}
#  }

# NUMERICAL VARIABLE FEATURE SELECTION creates a sample from the working set, then fits
# glm to sets of numeric predictors. Similarly to the above, the resulting coefficients are filtered
# only this time it's variables that are excluded from the model. At the end, a list of numeric predictors
# is created from the saved files.

# wset[,resp := log1p(like)]
# dependant <- "resp ~ "
#
# sample_size <- length(wset[,id]) * .1
# wset_sample <- sample(wset[,id], sample_size)
# wset_sample <- wset[id %in% wset_sample]
#
# vargroup = "traits"
# predictors <- retained
# predictors <- paste0("s(",predictors,")")
# predictors <- paste0(predictors, collapse = " + ")
#
# model_formula <- paste(dependant, predictors) %>% as.formula()
# model <- glm(model_formula, data = wset_sample, family = "gaussian")
# model_summary <- summary(model)
# model_summary
#
# model_variance_expl <- paste0(round((1 - model$deviance / model$null.deviance) * 100, 1),"%")
# print(model_variance_expl)
#
# ggplot() +
# 	stat_density(aes(model$residuals), fill = "darkcyan", alpha = 1) +
# 	stat_density(data = wset, mapping = aes(scale(resp)), fill = "darkred", alpha = .25) +
# 	coord_cartesian(xlim = c(-4,4)) +
# 	labs(x = "residuals") +
# 	annotate("text", x = 1, y = 0.3, hjust = 0, label = "log - likes", colour = "darkred") +
# 	annotate("text", x = -2, y = 0.15, hjust = 1, label = "model residuals", colour = "darkcyan") +
# 	geom_vline(xintercept = 0)
#
# ggplot(wset_sample, aes(model$fitted.values, resp, colour = category)) +
# 	geom_point() +
# 	geom_smooth(method = "lm") +
# 	labs(x = "predicted", y = "observed") +
# 	geom_abline(slope = 1) +
# 	coord_equal(xlim = c(0,12), ylim = c(0,12))

# plot(model)

# retained <- data.table(id = row.names(model_summary$coefficients), sign = model_summary$coefficients[,4])
# retained <- retained[id != "(Intercept)"]
# retained <- retained[sign < .1 & !is.na(sign),id] %>% as.character()
#
# setwd(utDir); filename = paste0(vargroup,"_variables.csv")
# fwrite(as.data.table(retained), filename)
# retained <- gsub("s\\(|\\)","",retained)

# once all the selections are made, build two vectors of variable names, with the list of factor and numeric predictors

setwd(utDir);
factor_predictors <- character()
for (var in factor_columns){
	filename <- paste0(var,"_levels.csv")
	factor_levels <- fread(filename, sep = ",")
	factor_levels <- factor_levels[,retained]
	levels_number <- length(factor_levels)
	if(levels_number){
		predictor <- paste0(var,"_adj")
		wset[get(var) %in% factor_levels, eval(predictor) := get(var)]
		wset[!(get(var) %in% factor_levels), eval(predictor) := "other"]
		factor_predictors <- c(factor_predictors,predictor)
	}
}

numeric_predictors <- character()
for (g in numeric_groups){
	filename <- paste0(g,"_variables.csv")
	group_vars <- fread(filename, sep = ",")
	group_size <- length(group_vars[,retained])
	if(group_size){numeric_predictors <- c(numeric_predictors,group_vars[,retained])}
}

rm("var","g","filename","levels_number","factor_levels","predictor","group_size","group_vars")


wset										<- wset[status == "ok"]
factor_table						<- data.table(id = wset[,id],wset[,..factor_predictors], key = "id")
dummy_vars_formula			<- paste0(factor_predictors,collapse = " + ")
dummy_vars_formula			<- paste("~",dummy_vars_formula) %>% as.formula()
dummies									<- dummyVars(dummy_vars_formula, factor_table, fullRank = F)
factor_table						<- predict(dummies, newdata = factor_table)
factor_dummies					<- gsub("[ ']", "", colnames(factor_table))
colnames(factor_table) 	<- factor_dummies
factor_table						<- data.table(id = wset[,id], factor_table, key = "id")
model_set								<- merge(wset,factor_table, by = "id")

rm("dummy_vars_formula", "dummies", "factor_table")

# discretize likes into quantiles and store the result in the 'response' variable of the working dataset

q <- quantile(model_set[,like], seq(0,1,.2))
l <- c("low", "average", "better than average", "good", "outstanding")
model_set[,response := as.ordered(cut(like, q, include.lowest = T, labels = l))]

rm("q")

# alternatively, log-transform likes
# model_set[,response := log1p(like)]

# setup a sample of the dataset, then split it into a train and test set

sample_size <- round(length(model_set[,id]) * .1)
sample_ids	<- sample(model_set[,id], sample_size)
wset_sample	<- model_set[id %in% sample_ids]
sample_size <- round(length(wset_sample[,id]) * .75)
sample_ids	<- sample(wset_sample[,id],sample_size)
wset_train	<- wset_sample[(id %in% sample_ids)]
wset_test		<- wset_sample[!(id %in% sample_ids)]

# setup the model formula

dependent			<- "response ~ "
predictors		<- c(factor_dummies, gsub("s\\(|\\)","",numeric_predictors))
model_formula <- paste0(predictors, collapse = " + ")
model_formula <- paste0(dependent,model_formula, collapse = "") %>% as.formula()

# fit an adjacent class model: note that the predictors must be turned into a matrix before processing
# variables importance coefficients

predictor_matrix <- as.matrix(wset_train[,..predictors])
ac_model <- ordinalNet(
  predictor_matrix,
  wset_train[,response],
  family = "cumulative",
  reverse = TRUE,
  printIter = TRUE
)

ac_model_coefficients <- coef(ac_model)
ac_model_coefficients <- data.table(coefficient = names(ac_model_coefficients), value = as.vector(ac_model_coefficients))

train_predictions <- predict(ac_model, type = "class")
train_predictions <- factor(train_predictions, labels = l, ordered = T)
train_predictions <- data.table(predicted = train_predictions, observed = wset_train[,response])
train_predictions <- train_predictions[,.(.N), by = c("observed,predicted")]

accuracy <- sum(train_predictions[observed == predicted, N]) / sum(train_predictions[,N])
print(paste0(round(accuracy * 100,1), "%", collapse = ""))

ggplot(train_predictions, mapping = aes(observed,predicted, fill = N)) +
	geom_raster()

test_predictions <- predict(ac_model, newx = as.matrix(wset_test[,..predictors]), type = "class")
test_predictions <- factor(test_predictions, labels = l, ordered = T)
test_predictions <- data.table(predicted = test_predictions, observed = wset_test[,response])
test_predictions <- test_predictions[,.(.N), by = c("observed,predicted")]

accuracy <- sum(test_predictions[observed == predicted, N]) / sum(test_predictions[,N])
print(paste0(round(accuracy * 100,1), "%", collapse = ""))

ggplot(test_predictions, mapping = aes(observed,predicted, fill = N)) +
	geom_raster()

train_predict_response <- predict(ac_model, type = "response")

# alternative modeling pattern

# dependant			<- "response ~ "
# predictors		<- c(factor_predictors, gsub("s\\(|\\)","",numeric_predictors))
# 
# control <- trainControl(verboseIter = T)
# model_gbm			<- train(wset_train[,..predictors],wset_train[,response], method = "gbm")
# 
# model <- model_gbm
# preds <- wset_test[,.(category,response)]
# preds[,predicted := predict(model, wset_test[,..predictors])]
# preds[,residuals := response - predicted]
# 
# visualized <- preds
# 
# ggplot() +
# 	stat_density(data = visualized, aes(scale(predicted)), fill = "darkcyan", alpha = 1) +
# 	stat_density(data = wset_train, mapping = aes(scale(response)), fill = "darkred", alpha = .25) +
# 	coord_cartesian(xlim = c(-4,4)) +
# 	labs(x = "residuals") +
# 	annotate("text", x = 1, y = 0.3, hjust = 0, label = "observed", colour = "darkred") +
# 	annotate("text", x = -2, y = 0.15, hjust = 1, label = "predicted", colour = "darkcyan") +
# 	geom_vline(xintercept = 0) +
# 	facet_wrap(facets = "category")
# 
# ggplot(visualized, aes(predicted, response, colour = category)) +
# 	geom_point() +
# 	geom_smooth(method = "lm") +
# 	labs(x = "predicted", y = "observed") +
# 	geom_abline(slope = 1) +
# 	coord_equal(xlim = c(0,12), ylim = c(0,12)) +
# 	facet_wrap(facets = "category")

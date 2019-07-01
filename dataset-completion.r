library(tm)
library(quanteda)
library(topicmodels)
library(tidytext)
library(plyr)
library(data.table)
library(jsonlite)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(tidyverse)
library(randomForest)
library(caret)
library(slam)
library(stm)
library(mgcv)
library(stringr)
library(ineq)

# retrieve the dataset and the topic modelling results

root						<- "path "
dataDir         <- paste0(root,"data", collapse = "")
outDir          <- paste0(root,"outputs", collapse = "")
utDir           <- paste0(root,"utilities",	collapse = "")

setwd(dataDir)

dset 							<- fread("dataset.csv")
pic_topic_scores	<- fread("pic_topic_scores.csv")
pic_topic_labels	<- fread("pic_topic_labels.csv")
it_post_labels		<- fread("it_post_labels.csv")
en_post_labels    <- fread("en_post_labels.csv")
de_post_labels    <- fread("de_post_labels.csv")
de_post_scores    <- fread("de_post_scores.csv")
en_post_scores    <- fread("en_post_scores.csv")
it_post_scores    <- fread("it_post_scores.csv")
messages					<- fread("messages.csv")

# separate the hashtags and the messages in a dedicated table, remove unprintable or unreadable characters
# if done previously, the messages file should already be available in the data directory

hashtag		<- "#[[:alnum:]]+"
badcoded	<- "<.+>"
url_http	<- "http[[:alnum:][:punct:]]+"
url_www		<- "www[[:alnum:][:punct:]]+"
new_line  <- "[\\n\\r]"
messages	<- dset[,.(id, clean_message = str_replace_all(message, badcoded, ""))]
messages	<- messages[,hashtags := str_extract_all(clean_message, hashtag)]
messages	<- messages[,hashtags := lapply(hashtags, paste, collapse = " ")]
messages	<- messages[,clean_message := str_replace_all(clean_message, hashtag, "")]
messages  <- messages[,clean_message := str_replace_all(clean_message, url_http, "")]
messages  <- messages[,clean_message := str_replace_all(clean_message, url_www, "")]
messages  <- messages[,clean_message := str_replace_all(clean_message, new_line, " ")]
messages	<- messages[,.(id, clean_message = as.character(clean_message), hashtags = as.character(hashtags))]

messages[hashtags == "" | hashtags == "NA" | is.na(hashtags), hashtags := "none"]
messages[clean_message == "" | is.na(clean_message), clean_message := "none"]
messages[,has_tags := ifelse(hashtags != "none", 1, 0)]

rm("hashtag","badcoded","url_http","url_www","root")

# merge all topic scores into the messages table, using "id" as key

messages <- merge(messages, it_post_scores, by = "id", all.x = T) %>%
  merge(en_post_scores, by = "id", all.x = T) %>%
  merge(de_post_scores, by = "id", all.x = T) %>%
  merge(pic_topic_scores, by = "id", all.x = T)

# create a color table with color data and post ids, then add color concentration
# and dominant color (if any)

vars_color <- grep("\\bcolor_[^cluster]", colnames(dset), value = T)

colors_temp <- dset[,c("id", vars_color), with = F] %>%
  melt(id.vars = c("id"), measure.vars = c(vars_color), variable.name = "summary_color_dominant", value.name = "summary_color_dominant_share", na.rm = T) %>%
  filter(summary_color_dominant_share != 0) %>%
  group_by(id)

colors_dominant <- top_n(colors_temp,1,summary_color_dominant_share)
colors_concentr <- summarise(colors_temp, summary_color_concentration = ineq(summary_color_dominant_share, type = "Gini"), summary_color_depth = length(summary_color_dominant))
colors <- merge(colors_dominant, colors_concentr, by = "id")

rm("colors_dominant","colors_concentr","colors_temp")

#	extract dominant topic from the color and messages tables

topic_pic_dominant <- melt(
		pic_topic_scores,
		id.vars = c("id"),
		measure.vars = c(pic_topic_labels$label),
		variable.name = "summary_topic_pic_dominant",
		value.name = "summary_topic_pic_dominant_score",
		na.rm = T
) %>%	group_by(id) %>% top_n(1, summary_topic_pic_dominant_score)

topic_it_dominant <- melt(
	it_post_scores,
	id.vars = c("id"),
	measure.vars = c(it_post_labels$label),
	variable.name = "summary_topic_it_dominant",
	value.name = "summary_topic_it_dominant_score",
	na.rm = T
) %>%	group_by(id) %>% top_n(1, summary_topic_it_dominant_score)

topic_en_dominant <- melt(
	en_post_scores,
	id.vars = c("id"),
	measure.vars = c(en_post_labels$label),
	variable.name = "summary_topic_en_dominant",
	value.name = "summary_topic_en_dominant_score",
	na.rm = T
) %>% group_by(id) %>% top_n(1, summary_topic_en_dominant_score)

topic_de_dominant <- melt(
	de_post_scores,
	id.vars = c("id"),
	measure.vars = c(de_post_labels$label),
	variable.name = "summary_topic_de_dominant",
	value.name = "summary_topic_de_dominant_score",
	na.rm = T
) %>% group_by(id) %>% top_n(1, summary_topic_de_dominant_score)

# remix all the transformations into the dataset and save a working version for processing in
# script 03_exploration

working_set <- merge(dset,messages, by = "id", all.x = T) %>%
  merge(colors, by = "id", all.x = T) %>%
	merge(topic_pic_dominant, by = "id", all.x = T) %>%
	merge(topic_it_dominant, by = "id", all.x = T) %>%
	merge(topic_de_dominant, by = "id", all.x = T) %>%
	merge(topic_en_dominant, by = "id", all.x = T)

rm("topic_pic_dominant","topic_it_dominant","topic_en_dominant","topic_de_dominant")

fwrite(working_set,"working_set.csv")

# Data preProcess()
library(zoo)
library(tidyverse)
train = read.csv("./data/train.csv")
tests = read.csv("./data/test.csv")

#--change name to title------------------------------------------
train$title = str_extract(train$Name_wiki, '\\w+\\.')
tests$title = str_extract(tests$Name_wiki, '\\w+\\.')

train = merge(train, train %>% group_by(title) %>% summarise(n()), by = 'title')
tests = merge(tests, tests %>% group_by(title) %>% summarise(n()), by = 'title')

train$title[train$`n()` < 8] = 'rare'
tests$title[tests$`n()` < 8] = 'rare'

#--create "alone"------------------------------------------
drop1 = c('Pclass', 'Age', 'Name', 'Embarked', 
          'Cabin', 
          # "PassengerId", 
          "n()", "WikiId")
train = train[, !names(train) %in% drop1]
tests = tests[, !names(tests) %in% drop1]

train$alone = "0"
train$alone[train$SibSp == 0] = "1"
tests$alone = "0"
tests$alone[tests$SibSp == 0] = "1"
#--modify cabin class------------------------------------------
# train$Cabin2 = c(train$Cabin %>% str_sub(1, 1))
# train$Cabin2[which((train$Cabin2) == "")] = "N"
# 
# tests$Cabin2 = c(tests$Cabin %>% str_sub(1, 1))
# tests$Cabin2[which((tests$Cabin2) == "")] = "N"
#--ticket manipulation------------------------------------------
train$Ticket_num = "0"
train$Ticket_num[which(is.na(str_extract(train$Ticket, '^\\d')) == F)] = "1"
tests$Ticket_num = "0"
tests$Ticket_num[which(is.na(str_extract(tests$Ticket, '^\\d')) == F)] = "1"

train$Ticket = gsub('[[:punct:]]', "", train$Ticket)
tests$Ticket = gsub('[[:punct:]]', "", tests$Ticket)

train$Ticket_num_len = grepl('^[[:alpha:]]+', train$Ticket)
tests$Ticket_num_len = grepl('^[[:alpha:]]+', tests$Ticket)
#--ticket manipulation2------------------------------------------
train$Ticket_num_len[train$Ticket_num == "1"] = 
  sapply(
    sapply(train$Ticket[train$Ticket_num == "1"], 
           as.numeric), 
    str_length) %>% array()

tests$Ticket_num_len[tests$Ticket_num == "1"] = 
  sapply(
    sapply(tests$Ticket[tests$Ticket_num == "1"], 
           as.numeric), 
    str_length) %>% array()

#--drop some------------------------------------------
# names(train)
dropList = c("Cabin", "Ticket", "Ticket_num", "Ticket_Loc")
train = train[, !names(train) %in% dropList]
tests = tests[, !names(tests) %in% dropList]
#--save_image------------------------------------------
# save.image("~/Downloads/booooks/semester5/pythonAAE/XGB_titanic/clean.RData")

#--carry_on-----------------------------------------
library(mice)
md.pattern(tests)
md.pattern(train)
# method 1, simple fill na in Age column:
tt_miss = which(is.na(train$title) == T)
tt_mis2 = which(is.na(tests$title) == T)
train$title[tt_miss] = str_extract(train$Name_wiki[tt_miss], "Miss|Master")
tests$title[tt_mis2] = str_extract(tests$Name_wiki[tt_mis2], "Miss|Master")

train$Age_wiki = na.fill(train$Age_wiki, "extend")
tests$Age_wiki = na.fill(tests$Age_wiki, "extend")
train$title = na.fill(train$title, "Mr.")
tests$title = na.fill(tests$title, "Mr.")

train$Class = na.fill(train$Class, "extend")
tests$Class = na.fill(tests$Class, "extend")
tests$Fare = na.fill(tests$Fare, "extend")
 
tests = tests[order(tests$PassengerId),] %>% data.frame(row.names = c(1:418))

tests = tests[, !names(tests) %in% "PassengerId"]

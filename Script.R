##############################################################################

## IMPORTANT FOREWORD - REQUIREMENTS

# In this project, there are two programs: Script.R and Report.Rmd. 

# Script.R has been run in 
# RStudio Version 1.1.456 - © 2009-2018 RStudio, Inc. 

# Report.Rmd has been knitted to HTML in 
# RStudio Version 1.1.456 - © 2009-2018 RStudio, Inc.

# The version of R that I use is
# R version 3.5.1 (2018-07-02) -- "Feather Spray"
# Copyright (C) 2018 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)

# The operating system on my PC is Windows 10.

# I cannot guarantee Script.R running on other versions. 
# Neither can I guarantee Report.Rmd being knitted on other versions.

                              ###

# Code is included both in this file (Sript.R) and in Report.Rmd. 

# When you knit Report.Rmd, please knit it to HTML. It can then easily be issued
# in PDF format as Report.pdf. On my laptop, running Script.R takes approximately 
# a quarter of an hour. Knitting Report.Rmd to HTML takes also more or less 
# a quarter of an hour. 

# Technical comments about code are included in this script (Script.R), analysis 
# of data and results is included in Report.Rmd and Report.pdf.

##############################################################################

## CLEANING USER INTERFACE FOR RAM MANAGEMENT
# The function invisible() is used to prevent technical information showing up 
# in Report.Rmd and, most important, in Report.pdf. That piece of information 
# would have no operational impact. 

# Clearing plots, cleaning workspace and console.
invisible(if(!is.null(dev.list())) dev.off())
rm(list=ls())
cat("\014")

#################################################################################

## LOADING PACKAGES
# P.S. kableExtra helps reshape tables.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

#################################################################################

## PALLIATING CLASH BETWEEN PACKAGES
# I have already loaded tidyverse and thus dplyr. Later on I will also load MASS. 
# dplyr and MASS both have a select() function that can mask each other. To prevent 
# dysfuntioning, I will particularize the select() function from dplyr.
dselect <- dplyr::select 
# I will systematically use dselect() instead of select().

#################################################################################

## DOWNLOADING DATA
myfile <- "https://raw.githubusercontent.com/Dev-P-L/Bank-Marketing/master/bank.csv"
bank <- read.csv(myfile)
rm(myfile)

#################################################################################

## PREPARING AND SPLITTING DATA
# Renaming the no/yes values as "no_deposit" and "deposit" to have "deposit" as positive. 
bank <- bank %>% mutate(y = as.character(y)) 
bank$y <- gsub("no", "no_deposit", bank$y)
bank$y <- gsub("yes", "deposit", bank$y)
bank <- bank %>% mutate(y = as.factor(y)) %>% as.data.frame()

# Creating the required 30% validation set that will only be used at the very last step
# only with the final model. 
set.seed(1)
ind <- createDataPartition(y = bank$y, times = 1, p = 0.3, list = FALSE)
temp <- bank[-ind,]
bank_val <- bank[ind,]
rm(ind)

# Splitting temp into a training set and a test set, which will both be used to train models.
set.seed(1)
ind <- createDataPartition(y = temp$y, times = 1, p = 0.2, list = FALSE)
bank_train <- temp[-ind,]
bank_test <- temp[ind,]
rm(ind, temp)

#################################################################################

## ATTRIBUTES CONTENT
# Target or dependent variable - What's the type in R and the meaning in marketing? 
tab <- data.frame(Variable_Name = "y", 
  Type = "Binary categorical - Response: deposit/no_deposit", 
  Meaning = "Has the customer subscribed a term deposit?") 
kable(tab, "html", align = "c") %>% kable_styling(bootstrap_options = "bordered", 
  full_width = F, font_size = 14)


# Predictors - Building up a descriptive table. First, name of variables and type in R.
temp <- bank_train %>% dselect(- y)
predictors_tab <- sapply(temp, class) %>% as.data.frame() %>% setNames("type")
predictors_tab <- data.frame(Variable = row.names(predictors_tab), 
                             R_Type = predictors_tab$type)

# Then, meaning of variables in marketing
Meaning_in_Marketing_Campaign <- c("Age in years", "Professional status",
  "Marital status", "Education", "Credit in default?", "Account balance",
  "Home loan?", "Personal loan?",
  "Medium of communication with this customer in this campaign",
  "Day (of the month) of last contact with this customer in this campaign",
  "Month of last contact with this customer in this campaign",
  "Duration in seconds of last contact with this customer in this campaign",
  "Number of contacts with this customer in this campaign",
  "Number of days since last contact with this customer in previous campaign",
  "Number of contacts with this customer before this campaign",
  "Outcome from previous marketing campaign for this customer")

# Types of information in marketing
Information_Type <- c(seq(1,1, length.out = 4), seq(2,2, length.out = 4),
                      seq(3, 3, length.out = 5), seq(4, 4, length.out = 3))
Information_Type <- gsub(1, "Customer's NON-FINANCIAL Profile", Information_Type)
Information_Type <- gsub(2, "Customer's FINANCIAL Profile", Information_Type)
Information_Type <- gsub(3, "THIS Marketing Campaign", Information_Type)
Information_Type <- gsub(4, "PREVIOUS Marketing Campaign", Information_Type)

# Assembling into a predictor table.
predictors_tab <- data.frame(predictors_tab, Meaning_in_Marketing_Campaign, 
                             Information_Type)
kable(predictors_tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = c("bordered", "condensed"), 
                full_width = F, font_size = 14) %>% 
  column_spec(1:2, width = "1in") %>% column_spec(3:4, width = "3.5in")
rm(tab, temp, Meaning_in_Marketing_Campaign, Information_Type, predictors_tab)

#################################################################################

## EXPLORATORY ANALYSIS AND VISUALIZATION IN TRAINING SET
# For analytical comments and insights, please see Report.Rmd and Report.pdf. 

# Target or dependent variable, y
tab <- summary(bank_train$y)
stats <- tab %>% as.vector()
total = sum(stats)
tab <- data.frame(y_value = names(tab), Occurrences_in_Training_Set = stats, 
                  Percentage = (stats / total) * 100) %>% 
  mutate(Percentage = format(Percentage, digits = 3, nsmall = 1))
kable(tab, "html", align = "c") %>% kable_styling(bootstrap_options = "bordered", 
  full_width = F, font_size = 14) %>% column_spec(1:3, width = "2.5in")
rm(stats, total, tab)

# Histogram according to age in training set
graph <- bank_train %>% dselect(age) %>% as.data.frame() %>% 
  ggplot(aes(age)) + 
  geom_histogram(bins = 30, color = "#007ba7", fill = "#9bc4e2") +
  ggtitle("Customers per Age in Training Set") + xlab("Age") + ylab("Count of Customers") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph
rm(graph)

# Subscription percentages according to age in training set
tab <- bank_train %>% dselect(y, age) %>% 
  mutate(age = cut(as.numeric(age), breaks = c(19, 25, 30, 35, 40, 45, 50, 55, 61, 88), 
                   right = FALSE, dig.lab = 5)) %>%
  mutate(y = as.numeric(y) - 1) %>% group_by(age) %>%
  summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>% 
  mutate(percent_yes = format(percent_yes, digits = 1, nsmall = 0)) %>% 
  arrange(desc(percent_yes)) %>%
  rename(Age = age, Number_of_Customers = n, Subscription_Percentage = percent_yes) %>% 
  as.data.frame()
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = c("bordered", "condensed"), 
  full_width = F, font_size = 14) %>% column_spec(1:3, width = "2in")
rm(tab)

# Exploratory analysis and visualization of job in training set - Barplot 
graph <- bank_train %>% dselect(job) %>% group_by(job) %>% 
  summarize(n = n()) %>% as.data.frame() %>% mutate(job = reorder(job, desc(n))) %>%
  ggplot(aes(job, n)) + 
  geom_bar(stat = "identity", width = 0.40, color = "#007ba7", fill = "#9bc4e2") + 
  ggtitle("Customers per Professional Status in Training Set") +
  xlab("Professional Status") + ylab("Count of Customers") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12))
graph
rm(graph)

# Subscription percentages according to job in training set
tab <- bank_train %>% dselect(y, job) %>% mutate(y = as.numeric(y) - 1) %>% 
  group_by(job) %>% summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>%  
  mutate(percent_yes = format(percent_yes, digits = 0, nsmall = 0)) %>%
  arrange(desc(percent_yes)) %>%
  rename(Professional_Status = job, Number_of_Customers = n, 
         Subscription_Percentage = percent_yes) %>% as.data.frame()
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = c("bordered", "condensed"), 
                full_width = F, font_size = 14) %>% column_spec(1:3, width = "2in")
rm(tab)

# Marital status in training set
tab <- bank_train %>% dselect(y, marital) %>% 
  mutate(y = as.numeric(y) - 1) %>% group_by(marital) %>%
  summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>%  
  mutate(percent_yes = format(percent_yes, digits = 2, nsmall = 0)) %>%
  arrange(desc(percent_yes)) %>%
  rename(Marital_Status = marital, Number_of_Customers = n, 
         Subscription_Percentage = percent_yes) %>% as.data.frame()
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 14) %>% 
  column_spec(1:3, width = "2in")
rm(tab)

# Education in training set
tab <- bank_train %>% dselect(y, education) %>% 
  mutate(y = as.numeric(y) - 1) %>% group_by(education) %>%
  summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>%  
  mutate(percent_yes = format(percent_yes, digits = 1, nsmall = 0)) %>%
  arrange(desc(percent_yes)) %>%
  rename(Education_Level = education, Number_of_Customers = n, 
         Subscription_Percentage = percent_yes) %>% as.data.frame()
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 14) %>% 
  column_spec(1:3, width = "2in")
rm(tab)

# Credit default in training set
tab <- bank_train %>% dselect(y, default) %>% 
  mutate(y = as.numeric(y) - 1) %>% group_by(default) %>%
  summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>%  
  mutate(percent_yes = format(percent_yes, digits = 1, nsmall = 0)) %>%
  arrange(desc(percent_yes)) %>%
  rename(Credit_Default = default, Number_of_Customers = n, 
         Subscription_Percentage = percent_yes) %>% as.data.frame()
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 14) %>% 
  column_spec(1:3, width = "2in")
rm(tab)

# Account balance in training set
tab <- bank_train %>% dselect(y, balance) %>% 
  mutate(balance = cut(as.numeric(balance), 
         breaks = c(- 5000, - 1000, 0, 0.1, 1000, 5000, 10000, 15000, 75000), 
         right = FALSE, dig.lab = 5)) %>%
  mutate(y = as.numeric(y) - 1) %>% 
  group_by(balance) %>% summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>%  
  mutate(percent_yes = format(percent_yes, digits = 1, nsmall = 0)) %>%
  rename(Account_Balance = balance, Number_of_Customers = n, 
         Subscription_Percentage = percent_yes) %>% as.data.frame()
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = c("bordered", "condensed"), 
  full_width = F, font_size = 14) %>% column_spec(1:3, width = "2in")
rm(tab)

# Home loan in training set
tab <- bank_train %>% dselect(y, housing) %>% 
  mutate(y = as.numeric(y) - 1) %>% group_by(housing) %>% 
  summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>%  
  mutate(percent_yes = format(percent_yes, digits = 1, nsmall = 0)) %>%
  rename(Home_Loan = housing, Number_of_Customers = n, 
         Subscription_Percentage = percent_yes) %>% as.data.frame()
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 14) %>% 
  column_spec(1:3, width = "2in")
rm(tab)

# Personal loan in training set
tab <- bank_train %>% dselect(y, loan) %>% 
  mutate(y = as.numeric(y) - 1) %>% group_by(loan) %>% 
  summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>%  
  mutate(percent_yes = format(percent_yes, digits = 1, nsmall = 0)) %>%
  rename(Personal_Loan = loan, Number_of_Customers = n, 
         Subscription_Percentage = percent_yes) %>% as.data.frame() %>%
  as.data.frame()
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 14) %>% 
  column_spec(1:3, width = "2in")
rm(tab)

# Medium of communication in this campaign - Training set
tab <- bank_train %>% dselect(y, contact) %>% mutate(y = as.numeric(y) - 1) %>% 
  group_by(contact) %>% summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>%  
  mutate(percent_yes = format(percent_yes, digits = 1, nsmall = 0)) %>%
  rename(Medium_of_Communication = contact, Number_of_Customers = n, 
         Subscription_Percentage = percent_yes) %>% as.data.frame()
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 14) %>% 
  column_spec(1:3, width = "2.5in")
rm(tab)

# Day of the month of last contact: numbers of customers - Training set
graph <- bank_train %>% dselect(day) %>% group_by(day) %>% 
  summarize(n = n()) %>% as.data.frame() %>% 
  ggplot(aes(day, n)) + 
  geom_bar(stat = "identity", width = 0.40, color = "#007ba7", fill = "#9bc4e2") + 
  ggtitle("Customers per Day of the Month of Last Contact") +
  xlab("Day of the Month") + ylab("Count of Customers") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
graph
rm(graph)

# Day of the month of last contact: average subscription percentages - Training set
graph <- bank_train %>% dselect(y, day) %>% 
  mutate(y = as.numeric(y) - 1) %>% group_by(day) %>%
  summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>% as.data.frame() %>% 
  ggplot(aes(day, percent_yes)) + 
  geom_bar(stat = "identity", width = 0.40, color = "#007ba7", fill = "#9bc4e2") + 
  ggtitle("Subscription Percentage per Day of Last Contact") +
  xlab("Day of the Month") + ylab("Subscription Percentage") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
graph
rm(graph) 

# Month of the last contact in this campaign: numbers of customers - Training set
bank_train$month_ord = factor(bank_train$month, ordered = TRUE, levels = c("jan", "feb", 
  "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
graph <- bank_train %>% dselect(month_ord) %>% group_by(month_ord) %>% 
  summarize(n = n()) %>% as.data.frame() %>% 
  ggplot(aes(month_ord, n)) + 
  geom_bar(stat = "identity", width = 0.40, color = "#007ba7", fill = "#9bc4e2") + 
  ggtitle("Customers per Month of Last Contact") +
  xlab("Month") + ylab("Count of Customers") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12))
graph
rm(graph)

# Month of the last contact in this campaign: subscription percentages - Training set
graph <- bank_train %>% dselect(y, month_ord) %>% 
  mutate(y = as.numeric(y) - 1) %>% group_by(month_ord) %>% 
  summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>%  
  as.data.frame() %>%
  ggplot(aes(x = month_ord, y = percent_yes)) + 
  geom_bar(stat = "identity", width = 0.40, color = "#007ba7", fill = "#9bc4e2") + 
  ggtitle("Subscription Percentage per Month of Last Contact") +
  xlab("Month") + ylab("Subscription Percentage") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12))
graph
rm(graph)
bank_train$month_ord <- NULL

# Number of contacts with customer in this campaign - Training set 
tab <- bank_train %>% dselect(y, campaign) %>% 
  mutate(campaign = cut(as.numeric(campaign), breaks = c(1, 2, 3, 5, 10, 51), 
                        right = FALSE, dig.lab = 5)) %>%
  mutate(y = as.numeric(y) - 1) %>% group_by(campaign) %>% 
  summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>%  
  mutate(percent_yes = format(percent_yes, digits = 1, nsmall = 0)) %>%
  rename(Number_of_Contacts_in_this_Campaign = campaign, Number_of_Customers = n, 
         Subscription_Percentage = percent_yes) %>% as.data.frame()
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 14) %>% 
  column_spec(2:3, width = "2in")
rm(tab)

# Number of days since last contact in previous campaign - Training set
tab <- bank_train %>% dselect(y, pdays) %>% 
  mutate(pdays = cut(as.numeric(pdays), 
                     breaks = c(- 1, 0, 0.1, 50, 100, 250, 500, 1000), 
                     right = FALSE, dig.lab = 5)) %>%
  mutate(y = as.numeric(y) - 1) %>% group_by(pdays) %>% 
  summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>%  
  mutate(percent_yes = format(percent_yes, digits = 1, nsmall = 0)) %>%
  rename(Days_since_Last_Contact_in_Previous_Campaign = pdays, 
         Number_of_Customers = n, Subscription_Percentage = percent_yes) %>% 
  as.data.frame()
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 14) %>% 
  column_spec(2:3, width = "2.5in")
rm(tab)

# Number of contacts with customers before this campaign - Training set
tab <- bank_train %>% dselect(y, previous) %>% 
  mutate(previous = cut(as.numeric(previous), breaks = c(0, 0.1, 5, 10, 26), 
                        right = FALSE, dig.lab = 5)) %>%
  mutate(y = as.numeric(y) - 1) %>% group_by(previous) %>% 
  summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>% 
  mutate(percent_yes = format(percent_yes, digits = 1, nsmall = 0)) %>%
  rename(Number_of_Previous_Contacts = previous, Number_of_Customers = n, 
         Subscription_Percentage = percent_yes) %>% as.data.frame()
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 14) %>% 
  column_spec(1:3, width = "2.5in")
rm(tab)

# Outcome from previous marketing campaign - Training set
tab <- bank_train %>% dselect(y, poutcome) %>% 
  mutate(y = as.numeric(y) - 1) %>% group_by(poutcome) %>% 
  summarize(n = n(), percent_yes = (1 - (sum(y) / n)) * 100) %>%  
  mutate(percent_yes = format(percent_yes, digits = 1, nsmall = 0)) %>%
  rename(Outcome_from_Previous_Campaign = poutcome, Number_of_Customers = n, 
         Subscription_Percentage = percent_yes) %>% as.data.frame()
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 14) %>% 
  column_spec(1:3, width = "1.5in")
rm(tab)

##############################################################################

## DATA PREPARATION
bank_train$duration <- NULL
bank_test$duration <- NULL
bank_val$duration <- NULL

##############################################################################

## MODELING ON TRAINING SET
# Choosing 4 models available in caret and downloading packages. 
models <- c("glm", "lda", "rf", "gbm")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")

# Applying the train() function. 
part <- c("glm", "lda", "rf")
fits <- lapply(part, function(model){
  set.seed(1)
  train(y ~ ., method = model, data = bank_train)
  }) 
set.seed(1)
gbm <- train(y ~ ., method = "gbm", data = bank_train, verbose = FALSE)
fits[[4]] <- gbm
names(fits) <- models
rm(part, gbm)

# Getting fitted values for all models.
fitted <- sapply(fits, function(object) predict(object)) %>% as.data.frame()

# Computing the numbers of subscribers and of customers.
subs_number <- bank_train %>% filter(y == "deposit") %>% summarize(n = n()) %>% .$n
cust_number <- nrow(bank_train)

# Calculating percentages of subscribers reached for each model. 
len_mod <- length(models)
seq_mod <- 1:len_mod
sensitivities <- sapply(seq_mod, function(i) 
  sensitivity(as.factor(fitted[, i]), bank_train$y))
subs_reached_pct <- as.vector(sensitivities) * 100

# Calculating percentages of not contacted customers for each model.
posPredValues <- sapply(seq_mod, function(i) 
  posPredValue(as.factor(fitted[, i]), bank_train$y))
TP <- round(subs_number * subs_reached_pct / 100)
FP <-  TP * (1 - posPredValues) / posPredValues
cust_not_contacted_pct <- (cust_number - TP - FP) / cust_number * 100
rm(sensitivities, posPredValues)

# Table of results, called results_tab_train_50 because it is calculated with 
# the default probability threshold 0.50 to decide whether it is "deposit" or "no_deposit". 
results_tab_train_50 <- data.frame(Model = as.character(models), 
  Subscribers_Reached = subs_reached_pct, 
  Global_Coverage_Reduction = cust_not_contacted_pct) %>%
  arrange(Subscribers_Reached) %>% as.data.frame()

# Reformating table of results on training set. 
tab <- results_tab_train_50 %>%  
  mutate(Subscribers_Reached = 
         format(Subscribers_Reached, digits = 0, nsmall = 0)) %>% 
  mutate(Subscribers_Reached = paste(Subscribers_Reached, "%", sep = " ")) %>%
  mutate(Global_Coverage_Reduction = 
         format(Global_Coverage_Reduction, digits = 0, nsmall = 0)) %>%
  mutate(Global_Coverage_Reduction = paste(Global_Coverage_Reduction, "%", sep = " "))
kable(tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 16) %>% 
  column_spec(1:2, width = "1.5in") 
rm(tab)

# Checking up on rf overfitting: table with FP and FN.
FN <- subs_number - TP 
check_up_tab <- data.frame(Model = as.character(models), FP = FP, FN = FN) %>% 
  as.data.frame() %>%  mutate(FP = format(FP, digits = 0, nsmall = 0)) %>% 
  mutate(FN = format(FN, digits = 0, nsmall = 0)) 
kable(check_up_tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 14) %>% 
  column_spec(1:3, width = "1.5in")
rm(FN, FP, TP, check_up_tab)

# Graph with first results from 4 models on the training set + target area
graph <- results_tab_train_50 %>%  
  ggplot(aes(x = Subscribers_Reached, y = Global_Coverage_Reduction, label = Model)) + 
  geom_point(size = 3, color = "#007ba7") + geom_text(nudge_y = 5) +
  geom_rect(aes(xmin = 75, xmax = 100, ymin = 50, ymax = 100), 
            fill = "#9bc4e2", alpha = 0.1) +
  ggtitle("First Results on Training Set") +
  xlim(0, 105) + xlab("Subscribers Reached (%)") + 
  ylim(0, 105) + ylab("Global Coverage Reduction (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
graph
rm(graph)
rm(cust_not_contacted_pct, subs_reached_pct, results_tab_train_50)

# Retrieving probabilities for the 4 models on the training set.
prob_train <- lapply(fits, function(object) predict(object, type = "prob"))
seq <- seq(1, (len_mod * 2) - 1, 2)
prob_train <- prob_train %>% as.data.frame() %>% dselect(seq)
names(prob_train) <- gsub(".deposit", "", names(prob_train))
rm(seq)

# Illustrative graph with probabilities from lda models
graph_prob_lda <- prob_train %>% dselect(lda) %>% ggplot(aes(lda)) + 
  geom_histogram(bins = 30, color = "#007ba7", fill = "#9bc4e2") + 
  ggtitle("lda Model") +
  xlab('Probabilities of "deposit"') + ylab("Count of Customers") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph_prob_lda
rm(graph_prob_lda)

# Tuning probability threshold on the training set for the 4 individual models.
cutoffs <- seq(0.05, 0.5, 0.001)
len_cut <- length(cutoffs)
seq_cut <- 1:len_cut
new_sensitivities <- data.frame(matrix(nrow = len_cut, ncol = len_mod) * 1)
names(new_sensitivities) <- models
new_posPredValues <- data.frame(matrix(nrow = len_cut, ncol = len_mod) * 1)
names(new_posPredValues) <- models

for (i in seq_cut) {
  for (j in seq_mod) { 
    refitted <- ifelse(prob_train[, j] > cutoffs[i], "deposit", "no_deposit") %>%
      factor(levels = levels(bank_train$y))
    new_sensitivities[i, j] <- confusionMatrix(refitted, bank_train$y)$byClass[1]
    new_posPredValues[i, j] <- confusionMatrix(refitted, bank_train$y)$byClass[3]
    } 
}
rm(i, refitted)

# Calculating the percentage of subscribers reached according to threshold and model
# on the training set.
new_subs_reached_pct <- new_sensitivities * 100

# Computing the percentage of customers not contacted according to threshold and to model. 
new_FP <- data.frame(matrix(nrow = len_cut, ncol = len_mod) * 1)
for (i in seq_cut) {
  for (j in seq_mod) { 
    new_FP[i, j] <- round(subs_number * new_subs_reached_pct[i, j] / 100) *  
      (1 - new_posPredValues[i, j]) / new_posPredValues[i, j]
  } 
} 
new_cust_not_contacted <- cust_number - 
  round(subs_number * new_subs_reached_pct / 100) - new_FP
new_cust_not_contacted_pct <- new_cust_not_contacted / cust_number * 100
rm(new_FP)

# Building a table of results with all probabilitiy thresholds 
# and the 4 individual models on the training set. 
evaluation_table_train_all <- data.frame(matrix(nrow = len_cut, ncol = len_mod * 2) * 1)
seq_odd <- seq(1, (len_mod * 2) - 1, 2)
colnames(evaluation_table_train_all)[seq_odd] <- c(paste(models, "s", sep = "_"))
seq_even <- seq(2, (len_mod * 2) , 2)
colnames(evaluation_table_train_all)[seq_even] <- c(paste(models, "r", sep = "_"))
rm(seq_odd, seq_even)

for (i in 1:len_mod) {
  evaluation_table_train_all[(i * 2) - 1] <- new_subs_reached_pct[, i]
  evaluation_table_train_all[(i * 2)] <- new_cust_not_contacted_pct[, i]
} 
evaluation_table_train_all <- evaluation_table_train_all %>% 
  mutate(Thresh = as.character(round(cutoffs, 5))) %>% 
  dselect(Thresh, everything())

# Let's print part of the table of results.
seq <- seq(1, 451, 10)
tab <- evaluation_table_train_all[seq, ] %>% as.data.frame()
kable(tab, "html", align = "c", digits = 0, nsmall = 0) %>% 
  kable_styling(bootstrap_options = c("bordered", "condensed"), 
                full_width = F, font_size = 16) %>% column_spec(1:9, width = "1.5in")
rm(seq, tab)

# Let's graphically visualize results on training set after tuning. 
graph <- evaluation_table_train_all %>% ggplot() + 
  geom_point(aes(x = glm_s, y = glm_r), size = 1, color = "green") +
  geom_point(aes(x = lda_s, y = lda_r), size = 1, color = "yellow") + 
  geom_point(aes(x = rf_s, y = rf_r), size = 2, color = "red", alpha = 0.3) +
  geom_point(aes(x = gbm_s, y = gbm_r), size = 1, color = "blue") +
  geom_rect(aes(xmin = 75, xmax = 100, ymin = 50, ymax = 100), 
            fill = "#9bc4e2", alpha = 0.01) +
  ggtitle("Training Set: glm(green) - lda(yellow) - rf(red) - gbm(blue)") + 
  xlim(0, 105) + xlab("Subscribers Reached (%)") + 
  ylim(0, 105) + ylab("Global Coverage Reduction (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph
rm(graph, cust_number, subs_number, cutoffs, len_cut, seq_cut, i, j, fitted, prob_train,
   evaluation_table_train_all, new_cust_not_contacted, new_cust_not_contacted_pct, 
   new_posPredValues, new_sensitivities, new_subs_reached_pct)

##############################################################

## TEST SET 
# Probabilities of "deposit" on the test set 
prob_test <- lapply(fits, function(object) 
  predict(object, newdata = bank_test, type = "prob"))
seq <- seq(1, (length(models) * 2) - 1, 2)
prob_test <- prob_test %>% as.data.frame() %>% dselect(seq)
names(prob_test) <- gsub(".deposit", "", names(prob_test))
rm(seq)

# Sequence of thresholds 
cutoffs <- seq(0.05, 0.5, 0.001)
len_cut <- length(cutoffs)
seq_cut <- 1:len_cut

# Sensitivity and precision for all combinations of thresholds and models on test set
test_sensitivities <- data.frame(matrix(nrow = len_cut, ncol = len_mod) * 1)
names(test_sensitivities) <- models
test_posPredValues <- data.frame(matrix(nrow = len_cut, ncol = len_mod) * 1)
names(test_posPredValues) <- models
for (i in seq_cut) {
  for (j in seq_mod) { 
    refitted <- ifelse(prob_test[, j] > cutoffs[i], "deposit", "no_deposit") %>%
      factor(levels = levels(bank_test$y))
    test_sensitivities[i, j] <- confusionMatrix(refitted, bank_test$y)$byClass[1]
    test_posPredValues[i, j] <- confusionMatrix(refitted, bank_test$y)$byClass[3]
  } 
}
rm(i, j, refitted)

# Number of customers and of subscribers in test set
test_cust_number <- nrow(bank_test)
test_subs_number <- bank_test %>% filter(y == "deposit") %>% nrow()

# Data frame of percentages of subscribers reached per threshold and model on test set
test_subs_reached_pct <- test_sensitivities * 100

# Data frame of percentages of customers not contacted
test_FP <- data.frame(matrix(nrow = len_cut, ncol = len_mod) * 1)
names(test_FP) <- models
for (i in seq_cut) {
  for (j in seq_mod) { 
    test_FP[i, j] <- round(test_subs_number * test_subs_reached_pct[i, j] / 100) *  
      (1 - test_posPredValues[i, j]) / test_posPredValues[i, j]
  } 
} 
test_cust_not_contacted <- test_cust_number - 
  round(test_subs_number * test_subs_reached_pct / 100) - test_FP
test_cust_not_contacted_pct <- test_cust_not_contacted / test_cust_number * 100

# Table of results with all thresholds and individual models on test set 
evaluation_table_test <- data.frame(matrix(nrow = len_cut, ncol = len_mod * 2) * 1)
seq_odd <- seq(1, (len_mod * 2) - 1, 2)
colnames(evaluation_table_test)[seq_odd] <- c(paste(models, "s", sep = "_"))
seq_even <- seq(2, (len_mod * 2) , 2)
colnames(evaluation_table_test)[seq_even] <- c(paste(models, "r", sep = "_"))
rm(seq_odd, seq_even)

for (i in 1:len_mod) {
  evaluation_table_test[(i * 2) - 1] <- test_subs_reached_pct[, i]
  evaluation_table_test[(i * 2)] <- test_cust_not_contacted_pct[, i]
} 
evaluation_table_test <- evaluation_table_test %>% 
  mutate(Thresh = as.character(round(cutoffs, 5))) %>% 
  dselect(Thresh, everything())

# Printing table of results with some thresholds and all individual models on test set.
seq <- seq(11, 51, 1)
tab <- evaluation_table_test[seq,] %>% as.data.frame()
kable(tab, "html", align = "c", digits = 0, nsmall = 0) %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 14) %>% 
  column_spec(1:9, width = "1.5in")
rm(seq, tab)

# Graph of results with all thresholds and all individual models on test set. 
graph <- evaluation_table_test %>% ggplot() + 
  geom_point(aes(x = glm_s, y = glm_r), size = 1, color = "green") +
  geom_point(aes(x = lda_s, y = lda_r), size = 1, color = "yellow") + 
  geom_point(aes(x = rf_s, y = rf_r), size = 2, color = "red", alpha = 0.2) +
  geom_point(aes(x = gbm_s, y = gbm_r), size = 1.5, color = "blue") +
  geom_rect(aes(xmin = 75, xmax = 100, ymin = 50, ymax = 100), 
            fill = "#9bc4e2", alpha = 0.01) +
  ggtitle("Test Set: glm(green) - lda(yellow) - rf(red) - gbm(blue)") + 
  xlim(0, 105) + xlab("Subscribers Reached (%)") + 
  ylim(0, 105) + ylab("Global Coverage Reduction (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y =element_text(size = 12))
graph
rm(graph)

# Ensemble model on test set for all thresholds with votes, sensitivity and precision.
test_ensemble <- data.frame(threshold = cutoffs, ens_s = 1:len_cut, 
                            ens_r = 1:len_cut, stringsAsFactors = FALSE)
for (i in 1:len_cut) {
  dummy <- prob_test %>% dselect(c("glm", "rf", "gbm"))
  dummy[dummy > cutoffs[i]] <- 1
  dummy[dummy <= cutoffs[i]] <- 0
  dummy <- dummy %>% as.data.frame()
  votes <- rowSums(dummy)
  votes <- ifelse(votes >= 2, "deposit", "no_deposit") %>% 
    factor(levels = levels(bank_test$y))
  
  sensitivity <- confusionMatrix(votes, bank_test$y)$byClass[1]
  TP <- round(test_subs_number * sensitivity) 
  test_ensemble[i, 2] <-  sensitivity * 100
  
  precision <- confusionMatrix(votes, bank_test$y)$byClass[3]
  FP <- round(TP * (1 - precision) / precision)
  test_ensemble[i, 3] <-  (test_cust_number - TP - FP) / test_cust_number * 100
} 

# Table of results on test set: all thresholds / 3 individual methods + ensemble model  
evaluation_table_test_ens <- evaluation_table_test %>% 
  dselect("Thresh", "glm_s", "glm_r", "rf_s", "rf_r", "gbm_s", "gbm_r") %>%
  mutate(ens_s = test_ensemble$ens_s, ens_r = test_ensemble$ens_r) 
seq <- seq(1, 51, 1)
tab <- evaluation_table_test_ens[seq, ] %>% as.data.frame()
kable(tab, "html", align = "c", digits = 0, nsmall = 0) %>% 
  kable_styling(bootstrap_options = c("bordered", "condensed"), 
  full_width = F, font_size = 14) %>% column_spec(1:9, width = "1.5in")
rm(seq, tab)

# Graph of results on test set: all thresholds / 3 individual methods + ensemble model 
graph <- evaluation_table_test_ens %>% ggplot() + 
  geom_point(aes(x = glm_s, y = glm_r), size = 1, color = "green") +
  geom_point(aes(x = rf_s, y = rf_r), size = 2, color = "red", alpha = 0.2) +
  geom_point(aes(x = gbm_s, y = gbm_r), size = 1.5, color = "blue") +
  geom_point(aes(x = ens_s, y = ens_r), size = 1.5, color = "yellow", alpha = 0.75) +
  geom_rect(aes(xmin = 75, xmax = 100, ymin = 50, ymax = 100), 
            fill = "#9bc4e2", alpha = 0.01) +
  ggtitle("Test Set: glm(green) - rf(red) - gbm(blue) - ensemble(yellow)") + 
  xlim(0, 105) + xlab("Subscribers Reached (%)") + 
  ylim(0, 105) + ylab("Global Coverage Reduction (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), 
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
graph

rm(graph, prob_test, test_cust_number, test_subs_number, test_cust_not_contacted, 
   test_cust_not_contacted_pct, test_ensemble, test_FP, test_posPredValues, 
   test_sensitivities, test_subs_reached_pct, cutoffs, TP, FP, i, j, votes,
   precision, sensitivity, len_cut, seq_cut, evaluation_table_test,
   evaluation_table_test_ens, dummy)

##############################################################

# VALIDATION SET
# Probabilities extracted from training set fits with threshold from test set 
v <- c("glm", "rf", "gbm")
fits_3 <- fits[v]
threshold <- 0.086
prob_val <- lapply(fits_3, function(object) 
  predict(object, newdata = bank_val, type = "prob"))
seq <- seq(1, (length(fits_3) * 2) - 1, 2)
prob_val <- prob_val %>% as.data.frame() %>% dselect(seq)
names(prob_val) <- v
rm(v, seq)

# Majority vote on validation set
val_ensemble <- data.frame(ens_s = 1, ens_r = 1, stringsAsFactors = FALSE)
dummy <- prob_val
dummy[dummy > threshold] <- 1
dummy[dummy <= threshold] <- 0
dummy <- dummy %>% as.data.frame()
votes <- rowSums(dummy)
votes <- ifelse(votes >= 2, "deposit", "no_deposit") %>% 
  factor(levels = levels(bank_val$y))

# Numbers of subscribers and customers on validation set
val_subs_number <- bank_val %>% filter(y == "deposit") %>% nrow() 
val_cust_number <- nrow(bank_val)

# Sensitivity and precision on validation set  
sensitivity <- confusionMatrix(votes, bank_val$y)$byClass[1]
TP <- round(val_subs_number * sensitivity) 
val_ensemble[1, 1] <-  sensitivity * 100

precision <- confusionMatrix(votes, bank_val$y)$byClass[3]
FP <- round(TP * (1 - precision) / precision)
val_ensemble[1, 2] <-  (val_cust_number - TP - FP) / val_cust_number * 100

# Results on validation set
results_tab <- val_ensemble %>% 
  mutate(ens_s = format(ens_s, digits = 0, nsmall = 0)) %>%
  mutate(ens_s = paste(ens_s, "%", sep = " ")) %>% 
  mutate(ens_r = format(ens_r, digits = 0, nsmall = 0)) %>%
  mutate(ens_r = paste(ens_r, "%", sep = " ")) %>%
  mutate(Model = "Ensemble glm-rf-gbm") %>% 
  mutate(Probability_Threshold = threshold) %>%
  rename(Subscribers_Reached = ens_s, Global_Coverage_Reduction = ens_r) %>% 
  dselect(Model, Probability_Threshold, Subscribers_Reached, Global_Coverage_Reduction) %>%
  as.data.frame()
kable(results_tab, "html", align = "c") %>% 
  kable_styling(bootstrap_options = "bordered", full_width = F, font_size = 14) %>% 
  column_spec(1:4, width = "2in")

# Removing objects.
rm(dummy, fits, fits_3, prob_val, val_ensemble, FP, TP, precision, 
   sensitivity, threshold, val_cust_number, val_subs_number, 
   bank, bank_test, bank_train, len_mod, models, seq_mod, dselect)

# 3 objects remain available in the workspace: 
# - "results_tab" (with the final results), 
# - "votes" (with the predicted values on the validation set) 
# - and "bank_val" (data from the validation set). 

##############################################################################
##############################################################################


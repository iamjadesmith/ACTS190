rm(list = ls())
#load packages
library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)
library(RColorBrewer)
library(dplyr)
library(randomForest)
library(statmod)

cases <- read.csv("cases.csv", sep = "|")

# Keeping only the columns that we want
cases <- subset(cases, select = c(COMPANY_ID, ACD, CREATION_DATE, LAST_MODIFIED_DATE, COMPANY_NAME, COUNTRY_CODE, CASE_CATEGORY,
                                  CASE_TYPE, CLASS_COLLECTIVE_ACTION, PRODUCT_SERVICE_INVOLVED, CASESTATUS,
                                  FIRST_NOTICE_DATE,FIRST_NOTICE_DATE_QUALIFIER, ACCIDENT_DATE, ACCIDENT_DATE_QUALIFIER,
                                  CASE_DESCRIPTION, PROXIMATE_CAUSE, SECONDARY_CAUSE, FILING_YEAR, DOCKET_NUMBER,
                                  JURIS_TRIGGER, JURIS_COUNTRY_CODE, PLAINTIFFS_COMPANY, PLAINTIFFS_LAWFIRM,
                                  AFFECTED_COUNT, SETTLEMENT_AMOUNT, FINANCIAL_DAMAGES_AMT))

# Data Cleaning ----

stats <- read.csv("stats.csv", sep = "|")

# Picking columns from the other data set
stats <- subset(stats, select = c(COMPANY_ID, COMPANY_STATUS, EMPLOYEES, REVENUES, NAIC_SECTOR))


cyber <- merge(cases, stats, by = "COMPANY_ID")

# Only keeping the observations that contain a settlement amount
cyber <- cyber[!is.na(cyber$SETTLEMENT_AMOUNT),]

# remove other columns we don't want
cyber <- subset(cyber, select = -c(COMPANY_ID, CASE_DESCRIPTION, CASE_CATEGORY, PRODUCT_SERVICE_INVOLVED,
                                   FIRST_NOTICE_DATE, FIRST_NOTICE_DATE_QUALIFIER, PROXIMATE_CAUSE, 
                                   SECONDARY_CAUSE, AFFECTED_COUNT))

# making variables factors

cyber$CLASS_COLLECTIVE_ACTION <- as.factor(cyber$CLASS_COLLECTIVE_ACTION)
cyber$COUNTRY_CODE <- as.factor(cyber$COUNTRY_CODE)
cyber$CASESTATUS <- as.factor(cyber$CASESTATUS)
cyber$FILING_YEAR <- as.factor(cyber$FILING_YEAR)
cyber$JURIS_TRIGGER <- as.factor(cyber$JURIS_TRIGGER)
cyber$JURIS_COUNTRY_CODE <- as.factor(cyber$JURIS_COUNTRY_CODE)
cyber$COMPANY_STATUS <- as.factor(cyber$COMPANY_STATUS)
cyber$NAIC_SECTOR <- as.factor(cyber$NAIC_SECTOR)

# Putting Case Type into less factors:
cyber$CASE_TYPE[cyber$CASE_TYPE %in% c("Data - Malicious Breach", "Data - Physically Lost or Stolen",
                                       "Data - Unintentional Disclosure")] <- "Data"
cyber$CASE_TYPE[cyber$CASE_TYPE %in% c("IT - Configuration/Implementation Errors", "IT - Processing Errors",
                                       "Identity - Fraudulent Use/Account Access", 
                                       "Phishing, Spoofing, Social Engineering", "Network/Website Disruption",
                                       "Skimming, Physical Tampering", "Undetermined/Other",
                                       "Cyber Extortion", "Industrial Controls & Operations")] <- "Other"
cyber$CASE_TYPE[cyber$CASE_TYPE %in% c("Privacy - Unauthorized Contact or Disclosure",
                                       "Privacy - Unauthorized Data Collection")] <- "Privacy"

cyber$CASE_TYPE <- as.factor(cyber$CASE_TYPE)
cyber$NAIC_SECTOR <- as.factor(cyber$NAIC_SECTOR)

# Exploratory Analysis ----

# Scatterplot matrix 
sub = subset(cyber, select = c("REVENUES", "FINANCIAL_DAMAGES_AMT", "SETTLEMENT_AMOUNT"))
sub = sub[complete.cases(sub),]
cor(sub)
vars = data.frame(sub$REVENUES, sub$FINANCIAL_DAMAGES_AMT, sub$SETTLEMENT_AMOUNT)
cor(cbind(vars), use = "pairwise.complete.obs")
pairs(vars, upper.panel=NULL)

cyber %>%
  ggplot(aes(x = CASE_TYPE)) +
  ggtitle("Case Type Frequency")+
  labs(x = "Type of cases", y = "Number of cases ") +
  geom_bar(fill = "#99d8c9")+
  coord_flip ()

cyber %>%
  ggplot(aes(x = CASESTATUS)) +
  ggtitle("Case Status Frequency")+
  labs(x = "Status", y = "Count ") +
  geom_bar(fill = "#99d8c9")+
  coord_flip ()

cyber %>%
  ggplot(aes(x = JURIS_TRIGGER)) +
  ggtitle("Trigger Frequency")+
  labs(x = "Trigger", y = "Count ") +
  geom_bar(fill = "#99d8c9")

cyber %>%
  ggplot(aes(x = NAIC_SECTOR )) +
  ggtitle("NAIC Sector Frequency")+
  labs(x = "Sectors", y = "Count ") +
  geom_bar(fill = "#99d8c9")

cyber %>%
  ggplot(aes(x = FILING_YEAR)) +
  ggtitle("Filing Year")+
  labs(x = "Filing Year", y = "Number of cases ") +
  geom_bar(fill = "#99d8c9") +
  coord_flip()


# Financial Damages Amount
cyber %>%
  ggplot() +
  geom_histogram(aes(x = FINANCIAL_DAMAGES_AMT), fill = "#99d8c9") +
  ggtitle("Histogram of Financial Damages Amount") +
  labs(x = "Financial Damages", y = "Frequency")
# Very right skewed. Will try a log transformation
cyber %>%
  ggplot() +
  geom_histogram(aes(x = log(FINANCIAL_DAMAGES_AMT + 1)), fill = "#99d8c9") +
  ggtitle("Histogram of Log(Financial Damages Amount + 1)") +
  labs(x = "Log(Financial Damages + 1)", y = "Frequency")
# This looks much better, will stick with log(FINANCIAL_DAMAGES_AMT)
cyber$LOG_FINANCIAL_DAMAGES <- log(cyber$FINANCIAL_DAMAGES_AMT + 1)

# Revenues
cyber %>%
  ggplot() +
  geom_histogram(aes(x = REVENUES), fill = "#99d8c9") +
  ggtitle("Histogram of Revenues") +
  labs(x = "Revenues", y = "Frequency")
# Log transformationX
cyber %>%
  ggplot() +
  geom_histogram(aes(x = log(REVENUES)), fill = "#99d8c9") +
  ggtitle("Histogram of Log(Revenues)") +
  labs(x = "Log(Revenues)", y = "Frequency")
# Looks Great
cyber$LOG_REVENUES <- log(cyber$REVENUES + 1)

# Employees
cyber %>%
  ggplot() +
  geom_histogram(aes(x = EMPLOYEES), fill = "#99d8c9") +
  ggtitle("Histogram of Employees") +
  labs(x = "Number of Employees", y = "Frequency")
# Log Transformation
cyber %>%
  ggplot() +
  geom_histogram(aes(x = log(EMPLOYEES)), fill = "#99d8c9") +
  ggtitle("Histogram of Log(Employees)") +
  labs(x = "Log(Employees)", y = "Frequency")
# Better
cyber$LOG_EMPLOYEES <- log(cyber$EMPLOYEES + 1)

# Y Variable - Settlement Amount
cyber %>%
  ggplot() +
  geom_histogram(aes(x = SETTLEMENT_AMOUNT), fill = "#99d8c9") +
  ggtitle("Histogram of Settlement Amount") +
  labs(x = "Settlement Amount", y = "Frequency")
# Trying a log transformation again
cyber %>%
  ggplot() +
  geom_histogram(aes(x = log(SETTLEMENT_AMOUNT + 1)), fill = "#99d8c9") +
  ggtitle("Histogram of Log(Settlement Amount + 1)") +
  labs(x = "Log(Settlement Amount + 1)", y = "Frequency")
# Much better
cyber$LOG_SETTLEMENT_AMOUNT <- log(cyber$SETTLEMENT_AMOUNT + 1)

cyber_cleaned <- subset(cyber, select = c(ACD, COUNTRY_CODE, CASE_TYPE, CLASS_COLLECTIVE_ACTION,
                                          CASESTATUS, FILING_YEAR, JURIS_TRIGGER, LOG_SETTLEMENT_AMOUNT, LOG_EMPLOYEES,
                                          LOG_REVENUES, NAIC_SECTOR, COMPANY_STATUS))
cyber_cleaned <- na.omit(cyber_cleaned)
# Random Forest ----

# RNGkind(sample.kind = "default")
# set.seed(1239845)
# train.idx <- sample(x = 1:nrow(cyber), size = .7*nrow(cyber))
# train.df <- cyber[train.idx, ]
# test.df <- cyber[-train.idx, ]
# 
# base_forest <- randomForest(SETTLEMENT_AMOUNT ~ . ,
#                             data = train.df,
#                             ntree = 1000,
#                             mtry = 5,
#                             importance = TRUE)

# Tweedie Model ----

# Base Tweedie Model
m1 <- glm(LOG_SETTLEMENT_AMOUNT ~ ., data = cyber_cleaned, 
          family = tweedie(var.power=0,link.power=1))

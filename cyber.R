rm(list = ls())

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
cyber$CASE_TYPE <- as.factor(cyber$CASE_TYPE)
cyber$CLASS_COLLECTIVE_ACTION <- as.factor(cyber$CLASS_COLLECTIVE_ACTION)
cyber$COUNTRY_CODE <- as.factor(cyber$COUNTRY_CODE)
cyber$CASESTATUS <- as.factor(cyber$CASESTATUS)
cyber$FILING_YEAR <- as.factor(cyber$FILING_YEAR)
cyber$JURIS_TRIGGER <- as.factor(cyber$JURIS_TRIGGER)
cyber$JURIS_COUNTRY_CODE <- as.factor(cyber$JURIS_COUNTRY_CODE)
cyber$COMPANY_STATUS <- as.factor(cyber$COMPANY_STATUS)
cyber$NAIC_SECTOR <- as.factor(cyber$NAIC_SECTOR)
rm(list = ls())

cyber <- read.csv("cases.csv", sep = "|")

# Data Cleaning ----

#Keeping only the columns that we want
cyber <- subset(cyber, select = c(ACD, CREATION_DATE, LAST_MODIFIED_DATE, COMPANY_NAME, COUNTRY_CODE, CASE_CATEGORY,
                                   CASE_TYPE, CLASS_COLLECTIVE_ACTION, PRODUCT_SERVICE_INVOLVED, CASESTATUS, 
                                   FIRST_NOTICE_DATE,FIRST_NOTICE_DATE_QUALIFIER, ACCIDENT_DATE, ACCIDENT_DATE_QUALIFIER, 
                                   CASE_DESCRIPTION, PROXIMATE_CAUSE, SECONDARY_CAUSE, FILING_YEAR, DOCKET_NUMBER, 
                                   JURIS_TRIGGER, JURIS_COUNTRY_CODE, PLAINTIFFS_COMPANY, PLAINTIFFS_LAWFIRM, 
                                   AFFECTED_COUNT, SETTLEMENT_AMOUNT))

#removes columns if there are more than 100,000 NAs in the column
# cyber <- cyber[,colSums(is.na(cyber))<100000]

#making a comment
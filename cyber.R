rm(list = ls())

cyber <- read.csv("cases.csv", sep = "|")
str(cyber)
summary(cyber)

# Data Cleaning ----

cyber <- subset(cyber, select = -c(STREET_ADDR_LINE_2))

is.na(cyber)

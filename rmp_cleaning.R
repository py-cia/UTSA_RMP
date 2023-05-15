# rm(list = ls())
library(tidyverse)
library(lubridate)

df <- read.csv("C:/Users/valen/OneDrive/Desktop/df1.csv")
# no null values
sum(is.na(df))

# data is for dates after 2018-05-10
date <- "2018-05-10"

# Only going to look at current economic professors at UTSA
current_prof_df <- (professor_name = c("Samson Alva", "Hayri Arslan", "David Beheshti", "Hamid Beladi", "Leslie Doss", "Fathali Firoozi", "Edgar Ghossoub", 
                               "Carlos Gustafon", "Shakira Johnson", "Donald Lien", "Jonathan Moreno-Medina", "Mohammed Partapurwala", "Keith Phillips",
                               "Viviana Rodriguez", "Stephen Schwab", "Bulent Temel"))

# There are currently 16 professors
length(current_prof_df)

# 28 professors in the past five years have been reviewed
df$professor_name %>% unique() %>% length()

# making sure the data is only from the past 5 years
df$review_date <- df$review_date %>% mdy()
df1 <- df %>% filter(review_date >= date)
View(df1)

# look at current professors
prof <- professor_name[professor_name %in% df1$professor_name]
df2 <- df1 %>% filter(professor_name %in% prof)

# some professors do not have many reviews. i will omit them.
table(df2$professor_name)

# only keeping professors with more than 10 reviews
n <- table(df2$professor_name)[table(df2$professor_name) > 10] %>% names()
df3 <- df2 %>% filter(professor_name %in% n)
# making sure the filter was applied correctly
table(df3$professor_name)


# Clean class names -------------------------------------------------------
# Macroeconomics <- 2013
# Microeconomics <- 2023
# EconomicPrinciplesandIssues <- 2003
# Intermediate Micro <- 3013
# Introduction to Mathematical Economics <- 3113
# Public Economics <- 3273
# Money and Banking <- 3313
# Environmental Economics <- 4273
# Microeconomic Theory <- 6013
# Mathematical Economics <- 6113
# Gametheory <- 6573
# majority are from ECO-2023 ECO2013, ECO2023. These are introductory economic courses:micro and macro
table(df3$class_name)

#remove hyphen
df3$class_name <- str_replace(df3$class_name, "-", "")
filter(df3, class_name == "MACROECO") %>% select(comment)

# based on class catalog for Spring2023 leslie doss taught an intro to macroeco course
df3$class_name <- str_replace(df3$class_name, "MACROECO", "ECO2013") 
df3$class_name <- str_replace(df3$class_name, "N", "")
df3$class_name[df3$class_name == "2023"] = "ECO2023"
table(df3$class_name)

df3$class_name[df3$class_name == "ECO2023001"] = "ECO2023"

# df3[24, ] this review was submitted at the start of the 2022 fall semester. so it could be a student from the summer or spring semester of 2022.
# All summer eco courses were online and the review states they were in class. Doss taught 2 intro macro courses and 1 intro micro course. 
# there are more students in his macro course. I will impute this to eco2013
df3[24, 8] = "ECO2013"
table(df3$class_name)

# There is no ECO1013 this is likely a typo.
df3$class_name[df3$class_name == "ECO1013"] = "ECO2013"
table(df3$class_name)

# removing the emoji from emotion column
df3$emotion <- str_sub(df3$emotion, 14)











# rm(list = ls())
library(tidyverse)
df <- read.csv("C:\\Users\\valen\\Downloads\\rmp_df.csv", stringsAsFactors = FALSE) %>% select(-1)
df %>% head(5)

# summary statistics
# average total quality is about 3 and the average total difficulty is close to 3.4
# values range from 1-5, with 5 being the highest
summary(df)

# the sd for both measurements are large for the scale of the variables 
sd(df$difficulty)
sd(df$quality)

# 4 difficulty appears most
table(df$difficulty)
# quality 1 appears the most
table(df$quality)

# bimodal distribution two types of students who think the classes are "awesome" or "awful"
ggplot(mapping = aes(x = df$quality)) +
  geom_bar()

# avg difficulty of a class in the economic department is greater than the average quality in the past 5 years
# there is correlation here as the difficulty of a professor could impact the quality of the overall class.
ggplot(mapping = aes(x = df$difficulty)) +
  geom_bar()

# in the past five years which professor had the lowest quality and difficulty
table(df$professor_name)

temel <- df %>% filter(professor_name == "Bulent Temel")
firoozi <- df %>% filter(professor_name == "Fathali Firoozi")
doss <- df %>% filter(professor_name == "Leslie Doss")
alva <- df %>% filter(professor_name == "Samson Alva")
johnson <- df %>% filter(professor_name == "Shakira Johnson")

# 3.76
temel$quality %>% mean()
# 2.85
temel$difficulty %>% mean()

# 4.33
firoozi$quality %>% mean()
# 2.42
firoozi$difficulty %>% mean()

# 2.68
doss$quality %>% mean()
# 3.15
doss$difficulty %>% mean()

# 2.74
alva$quality %>% mean()
# 4.21
alva$difficulty %>% mean()

# 2.38
johnson$quality %>% mean()
# 3.63 
johnson$difficulty %>% mean()

# In the past five years, professor firoozi has the highest quality and professor alva has the highest difficulty
# In the past five years, professor johnson has the lowest quality and professor firoozi has the lowest difficulty

# can see that graduate students leave less reviews than undergraduate students. 4/151 reviews are from graduate courses.
table(df$class_name)

df2 <- separate(df, review_date, into = c("year", "month", "day"), sep = "-", remove = FALSE)
# most reviews came from 2022
df2 %>% select(year) %>% count(year) %>% 
  ggplot(mapping = aes(x = year, y = n)) +
  geom_col()

# Most reviews are left within the last two months of the semester. 
df2 %>% select(month) %>% count(month) %>% 
  ggplot(mapping = aes(x = month, y = n)) +
  geom_col()

# no apparent pattern
df2 %>% select(day) %>% count(day) %>% 
  ggplot(mapping = aes(x = day, y = n)) +
  geom_col()

# statistical analysis ----------------------------------------------------

# normality test for kruskal-wallis
alva %>% 
  count(quality) %>% 
  ggplot(mapping = aes(x = quality, y = n)) +
  geom_col()

# not normal
alva$quality %>% shapiro.test()

doss %>% 
  count(quality) %>% 
  ggplot(mapping = aes(x = quality, y = n)) +
  geom_col()

# not normal
doss$quality %>% shapiro.test()

firoozi %>% 
  count(quality) %>%
  ggplot(mapping = aes(x = quality, y = n)) +
  geom_col()

# not normally distributed
firoozi$quality %>% shapiro.test()

johnson %>% 
  count(quality) %>% 
  ggplot(mapping = aes(x = quality, y = n)) +
  geom_col()

shapiro.test(johnson$quality)


# there is some correlation between the difficulty and quality. difficulty is able to explain .1433 percent of the 
# variability in quality
lm(df$quality ~ df$difficulty) %>% summary()

alva_group <- alva %>% select(professor_name, quality)
doss_group <- doss %>% select(professor_name, quality)
firoozi_group <- firoozi %>% select(professor_name, quality)
johnson_group <- johnson %>% select(professor_name, quality)
temel_group <- temel %>% select(professor_name, quality)
group_df <- rbind(alva_group, doss_group, firoozi_group, johnson_group, temel_group)

# test for homogeneity of variances
bartlett.test(quality ~ professor_name, data = group_df)

kruskal.test(quality ~ professor_name, data = group_df)
# Report: H = 23.474 (4), p < .001
# we can reject the null hypothesis, there is a difference between the mean ranks of at least two groups in this test.
ggplot(data = group_df, mapping = aes(x = professor_name, y = quality)) +
  geom_boxplot()

dunnTest(quality ~ professor_name, data = group_df, method = "bonferroni")
# There are significant differences between Temel & Doss, Firoozi & Doss, Firoozi & Alva, Temel & Johnson, Firoozi & Johnson at the 
# 5% significance level


# testing differences between classes -------------------------------------
# Most of the reviews are from the introductory economic courses, macro and micro. comprising of 129/151 of the reviews
# ECO2013:Macro, ECO2023:Micro
intro_courses <- df %>% filter(class_name %in% c("ECO2013", "ECO2023"))
intro_courses$quality[intro_courses$class_name == "ECO2013"] %>% mean()
intro_courses$quality[intro_courses$class_name == "ECO2023"] %>% mean()

# as expected, microeconomics is more difficult than macroeconomics. this could be because there is more mathematics involved in a micro
# class than a macro class
intro_courses$difficulty[intro_courses$class_name == "ECO2013"] %>% mean()
intro_courses$difficulty[intro_courses$class_name == "ECO2023"] %>% mean()

intro_courses %>% filter(class_name == "ECO2013") %>%
  count(difficulty) %>% 
  ggplot(mapping = aes(x = difficulty, y = n)) +
  geom_col()

intro_courses %>% filter(class_name == "ECO2023") %>%
  count(difficulty) %>% 
  ggplot(mapping = aes(x = difficulty, y = n)) +
  geom_col()

intro_courses %>% filter(class_name == "ECO2013") %>%
  .$difficulty %>% shapiro.test()

intro_courses %>% filter(class_name == "ECO2023") %>%
  .$difficulty %>% shapiro.test()

# Because the p-value is less than the signigicance level of .05 for both courses there is sufficient evidence to conclude that 
# the data is not normally distributed

eco_2013 <- intro_courses %>% filter(class_name == "ECO2013") %>% select(difficulty)
eco_2023 <- intro_courses %>% filter(class_name == "ECO2023") %>% select(difficulty)

# significant difference in the medians between the two groups
wilcox.test(unlist(eco_2013$difficulty), unlist(eco_2023$difficulty))

intro_courses %>% select(class_name, difficulty) %>%
  ggplot(mapping = aes(x = class_name, y = difficulty)) +
  geom_boxplot()

# checking quality
intro_courses %>% filter(class_name == "ECO2013") %>% select(class_name, quality) %>%
  count(quality) %>%
  ggplot(aes(x = quality, y = n)) +
  geom_col()

intro_courses %>% filter(class_name == "ECO2023") %>% select(class_name, quality) %>%
  count(quality) %>%
  ggplot(aes(x = quality, y = n)) +
  geom_col()

# Both are not normally distributed
intro_courses$quality[intro_courses$class_name == "ECO2013"] %>% shapiro.test()
intro_courses$quality[intro_courses$class_name == "ECO2023"] %>% shapiro.test()

eco_2013 <- intro_courses$quality[intro_courses$class_name == "ECO2013"]
eco_2023 <- intro_courses$quality[intro_courses$class_name == "ECO2023"]

# no significant difference between medians
wilcox.test(eco_2013, eco_2023)

intro_courses %>% select(class_name, quality) %>% 
  ggplot(aes(x = class_name, y = quality)) +
  geom_boxplot()






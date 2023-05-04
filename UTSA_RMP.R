# rm(list = ls())
library(RSelenium)
library(tidyverse)
library(netstat)
library(lubridate)
rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "112.0.5615.28",
                             verbose = FALSE,
                             port = free_port())

rmDr <- rs_driver_object$client

# If not already open
# rmDr$open()

# Test run ----------------------------------------------------------------
# 
rmDr$maxWindowSize()
rmDr$navigate("https://www.ratemyprofessors.com/search/teachers?query=*&sid=1516")
# closes cookie pop-up
rmDr$findElement(using = "xpath", "//img[@role='button']")$clickElement()
# closes ad pop-up
rmDr$findElement(using = "xpath", "//*[@id='bx-close-inside-1177612']")$clickElement()

# navigate to different site
rmDr$navigate("https://www.ratemyprofessors.com/professor/147770")
# total ratings
num_rating <- rmDr$findElement(using = "xpath", "//a[@href = '#ratingsList']")$getElementText() %>%
  unlist() %>% 
  str_extract("\\d+") %>% 
  as.numeric()

# Number of times to click more ratings -----------------------------------

list <- ceiling((num_rating - 20) / 10)
if (list >= 1){
  for (i in 1:list){
button <- rmDr$findElement(using = "class name", "Buttons__Button-sc-19xdot-1")
y_pos <- button$getElementLocation()$y - 100
rmDr$executeScript(sprintf("window.scrollTo(0, %f)", y_pos)) # scrolls to button
button$clickElement()
Sys.sleep(1.5)
  }
}


# Get Data ----------------------------------------------------------------

# Now that we dont see 'load more ratings' we can look for our variables of interest
professor_id <- rmDr$getCurrentUrl() %>% str_extract("\\d+") %>% as.numeric()
professor_name <- rmDr$findElement(using = "xpath", ".//div[starts-with(@class, 'NameTitle__')]")$getElementText() %>% unlist()
university <- rmDr$findElement(using = "xpath", "//a[@href='/school/1516']")$getElementText() %>% unlist()
department <- rmDr$findElement(using = "xpath", "//a[starts-with(@class, 'TeacherDepartment__')]")$getElementText() %>% unlist()

# getting data from root node

# make sure to use $findElements
rating_root <- rmDr$findElements(using = "xpath", "//div[starts-with(@class, 'Rating__RatingBody')]")

# length must match number of ratings
rating_root %>% length()
quality <- rmDr$findElement(using = "xpath", "//div[starts-with(@class, 'CardNumRating__CardNumRatingNumber')]")$getElementText() %>% unlist() %>% as.numeric()
class <- rmDr$findElement(using = "xpath", "(//div[starts-with(@class, 'RatingHeader__StyledClass')])[2]")$getElementText() %>% unlist()
diffulty <- rmDr$findElement(using = "xpath", "(.//div[starts-with(@class, 'CardNumRating__CardNumRatingNumber')])[2]")$getElementText() %>% unlist()
# this one is tricky, there are two instances of this 
emotion <- rmDr$findElement(using = "xpath", "(//div[starts-with(@class, 'EmotionLabel')])[2]")$getElementText() %>% unlist()
comment <- rmDr$findElement(using = "xpath", "//div[starts-with(@class, 'Comments__StyledComments')]")$getElementText() %>% unlist()
thumbs_up <- rmDr$findElement(using = "xpath", "(//div[starts-with(@class, 'Thumbs__HelpTotal')])[1]")$getElementText() %>% unlist()
thumbs_down <- rmDr$findElement(using = "xpath", "(//div[starts-with(@class, 'Thumbs__HelpTotal')])[2]")$getElementText() %>% unlist()
review_date <- rmDr$findElement(using = "xpath", "(//div[starts-with(@class, 'TimeStamp__StyledTimeStamp')])[2]")$getElementText() %>% unlist()


# Now turn into a function ------------------------------------------------
df1 <- data.frame()
collect <- function(root){
  quality <- root$findChildElement(using = "xpath", "(.//div[starts-with(@class, 'CardNumRating__CardNumRatingNumber')])[1]")$getElementText() %>%
  unlist() %>%
  as.numeric()
  class_name <- root$findChildElement(using = "xpath", "(.//div[starts-with(@class, 'RatingHeader__StyledClass')])[2]")$getElementText() %>%
    unlist()
  difficulty <- root$findChildElement(using = "xpath", "(.//div[starts-with(@class, 'CardNumRating__CardNumRatingNumber')])[2]")$getElementText() %>% 
    unlist() %>%
    as.numeric()
  emotion <- root$findChildElement(using = "xpath", "(.//div[starts-with(@class, 'EmotionLabel')])[2]")$getElementText() %>% 
    unlist()
  comment <- root$findChildElement(using = "xpath", ".//div[starts-with(@class, 'Comments__StyledComments')]")$getElementText() %>% 
    unlist()
  thumbs_up <- root$findChildElement(using = "xpath", "(.//div[starts-with(@class, 'Thumbs__HelpTotal')])[1]")$getElementText() %>% 
    unlist() %>% 
    as.numeric()
  thumbs_down <- root$findChildElement(using = "xpath", "(.//div[starts-with(@class, 'Thumbs__HelpTotal')])[2]")$getElementText() %>% 
    unlist() %>%
    as.numeric()
  review_date <- root$findChildElement(using = "xpath", "(.//div[starts-with(@class, 'TimeStamp__StyledTimeStamp')])[2]")$getElementText() %>% 
    unlist()
  df1 <- rbind(df1, data.frame(professor_id, professor_name, department, university, quality, difficulty, emotion, class_name, comment,
                             thumbs_up, thumbs_down, review_date))
  
}

jm <- rating_root %>% map_dfr(~collect(.))


# Now to get all the professors -------------------------------------------


num_prof <- rmDr$findElement(using = "xpath", "//h1[@data-testid = 'pagination-header-main-results']")$getElementText() %>% 
  unlist() %>% 
  str_extract("\\d+") %>%
  as.numeric()

# There are 42 professors and each page shows 8 and loads 8.
num_of_iter <- ceiling((42 - 8) / 8)

for (i in 1:num_of_iter){
  button <- rmDr$findElement(using = "xpath", "//button[starts-with(@class, 'Buttons__Button')]")
  y_pos <- button$getElementLocation()$y- 60
  rmDr$executeScript(sprintf("window.scrollTo(0, %f)", y_pos))
  button$clickElement()
  Sys.sleep(1.5)
}

list_prof <- rmDr$findElements("xpath", "//a[starts-with(@class, 'TeacherCard__StyledTeacherCard')]")

# using $getElementAttribute
url <- list_prof %>% map(~.$getElementAttribute("href") %>% unlist())
url <- url %>% unlist()

# get last five years of data
target_date <- "2018-05-10" %>% as.Date()
df1 <- data.frame()
for (i in url3){
  rmDr$navigate(i)
  if (rmDr$findElement("xpath", "//div[starts-with(@class, 'RatingValue__NumRatings')]")$getElementText() == "No ratings yet. Add a rating."){
    next
  }
  num_rating <- rmDr$findElement(using = "xpath", "//a[@href = '#ratingsList']")$getElementText() %>%
    unlist() %>% 
    str_extract("\\d+") %>% 
    as.numeric()
  list <- ceiling((num_rating - 20) / 10)
  if (list >= 1){
    for (i in 1:list){
      button <- rmDr$findElement(using = "class name", "Buttons__Button-sc-19xdot-1")
      y_pos <- button$getElementLocation()$y - 100
      rmDr$executeScript(sprintf("window.scrollTo(0, %f)", y_pos)) # scrolls to button
      button$clickElement()
      Sys.sleep(3.5)
    }
  }
  professor_id <- rmDr$getCurrentUrl() %>% str_extract("\\d+") %>% as.numeric()
  professor_name <- rmDr$findElement(using = "xpath", ".//div[starts-with(@class, 'NameTitle__')]")$getElementText() %>% unlist()
  university <- rmDr$findElement(using = "xpath", "//a[@href='/school/1516']")$getElementText() %>% unlist()
  department <- rmDr$findElement(using = "xpath", "//a[starts-with(@class, 'TeacherDepartment__')]")$getElementText() %>% unlist()
  rating_root <- rmDr$findElements(using = "xpath", "//div[starts-with(@class, 'Rating__RatingBody')]")
  for (r in rating_root){
    rating_date <- r$findChildElement(using = "xpath", "(.//div[starts-with(@class, 'TimeStamp__StyledTimeStamp')])[2]")$getElementText() %>% 
      unlist() %>% mdy()
    if (rating_date < target_date){
      break
    }
    df1 <- collect(r)
  }
  Sys.sleep(3.0)
}

write.csv(df1, "C:\\Users\\valen\\OneDrive\\df1.csv", row.names = FALSE)
system('taskkill /im java.exe /f')

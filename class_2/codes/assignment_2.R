##########################
##     HOMEWORK         ##
##      CLASS 2         ##
##       CEU            ##
##########################

library(tidyverse)
library(data.table)
library(kableExtra)

# 0) Clear work space
rm(list = ls())


# 1) Load both data from github page and inspect (summary,glimpse)
#   Hint: you will need the `raw.github...` url address

df_schools <- read.csv("https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/class_2/data/assignment_2/raw/CASchools_schools.csv")
df_scores <- read.csv("https://raw.githubusercontent.com/CEU-Economics-and-Business/ECBS-5208-Coding-1-Business-Analytics/master/class_2/data/assignment_2/raw/CASchools_scores.csv")

summary(df_schools)
summary(df_scores)

# 2) Merge the two data table into one called `df`

df <- left_join(df_schools, df_scores, by = "district")
summary(df)

# 3) Put the district variable into numeric format

df$district <- as.numeric(df$district)
typeof(df$district)

# 4) Create two new variables from `school_county`: 
#     - school should contain only the name of the school - character format
#     - county should only contain the name of the county - factor format

df <- separate(df,col=school_county,sep=" - ",into=c("school","country"))

# 5) Find missing values, write 1 sentence what and why to do what you do and execute.
# as they seems to be completly random, we can drop these observations

filter( df , is.na( school ) )
filter( df , is.na( students ) )
filter( df , is.na( teachers ) )
filter( df , is.na( english ) )  #10 missing values, all from different  schools and countries
filter( df , is.na( read ) )   # 5 missing vales, al from different  schools and countries
filter( df , is.na( math ) ) #2 missing values

#dropping missing values as they are few compared to dataset and seem to have no information value
df <- filter( df , !is.na( english ) )
df <- filter( df , !is.na( read ) )
df <- filter( df , !is.na( math ) )

#cheking if they are dropped
table(df$english, useNA = "ifany")

# 6) Create a new variable called `score` which averages the english, math and read scores
df$score <- rowMeans(data.frame(df$math, df$english, df$read))
#check with cbind and remove duplicate
df$score2 <- rowMeans(cbind(df$math, df$english, df$read))
df$score2 <- NULL

# 7) Find the county which has the largest number of schools in the data 
#     and calculate the average and the standard deviation of the score.

head(arrange(count(group_by(df,country)), desc(n)),1)[1]
#with piping
group_by(df,country) %>% count() %>% arrange(desc(n)) %>% head(1)[1]
group_by(df,country) %>% count() %>% arrange(desc(n)) %>% head(1)[2]
#Sonoma has the most schools, 28


#calculating mean
sum(filter(df,country == "Sonoma")$score) / length(filter(df,country == "Sonoma")$score)
mean=mean(filter(df,country == "Sonoma")$score)
#calculating standanrd dev
stdev=sd(filter(df,country == "Sonoma")$score)

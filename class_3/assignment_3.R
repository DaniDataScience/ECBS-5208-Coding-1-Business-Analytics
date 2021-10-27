#########################
## Assignment 3        ##
##      CLASS 3        ##
##  Deadline:          ##
##  2021/10/27 13:30   ##
#########################

# In this assignment we are interested if 
# Males go more frequently to doctors than females. In order to answer this question we do an analysis on
#   a cross-section data originating from the 1977–1978 Australian Health Survey.

# 0) Clear work space
rm(list=ls())


# 1) Load tidyverse and modelsummary packages and install and load the "AER" package
library(tidyverse)
library(modelsummary)
install.packages("AER")
library(AER)


# Load the data of doctor visits from AER package (more info: https://rdrr.io/cran/AER/man/DoctorVisits.html)
data("DoctorVisits")
df <- DoctorVisits
rm(DoctorVisits)

# 2) Make a quick data summary on all the variables
glimpse(df)
view(df)
datasummary_skim(df)
summary(df)


# 3) Make descriptive statistics on the variable visits conditional on the gender
df$gender <- as.factor(df$gender)
datasummary(gender*(visits)~ 
              Mean + Median + SD + Min + Max + P25 + P75 + N + PercentMissing,
            data=df)


# 4) Create a ggplot where you display the conditional distributions of visits given the gender

#using relative frequency
ggplot(data = df , aes( x = visits , fill = gender ) ) +
  geom_histogram( aes( y = ..density.. ), binwidth=1 ) +
  labs( x = "# of visits in past 2 weeks" , y = 'Relative Frequency' ,
        fill = 'Gender' ) + 
  facet_wrap(~gender)  + 
  scale_x_discrete(limits=c(0:9))

#using absolute number of visits per bin 
ggplot(data = df , aes( x = visits , fill = gender ) ) +
  geom_bar( aes( x = visits) ) +
  labs( x = "# of visits in past 2 weeks" , y = 'Relative Frequency' ,
        fill = 'Gender' ) + 
  facet_wrap(~gender) + 
  scale_x_discrete(limits=c(0:9))


# 5-6) Would you consider this a useful graph? Use instead a stacked bar graph!
#   a) create a new tibble, which groups by gender and visits and count the cases (summarise)
#   b) create a ggplot using aux with the geometry: geom_bar. 
#       You should specify the following arguments: stat = "identity", position = "fill"


# creating the new table
stacked <- df %>% 
  group_by(gender, visits) %>%
  summarise("visit_count"=n())

# creating 100% stacked bars from new tibble
ggplot(data = stacked, aes( x = visits, y=visit_count , fill = gender )  ) +
  geom_bar( stat="identity", position="fill") + 
  labs( x = "# of visits in past 2 weeks" , y = 'ratio of visits' ,
        fill = 'Gender') +
  scale_x_discrete(limits=c(0:9))

# creating stacked bars from original df
ggplot(data = df , aes( x = visits , fill = gender ) ) +
  geom_bar( aes( x = visits) ) +
  labs( x = "# of visits in past 2 weeks" , y = 'Visit count' ,
        fill = 'Gender'
  ) + 
  scale_x_discrete(limits=c(0:9))

# 7) Test whether the number of visits are the same for male and female
#   Hint: check the t.test function and use `~` sign to define the two groups.
#   You should have a Welch-Two Sample t-test and its results.

t.test(visits ~ gender, data = df)
#t is greater than 1.98, so the difference is more significant than the noise

# The difference in means for our data is -0.126 (0.236 - 0.362).

# The confidence interval shows that the true difference in means
# is between -0.169 and -0.083.So, 95% of the time, the true difference
# will be different from 0.

# The p value is much smaller than 5%, so we can conclude that the difference is
# not equal to zero. The difference in number of visits for man and woman
# was significant (t= -5.722; p < 1.1e-8)

#first we'll load our libraries
library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(gt)
library(kableExtra)
options(dplyr.summarise.inform = FALSE)


# run this to load the data for this assignment
# it will create a dataframe called "impeach," with all House Democrats
impeach <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRh8d5JaDqBtByzNw2JZF3idaACobDhhk-p7chJoktA0HawELHFOvjQOqCpzGA4MGONvPlR7GASqW-K/pub?gid=1765341510&single=true&output=csv")


# FOR EACH OF THE QUESTIONS BELOW, WRITE YOUR WORKING R CODE TO RETURN THE REQUESTED RESULTS
# USE COMMENTS (PREFACED BY THE #) BEFORE THE WORKING CODE TO EXPLAIN WHAT YOU'RE DOING FOR EACH STEP

# 1) The column "for_impeachment" indicates whether the member has publicly called for
# an impeachment inquiry. Filter to return only the ones where the answer is NO.    

#used the filter function to filter the data to only show results where answer is no

table1 <- filter(impeach, for_impeachment == "NO")

table1 %>%
  knitr::kable() %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", font_size = 12, fixed_thead = T) %>%
  row_spec(0, bold = T, color = "pink", font_size = 15) %>%
  column_spec(2, bold = T) %>%
  column_spec(3, bold = T)

# 2) Filter to return only results where a member is both against impeachment, and comes from a 
# district that President Trump won in 2016 (which is noted in the "p16winningparty" column)

#used the filter function to filter the data in impeach to show members who voted no who come from a republican voting district

table2 <- filter(impeach, for_impeachment == "NO", p16winningparty == "R")

table2 %>%
  knitr::kable() %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", font_size = 12, fixed_thead = T) %>%
  row_spec(0, bold = T, color = "pink", font_size = 15) %>%
  column_spec(2, bold = T)%>%
  column_spec(3, bold = T)


# 3) Filter for only results where a member is against impeachment, comes from a 
# district that President Trump won in 2016 (which is noted in the "p16winningparty" column),
# and also comes from a district that Mitt Romney won in 2012 ("p12winningparty").

#used the filter function to filter on the basis of three variables, voting against impeachment, belonging to a district Trump won and that Mitt Romney also won

table3 <- filter(impeach, for_impeachment == "NO", p16winningparty == "R", p12winningparty == "R")

table3 %>%
  knitr::kable() %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", font_size = 12, fixed_thead = T) %>%
  row_spec(0, bold = T, color = "pink", font_size = 15) %>%
  column_spec(2, bold = T)%>%
  column_spec(3, bold = T)

# 4) Filter for only results from September 2019 where a member is a YES for impeachment. 

#used the filter function to filter members who voted yes, values in the date month column for september and values in the date year column for 2019

table4 <- filter(impeach, for_impeachment =="YES", date_month == 9, date_year == 2019)

table4 %>%
  knitr::kable() %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", font_size = 12, fixed_thead = T) %>%
  row_spec(0, bold = T, color = "pink", font_size = 15) %>%
  column_spec(2, bold = T)%>%
  column_spec(3, bold = T)


# 5) Filter for only results where a member is a YES for impeachment and is from a district
# where Clinton won more than 70 percent of the vote in 2016 (found in column "clinton_percent")

#used the filter function to filter members who voted yes and where clinton_percent was more than 70%

table5 <- filter(impeach, for_impeachment == "YES", clinton_percent > 70.0)

table5 %>%
  knitr::kable() %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", font_size = 12, fixed_thead = T) %>%
  row_spec(0, bold = T, color = "pink", font_size = 15) %>%
  column_spec(2, bold = T)%>%
  column_spec(3, bold = T)

# 6) Sort the entire dataframe based on the percentage of a district that has a 
# bachelor's degree or higher ("pct_bachelors"), from lowest to highest

#used the arrange filter to sort the dataframe based on ascending order of districts with percentage of bachelor's degree or higher

table6 <- arrange(impeach,pct_bachelors)

table6 %>%
  knitr::kable() %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", font_size = 12, fixed_thead = T) %>%
  row_spec(0, bold = T, color = "pink", font_size = 15) %>%
  column_spec(2, bold = T)%>%
  column_spec(3, bold = T)


# 7) Sort the just those who are NO on impeachment based on the percentage of a district that has a 
# bachelor's degree or higher ("pct_bachelors"), from lowest to highest

#used the pipe shortcut to combine the functions of filtering and sorting. 
#Used the filter function to find members who voted no and used the sort function to then arrange them in ascending order based on pct_bachelors

table7 <- impeach %>%
  filter(for_impeachment == "NO") %>%
  arrange(pct_bachelors)

table7 %>%
  knitr::kable() %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", font_size = 12, fixed_thead = T) %>%
  row_spec(0, bold = T, color = "pink", font_size = 15) %>%
  column_spec(2, bold = T)%>%
  column_spec(3, bold = T)

# 8) Sort the just those who are NO on impeachment based on the percentage of a district that has a 
# bachelor's degree or higher ("pct_bachelors"), from lowest to highest.
# Then filter those records by only those whose bachelor's percentage is below the national average (found
# in the "pct_bachelors_compared_to_national" column).

#used the pipe shortcut to combine the functions of filtering and sorting.
#Used the filter function to find members who voted no and used the sort function to then arrange them in ascending order based on pct_bachelors
#then used filter again to only show records of districts with bachelor's percentages below national average

table8 <- impeach %>%
  filter(for_impeachment == "NO") %>%
  arrange(pct_bachelors) %>%
  filter(pct_bachelors_compared_to_national == "BELOW")

table8 %>%
  knitr::kable() %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", font_size = 12, fixed_thead = T) %>%
  row_spec(0, bold = T, color = "pink", font_size = 15) %>%
  column_spec(2, bold = T)%>%
  column_spec(3, bold = T)


# 9) Filter for only members from New Jersey who are NO on impeachment

#used the pipe shortcut to combine the two functions of filtering

table9 <- impeach %>%
  filter(for_impeachment == "NO") %>%
  filter(state == "NJ")

table9 %>%
  knitr::kable() %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", font_size = 12, fixed_thead = T) %>%
  row_spec(0, bold = T, color = "pink", font_size = 15) %>%
  column_spec(2, bold = T)%>%
  column_spec(3, bold = T)


# 10) Filter for those who were YES on impeachment, with a declared date prior to 2019. So only
# those with dates before 2019.  Then sort those so that the highest Clinton vote percentages are 
# at the top.   

#used the pipe shortcut to combine the functions of filtering and sorting.
#filtered for those who voted yes and had a date prior to 2019
#then sorted it in descending order to have highest Clinton percentages first

table10 <- impeach %>%
  filter(for_impeachment == "YES") %>%
  filter(date_year < 2019) %>%
  arrange(desc(clinton_percent))

table10 %>%
  knitr::kable() %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", font_size = 12, fixed_thead = T) %>%
  row_spec(0, bold = T, color = "pink", font_size = 15) %>%
  column_spec(2, bold = T)%>%
  column_spec(3, bold = T)

# 11) Answer this question with a single numeric answer, and show the R code you
# used to reach that answer: How many members in the dataset who are holdouts on impeachment
# comes from districts with a GDP below the national figure?


#19

table11 <- count(impeach %>%
  filter(party == "D") %>%
  filter(for_impeachment == "NO") %>%
  filter(gdp_above_national == "BELOW"))

table11 %>%
  knitr::kable() %>%
  kable_classic() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "center", font_size = 12, fixed_thead = T) %>%
  row_spec(0, bold = T, color = "pink", font_size = 15) 






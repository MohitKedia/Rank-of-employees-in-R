library(readxl)

employee <- read.csv("employee_data.csv")
View(employee)
create_report(employee)
library(dplyr)

#Top 10 salaries 

employee %>% mutate(Rank=dense_rank(desc(salary))) %>% arrange(Rank) %>% filter(Rank<=10) #top 10 salaries 

#Top 10 salaries by groups

employee %>% filter(!is.na(salary)) %>% group_by(groups) %>% mutate(Rank=dense_rank(desc(salary))) %>% arrange(groups,Rank) %>%filter(Rank<=10)-> Ranks
View(Ranks) # top 10 salaries by groups

#Ranking groups by avg salaries

employee %>% group_by(groups) %>% summarise(avg_salary=mean(salary)) %>% mutate(Rank=rank(desc(avg_salary))) %>% arrange(Rank)

#Difference between 1st and 2nd salary across each groups

Ranks %>% group_by(groups) %>% mutate(second_high=lead(salary),diff=salary-lead(salary)) %>% filter(Rank<=1)

#Cumulative sum or Running sum

employee %>% mutate(Running_sum=cumsum(salary))

#IF-ELSE 
flights %>%
  transmute(carrier = carrier,
            isUA = if_else(carrier == "UA", "United", "Otherwise"))
#CASE WHEN
flights %>%
  transmute(carrier = carrier,
            newCol = case_when(
              carrier == "UA" ~ "United",
              carrier == "B6" ~ "JetBlue",
              TRUE ~ "Otherwise"
            ))

install.packages("nycflights13")
library("nycflights13")

View(flights)

#Which airline has the highest number of delayed departures?
flights %>% filter(dep_delay>0) %>% group_by(carrier) %>% summarise(Count=n()) %>% mutate(Ranks=rank(desc(Count))) %>% arrange(Ranks) %>% filter(Ranks==1)

#On average, to which airport do flights arrive most early?
flights %>% group_by(dest) %>% summarise(Avg=mean(arr_delay,na.rm = TRUE)) %>% mutate(Rank=rank(Avg)) %>% arrange(Rank)

#Using SELF JOIN

sqldf("SELECT e1.Name as Name1,e1.Salary,e2.Name as Name2 FROM Employees e1 JOIN Employees e2 
ON e1.Salary=e2.Salary AND e1.Name<>e2.Name")
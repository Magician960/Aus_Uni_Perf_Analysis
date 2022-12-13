install.packages("tidyverse")
library(tidyverse)
library(scales)

setwd('D:\\Comp_Sci_Projects\\Case Study')

uni_ranking <- read.csv("universities_ranking_v2.csv")
uni_score <- read.csv("universities_scores_v2.csv")

uni_data <- uni_ranking %>% inner_join(uni_score, 
                                       by = c("ranking", "title", "location"))
##OR

uni_data_v2 <- read.csv("uni_data_v2.csv")

head(uni_data)

view(uni_data)

## Relationship between student:staff ratio and teaching

ggplot(data = uni_data_v2) +
  geom_point(mapping = aes(x = students.staff.ratio, y = teaching.score,
                           color = is.Australia, alpha = is.Australia))+
  labs( x = "Students per Staff Ratio", y = "Teaching Score",
        title = "Student:Staff Ratio vs Teaching Quality",
        subtitle = "Score out of 100",
        caption = "Source: https://www.timeshighereducation.com/world-
        university-rankings/2021/world-ranking#!/page/0/length/-1/sort_by/rank/
        sort_order/asc/cols/scores")

## Relationship between student:staff ratio and research

ggplot(data = uni_data_v2) +
  geom_point(mapping = aes(x = students.staff.ratio, y = research.score,
                           color = is.Australia, alpha = is.Australia))+
  scale_fill_discrete(name = "Australia")+
  labs( x = "Students per Staff Ratio", y = "Research Score",
        title = "Student:Staff Ratio vs Research Quality",
        subtitle = "Score out of 100",
        caption = "Source: https://www.timeshighereducation.com/world-
        university-rankings/2021/world-ranking#!/page/0/length/-1/sort_by/rank/
        sort_order/asc/cols/scores")
## Relationship between enrollment numbers and teaching

ggplot(data = uni_data_v2) +
  geom_point(mapping = aes(x = number.students, y = teaching.score,
                           color = is.Australia, alpha = is.Australia))+
  scale_fill_discrete(name = "Australia")+
  labs( x = "Total Student Enrolment", y = "Teaching Score",
        title = "Total Student Enrolment vs Teaching Quality",
        subtitle = "Score out of 100",
        caption = "Source: https://www.timeshighereducation.com/world-
        university-rankings/2021/world-ranking#!/page/0/length/-1/sort_by/rank/
        sort_order/asc/cols/scores")+ 
  scale_x_continuous(labels = label_comma())


## Relationship between enrollment numbers and research

ggplot(data = uni_data_v2) +
  geom_point(mapping = aes(x = number.students, y = research.score,
                           color = is.Australia, alpha = is.Australia))+
  scale_fill_discrete(name = "Australia")+
  labs( x = "Total Student Enrolment", y = "Research Score",
        title = "Total Student Enrolment vs Research Quality",
        subtitle = "Score out of 100",
        caption = "Source: https://www.timeshighereducation.com/world-
        university-rankings/2021/world-ranking#!/page/0/length/-1/sort_by/rank/
        sort_order/asc/cols/scores")+ 
  scale_x_continuous(labels = label_comma())

## Creating averages for Australian vs rest-of-the-world average table

# For scores
overall_score_avg <- uni_data %>%
  group_by(group = ifelse(location == "Australia", "Australia", "Global")) %>%
  summarise(overall_score = mean(overall.score))

teaching_score_avg <- uni_data %>%
  group_by(group = ifelse(location == "Australia", "Australia", "Global")) %>%
  summarise(teaching_score = mean(teaching.score))

research_score_avg <- uni_data %>%
  group_by(group = ifelse(location == "Australia", "Australia", "Global")) %>%
  summarise(research_score = mean(research.score))

citations_score_avg <- uni_data %>%
  group_by(group = ifelse(location == "Australia", "Australia", "Global")) %>%
  summarise(citations_score = mean(citations.score))

industry_income_avg <- uni_data %>%
  group_by(group = ifelse(location == "Australia", "Australia", "Global")) %>%
  summarise(industry_income_score = mean(industry.income.score))

intl_outlook_avg <- uni_data %>%
  group_by(group = ifelse(location == "Australia", "Australia", "Global")) %>%
  summarise(intl_outlook_score = mean(intl.outlook.score))

aus_vs_global_score_avg <- list(overall_score_avg, teaching_score_avg, research_score_avg,
     citations_score_avg, industry_income_avg, intl_outlook_avg) %>%
  reduce(full_join, by = "group")

##Comparing AUS vs. Global chart

order <- c("overall_score", "teaching_score", "research_score", "citations_score", "industry_income_score", 
           "intl_outlook_score")

aus_vs_global_score_avg %>% pivot_longer(cols=c('overall_score',
                                                'teaching_score',
                                                'research_score',
                                                'citations_score',
                                                'industry_income_score',
                                                'intl_outlook_score'),
                                         names_to = "category",
                                         values_to = "score") %>%
  ggplot() + geom_col(mapping = aes( x = category, y = score, fill = group), position = "dodge") +
  scale_x_discrete(limits = order) +
  labs( x = "Category", y = "Avg Score out of 100",
        title = "Australian vs Global Universities Compared",
        subtitle = "Average based off teaching & research qualities and other metrics",
        caption = "Source: https://www.timeshighereducation.com/world-
        university-rankings/2021/world-ranking#!/page/0/length/-1/sort_by/rank/
        sort_order/asc/cols/scores") +
  theme(axis.text.x = element_text(angle = 75 , vjust = 0.6, face = "bold"))+
  scale_fill_discrete(name = "")

# For uni demographics




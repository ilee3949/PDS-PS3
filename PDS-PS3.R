###Problem Set 3
###Name: Isaac Lee
###ID: 438148

rm(list = ls())
library(dplyr)

##Part I: ggplot2
#Q1:
#first load the dataset
library(ggplot2)
PrimaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
#then make sure dates are in the correct format
PrimaryPolls$start_date<-as.Date(PrimaryPolls$start_date, "%m/%d/%Y")
#filter to states that are part of Super Tuesday using pipe
SuperTuesday = c('Alabama', 'Arkansas','California','Colorado','Maine','Massachusetts','Minnesota','North Carolina','Oklahoma',"Tennessee",'Texas','Utah','Vermont')
PrimaryPolls = PrimaryPolls %>% filter(state %in% SuperTuesday)
#also filter to the top six candidates
TopCands = c('Amy Klobuchar', 'Bernard Sanders','Elizabeth Warren','Joseph R. Biden Jr.','Michael Bloomberg','Pete Buttigieg')
PrimaryPolls = PrimaryPolls %>% filter(candidate_name %in% TopCands)
#Use Left Join to Include # of Delegates per state
state = SuperTuesday
delegate = c(52,31,415,67,24,91,75,110,37,64,228,29,16)
PrimaryPolls = left_join(PrimaryPolls,data.frame(state,delegate,stringsAsFactors = F), by= 'state')
#Plot the Data
ggplot(PrimaryPolls) +
  geom_point(mapping = aes(x=start_date,y=pct,color=delegate,),alpha=1) +
  geom_smooth(mapping = aes(x=start_date,y=pct),se=F,color='violet',alpha=.7) +
  facet_wrap(~ candidate_name,nrow=2) +
  labs(title = "Super Tuesday Polls by Date for Top Democratic Contestants") +
  labs() +
  xlab("Start Date") +
  ylab("Poll Percentage") +
  theme_minimal() 

#Q2:
#Re-read original dataset
data1 <- read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
data1$start_date<-as.Date(data1$start_date, "%m/%d/%Y")
#Filter data for only necessary data. 
data2 = data1 %>%
  filter(candidate_name %in% TopCands) %>%
  select(candidate_name,state,start_date,pct)
#Re-organize the dataset so that there is only one row for each candidate-state dyad
dyad <- data2 %>%
  group_by(candidate_name, state) %>%
  summarise(average_candidate=mean(pct), count=n())
#Compare Object Sizes
object_size(data2) #The filtered data is 160 kb
object_size(dyad) #the reorganized-by-dyad data is 12.1 kb

##Part II: Tidyverse
#Q3: 
#Create New objects as shown in the PSet PDF
library(fivethirtyeight)
library(tidyverse)
polls <- read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
Endorsements <- endorsements_2020
#Changed the Endorsements variable name endorsee to candidate_name
Endorsements <- rename(Endorsements, candidate_name=endorsee)
#Changed the Endorsements dataframe into a tibble object
as_tibble(Endorsements)
#Filter to top candidates and subset to the the five variables
polls = polls %>%
  filter(candidate_name %in%c("Amy Klobuchar","Bernard Sanders","Elizabeth Warren","Joseph R. Biden Jr.","Michael Bloomberg","Pete Buttigieg")) %>%
  select(candidate_name,sample_size,start_date,party,pct)
#Fix name discrepancy
polls$candidate_name = recode(polls$candidate_name, "Bernard Sanders" = "Bernie Sanders")
polls$candidate_name = recode(polls$candidate_name, "Joseph R. Biden Jr." = "Joe Biden")
#Combine two datasets by candidate name using dplyr
newdata <- inner_join(Endorsements,polls,by='candidate_name')
#Create variable for num endorsements for each candidate
end = Endorsements %>% count(candidate_name) %>% filter(candidate_name %in% polls$candidate_name)
#Plot
p = ggplot(end,aes(x=candidate_name,y=n))+geom_bar(stat="identity")
p
#Apply Dark Theme
p=p+theme_dark()
#Save this version of the plot (Saved as RPlot_Q3_Version1.png)
#Make the graph prettier by adding labels and other cool features
q= ggplot(end,aes(x=candidate_name,y=n))+geom_bar(stat="identity")+
  labs(title = "# of Endorsements by Candidate") +
  labs() +
  xlab("Candidate Name") +
  ylab("# of Endorsements") +
  theme_minimal() 
q=q+theme_dark()
q
#Save this prettier version too (Saves as RPlot_Q3_Version2.png)

##Part III: Text-as-Data
#Load prelim data as shown
library(tidyverse)
install.packages('tm')
library(tm)
install.packages('lubridate')
library(lubridate)
install.packages('wordcloud')
library(wordcloud)
tweets <- read_csv('https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv')
#separate tweets by date and time
library(dplyr)
library(tidyr)
tweets <- separate(tweets,created_at,into=c("date","time"),sep=" ",remove=TRUE)
#Find date range of tweets
range(tweets$date) #The earliest date is 1/1/2014 and the last date is 9/9/2019



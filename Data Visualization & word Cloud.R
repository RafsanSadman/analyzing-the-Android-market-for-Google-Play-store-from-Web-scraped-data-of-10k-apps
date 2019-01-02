library(dplyr)
library(ggplot2)
library(readr)
data_set <- read.csv('D:/Linkedin Learing path Data Science/Project/google-play-store-apps/googleplaystore.csv')
head(data_set)
data_set$Category <- tolower(data_set$Category)

#we visualize the most installed (one billion) apps's categories.
a <- data_set %>% select(Category,Installs) %>% filter(Installs == "1,000,000,000+") %>% group_by(Category) %>%arrange(Category)
ggplot(a,aes(x = Installs, fill = Category)) + geom_bar(position = "dodge")+coord_flip()
#communication, social and game apps are the most installed by over one billion people.

#How many apps included in each category ?
c <- data_set %>% group_by(Category) %>% summarise(count = n())%>%arrange(desc(count))
c <- head(c, 10)
ggplot(c, aes(x = Category, y = count)) +geom_bar(stat = "identity", width = 0.5, fill = "firebrick4") +labs(title = "Top 10 Categories")+theme(axis.text.x = element_text(angle = 45,vjust = 0.6))

#Installs' column have char value. We should do;
#getting rid of commas,
#removing the last letter ( + sign ) ,
#converting numeric to able to calculate sum of installs

data_set$Rating[data_set$Rating == ""] <- "None"
data_set <- data_set %>%filter(Installs != "0")
options(scipen = 999)

data_set$Installs <- gsub(",","", gsub("\\.","",data_set$Installs))

data_set$Installs <-as.character(data_set$Installs)
data_set$Installs = substr(data_set$Installs,1,nchar(data_set$Installs)-1)
data_set$Installs <- as.numeric(data_set$Installs)

#Because, family category apps have the highest income. (Money talks :) )
data_set %>% group_by(Category) %>% summarise(totalInstalls =sum(Installs))%>%arrange(desc(totalInstalls)) %>% head(10)%>%ggplot(aes(x = Category,y = totalInstalls, fill = Category)) + geom_bar(stat = "identity", fill = "REd")+ labs(title = "Top 10 Installed Categories")+ theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
data_set %>% filter(Type == "Paid")%>%group_by(Category)%>%summarise(totalInstalls = sum(Installs))%>%arrange(desc(totalInstalls))%>% head(10)%>%ggplot(aes(x =Category,y=totalInstalls))+geom_bar(stat = "identity",width = 0.5,fill = "Blue")+labs(title = "Top 10 paid categories")+ theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())
data_set %>% filter(Category == "family")%>%group_by(Genres)%>%summarise(Count = n()) %>% arrange(desc(Count))%>%head(10)%>%ggplot(aes(x = Genres,y = Count))+geom_bar(stat = "Identity",width = 0.5, fill ="Orange") + labs(title = "Top 10 Genres of 'Family Category")+theme(axis.text.x = element_text(angle = 45,vjust = 0.6))
#parents tend to pay for apps especially entertainment and education genres
data_set %>% filter(Type == "Free")%>%group_by(Category)%>%summarise(totalInstalls =sum(Installs))%>%arrange(desc(totalInstalls))%>%head(10)%>%ggplot(aes(x = Category,y = totalInstalls))+geom_bar(stat = "Identity",width = 0.5, fill ="DeepSkyblue2") + labs(title = "Top 10 Free Categories")+theme(axis.text.x = element_text(angle = 45,vjust = 0.6))
data_set %>% filter(Category == "game")%>%group_by(Genres)%>%summarise(Count = n()) %>% arrange(desc(Count))%>%head(10)%>%ggplot(aes(x = Genres,y = Count))+geom_bar(stat = "Identity",width = 0.5, fill ="cyan2") + labs(title = "Top 10 Genres of 'Game' Category")+theme(axis.text.x = element_text(angle = 45,vjust = 0.6))
#Action and Arcade game genres is the most preferred.
# Load Libraries, Set Working Directory, and Open Data
# setwd("c:/path/to/working/directory")
library(ggplot2)
library(scales)
library(reshape2)
library(lubridate)
source("z_theme.r")
resp <- read.csv("responses-curated.csv")

# Change the headers on our Response table
names(resp)<-c("time","color","gender","age","mbti")

# Let's get our date format right, in case we want to bin responses by time
resp$time<-strptime(resp$time,"%m/%d/%Y %H:%M:%S")

# Filtering out trolls, blank replies, and other nuisances
resp$status<-"Answered"
for(n in 1:nrow(resp)){
  if(resp$color[n]=="Troll"){resp$status[n]<-"Rejected"}
  if(resp$color[n]==""){resp$status[n]<-"Rejected"}
  if(resp$gender[n]=="Troll"){resp$status[n]<-"Rejected"}
  if(resp$gender[n]==""){resp$status[n]<-"Rejected"}
}; rm(n)
resp2<-subset(resp,status=="Answered")

resp2$color <- factor(resp2$color,
                   c("Red","Orange","Yellow","Green","Cyan",
                     "Blue","Violet","Magenta","Pink","Brown",
                     "White","Black","Grey","Other"))
resp2$gender<-factor(resp2$gender,
                     rev(c("Male","Female","Other")))

# Break Down by Gender
ggplot(resp2,aes(gender))+
  geom_bar(stat="count",position="fill",aes(fill=color),alpha=.8,color="black")+
  scale_fill_manual("Color",values=c("#FF0000","#FF8000","#FFFF00","#00C000","#00FF80",
                                     "#0000FF","#8000FF","#FF00FF","#FF80FF","#804000",
                                     "#FFFFFF","#000000","#808080","#F0F0F0"))+
  scale_y_continuous(labels=scales::percent)+
  labs(title="Gender Differences Between Color Choices",
       subtitle="The study Reddit actually signed up for.",
       x="",y="",
       caption="created by /u/zonination")+
  coord_flip()+
  z_theme()
ggsave("c-by-gender.png",height=5,width=9,dpi=100,type="cairo-png")

# Break Down by Age
ggplot(resp2,aes(age))+
  geom_histogram(position="fill",binwidth=5,aes(fill=color),alpha=.8,color="black")+
  scale_fill_manual("Color",values=c("#FF0000","#FF8000","#FFFF00","#00C000","#00FF80",
                                     "#0000FF","#8000FF","#FF00FF","#FF80FF","#804000",
                                     "#FFFFFF","#000000","#808080","#F0F0F0"))+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(limits=c(12.5,42.5))+
  labs(title="Age Differences Between Color Choices",
       subtitle="The study Reddit actually signed up for.",
       x="Age",y="",
       caption="created by /u/zonination")+
  facet_grid(.~gender)+
  z_theme()+
  theme(strip.text.x = element_text(size = 12, colour = "#525252"))+
  theme(strip.text.y = element_text(size = 12, colour = "#525252"))
ggsave("c-by-age.png",height=5,width=9,dpi=100,type="cairo-png")

# Break Down by MBTI
resp2$temp<-NA
for(n in 1:nrow(resp2)){
  if(resp2$mbti[n]=="INTJ" | resp2$mbti[n]=="INTP" | resp2$mbti[n]=="ENTJ" | resp2$mbti[n]=="ENTP" ){resp2$temp[n]<-"Rationals"}
  if(resp2$mbti[n]=="INFJ" | resp2$mbti[n]=="INFP" | resp2$mbti[n]=="ENFJ" | resp2$mbti[n]=="ENFP" ){resp2$temp[n]<-"Idealists"}
  if(resp2$mbti[n]=="ISTP" | resp2$mbti[n]=="ISFP" | resp2$mbti[n]=="ESTP" | resp2$mbti[n]=="ESFP" ){resp2$temp[n]<-"Artisans"}
  if(resp2$mbti[n]=="ISTJ" | resp2$mbti[n]=="ISFJ" | resp2$mbti[n]=="ESTJ" | resp2$mbti[n]=="ESFJ" ){resp2$temp[n]<-"Guardians"}}
# Plot
ggplot(resp2,aes(x=mbti))+
  geom_bar(position="fill",stat="count",aes(fill=color),alpha=.8,color="black")+
  scale_fill_manual("Color",values=c("#FF0000","#FF8000","#FFFF00","#00C000","#00FF80",
                                     "#0000FF","#8000FF","#FF00FF","#FF80FF","#804000",
                                     "#FFFFFF","#000000","#808080","#F0F0F0"))+
  scale_y_continuous(labels=scales::percent)+
  labs(title="MBTI Differences Between Color Choices",
       subtitle="The study Reddit actually signed up for.",
       x="",y="",
       caption="created by /u/zonination")+
  facet_grid(temp~.,scale="free_y",space="free_y")+
  coord_flip()+
  z_theme()+
  theme(strip.text.x = element_text(size = 12, colour = "#525252"))+
  theme(strip.text.y = element_text(size = 12, colour = "#525252"))
ggsave("c-by-mbti.png",height=9,width=9,dpi=100,type="cairo-png")
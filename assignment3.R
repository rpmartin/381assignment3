###### This is the file where you save your R code that produces the objects (graphs, tables) that you want to embed into 
# your assignment.  Make sure that there are no errors when you source this file before sourcing it from assignment3.Rmd file.
# To show you how it is done I have included code below that answers questions 1 and 2.

rm(list=ls()) #makes sure that your work environment is clean.
library("tidyverse") #we use functions from this library.
slash <- ifelse(.Platform$OS.type=="unix", "/", "\\") #The eternal directory battle: Windows vs. *nix
####################which section?
section <- 1
#########################
mydf <- read_csv(paste("publicdata",slash,"wtpvswta.csv",sep="")) # reads in the data
mydf$strategy <- factor(mydf$strategy,labels=c("no hints","hints")) # strategy is coded 0/1: instead give it meaningful names.
mydf$size <- factor(mydf$size,labels=c("group of 4","group of 64")) # size is coded 4/64: give it meaningful names.
mydf <- mydf%>%
  mutate(endowment_effect=ask-bid)

wrong_belief <- mydf%>%
  group_by(size,prob)%>%
  count()%>%
  filter((size=="group of 4" & prob=="low")|(size=="group of 64" & prob=="high"))%>%
  ungroup()%>%
  summarise(sum(n))%>%
  pull()

######Question 3

first_plot <- ggplot(mydf,aes(x=size, y=bid, fill=strategy))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(position=position_jitterdodge(jitter.width=.5,jitter.height=1),alpha=0.3) + 
  labs(y="bids", 
       x="group size",
       fill="strategy manipulation")


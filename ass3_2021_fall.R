###### This is the file where you save your R code that produces the objects (graphs, tables) that you want to embed into 
# your assignment.  Make sure that there are no errors when you source this file before sourcing it from assignment3.Rmd file.
# To show you how it is done I have included code below that answers questions 1 and 2.

library("tidyverse") #we use functions from this library.
library("plotly")
library("here")
here::i_am("ass3_2021_fall.R")
###########################which section?
section <- 1
##########################The program
public_folder <- paste0("publicdata",section)
all_data <- paste0("chocolate",section,".csv")
mydf <- read_csv(here(public_folder,all_data))%>%
   mutate(group_size=factor(group_size,levels=c("low","high"), labels=c("small group","large group")),
         hints=factor(hints,levels=c("yes","no"),labels=c("hints","no hints")),
         treatment=paste(group_size,hints,sep=", "))

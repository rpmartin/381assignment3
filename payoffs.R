library("tidyverse")
library("here")
here::i_am("payoffs.R")
set.seed(123)
section <- 1
public_folder <- paste0("publicdata",section)
all_data <- paste0("chocolate",section,".csv")
#payment_file <- paste0("payment",section,".csv")
mydf <- read_csv(here(public_folder,all_data))%>%
  group_by(oneid)%>%
  mutate(sum_prob_endowed=sum(prob_endowed))%>%
  ungroup()

#functions
highest_bidder <- function(mydf){
  mydf%>%
    slice_max(order_by=bid,n=1,with_ties = FALSE)%>%
    select(oneid)%>%
    pull()
}
second_highest_bid <- function(mydf){
  top_two <- mydf%>%
    slice_max(order_by=bid,n=2, with_ties = FALSE)%>%
    slice_min(order_by=bid,n=1, with_ties= FALSE)%>%
    select(bid)%>%
    pull()
}
lowest_asker <- function(mydf){
  mydf%>%
    slice_min(order_by=ask,n=1,with_ties = FALSE)%>%
    select(oneid)%>%
    pull()
}
second_lowest_ask <- function(mydf){
  bottom_two <- mydf%>%
    slice_min(order_by=ask,n=2,with_ties = FALSE)%>%
    slice_max(order_by=ask,n=1,with_ties= FALSE)%>%
    select(ask)%>%
    pull()
}

#program

fake_data <- mydf%>% #sometimes odd number problem... pad out their group with a fake student who bids and asks 5.
  group_by(round,hints,group_size,grp)%>%
  summarize(group_sizes=n())%>%
  filter(group_sizes==1)%>%
  mutate(oneid=55555,
         bid=5,
         ask=5)%>%
  select(-group_sizes)


mydf <- bind_rows(fake_data,mydf)

nested_df <- mydf%>%
  group_by(round, hints, group_size, grp)%>%
  nest()%>%
  mutate(highest_bidder=map_dbl(data,highest_bidder),
         second_highest_bid=map_dbl(data,second_highest_bid),
         lowest_asker=map_dbl(data,lowest_asker),
         second_lowest_ask=map_dbl(data,second_lowest_ask),
         sell_back=ifelse(highest_bidder==lowest_asker,TRUE,FALSE))

auctions_won <- nested_df%>%
  group_by(highest_bidder)%>%
  summarize(auctions_won=n())

best_guesser <- full_join(mydf,auctions_won,by=c("oneid"="highest_bidder"))%>%
  mutate(error=abs(auctions_won-sum_prob_endowed))%>%
  ungroup()%>%
  slice_min(error)%>%
  filter(round==1)

buys_bar <- nested_df%>%
  filter(sell_back==FALSE)%>%
  select(highest_bidder,second_highest_bid)%>%
  rename(oneid=highest_bidder,pays=second_highest_bid)%>%
  mutate(purchased="n")



sells_back <- nested_df%>%
  filter(sell_back==TRUE)%>%
  mutate(receives=second_lowest_ask-second_highest_bid)%>%
  select(highest_bidder,receives)%>%
  rename(oneid=highest_bidder)%>%
  mutate(paid="n")

which_round <- sells_back%>%
  group_by(round)%>%
  summarize(min=min(receives),
            sum=sum(receives))%>%
  slice_max(order_by=min,n=5)%>%# don't want to be taking $ from students.
  slice_min(order_by=sum,n=1)%>%# don't want too spend much.
  select(round)%>%
  pull()

buys_bar <- buys_bar%>%
  filter(round==which_round)

write_csv(buys_bar,here("privatedata","buysbar.csv"))

sells_back <-sells_back %>%
  filter(round==which_round)

write_csv(sells_back,here("privatedata","sellsback.csv"))

buy_ids <- buys_bar%>%
  ungroup()%>%
  select(oneid)%>%
  pull()

sell_ids <- sells_back%>%
  ungroup()%>%
  select(oneid)%>%
  pull()

come_on_up <- c(buy_ids,sell_ids)%>%
  matrix(nrow=3)

# libraries
library(DT)
library(shiny)
library(dplyr)
library(tigris)
library(leaflet)

#removing environment
#rm(list=ls())
par(mar=c(1,1,1,1))

# color palettes
#install.packages("wesanderson")
library(wesanderson)

#data
d <- read.csv('f1000.csv', stringsAsFactors=FALSE)
female <- read.csv('Female_2018_Fortune500.csv', stringsAsFactors=FALSE)

#what to display
d.utf8 <- read.csv('f1000.csv', encoding="UTF-8", stringsAsFactors=FALSE) #encoded
disp = d.utf8 %>% select(rank, title, CEO, CEO.gender, Sector, Industry, City, State)

#cleaning (revenue and profits as numeric)
d$Revenues...M. <- as.numeric(gsub('[$,]', '', d$Revenues...M.))
d$Profits...M. <- as.numeric(gsub('[$,]', '', d$Profits...M.))

#themes
#install.packages("semantic.dashboard")

#dropdowns vectors
sectors <- levels(d$Sector)
states.unique <- unique(d$State)

#data after omitting CEO.gender==NAs 
wo.na <- d[!is.na(d$CEO.gender),]

# state descending order
wo.na %>%
  group_by(State) %>% 
  count(State) %>%
  arrange(n) -> o
stateorder = o$State

#sector descending order
wo.na %>%
  group_by(Sector) %>% 
  count(Sector) %>%
  arrange(n) -> o
secorder = o$Sector

# for maps legends
title.ceo <- paste0('<strong>',d$title,'</strong><br>CEO: ',d$CEO)

#states
#install.packages('tigris')
states <- states(cb=T)
# for choropleth (by states)
d.state <- d %>% group_by(State) %>% summarize(total=n())
d.merged <- geo_join(states,d.state,"STUSPS","State")
pal <- colorNumeric("YlGnBu",domain=d.merged$total)
qpal <- colorQuantile("YlGnBu",domain=d.merged$total)
d.merged <- subset(d.merged, !is.na(total))
d.popup <- paste(d.merged$NAME," (",as.character(d.merged$total),")",sep="")

library('quantmod')
#library('plotly')
#stock
comp.titles <- d$title
fem.titles <-d$title[d$CEO.gender=='female'&!is.na(d$CEO.gender)]
#comp.titles

stock.plot <- function(input,date,log){
  symbol <- getSymbols(d$ticker[d$title==input], src = "yahoo", env=.GlobalEnv,
                       from = date[1], to = date[2])
  return(chartSeries(get(symbol),log.scale = log,
                     show.grid = FALSE,type="line",
                     theme = chartTheme("white",up.col='navy'),name=symbol))
}

# facts data
ceo.trend <- read.csv('women_ceos_fortune_500_2018.csv')
board.trend <- read.csv("women_board_members_f500_2018.csv")

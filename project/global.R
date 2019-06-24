library(DT)
library(shiny)
library(dplyr)
library(tigris)
library(leaflet)

rm(list=ls())
par(mar=c(1,1,1,1))

# color palettes
#install.packages("wesanderson")
library(wesanderson)

#data
setwd("~/Documents/NYCDSA")
d <- read.csv('./project.fortune1000/f1000.csv', stringsAsFactors=FALSE) #encoding="UTF-8",
female <- read.csv('./project.fortune1000/Female_2018_Fortune500.csv', stringsAsFactors=FALSE)

#what to display
d.utf8 <- read.csv('./project.fortune1000/f1000.csv', encoding="UTF-8", stringsAsFactors=FALSE)
disp = d.utf8 %>% select(rank, title, CEO, CEO.gender, Sector, Industry, City, State)

#cleaning (revenue and profits as numeric)
d$Revenues...M. <- as.numeric(gsub('[$,]', '', d$Revenues...M.))
d$Profits...M. <- as.numeric(gsub('[$,]', '', d$Profits...M.))

#themes
#install.packages("semantic.dashboard")

#dropdowns
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


# maps
title.ceo <- paste0('<strong>',d$title,'</strong><br>CEO: ',d$CEO)

#states
#install.packages('tigris')
states <- states(cb=T)
# for choropleth (by states)
d.state <- d %>% group_by(State) %>% summarize(total=n())
d.merged <- geo_join(states,d.state,"STUSPS","State")
#genderpalette <- colorFactor(wes_palette("Cavalcanti1")[1:2], domain = c("male", "female"))
pal <- colorNumeric("YlGnBu",domain=d.merged$total)
qpal <- colorQuantile("YlGnBu",domain=d.merged$total)
d.merged <- subset(d.merged, !is.na(total))
d.popup <- paste(d.merged$NAME," (",as.character(d.merged$total),")",sep="")

library('quantmod')
library('plotly')
#stock
comp.titles <- d$title
fem.titles <-d$title[d$CEO.gender=='female'&!is.na(d$CEO.gender)]
#comp.titles
start_date <- as.Date("2017-01-02")
end_date <- as.Date("2019-06-22")

stock.plot <- function(input,date,log){
  symbol <- getSymbols(d$ticker[d$title==input], src = "yahoo", env=.GlobalEnv,
                       from = date[1], to = date[2])
  return(chartSeries(get(symbol),log.scale = log,
                     show.grid = FALSE,type="line", #get(symbol)
                     theme = chartTheme("white",up.col='navy'),name=symbol))
}

# namedStock <- function(name="Microsoft",
#                        start=Sys.Date()-365,
#                        end=Sys.Date()-1){
#   ticker <- symbols[agrep(name,symbols[,2]),1]
#   getSymbols(
#     Symbols=ticker,
#     src="yahoo",
#     env=.GlobalEnv,
#     from=start_date,to=end_date)
# }

# facts data
ceo.trend <- read.csv('./project.fortune1000/women_ceos_fortune_500_2018.csv')
board.trend <- read.csv("./project.fortune1000/women_board_members_f500_2018.csv")

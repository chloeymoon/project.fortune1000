rm(list=ls())


library(dplyr)

#d <- getSymbols(c("AAPL","IBM"))

# Getting Data
setwd("~/Documents/NYCDSA/")
f1000 <- read.csv('./shiny_project/fortune1000-final.csv')
#female <- read.csv('Female_2018_Fortune500.csv')

# Cleaning: female names are without middle names, thus getting rid of middle names from the CEO's title

# inner joining inner_join(x,y,by='name_common_col')
#female.comps <- female %>% select(title=Company)
#f <- inner_join(f500,female.comps)

# another way to find female names is to use package "gender"
#install.packages('gender')
library(gender)
f1000$CEO <- as.character(f1000$CEO)

# Getting rid of initials
firstname = function(full){
  split = strsplit(gsub("[.]","",full) , " ",useBytes = TRUE)[[1]]
  no.init = split[nchar(split)>1]
  return(paste(no.init,collapse=" "))
}
f1000$CEO <- sapply(f1000$CEO,firstname)

# Assigning gender based on first names
gen = function(name){
  gen = gender(gsub("^(.*?)\\s.*", "\\1", name,useBytes = TRUE))
  ifelse(length(gen)!=1, gen$gender, NA)
}
f1000$CEO.gender <- sapply(f1000$CEO, gen)


# manually changing male CEOs with female names
actually.men = c("Satya Nadella","Inge Thulin","Leslie Moonves","Kelly King","Jan Carlson",
                 "Leslie Wexner","Ajita Rajendra","Ara Hovnanian","Gale Klappa",
                 "Robin Hayes","Patrice Louvet","Jan Zijderveld")
f1000$CEO.gender[f1000$CEO %in% actually.men] <- "male"

#f1000 %>% filter(f1000$CEO.gender=="female") %>% select(title, CEO, CEO.gender)


# ticker
library(stringr)
symbols <- stockSymbols()
symbols2 <- read.csv(
  "ftp://ftp.nasdaqtrader.com/SymbolDirectory/nasdaqlisted.txt",
  sep="|")
get.ticker <- function(name){
  first2 = ifelse(!is.na(word(name, 1,2, sep=" ")),word(name, 1,2, sep=" "),name)
  v1=symbols[agrep(first2,symbols[,2]),1]
  ifelse(length(v1)>0,v1,symbols2[agrep(name,symbols2[,2]),1])
}
f1000 <- f1000 %>% mutate(ticker=sapply(enc2utf8(as.character(title)),get.ticker))

# writing 
setwd("~/Documents/NYCDSA/shiny_project")
write.csv(f1000, file = "f1000.csv")

####### MAPS
# Comp name & ceo for popups in maps
# FOR STATE
# d.state <- d %>% group_by(State) %>% summarize(total=n())
# d.merged <- geo_join(states,d.state,"STUSPS","State")
# d.merged <- subset(d.merged, !is.na(total))
# d.popup <- paste0("Total: ",as.character(d.merged$total))
# palette <- colorNumeric("Greens",domain=d.merged$total)
# 
# title.ceo <- paste0(d$title,"\n","CEO: ",d$CEO)




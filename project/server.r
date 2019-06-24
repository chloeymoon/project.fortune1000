library(googleVis)
library(ggplot2)
#install.packages('leaflet')
library(leaflet)
#install.packages('quantmod')
#install.packages('xts')
#library('xts')
#require(devtools)
#install_version("xts", version = "0.9-7", repos = "http://cran.us.r-project.org")

shinyServer(function(input, output){

  #### visualizations
  # Infographics comparing overall vs. Female ceos
  output$trend <- renderPlot(
    ceo.trend %>% 
      ggplot(aes(x=year)) +
      geom_line(aes(y=share_of_ceos_who_are_women_in_percents,colour="CEO"),size=.75) +
      geom_point(aes(y=share_of_ceos_who_are_women_in_percents,colour="CEO"),size=1) +
      geom_line(data=board.trend,aes(x=year,y=share_of_board_members_who_are_women_in_percents,
                                     colour="Board"), size=.75) + 
      geom_point(data=board.trend,aes(y=share_of_board_members_who_are_women_in_percents,colour="Board"),size=1) +
      scale_colour_manual("", labels = c("Board","CEO"),
                          values = wes_palette(n=2, name="Moonrise2")) +
      ylab("Share (%)")
  )
  
  
  # by sectors
  output$bysector <- renderPlot( #input$sec
    if(input$sec=='All Sectors'){
      ggplot(wo.na,aes(x=Sector)) + 
        geom_bar(aes(fill=CEO.gender)) + 
        scale_x_discrete(limits = secorder) +
        scale_fill_manual("", values = wes_palette(n=2, name="Cavalcanti1")) +
        coord_flip() +
        theme(axis.text.y=element_text(size=14,face = "bold"),
              axis.title.y=element_blank())
    } else {
        d.bysector() %>%
        count() %>%
        ggplot(aes(x=CEO.gender,y=n,fill=CEO.gender)) + 
        geom_col() + ylab("Count") + xlab("CEO Gender") + 
        ggtitle(paste0("Sector: ",input$sec)) +
        scale_fill_manual("", values = wes_palette(n=2, name="Cavalcanti1")) +
        theme(plot.title = element_text(size=16,face = "bold"), 
              axis.text.x=element_text(size=12,face = "bold"))
    }
  )
  
  d.bysector <- reactive({
    wo.na %>%
      filter(Sector==input$sec) %>% 
      group_by(CEO.gender)
  })

  
  output$pie.sector <- renderPlot(
    if(input$sec!='All Sectors'){
      count.bygender() %>% 
        arrange(desc(n)) %>%
        ggplot(aes(x="", y=n, fill=CEO.gender)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start=0) +
        scale_fill_brewer(palette="Dark2") +
        geom_text(aes(y = n/2 + c(0, cumsum(n)[-length(n)]), 
                      label = paste0(round(n/sum(n)*100),"%\n",CEO.gender)),
                  size=5, fontface = "bold") +
        theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
              axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),
              legend.position="none",
              panel.background = element_blank()
           )
    }
  )
  
  # reactive -- data is grouped by gender and count
  count.bygender <- reactive({
    if(input$sec=='All Sectors'){
      wo.na %>%
        group_by(CEO.gender) %>%
        count()
    } else {
      d.bysector() %>%
        count()
    }
  })
  
  male.count <- reactive({
    a <- count.bygender()
    a$n[a$CEO.gender=='male']
  })
  
  female.count <- reactive({
    a <- count.bygender()
    a$n[a$CEO.gender=='female']
  })
  
  # infoboxes
  output$male.count <- renderUI({ #renderInfoBox
    d <- count.bygender()
    infoBox("Male CEOs",
            value= male.count(),
            color="grey",
            icon=icon('man'),
            size="small", width=1)
  })
  
  output$female.count <- renderUI({
    d <- count.bygender()
    infoBox("Female CEOs",
            value= female.count(),
            color="grey",
            icon=icon('woman'),
            size="small",width=1)
  })
  
  # by sector - pi chart
  
  # financials (revenue $ profits)
  output$financials.box <- renderPlot(
    d %>% filter(!is.na(CEO.gender)) %>%
      ggplot(aes(x=CEO.gender,y=get(input$radio))) + 
      geom_boxplot(outlier.colour=NA) +xlab("CEO's gender") + ylab(ifelse(input$radio=="Revenues...M.","Revenue (mil)","Profit (mil)")) +
      coord_cartesian(ylim=c(0,ifelse(input$radio=="Revenues...M.",30000,3100)))
  )
  


  # by location
  output$map <- renderLeaflet(
    leaflet(d) %>%
      addProviderTiles("CartoDB.Positron") %>% #addTiles() 
      setView(-95.7,37.09,3.5) %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       fillOpacity=0.7,
                       #fillColor=~(as.factor(d$CEO.gender)),
                       color=~(colorFactor(wes_palette("Cavalcanti1")[1:2], domain = c("male", "female")))(CEO.gender),
                       stroke=FALSE, 
                       label=lapply(title.ceo,HTML)) %>%
      leaflet::addLegend("bottomright", title="", opacity=1,
                         labels= c("female CEOs","male CEOs"), colors=wes_palette("Cavalcanti1")[1:2])
  )
  

  
  # by state -- chloropleth
  output$states <- renderLeaflet(
    leaflet(d) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-95.7,37.09,3.5) %>%
      addPolygons(data = d.merged, 
                  fillColor = ~pal(d.merged$total), 
                  fillOpacity = 0.7, 
                  weight = 0.2, 
                  smoothFactor = 0.2, 
                  popup = ~d.popup) %>%
      addCircleMarkers(data=d[d$CEO.gender=='female',],
                       lng=~Longitude,lat=~Latitude,
                       stroke=FALSE,
                       fillColor='#972D15', fillOpacity=0.9,
                       radius=5,
                       label=lapply(paste0('<strong>',
                                           d[d$CEO.gender=='female',]$title,
                                           '</strong><br>CEO: ',
                                           d[d$CEO.gender=='female',]$CEO),
                                    HTML)) %>%
      leaflet::addLegend(pal = pal,
                values = d.merged$total,
                position = "bottomright",
                title = "Count (All)", opacity=1)
    )
  
  ### 
  d.bystate <- reactive({
    d %>% filter(State==input$state&!is.na(State)) %>% group_by(CEO.gender)
  })
  
  output$state.barchart <- renderPlot(
    if(input$state=='All States'){
      wo.na %>% 
      ggplot(aes(x=State)) + 
        geom_bar(aes(fill=CEO.gender)) + 
        scale_x_discrete(limits = stateorder) +
        scale_fill_manual("", values = wes_palette("Cavalcanti1")[3:4]) +
        coord_flip() +
        theme(axis.text.y=element_text(size=9),
              axis.title.y=element_blank())
    } else {
      d.bystate() %>% 
        count() %>%
        ggplot(aes(x=CEO.gender,y=n,fill=CEO.gender)) + 
        geom_col() + ylab("Count") + xlab("CEO Gender") + 
        ggtitle(paste0("State: ",input$state)) +
        scale_x_discrete(limits = c("female","male")) +
        scale_fill_manual("", values = wes_palette("Cavalcanti1")[3:4]) +
        theme(plot.title = element_text(size=16,face = "bold"), 
              axis.text.x=element_text(size=12,face = "bold"))
    }
  )
  
  
  # #### Observe event
  # observeEvent(input$Map_shape_click, { # update the location selectInput on map clicks
  #   p <- input$Map_shape_click
  #   print(p)
  # })
  
  state.prop <- reactive({
    t <- d.bystate() %>% filter(!is.na(CEO.gender)) %>% count()
    malecount = t$n[t$CEO.gender=='male']
    fcount = ifelse(t$CEO.gender=='female',t$n[t$CEO.gender=='female'],0)
    paste0(fcount,"/",(malecount+fcount))
  })
  
  ######### Value Boxes by state
  output$state.count <- renderValueBox(
    if(input$state!='All States'){
      valueBox(value=state.prop(), 
               subtitle='#female/#total',size="small",width=1)
    }
  )

  
  # stock prices since they became CEOs
  output$stock <- renderPlot(
    stock.plot(input$selected.comp,input$stock.dates,input$log)
  )
  
  joined <- reactive({
    female <- female %>% select(ticker=Ticker,ceo.date=Date)
    dat = left_join(d,female,by="ticker")
  })
  
  comp.date <- reactive({
    input.data <- joined()
    input.data$ceo.date[input.data$title==input$selected.comp]
  })
  
  output$fem.ceo.date <- renderText(
    paste0("CEO: ", d$CEO[d$title==input$selected.comp]," since ",comp.date())
  )
  
  output$stock2 <- renderPlot(
    stock.plot(input$selected.comp2,input$stock.dates2,input$log2)
  )
  
  # data
  output$table <- renderDataTable({
    datatable(disp, rownames=FALSE) %>%
      formatStyle('CEO.gender', target="row", 
                  backgroundColor=styleEqual(c("female","male"),c("aliceblue","white")))
  })
})
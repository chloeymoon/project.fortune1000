head(d)

wo.na <- d[!is.na(d$CEO.gender),]
ggplot(wo.na,aes(x=Sector)) + geom_bar(aes(fill=CEO.gender)) + coord_flip()



#@ server
# show map using googleVis
# output$map <- renderGvis({
#   gvisGeoChart(state_stat, "state.name", input$selected,
#                options=list(region="US", displayMode="regions", 
#                             resolution="provinces",
#                             width="auto", height="auto"))
# })

# show histogram using googleVis
# output$hist <- renderGvis({
#   gvisHistogram(state_stat[,input$selected, drop=FALSE])
# })

wo.na %>%
  group_by(CEO.gender) %>%
  count() %>%
  ggplot(aes(x=CEO.gender,y=n)) + geom_col() + ylab("Count") + xlab("CEO Gender")




####################################################################################
############ density plots for financials ################################################
# helper reactive function for financial.hist
# d <- reactive({
#   if(length(input$checkGroup)==1){
#     selected=input$checkGroup
#     ggplot((d[d$CEO.gender==selected&!is.na(d$CEO.gender==selected),]),aes(x=Revenues...M.)) + 
#       geom_density() +xlim(0,100000)
#   }
# })

# output$financials.hist <- renderPlot(
#   
# )
# 
output$rev2 <- renderPlot(
  # #ggplot(d,aes(x=Revenues...M.)) + xlim(0,100000) + geom_density(),
  # if(input$checkGroup==1){
  #   ggplot(d,aes(x=Revenues...M.)) + xlim(0,100000) + geom_density()
  # } else if(input$checkGroup==2){
  #   ggplot((d[d$CEO.gender=='female'&!is.na(d$CEO.gender=='female'),]),aes(x=Revenues...M.)) + geom_density() +xlim(0,100000)
  # } else {
  #   ggplot((d[d$CEO.gender=='male'&!is.na(d$CEO.gender=='male'),]),aes(x=Revenues...M.)) + geom_density() +xlim(0,100000)
  # }
  ggplot(d,aes(x=Revenues...M., color=CEO.gender)) + geom_density() + xlim(0,100000)
)

# ggplot(d,aes(x=Revenues...M.)) + xlim(0,100000) + geom_density()
# if (input$radio==1){
#   # LIMITED OUTLIERS e.g. excluded those with revenue > 100000
#   # ggplot(d,aes(x=c(d$CEO.gender),y=d$Revenues...M.)) + geom_boxplot() +coord_cartesian(ylim = c(0,100000))
#   ggplot(d,aes(x=Revenues...M.)) + xlim(0,100000) + geom_density()
# } else if (input$radio==2){ #male
#   ggplot((d[d$CEO.gender=='male'&!is.na(d$CEO.gender=='male'),]),aes(x=Revenues...M.)) + geom_density() +xlim(0,100000)
# } else { #female
#   ggplot((d[d$CEO.gender=='female'&!is.na(d$CEO.gender=='female'),]),aes(x=Revenues...M.)) + geom_density() + xlim(0,100000)
# }



############# RADIO BUTTONS
fluidRow(
  checkboxGroupInput("checkGroup", label = h2("Checkbox group"), 
                     choices = list("All"="all","Male CEO"="male", "Female CEO"="female"),
                     selected="all"),
  verbatimTextOutput("rev"),
  plotOutput('rev2')
)

ggplot(d,aes(x=Revenues...M.)) + xlim(0,100000) + geom_density() +
  geom_density(data=d[d$CEO.gender=='female'&!is.na(d$CEO.gender=='female'),])
ggplot((d[d$CEO.gender=='female'&!is.na(d$CEO.gender=='female'),]),aes(x=Revenues...M.)) + geom_density() +xlim(0,100000)

output$rev <- renderPrint(
  length(input$checkGroup)
)


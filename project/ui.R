library(shinydashboard)
# https://appsilon.com/create-outstanding-dashboards-with-the-new-semantic-dashboard-package/
library(semantic.dashboard)
#library(shinythemes)


shinyUI(dashboardPage(
  # theme: https://semantic-ui-forest.com/themes/
  # icons: https://semantic-ui.com/elements/icon.html
  #theme = "lumen", #lumen, simplex, paper, flatly, sandstone
  # header
  dashboardHeader(
    title = "Fortune 1000 Companies with emphasis on Female CEOs"
    ),
  
  # sidebar
  dashboardSidebar(
    # sidebarUserPanel("",image = ""),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("angle down")),
   #   menuItem("Facts & Trends", tabName = "facts", icon = icon("check")),
      menuItem("Sector", tabName = "sector", icon = icon("industry")),
      menuItem("Location", tabName = "location", icon = icon("compass")),
      menuItem("Financials", tabName = "financials", icon = icon("money")),
     # menuItem("Case Study", tabName = "case", icon = icon("venus")),
      menuItem("Stock", tabName = "stock", icon = icon("chart line")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    )
  ),
  
  # by.sector <- function(){
  #   return(fluidRow(
  #       column(3, box(h3("Choose a sector"),
  #                     selectizeInput("sec", "",rev(secorder)),
  #                     plotOutput("bysector"))),
  #       column(3,box(infoBoxOutput('male.count'),
  #                 infoBoxOutput('female.count')))
  #   ))},
  

  # body
  dashboardBody(
    tabItems(
      tabItem(tabName="overview",
              fluidRow(h4("Share of Female CEOs and Board Members (%)")),
              fluidRow(plotOutput('trend',width="400px",height="250px"))
      ),
      # tabItem(tabName="facts", br(),
      #         box(h3("Share of Female CEOs and Board Members (%)"),br(),
      #             fluidRow(plotOutput('trend')),width=9)
      # ),
      
      tabItem(tabName="sector",
              fluidRow(h2("Fortune 1000 Companies by Sector & CEO Gender")),
              fluidRow(
                column(10, h3("Choose a sector"),
                       selectizeInput("sec", "",c("All Sectors",rev(secorder))),
                       plotOutput("bysector")),
                column(6, br(),br(),
                       infoBoxOutput('male.count'), br(),
                       infoBoxOutput('female.count'), 
                       plotOutput('pie.sector'))
              )
      ),
      
      tags$style("#male.count {width:10px;}"),
      
      tabItem(tabName="location",
              fluidRow(h2("Fortune 1000 Companies by Location & CEO Gender")),
              fluidRow(
                tab_box(
                #  title = "", ribbon=FALSE, title_side="top",
                  id = "location",  width=14,
                  tabs=list(
                    list(menu="Location",content=leafletOutput('map')),
                    list(menu="States", content=leafletOutput('states'))
                  )
                )
              ),
              fluidRow(
                
              )
      ),
      tabItem(tabName="financials",
              fluidRow(
                radioButtons("radio", label = "",
                             choices = list("Revenue" = "Revenues...M.", "Profit" = "Profits...M."), 
                             selected = "Revenues...M."),
                plotOutput('financials.box',width="70%")
              )
      ),

      tabItem(tabName="stock",
              fluidRow(h2("Stock Prices Comparison")),
              fluidRow(
                column(7,
                       box(
                         title=NULL, 
                         h4("Choose a company owned by a female CEO"),
                         selectizeInput(inputId="selected.comp",label=NULL,
                                        choices=fem.titles, selected = NULL, multiple = FALSE),
                         dateRangeInput("stock.dates", label = ("Date range"),
                                        start=as.Date("2017-01-02"), end=Sys.Date(),
                                        max=Sys.Date(), width="400px"),br(),
                         checkboxInput("log", label = "Plot price on log scale", value = TRUE)
                       ),br(),br(),br(),br(),
                       box(
                         title=NULL,
                         h4("Choose any company to compare"),
                         selectizeInput(inputId="selected.comp2", label=NULL,
                                        comp.titles, selected = NULL, multiple = FALSE),
                         dateRangeInput("stock.dates2", label = ("Date range"),
                                        start=as.Date("2017-01-02"), end=Sys.Date(),
                                        max=Sys.Date(), width="400px"),
                         br(),
                         checkboxInput("log2", label = "Plot price on log scale", value = TRUE)
                       )),
                column(9,
                       plotOutput('stock',height="300px",width="90%"),br(),
                       plotOutput('stock2',height="300px",width="90%"))
              )
              
      ),
      tabItem(tabName = "data",
              fluidRow(h1("Data")),
              fluidRow(box(DT::dataTableOutput("table"), width = 15)))
      )
  )
))


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
      menuItem("Sector", tabName = "sector", icon = icon("industry")),
      menuItem("Location", tabName = "location", icon = icon("compass")),
      menuItem("Stock", tabName = "stock", icon = icon("chart line")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("check")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    )
  ),
  

  # body
  dashboardBody(
    tabItems(
      tabItem(tabName="overview",
              fluidRow(h2("Fortune 1000 Companies and Female CEOs")),
              br(),br(),br(),
              fluidRow(HTML("Objectives: different types of visualizations")),
              HTML("<p>&nbsp&nbsp&nbsp&nbspe.g. bar charts, maps, boxplots, pie charts, infoboxes, valueboxes, tabs, radio buttons, date range</p>"),
              fluidRow(h4(" - Data: Kaggle Fortune 1000 (Title, Industry, Profit, Location..) + some manual labor (CEO date)")),
              fluidRow(h4(" - Gender: library(gender) based on first name")),
              fluidRow(h4(" - Location: Leaflet, markers & states (polygons)")),
              fluidRow(h4(" - Stock: quantmod, ticker matching (nasdaq.csv, stockSymbols() from stringr)")),
              fluidRow(h4(" - UI: Semantic Dashboard"))
      ),
      
      tabItem(tabName="sector",
              fluidRow(h2("Fortune 1000 Companies by Sector & CEO Gender"),width="80%"),
              fluidRow(
                column(10, h3("Choose a sector"),
                       selectizeInput("sec", "",c("All Sectors",rev(secorder))),
                       plotOutput("bysector")),
                column(6, br(),br(),
                       infoBoxOutput('male.count'), br(),
                       infoBoxOutput('female.count'), 
                       plotOutput('pie.sector')),
                width="80%"
              )
      ),

      tabItem(tabName="location",
              fluidRow(h2("Fortune 1000 Companies by Location & CEO Gender")),
              fluidRow(
                tab_box(
                  title = NULL,
                  id = "location",  width=16,
                  tabs=list(
                    list(menu="Map",content=leafletOutput('map')),
                    list(menu="State: Map", content=leafletOutput('states')),
                    list(menu="State: Barchart", 
                         content=fluidRow(selectizeInput("state", "",c("All States",sort(states.unique))),
                                          valueBoxOutput('state.count'),
                                          plotOutput('state.barchart')
                         ))
                    )
                )
              ),
              fluidRow(
                
              )
      ),

      tabItem(tabName="stock",
              fluidRow(h2("Stock Prices Comparison")),
              fluidRow(
                column(7,
                       box(
                         title=NULL, 
                         h4("Choose a company owned by a female CEO"),
                         h4(textOutput('fem.ceo.date')),
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
      
      tabItem(tabName="conclusion",
              fluidRow(h2("Conclusion")),
              box(
                fluidRow(h4("Profit and Revenue by CEO gender")),
                fluidRow(p("*Note: Outliers omitted for simplicity")),
                br(),br(),
                fluidRow(radioButtons("radio", label = "",
                                      choices = list("Revenue" = "Revenues...M.", "Profit" = "Profits...M."),
                                      selected = "Revenues...M.", inline=TRUE),width=16),
                fluidRow(plotOutput('financials.box',width="400px"))
              ),
              box(
                fluidRow(h4("Share of Female CEOs and Board Members (%)")),
                fluidRow(p("*Fortune 500 companies")),br(),
                fluidRow(plotOutput('trend', height="400px"))
              )
      ),
      
      tabItem(tabName = "data",
              fluidRow(h1("Data")),
              fluidRow(box(DT::dataTableOutput("table"), width = 15)))
      )
  )
))


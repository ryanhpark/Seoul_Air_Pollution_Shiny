# ui design
source('helper.R')

fluidPage(
dashboardPage(
  dashboardHeader(title = "Air Pollution in Seoul"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info")),
      menuItem("Fine Dust Monitor", tabName = "map", icon = icon("map")),
      menuItem("Air Quality Trend", tabName = "trend",icon = icon("line-chart")),
      menuItem("Number of Good Air Days", tabName = "goodday", icon = icon("bar-chart")),
      menuItem("About Author", tabName = "author", icon = icon("user"),
               menuItemOutput("lkdn"), menuItemOutput("github"))
      )),
  dashboardBody(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    
    tabItem(tabName = "overview",
            fluidRow(column(12, overviewbox)),
            fluidRow(
              column(
                6,
                box(
                  width = NULL,
                  baselineinfobox,
                  title = "Baseline Information",
                  solidHeader = TRUE,
                  status = "primary"
                ),
                box(
                  width = NULL,
                  questionbox,
                  title = "Questions of Interest",
                  solidHeader = TRUE,
                  status = "primary"
                )
              ),
              box(
                width = 6,
                pollutantbox,
                title = "Pollutant Description",
                solidHeader = TRUE,
                status = "primary"
              )
            ),
            fluidRow(
              tabBox(width = 12,
                tabPanel("Overall Dataset", DT::dataTableOutput("data")),
                tabPanel("Pollutant Information", DT::dataTableOutput("infodata")),
                tags$b("Data Source: Kaggle"),
                br(),
                tags$a(href = "https://www.kaggle.com/bappekim/air-pollution-in-seoul",
                              "Air Pollution in Seoul"))
              )
            ),
    tabItem(tabName = "map",
            fluidRow(
              box(
                width = 9,
                leafletOutput("map", height = 500),
                title = "Map of Seoul City",
                solidHeader = TRUE,
                status = "primary"
              ),
              column(
                2,
                radioButtons(
                  inputId = "finedust",
                  label = h4("Select Pollutant"),
                  choices = c("PM10", "PM2.5"),
                  inline = TRUE
                ),
                br(),
                sliderInput(
                  inputId = "years",
                  label = h4("Select Year"),
                  min = 2017,
                  max = 2019,
                  value = c(2017,2017),
                  sep = ""
                ),
                sliderInput(
                  inputId = "months",
                  label = h4("Select Month Range"),
                  min = 1,
                  max = 12,
                  value = c(1,1)
                ),
                sliderInput(
                  inputId = "dates",
                  label = h4("Select Date Range"),
                  min = 1,
                  max = 31,
                  value = c(1,1)
                ),
                sliderInput(
                  inputId = "times",
                  label = h4("Select Hour Range"),
                  min = 0,
                  max = 23,
                  value = c(0, 0)
                )
              )
            ),
            h5(strong("*The minimum, average, maximum values are calculated with the timeline specified")),
            h5(strong("*The unit of measurement for the values is 'microgram/m3'")),
            fluidRow(
              column(
                11,
                infoBoxOutput("good_neighborhood", width = 3),
                infoBoxOutput("good_min", width = 2),
                infoBoxOutput("good_avg", width = 2),
                infoBoxOutput("good_max", width = 2)
              )
            ),
            fluidRow(
              column(
                11,
                infoBoxOutput("bad_neighborhood", width = 3),
                infoBoxOutput("bad_min", width = 2),
                infoBoxOutput("bad_avg", width = 2),
                infoBoxOutput("bad_max", width = 2)
              ),
            )
            ),
    tabItem(tabName = "trend",
            fluidRow(
              box(
                width = 10,
                plotlyOutput("trend"),
                title = "Trends of The Pollutant Levels",
                solidHeader = TRUE,
                status = "primary"
              ),
              column(
                2,
                checkboxGroupInput(
                  inputId = "pollutants",
                  label = h4("Select Pollutants"),
                  choices = c("SO2", "NO2", "O3", "CO", "PM10", "PM2.5"),
                  selected = "SO2"
                ),
                selectInput(
                  inputId = "years2",
                  label = h4("Select Year"),
                  choices = c("All", 2017:2019)
                ),
                selectInput(
                  inputId = "months2",
                  label = h4("Select Month"),
                  choices = c("All", 1:12)
                )
              )
            ),
            fluidRow(
              box(
                width = 5,
                plotlyOutput("pollutant_box"),
                title = "Distribution of Pollutants",
                solidHeader = TRUE,
                status = "primary"
              ),
              box(
                width = 5,
                plotlyOutput("correlation"),
                title = "Correlation of the Pollutants",
                solidHeader = TRUE,
                status = "primary"
              )
            )),
    tabItem(tabName = "goodday",
            fluidRow(
              box(
                width = 5,
                plotlyOutput("good_avg_bar"),
                title = "Count of Days with Daily Average",
                solidHeader = TRUE,
                status = "primary"
              ),
              box(
                width = 5,
                plotlyOutput("good_max_bar"),
                title = "Count of Days with Daily Maximum",
                solidHeader = TRUE,
                status = "primary"
              ),
              column(2,
                     radioButtons(
                       inputId = "pollute_bar",
                       label = h4("select Pollutant"),
                       choices = c("SO2", "NO2", "O3", "CO", "PM10", "PM2.5")
                     ))
            ),
            fluidRow(
              box(
                width = 10,
                plotlyOutput("pollutant_density"),
                title = "Density Plot of Pollutant by Year",
                solidHeader = TRUE,
                status = "primary"
              )
              )
            )
    )
  )
  )
)

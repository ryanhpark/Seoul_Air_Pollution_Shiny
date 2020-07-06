# helper.R is created separately to make ui.R look cleaner.
# The boxes created here are used in the overview tab on the Shiny App.

overviewbox = column(width = 12,
                  h4("[Data Visualisation Project]"),
                  h1("Air Pollution in Seoul"),
                  p("This app aims to help users better understand 
                    the levels and trends of air pollution in Seoul, South Korea."),
                  br())
baselineinfobox = column(width = 12,
                      p(tags$li("There are 25 air quality monitors in Seoul city")),
                      p(tags$li("3 years (2017, 2018, 2019) are included in the dataset")),
                      p(tags$li("6 pollutants (SO2, NO2, CO, O3, PM10, PM2.5) 
                    are measured every hour"))
                      
                      )
pollutantbox = column(width = 12,
                      p(tags$li(strong("Sulfur Dioxide (SO2):"))), 
                      p("Produced by volcanoes and in various industrial processes. It is a precursor to acid rain."),
                      p(tags$li(strong("Nitrogen Dioxide (NO2):"))), 
                      p("The result of traffic from motor vehicles and cigarette smoke. It has sharp odor."),
                      p(tags$li(strong("Ozone (O3):"))), 
                      p("Formed from NOx. It is a constituent of smog and can cause respiratory  illness."),
                      p(tags$li(strong("Carbon Monoxide (CO):"))), 
                      p("A product of combustion of fuel such as natural gas, coal or wood. It can cause headache and nausea."),
                      p(tags$li(strong("Particulate Matter 10 (PM10):"))), 
                      p("Complex mixture of dust, pollen, soot, smoke, which causes respiratory infections and heart disease."),
                      p(tags$li(strong("Particulate Matter 2.5 (PM2.5):"))), 
                      p("A subset of PM10, which is thinner than a human hair. It can be inhaled into lung tissue.")
                      )
questionbox = column(width = 12,
                  p(tags$li(strong("Does the fine dust level (PM) vary 
                    across different neighborhoods in Seoul?"))),
                  p("-----> Explore the Fine Dust Monitor Tab"),
                  
                  p(tags$li(strong("Is there any noticeable trend in air pollution level?"))),
                  p("-----> Explore the Air Quality Trend Tab"),
                  
                  p(tags$li(strong("In a given year, how many days were with good air quality?"))),
                  p("-----> Explore the Number of Good Air Day Tab")
                  
                  )
### UI ---------------------------------------------------------------------------------------------

shinyUI({

  fluidPage(
    theme = "stylesheet.css",
    titlePanel("The Support Vector Machine Democracy Index (SVMDI)"),
    
    ### Header -----------------------------------------------------------------
    
    fluidRow(div(id = "spacer")),
    
    sidebarLayout(
      ## BEGIN sidebarPanel ------------------------
      sidebarPanel(width = 3,
        fluidRow(
          h4("Explantion"),
          helpText(
            "Chose up to 15 countries or regions to display.", 
            "To delete a selected country use backspace."
          ),
          # helpText("Choose countries to compare"),
          selectizeInput("select.country", label = NULL, multiple = TRUE, 
                         options = list(maxItems = 15), selected = "Germany",
                         choices = list(
                           "Countries" = unique(dat_clean$country_new),
                           "Regions"   = unique(dat_clean$regioniii)
                         )
          ) # END selectInput select.country
        ), # END fluidRow
        wellPanel(
          h4("Data"),
          helpText(
            "The full data set used is available in most common formats. To get the data choose",
            "a format and click on the download button."),
          selectInput("select.data", label = NULL, choices = c(
            "Excel (.csv)",
            "R (.RData)",
            "SAS (.sas7bdat)",
            "Stata (.dta)"
          ), selected = "Excel (.csv)"
          ), # END selectInput select.data
          downloadButton("downloadData", "Download Data")
        ) # END wellPanel
      ), # END sidebarPanel
      
      ## BEGIN mainPanel -------------------
      mainPanel(width = 9,
        fluidRow(
          column(width = 8,
                 uiOutput('info')
          ),
          column(width = 4,
                 h3("About the authors"),
                 tags$div(
                   tags$p(tags$b("Klaus Gründler"), "is a post doc at the University of Wuerzburg, Germany.", 
                          br(), br(), "For more info visit:", 
                          tags$a(href = "http://klausgruendler.de", "www.klausgruendler.de", target = "_blank")), 
                   tags$p(tags$b("Tommy Krieger"), "is a research assistant at the University of Konstanz, Germany.", 
                          br(), br(), "For more info visit:", 
                          tags$a(href = "http://klausgruendler.de", "www.thisisnothewebsiteyet.de", target = "_blank"))
                 )
          ) # END column
        ), # End fluidRow
        hr(),
        h3("Explore the CSVMD index on a map"),
        wellPanel(
        helpText("Move the slider to the year of interest. Hovering over a selected country will display addtional information."),
        sliderInput("slider.year.map", label = NULL, min = 1960, max = 2014, value = 2000, sep = ""),
        leafletOutput("map")
        ),
        h3("The CSVMD index over time"),
        wellPanel(
        fluidRow(
          column(width = 4, 
                 helpText("Choose the date range for the lineplot"),
                 sliderInput("slider.year", label = NULL,
                             min = 1960, max = 2014,
                             value = c(1980, 2000), sep = ""
                 ) # END sliderInput
          ), # END column
          column(width = 3,
                 helpText("Choose the confidence intervall to be drawn?"),
                 selectInput("select.ci", label = NULL, choices = c(
                   "None", 
                   "90% Confidence Intervall",
                   "95% Confidence Intervall"), selected = "None"
                 ) # END selectInput 
          ) # END column
        ), # END fluidRow
        plotOutput("lineplot")
        ),
        br()
      ) # END mainPanel
    ), # END sidebarLayout
    fluidPage(
      fluidRow(
        tags$p("Powered by", tags$a(href = "https://www.rstudio.com/", "RStudio", target = "_blank"), 
               "and", tags$a(href = "https://www.rstudio.com/products/shiny/", "Shiny.", target = "_blank"),
               "Page created and maintained by Manuel Steiner.")
      )
    )
  ) # END fluidPage
}) # END shinyUI
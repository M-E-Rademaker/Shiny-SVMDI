### UI ---------------------------------------------------------------------------------------------

shinyUI({

  fluidPage(
    theme = shinytheme("flatly"),
    
    titlePanel(tags$div("The Support Vector Machine Democracy Index (SVMDI)", br(), hr(), 
                        style = "text-align: center"),
               windowTitle = "The SVMD Index"
    ), # END titlePanel
    
    ### Header -----------------------------------------------------------------
    sidebarLayout(
      ## BEGIN sidebarPanel ------------------------
      sidebarPanel(width = 3,
        fluidRow(
          h4("Explanation"),
          helpText(
            "Chose up to 15 countries or regions to display.", 
            "To delete a selected country use backspace."
          ), # END helpText
          selectizeInput("select.country", label = NULL, multiple = TRUE, 
                         options = list(maxItems = 15), selected = "Germany",
                         choices = list(
                           "Countries" = unique(dat_clean$country),
                           "Regions"   = unique(dat_clean$region)
                         )
          ) # END selectInput select.country
        ), # END fluidRow
        wellPanel(
          h4("Data"),
          helpText(
            "The full data set used is available in most common formats. To get the data choose",
            "a format and click on the ", strong("Download Data"), " button."),
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
      
      ## BEGIN mainPanel ----------------------------------
      mainPanel(width = 9,
        fluidRow(
          column(width = 8,
                 uiOutput('info')
          ),
          column(width = 4,
                 h3("About the authors"),
                 tags$div(
                   tags$img(src = "Klaus_G.png", height="60", width="60", style = "border-radius: 50%"),
                   tags$p(strong("Klaus Gründler"), "is a post doc at the University of Wuerzburg, Germany.", 
                          br(), tags$a(href="mailto:klaus.gruendler@uni-wuerzburg.de", icon("envelope-o", "fa-lg")), 
                          HTML("&nbsp", "&nbsp"),  tags$a(href="http://klausgruendler.de/", icon("globe", "fa-lg"), target = "_blank")
                          ) # END p
                 ), # END div
                 tags$div(
                   tags$img(src = "Tommy_K.png", height="60", width="60", style = "border-radius: 50%"),
                   tags$p(tags$b("Tommy Krieger"), "is a research assistant at the University of Konstanz, Germany.", 
                          br(), tags$a(href="mailto:tommy.krieger@uni-konstanz.de", icon("envelope-o", "fa-lg"))
                          # HTML("&nbsp", "&nbsp"),  tags$a(href="http://klausgruendler.de/", icon("globe", "fa-lg"), target = "_blank")
                   ) # END p
                 ) # END div
          ) # END column
        ), # End fluidRow
        hr(),
        h3("Explore the SVMDI on a map"),
        wellPanel(
        helpText("Move the slider to the year of interest. Hovering over a selected country will display addtional information."),
        sliderInput("slider.year.map", label = NULL, min = 1960, max = 2014, value = 2000, sep = ""),
        leafletOutput("map")
        ), # END wellPanel
        h3("The SVMDI over time"),
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
                 helpText("Draw upper and lower bound?"),
                 radioButtons("checkbox.minmax", label = NULL, choices = c("Yes", "No"), selected = "No")
          ), # END column
          column(width = 3,
                 helpText("Draw dichotomized SVMD index as well?"),
                 radioButtons("checkbox.dsvmdi", label = NULL, choices = c("Yes", "No"), selected = "No")
          ) # END column
        ), # END fluidRow
        plotOutput("lineplot")
        ), # END wellPanel
        br()
      ) # END mainPanel
    ), # END sidebarLayout
    fluidPage(
      fluidRow(
        hr(),
        tags$p(HTML('This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-sa/3.0/">
                    Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.'), 
               HTML("&nbsp"), "Powered by", tags$a(href = "https://www.rstudio.com/", "RStudio", target = "_blank"), 
               "and", tags$a(href = "https://www.rstudio.com/products/shiny/", "Shiny.", target = "_blank"),
               "Page created and maintained by Manuel Steiner:", HTML("&nbsp", "&nbsp"), 
               tags$a(href="mailto:manuel.steiner@uni-wuerzburg.de", icon("envelope-o", "fa-lg")),
               HTML("&nbsp", "&nbsp"), 
               tags$a(href="https://github.com/M-E-Steiner", icon("github", "fa-lg"), target = "_blank"),
               HTML("&nbsp", "&nbsp"), 
               tags$a(href="https://www.wiwi.uni-wuerzburg.de/lehrstuhl/qwf/startseite/allgemeines/team/wissenschaftliche_mitarbeiterinnen/", 
                      icon("globe", "fa-lg"), target = "_blank"),
               HTML("&nbsp", "&nbsp"), 
               tags$a(href="https://stackoverflow.com/users/4046004/manuel-s", 
                      icon("stack-overflow ", "fa-lg"), target = "_blank")
        ) # END p
      ) # END fluidRow
    ) # END fluidPage
  ) # END fluidPage
}) # END shinyUI
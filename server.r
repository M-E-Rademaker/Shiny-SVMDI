shinyServer(function(input, output) {
  
  withProgress(expr = {load("data/dat_csvmdi.RData")}, message = "Loading... Please wait")
  
  ### Lineplot
  output$lineplot <- renderPlot({
    
    validate(
      need(input$select.country != "", "Please select at least one country or region.")
    )
    
    a1 <- dat_clean %>% 
      filter(country %in% input$select.country,
             between(year, input$slider.year[1], input$slider.year[2]))
    
    a2 <- dat_clean %>% 
      filter(region %in% input$select.country,
             between(year, input$slider.year[1], input$slider.year[2]))
    
    p <- ggplot(a1, aes(x = year, y = csvmdi)) +
      geom_line(aes(color = country), size = 1.05) +
      geom_line(data = a2, aes(y = region_csvmdi, color = region), size = 1.05) + 
      scale_x_continuous(breaks = seq.int(input$slider.year[1], input$slider.year[2], by = 2)) + 
      labs(title = paste0("The SVMDI from ", input$slider.year[1], " until ", input$slider.year[2]),
           color = "Country/Region",
           y = "SVMDI",
           x = "Year",
           caption = "Source: Gründler & Krieger (2018), Machine Learning Indicators, Political Institutions, and Economic Development") + 
      theme(
        title = element_text(size = 14),
        axis.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)
        )
    
    if(input$checkbox.dsvmdi == "Yes") {
      
      p <- p + geom_line(aes(y = dsvmdi, color = country), linetype = "dashed")
    }
    
    if(input$checkbox.minmax == "Yes") {
      
    p + geom_ribbon(data = a1, aes(ymin = csvmdimin, ymax = csvmdimax, fill = country), alpha = 0.4, show.legend = FALSE) +
        geom_ribbon(data = a2, aes(ymin = region_csvmdimin, ymax = region_csvmdimin, fill = region), alpha = 0.4, show.legend = FALSE)

    } else {
    p 
    } 
  }) # END renderPlot
  
  output$map <- renderLeaflet({
    
    ## Choices made by user
    choice.country <- input$select.country
    choice.year    <- input$slider.year.map
    
    ## Check if at least one input was given and ask for one if not.
    validate(
      need(input$select.country != "", "Please select at least one country or region."),
      need(!(any(input$select.country %in% unique(dat_clean$region))), 
           "Currently only countries can be displayed on the map. To compare regions refer to the lineplot below.")
    )
     
    # Filter relevant countries and year
    dd <- dat_leaflet %>%
      filter(country %in% choice.country, year == choice.year)
    
    ## Create labels that appear when hovering over a country
    labels <- sprintf(
      "<strong>%s</strong><br/> Year: %d<br/> CSVMD Index country: %.2f<br/> CSVMD Index region (%s): %.2f",
      dd$country, dd$year, dd$csvmdi, 
      dd$region, dd$region_csvmdi
    ) %>% lapply(htmltools::HTML)
    
    ## Leaflet object
    leaflet(unnest(dd)) %>% 
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = "pk.eyJ1IjoibWFudWVsc3RlaW5lciIsImEiOiJjajl4emFqYTk4MWhrMzNsZ3p4ejN1bGRkIn0.3tUJ9WwZTK69UrUea-5R_A")) %>% 
      addPolygons(
        lat = ~ lat,
        lng = ~ long,
        color = "white",
        opacity = 1,
        weight = 2,
        dashArray = "3",
        fillColor = dd$color_country,
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE,
          sendToBack = TRUE),
        label = labels, labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>% 
      addLegend(pal = pal, values = dd$csvmdi, opacity = 0.7, title = "CSVMD Index",
                position = "bottomright")
  }) # END renderLeaflet 
  
  output$info <- renderUI({
    withMathJax(includeMarkdown("docs/info.md"))
  }) # END renderUI
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("CSVMDI_full_data.", str_extract_all(input$select.data, pattern = "(?<=\\.)[^\\)]+"))
    },
    content = function(file) {
      switch (input$select.data,
        "Excel (.csv)" = write.csv(dat_full, file, row.names = FALSE),
        "R (.RData)"   = save(dat_full, file = file),
        "Stata (.dta)" = write_dta(dat_full, file),
        "SAS (.sas7bdat)" = write_sas(dat_full, file)
      )
    }
  ) # END OutputHandler
}) # END shinyServer
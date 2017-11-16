shinyServer(function(input, output) {
  
  ### Lineplot
  output$lineplot <- renderPlot({
    
    validate(
      need(input$select.country != "", "Please select at least one country or region.")
    )
    
    a1 <- dat_clean %>% 
      filter(country_new %in% input$select.country,
             between(year, input$slider.year[1], input$slider.year[2]))
    
    a2 <- dat_clean %>% 
      filter(regioniii %in% input$select.country,
             between(year, input$slider.year[1], input$slider.year[2]))
    
    p <- ggplot(a1, aes(x = year, y = csvmdi)) +
      geom_line(aes(color = country_new)) +
      geom_line(data = a2, aes(y = regioniii_csvmdi, color = regioniii)) + 
      scale_x_continuous(breaks = seq.int(input$slider.year[1], input$slider.year[2], by = 2)) + 
      labs(title = paste0("The CSVMD Index from ", input$slider.year[1], " until ", input$slider.year[2]),
           subtitle = "The CSVMDI for is a nindex based on bla bla bla support vector machine bla bla.",
           color = "Country/Region",
           y = "CSVMD Index",
           x = "Year",
           caption = "Source: Gründler & Krieger (2016), Democracy and Growth: Evidence from a machine learning indicator")
    
    if(input$select.ci == "90% Confidence Intervall") {
      
    p + geom_ribbon(data = a1, aes(ymin = csvmdiq003, ymax = csvmdiq097, fill = country_new), alpha = 0.4, show.legend = FALSE) +
        geom_ribbon(data = a2, aes(ymin = regioniii_csvmdiq003, ymax = regioniii_csvmdiq097, fill = regioniii), alpha = 0.4, show.legend = FALSE)

    } else if(input$select.ci == "95% Confidence Intervall") {
      
    p + geom_ribbon(aes(ymin = csvmdiq005, ymax = csvmdiq095, fill = country_new), alpha = 0.4, show.legend = FALSE) + 
        geom_ribbon(data = a2, aes(ymin = regioniii_csvmdiq005, ymax = regioniii_csvmdiq095, fill = regioniii), alpha = 0.4, show.legend = FALSE)
      
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
      need(!(any(input$select.country %in% unique(dat_clean$regioniii))), 
           "Currently only countries can be displayed on the map. To compare regions refer to the lineplot below.")
    )
     
    # Filter relevant countries and year
    dd <- dat_leaflet %>%
      filter(country_new %in% choice.country, year == choice.year)
    
    ## Create labels that appear when hovering over a country
    labels <- sprintf(
      "<strong>%s</strong><br/> Year: %d<br/> CSVMD Index country: %.2f<br/> CSVMD Index region (%s): %.2f",
      dd$country_new, dd$year, dd$csvmdi, 
      dd$regioniii, dd$regioniii_csvmdi
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
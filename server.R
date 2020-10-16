source("data.R")

# SERVER
shinyServer(
  function(input, output, session) {
    
    # ENTIRE PAGE ####################################################################################################################################
    observe_helpers()
    
    # HOME PAGE ######################################################################################################################################                         
    # Making Home Page boxes take you to the corresponding tabs  
    observeEvent(input$GoToLocalAuthoritiesTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "LocalAuthoritiesTab"
      )
    })
    observeEvent(input$GoToRegionsTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "RegionsTab"
      )
    })
    observeEvent(input$GoToRuralAndUrbanTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "RuralAndUrbanTab"
      )
    })
    observeEvent(input$GoToEnterpriseRegionAreasTab, {
      updateTabsetPanel(session, "MainNav",
                        selected = "EnterpriseRegionAreasTab"
      )
    }) 
    
    # LOCAL AUTHORITY ################################################################################
    
    # Leaflet map for t1_la_long (GVA per head) ####
    output$t1_la_map_caption <- renderText({
      paste("Map 1. GVA per Head across Scottish council areas in ", as.character(input$t1_la_input), " (£ per Head)", sep="")
    })
    t1_la_map_data <- reactive({
      t1_la_one <- t1_la_long[ which(t1_la_long$Year == input$t1_la_input), ]
      t1_la_one <- t1_la_one[ ,c(1,3)]
      merged <- merge(mapex@data, t1_la_one, by = intersect(names(mapex@data), names(t1_la_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_t1_la <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_t1_la = choropleth_t1_la)
      return(list_return)
    })
    output$t1_la_map <- renderLeaflet({
      mapex <- t1_la_map_data()$mapex
      choropleth_t1_la <- t1_la_map_data()$choropleth_t1_la
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_t1_la(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " £", as.character(mapex$Value), " per Head", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_t1_la, values = mapex$Value, title = paste("GVA per Head", " (£)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for t3b_la_long (Business registrations per 10,000 adults) ####
    output$t3b_la_map_caption <- renderText({
      paste("Map 2. Business registrations per 10,000 adults across Scottish council areas in ", as.character(input$t3b_la_input), sep="")
    })
    t3b_la_map_data <- reactive({
      t3b_la_one <- t3b_la_long[ which(t3b_la_long$Year == input$t3b_la_input), ]
      t3b_la_one <- t3b_la_one[ ,c(1,3)]
      merged <- merge(mapex@data, t3b_la_one, by = intersect(names(mapex@data), names(t3b_la_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_t3b_la <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_t3b_la = choropleth_t3b_la)
      return(list_return)
    })
    output$t3b_la_map <- renderLeaflet({
      mapex <- t3b_la_map_data()$mapex
      choropleth_t3b_la <- t3b_la_map_data()$choropleth_t3b_la
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_t3b_la(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME)," ", as.character(mapex$Value), " per 10,000 adults", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_t3b_la, values = mapex$Value, title = paste("Business registrations per 10,000 adults", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for t4b_la_long (Business de-registrations per 10,000 adults) ####
    output$t4b_la_map_caption <- renderText({
      paste("Map 3. Business de-registrations per 10,000 adults across Scottish council areas in ", as.character(input$t4b_la_input), sep="")
    })
    t4b_la_map_data <- reactive({
      t4b_la_one <- t4b_la_long[ which(t4b_la_long$Year == input$t4b_la_input), ]
      t4b_la_one <- t4b_la_one[ ,c(1,3)]
      merged <- merge(mapex@data, t4b_la_one, by = intersect(names(mapex@data), names(t4b_la_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_t4b_la <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_t4b_la = choropleth_t4b_la)
      return(list_return)
    })
    output$t4b_la_map <- renderLeaflet({
      mapex <- t4b_la_map_data()$mapex
      choropleth_t4b_la <- t4b_la_map_data()$choropleth_t4b_la
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_t4b_la(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$Value), " per 10,000 adults", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_t4b_la, values = mapex$Value, title = paste("Business de-registrations per 10,000 adults", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for t5b_la_long (Business 3-year survival rate) ####
    output$t5b_la_map_caption <- renderText({
      paste("Map 4. Business 3-year survival rate across Scottish council areas in ", as.character(input$t5b_la_input), sep="")
    })
    t5b_la_map_data <- reactive({
      t5b_la_one <- t5b_la_long[ which(t5b_la_long$Year == input$t5b_la_input), ]
      t5b_la_one <- t5b_la_one[ ,c(1,3)]
      merged <- merge(mapex@data, t5b_la_one, by = intersect(names(mapex@data), names(t5b_la_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_t5b_la <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_t5b_la = choropleth_t5b_la)
      return(list_return)
    })
    output$t5b_la_map <- renderLeaflet({
      mapex <- t5b_la_map_data()$mapex
      choropleth_t5b_la <- t5b_la_map_data()$choropleth_t5b_la
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_t5b_la(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$Value), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_t5b_la, values = mapex$Value, title = paste("Business 3-year survival rate", " (%)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for t8a_la_long (International exports per head) ####
    output$t8a_la_map_caption <- renderText({
      paste("Map 5. International exports per head across Scottish council areas in ", as.character(input$t8a_la_input), sep="")
    })
    t8a_la_map_data <- reactive({
      t8a_la_one <- t8a_la_long[ which(t8a_la_long$Year == input$t8a_la_input), ]
      t8a_la_one <- t8a_la_one[ ,c(1,3)]
      merged <- merge(mapex@data, t8a_la_one, by = intersect(names(mapex@data), names(t8a_la_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_t8a_la <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_t8a_la = choropleth_t8a_la)
      return(list_return)
    })
    output$t8a_la_map <- renderLeaflet({
      mapex <- t8a_la_map_data()$mapex
      choropleth_t8a_la <- t8a_la_map_data()$choropleth_t8a_la
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_t8a_la(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", " £", as.character(mapex$Value), " per Head", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_t8a_la, values = mapex$Value, title = paste("International exports per head", " (£)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for t9_la_long (BERD per head) ####
    output$t9_la_map_caption <- renderText({
      paste("Map 6. Business Enterprise Research and Development (BERD) expenditure - Current prices (£million) across Scottish council areas in ", as.character(input$t9_la_input), sep="")
    })
    t9_la_map_data <- reactive({
      t9_la_one <- t9_la_long[ which(t9_la_long$Year == input$t9_la_input), ]
      t9_la_one <- t9_la_one[ ,c(1,3)]
      merged <- merge(mapex@data, t9_la_one, by = intersect(names(mapex@data), names(t9_la_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_t9_la <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_t9_la = choropleth_t9_la)
      return(list_return)
    })
    output$t9_la_map <- renderLeaflet({
      mapex <- t9_la_map_data()$mapex
      choropleth_t9_la <- t9_la_map_data()$choropleth_t9_la
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_t9_la(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", " £", as.character(mapex$Value), " million", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_t9_la, values = mapex$Value, title = paste("BERD per Head", " (£ million)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for t14a_la_long (Employment rate) ####
    output$t14a_la_map_caption <- renderText({
      paste("Map 7. Employment rate across Scottish council areas in ", as.character(input$t14a_la_input), sep="")
    })
    t14a_la_map_data <- reactive({
      t14a_la_one <- t14a_la_long[ which(t14a_la_long$Year == input$t14a_la_input), ]
      t14a_la_one <- t14a_la_one[ ,c(1,3)]
      merged <- merge(mapex@data, t14a_la_one, by = intersect(names(mapex@data), names(t14a_la_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_t14a_la <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_t14a_la = choropleth_t14a_la)
      return(list_return)
    })
    output$t14a_la_map <- renderLeaflet({
      mapex <- t14a_la_map_data()$mapex
      choropleth_t14a_la <- t14a_la_map_data()$choropleth_t14a_la
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_t14a_la(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$Value), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_t14a_la, values = mapex$Value, title = paste("Employment rate", " (%)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for t15b_la_long (Self-employment rate) ####
    output$t15b_la_map_caption <- renderText({
      paste("Map 8. Self-employment rate across Scottish council areas in ", as.character(input$t15b_la_input), sep="")
    })
    t15b_la_map_data <- reactive({
      t15b_la_one <- t15b_la_long[ which(t15b_la_long$Year == input$t15b_la_input), ]
      t15b_la_one <- t15b_la_one[ ,c(1,3)]
      merged <- merge(mapex@data, t15b_la_one, by = intersect(names(mapex@data), names(t15b_la_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_t15b_la <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_t15b_la = choropleth_t15b_la)
      return(list_return)
    })
    output$t15b_la_map <- renderLeaflet({
      mapex <- t15b_la_map_data()$mapex
      choropleth_t15b_la <- t15b_la_map_data()$choropleth_t15b_la
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_t15b_la(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$Value), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_t15b_la, values = mapex$Value, title = paste("Self-employment rate", " (%)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for t17b_la_long (Unemployment rate) ####
    output$t17b_la_map_caption <- renderText({
      paste("Map 9. Unemployment rate across Scottish council areas in ", as.character(input$t17b_la_input), sep="")
    })
    t17b_la_map_data <- reactive({
      t17b_la_one <- t17b_la_long[ which(t17b_la_long$Year == input$t17b_la_input), ]
      t17b_la_one <- t17b_la_one[ ,c(1,3)]
      merged <- merge(mapex@data, t17b_la_one, by = intersect(names(mapex@data), names(t17b_la_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_t17b_la <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_t17b_la = choropleth_t17b_la)
      return(list_return)
    })
    output$t17b_la_map <- renderLeaflet({
      mapex <- t17b_la_map_data()$mapex
      choropleth_t17b_la <- t17b_la_map_data()$choropleth_t17b_la
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_t17b_la(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME), " ", as.character(mapex$Value), " %", sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_t17b_la, values = mapex$Value, title = paste("Unemployment rate", " (%)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    # Leaflet map for t18a_la_long (Median gross weekly pay) ####
    output$t18a_la_map_caption <- renderText({
      paste("Map 10. Median gross weekly pay across Scottish council areas in ", as.character(input$t18a_la_input), sep="")
    })
    t18a_la_map_data <- reactive({
      t18a_la_one <- t18a_la_long[ which(t18a_la_long$Year == input$t18a_la_input), ]
      t18a_la_one <- t18a_la_one[ ,c(1,3)]
      merged <- merge(mapex@data, t18a_la_one, by = intersect(names(mapex@data), names(t18a_la_one)), all.x = TRUE, all.y = FALSE, sort=FALSE)
      mapex@data <- merged[match(mapex@data$NAME, merged$NAME),]
      choropleth_t18a_la <- colorBin(palette=brewer.pal(n=9, name="RdYlGn"), mapex@data$Value, bins = 9)
      list_return <- list(mapex = mapex, choropleth_t18a_la = choropleth_t18a_la)
      return(list_return)
    })
    output$t18a_la_map <- renderLeaflet({
      mapex <- t18a_la_map_data()$mapex
      choropleth_t18a_la <- t18a_la_map_data()$choropleth_t18a_la
      leaflet(mapex) %>%
        setView(zoom = 6, lat = 57, lng= -4) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke=FALSE, layerId = ~mapex$NAME, fillColor = ~choropleth_t18a_la(mapex$Value), fillOpacity=1, popup = ~paste(as.character(mapex$NAME)," " , " £", as.character(mapex$Value), sep = ""),
                    highlightOptions = highlightOptions(color="black", opacity = 1, fillOpacity = 0.6, fillColor = "navy")
        ) %>%
        addLegend("bottomright", pal = choropleth_t18a_la, values = mapex$Value, title = paste("Median gross weekly pay", " (£)", sep=""), opacity = 1, labFormat = labelFormat(prefix = ""))
    })
    
    
    # local_authorities_table aka SUMMARY TOOL####
    output$local_authorities_table <- DT::renderDT({
      
      # data_lat <- subset(summary_tool_la[, c("AREA", input$checked_indicators_la)], summary_tool_la[, c("AREA", input$checked_indicators_la)]$AREA == input$checked_areas_la)
      data_lat <- summary_tool_la
      data_lat <- data_lat[, c("AREA", input$checked_indicators_la)]
      data_lat <- data_lat[data_lat$AREA %in% input$checked_areas_la,] 
      
      DT::datatable(
        data_lat,
        # summary_tool_la,
        # colnames = c("Sector", "Subsector", as.character(seq(St,Yr))),
        extensions = c("Buttons", "KeyTable", "Scroller", "FixedColumns"),
        rownames = FALSE,
        options = list(
          buttons = list(
            list(extend='excel', filename="Summary Tool"),
            list(extend='csv', filename="Summary Tool"),
            list(extend='pdf', filename="Summary Tool"),
            list(extend='copy', filename="Summary Tool"),
            list(extend='print', filename="Summary Tool")
          ),
          dom = 'frtBip',
          scrollX = TRUE,
          paging = TRUE,
          searching = TRUE,
          keys = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE,
          fixedColumns = list(leftColumns = 1)
        )
      ) #  %>%
       # formatCurrency(columns = seq(1, ncol(TABLE_ALL)), currency = "", interval = 3, mark = ",", digits=0) %>% 
       #  formatStyle("Subsector", target = 'row', fontWeight = styleEqual(c("Total"), c("bold"))
       # )
    })
    
    # OVERVIEW VISUALIZATION #####
    # ggplot barplot for t1_la_barplot
    output$t1_la_barplot <- renderPlot({
      
      t1_la_plot <- subset(t1_la_long, t1_la_long$Year == 2018)
      t1_la_plot <- t1_la_long[,c(1,3)]
      t1_la_plot <- t1_la_plot %>%
        mutate(NAME = (NAME),
               Value = as.numeric(Value),
               fill_type = ifelse(t1_la_plot$NAME == "Scotland","blue","grey"))

      ggplot(t1_la_plot, aes(x=NAME, y=Value)) +
        geom_col( show.legend = FALSE, width=0.9) +
        coord_flip(clip="off", expand=TRUE) +
        labs(x="", y="") +
        theme_minimal() +
        theme(
          axis.text.x=element_blank()
        ) +
        ggtitle("Title") +
        geom_text(aes(x=NAME, y=Value, label=Value),
                  vjust=0.3, size=4, hjust=1.1, col="white")
    })
    
    # TIME SERIES ####

    output$t1_la_graph <- renderDygraph({
      dygraph(
        t1_la_wide[,c(1, as.numeric(input$t1_la_input_time_series))]
      ) %>%
        dyGroup(names(t1_la_wide)[c(1, as.numeric(input$t1_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "GDP Growth Rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t1_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t2a_la_graph <- renderDygraph({
      dygraph(
        t2a_la_wide[,c(1, as.numeric(input$t2a_la_input_time_series))]
      ) %>%
        dyGroup(names(t2a_la_wide)[c(1, as.numeric(input$t2a_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE registered enterprises") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t2a_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t2b_la_graph <- renderDygraph({
      dygraph(
        t2b_la_wide[,c(1, as.numeric(input$t2b_la_input_time_series))]
      ) %>%
        dyGroup(names(t2b_la_wide)[c(1, as.numeric(input$t2b_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Small businesses") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t2b_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t3a_la_graph <- renderDygraph({
      dygraph(
        t3a_la_wide[,c(1, as.numeric(input$t3a_la_input_time_series))]
      ) %>%
        dyGroup(names(t3a_la_wide)[c(1, as.numeric(input$t3a_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Births)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t3a_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t3b_la_graph <- renderDygraph({
      dygraph(
        t3b_la_wide[,c(1, as.numeric(input$t3b_la_input_time_series))]
      ) %>%
        dyGroup(names(t3b_la_wide)[c(1, as.numeric(input$t3b_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Births) per 10,000 adults") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t3b_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t4a_la_graph <- renderDygraph({
      dygraph(
        t4a_la_wide[,c(1, as.numeric(input$t4a_la_input_time_series))]
      ) %>%
        dyGroup(names(t4a_la_wide)[c(1, as.numeric(input$t4a_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Deaths)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t4a_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t4b_la_graph <- renderDygraph({
      dygraph(
        t4b_la_wide[,c(1, as.numeric(input$t4b_la_input_time_series))]
      ) %>%
        dyGroup(names(t4b_la_wide)[c(1, as.numeric(input$t4b_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Deaths) per 10,000 adults") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t4b_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t5a_la_graph <- renderDygraph({
      dygraph(
        t5a_la_wide[,c(1, as.numeric(input$t5a_la_input_time_series))]
      ) %>%
        dyGroup(names(t5a_la_wide)[c(1, as.numeric(input$t5a_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "3-year business survival") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t5a_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t5b_la_graph <- renderDygraph({
      dygraph(
        t5b_la_wide[,c(1, as.numeric(input$t5b_la_input_time_series))]
      ) %>%
        dyGroup(names(t5b_la_wide)[c(1, as.numeric(input$t5b_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "3-year business survival rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t5b_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t6a_la_graph <- renderDygraph({
      dygraph(
        t6a_la_wide[,c(1, as.numeric(input$t6a_la_input_time_series))]
      ) %>%
        dyGroup(names(t6a_la_wide)[c(1, as.numeric(input$t6a_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned enterprises") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t6a_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t6b_la_graph <- renderDygraph({
      dygraph(
        t6b_la_wide[,c(1, as.numeric(input$t6b_la_input_time_series))]
      ) %>%
        dyGroup(names(t6b_la_wide)[c(1, as.numeric(input$t6b_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned enterprises (EU)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t6b_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t7a_la_graph <- renderDygraph({
      dygraph(
        t7a_la_wide[,c(1, as.numeric(input$t7a_la_input_time_series))]
      ) %>%
        dyGroup(names(t7a_la_wide)[c(1, as.numeric(input$t7a_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned business jobs") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t7a_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t7b_la_graph <- renderDygraph({
      dygraph(
        t7b_la_wide[,c(1, as.numeric(input$t7b_la_input_time_series))]
      ) %>%
        dyGroup(names(t7b_la_wide)[c(1, as.numeric(input$t7b_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned business jobs (EU)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t7b_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t8a_la_graph <- renderDygraph({
      dygraph(
        t8a_la_wide[,c(1, as.numeric(input$t8a_la_input_time_series))]
      ) %>%
        dyGroup(names(t8a_la_wide)[c(1, as.numeric(input$t8a_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "International Exports Value") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t8a_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t8b_la_graph <- renderDygraph({
      dygraph(
        t8b_la_wide[,c(1, as.numeric(input$t8b_la_input_time_series))]
      ) %>%
        dyGroup(names(t8b_la_wide)[c(1, as.numeric(input$t8b_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "International Exports Value (EU)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t8b_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t9_la_graph <- renderDygraph({
      dygraph(
        t9_la_wide[,c(1, as.numeric(input$t9_la_input_time_series))]
      ) %>%
        dyGroup(names(t9_la_wide)[c(1, as.numeric(input$t9_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "BERD") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t9_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t10_la_graph <- renderDygraph({
      dygraph(
        t10_la_wide[,c(1, as.numeric(input$t10_la_input_time_series))]
      ) %>%
        dyGroup(names(t10_la_wide)[c(1, as.numeric(input$t10_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "BERD Jobs") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t10_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t11_la_graph <- renderDygraph({
      dygraph(
        t11_la_wide[,c(1, as.numeric(input$t11_la_input_time_series))]
      ) %>%
        dyGroup(names(t11_la_wide)[c(1, as.numeric(input$t11_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "High growth enterprises") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t11_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t12_la_graph <- renderDygraph({
      dygraph(
        t12_la_wide[,c(1, as.numeric(input$t12_la_input_time_series))]
      ) %>%
        dyGroup(names(t12_la_wide)[c(1, as.numeric(input$t12_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE Businesses Jobs") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t12_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t13_la_graph <- renderDygraph({
      dygraph(
        t13_la_wide[,c(1, as.numeric(input$t13_la_input_time_series))]
      ) %>%
        dyGroup(names(t13_la_wide)[c(1, as.numeric(input$t13_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Employment") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t13_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t14a_la_graph <- renderDygraph({
      dygraph(
        t14a_la_wide[,c(1, as.numeric(input$t14a_la_input_time_series))]
      ) %>%
        dyGroup(names(t14a_la_wide)[c(1, as.numeric(input$t14a_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Working age employment rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t14a_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t14b_la_graph <- renderDygraph({
      dygraph(
        t14b_la_wide[,c(1, as.numeric(input$t14b_la_input_time_series))]
      ) %>%
        dyGroup(names(t14b_la_wide)[c(1, as.numeric(input$t14b_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Working age employment rate (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t14b_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t15a_la_graph <- renderDygraph({
      dygraph(
        t15a_la_wide[,c(1, as.numeric(input$t15a_la_input_time_series))]
      ) %>%
        dyGroup(names(t15a_la_wide)[c(1, as.numeric(input$t15a_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t15a_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t15b_la_graph <- renderDygraph({
      dygraph(
        t15b_la_wide[,c(1, as.numeric(input$t15b_la_input_time_series))]
      ) %>%
        dyGroup(names(t15b_la_wide)[c(1, as.numeric(input$t15b_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t15b_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t15c_la_graph <- renderDygraph({
      dygraph(
        t15c_la_wide[,c(1, as.numeric(input$t15c_la_input_time_series))]
      ) %>%
        dyGroup(names(t15c_la_wide)[c(1, as.numeric(input$t15c_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t15c_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t16a_la_graph <- renderDygraph({
      dygraph(
        t16a_la_wide[,c(1, as.numeric(input$t16a_la_input_time_series))]
      ) %>%
        dyGroup(names(t16a_la_wide)[c(1, as.numeric(input$t16a_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed Females") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t16a_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t16b_la_graph <- renderDygraph({
      dygraph(
        t16b_la_wide[,c(1, as.numeric(input$t16b_la_input_time_series))]
      ) %>%
        dyGroup(names(t16b_la_wide)[c(1, as.numeric(input$t16b_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed Females rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t16b_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t16c_la_graph <- renderDygraph({
      dygraph(
        t16c_la_wide[,c(1, as.numeric(input$t16c_la_input_time_series))]
      ) %>%
        dyGroup(names(t16c_la_wide)[c(1, as.numeric(input$t16c_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed Females rate (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t16c_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t17a_la_graph <- renderDygraph({
      dygraph(
        t17a_la_wide[,c(1, as.numeric(input$t17a_la_input_time_series))]
      ) %>%
        dyGroup(names(t17a_la_wide)[c(1, as.numeric(input$t17a_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Unemployment") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t17a_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t17b_la_graph <- renderDygraph({
      dygraph(
        t17b_la_wide[,c(1, as.numeric(input$t17b_la_input_time_series))]
      ) %>%
        dyGroup(names(t17b_la_wide)[c(1, as.numeric(input$t17b_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Unemployment rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t17b_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t17c_la_graph <- renderDygraph({
      dygraph(
        t17c_la_wide[,c(1, as.numeric(input$t17c_la_input_time_series))]
      ) %>%
        dyGroup(names(t17c_la_wide)[c(1, as.numeric(input$t17c_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Unemployment rate (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t17c_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t18a_la_graph <- renderDygraph({
      dygraph(
        t18a_la_wide[,c(1, as.numeric(input$t18a_la_input_time_series))]
      ) %>%
        dyGroup(names(t18a_la_wide)[c(1, as.numeric(input$t18a_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (RA)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t18a_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t18b_la_graph <- renderDygraph({
      dygraph(
        t18b_la_wide[,c(1, as.numeric(input$t18b_la_input_time_series))]
      ) %>%
        dyGroup(names(t18b_la_wide)[c(1, as.numeric(input$t18b_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (RA) (SE)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t18b_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t19a_la_graph <- renderDygraph({
      dygraph(
        t19a_la_wide[,c(1, as.numeric(input$t19a_la_input_time_series))]
      ) %>%
        dyGroup(names(t19a_la_wide)[c(1, as.numeric(input$t19a_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (WE)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t19a_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t19b_la_graph <- renderDygraph({
      dygraph(
        t19b_la_wide[,c(1, as.numeric(input$t19b_la_input_time_series))]
      ) %>%
        dyGroup(names(t19b_la_wide)[c(1, as.numeric(input$t19b_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (WE) (SE)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t19b_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t20_la_graph <- renderDygraph({
      dygraph(
        t20_la_wide[,c(1, as.numeric(input$t20_la_input_time_series))]
      ) %>%
        dyGroup(names(t20_la_wide)[c(1, as.numeric(input$t20_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Population") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t20_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t21_la_graph <- renderDygraph({
      dygraph(
        t21_la_wide[,c(1, as.numeric(input$t21_la_input_time_series))]
      ) %>%
        dyGroup(names(t21_la_wide)[c(1, as.numeric(input$t21_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Population (16+)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t21_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t22_la_graph <- renderDygraph({
      dygraph(
        t22_la_wide[,c(1, as.numeric(input$t22_la_input_time_series))]
      ) %>%
        dyGroup(names(t22_la_wide)[c(1, as.numeric(input$t22_la_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Population (16-64)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t22_la_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # REGIONS ####
      # t1_rg_barchart
      output$t1_rg_barchart <- renderPlot({
      DD <- t1_rg_long
      DD <- subset(DD, DD$Year == input$t1_rg_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t1_rg_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Value (£)") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 50, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t3b_rg_barchart
    output$t3b_rg_barchart <- renderPlot({
      DD <- t3b_rg_long
      DD <- subset(DD, DD$Year == input$t3b_rg_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t3b_rg_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Business registrations per 10,000 adults") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 1, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t4b_rg_barchart
    output$t4b_rg_barchart <- renderPlot({
      DD <- t4b_rg_long
      DD <- subset(DD, DD$Year == input$t4b_rg_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t4b_rg_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Business de-registrations per 10,000 adults") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 1, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t5b_rg_barchart
    output$t5b_rg_barchart <- renderPlot({
      DD <- t5b_rg_long
      DD <- subset(DD, DD$Year == input$t5b_rg_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t5b_rg_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Business 3-year survival rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.01, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t8a_rg_barchart
    output$t8a_rg_barchart <- renderPlot({
      DD <- t8a_rg_long
      DD <- subset(DD, DD$Year == input$t8a_rg_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t8a_rg_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "International exports per head") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 100, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t9_rg_barchart
    output$t9_rg_barchart <- renderPlot({
      DD <- t9_rg_long
      DD <- subset(DD, DD$Year == input$t9_rg_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t9_rg_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "BERD per head") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 10, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t14a_rg_barchart
    output$t14a_rg_barchart <- renderPlot({
      DD <- t14a_rg_long
      DD <- subset(DD, DD$Year == input$t14a_rg_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t14a_rg_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Employment rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.001, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t15b_rg_barchart
    output$t15b_rg_barchart <- renderPlot({
      DD <- t15b_rg_long
      DD <- subset(DD, DD$Year == input$t15b_rg_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t15b_rg_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Self-employment rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.001, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t17b_rg_barchart
    output$t17b_rg_barchart <- renderPlot({
      DD <- t17b_rg_long
      DD <- subset(DD, DD$Year == input$t17b_rg_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t17b_rg_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Unemployment rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.001, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # # t18a_rg_barchart no data available
    # output$t18a_rg_barchart <- renderPlot({
    #   DD <- t18a_rg_long
    #   DD <- subset(DD, DD$Year == input$t18a_rg_input)
    #   order.scores <- order(-DD$Value, DD$NAME)
    #   DD$Rank <- NA
    #   DD$Rank[order.scores] <- 1:nrow(DD)
    #   ggplot(subset(DD, DD$Year == input$t18a_rg_input)) + 
    #     geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
    #     coord_flip(clip = "off", expand = FALSE) + # Flip
    #     labs(x = "", y = "Median gross weekly pay") + # Labels
    #     theme_minimal() + # Theme
    #     geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
    #     geom_text(aes(x = Rank*120, y = Value + 0.1, label = as.character(Value)), hjust = 0, color = "black") + # Values
    #     scale_y_continuous(labels = scales::comma) + # Format y-axis values
    #     scale_x_reverse() + # Highest values on top
    #     theme(
    #       plot.margin = margin(0,2,0,7,"cm"),
    #       axis.text.y  = element_blank()
    #     )
    # })
    
    # TIME SERIES ####
    
    output$t1_rg_graph <- renderDygraph({
      dygraph(
        t1_rg_wide[,c(1, as.numeric(input$t1_rg_input_time_series))]
      ) %>%
        dyGroup(names(t1_rg_wide)[c(1, as.numeric(input$t1_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "GDP Growth Rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t1_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t2a_rg_graph <- renderDygraph({
      dygraph(
        t2a_rg_wide[,c(1, as.numeric(input$t2a_rg_input_time_series))]
      ) %>%
        dyGroup(names(t2a_rg_wide)[c(1, as.numeric(input$t2a_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE registered enterprises") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t2a_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t2b_rg_graph <- renderDygraph({
      dygraph(
        t2b_rg_wide[,c(1, as.numeric(input$t2b_rg_input_time_series))]
      ) %>%
        dyGroup(names(t2b_rg_wide)[c(1, as.numeric(input$t2b_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Small businesses") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t2b_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t3a_rg_graph <- renderDygraph({
      dygraph(
        t3a_rg_wide[,c(1, as.numeric(input$t3a_rg_input_time_series))]
      ) %>%
        dyGroup(names(t3a_rg_wide)[c(1, as.numeric(input$t3a_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Births)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t3a_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t3b_rg_graph <- renderDygraph({
      dygraph(
        t3b_rg_wide[,c(1, as.numeric(input$t3b_rg_input_time_series))]
      ) %>%
        dyGroup(names(t3b_rg_wide)[c(1, as.numeric(input$t3b_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Births) per 10,000 adults") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t3b_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t4a_rg_graph <- renderDygraph({
      dygraph(
        t4a_rg_wide[,c(1, as.numeric(input$t4a_rg_input_time_series))]
      ) %>%
        dyGroup(names(t4a_rg_wide)[c(1, as.numeric(input$t4a_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Deaths)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t4a_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t4b_rg_graph <- renderDygraph({
      dygraph(
        t4b_rg_wide[,c(1, as.numeric(input$t4b_rg_input_time_series))]
      ) %>%
        dyGroup(names(t4b_rg_wide)[c(1, as.numeric(input$t4b_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Deaths) per 10,000 adults") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t4b_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t5a_rg_graph <- renderDygraph({
      dygraph(
        t5a_rg_wide[,c(1, as.numeric(input$t5a_rg_input_time_series))]
      ) %>%
        dyGroup(names(t5a_rg_wide)[c(1, as.numeric(input$t5a_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "3-year business survival") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t5a_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t5b_rg_graph <- renderDygraph({
      dygraph(
        t5b_rg_wide[,c(1, as.numeric(input$t5b_rg_input_time_series))]
      ) %>%
        dyGroup(names(t5b_rg_wide)[c(1, as.numeric(input$t5b_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "3-year business survival rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t5b_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t6a_rg_graph <- renderDygraph({
      dygraph(
        t6a_rg_wide[,c(1, as.numeric(input$t6a_rg_input_time_series))]
      ) %>%
        dyGroup(names(t6a_rg_wide)[c(1, as.numeric(input$t6a_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned enterprises") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t6a_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t6b_rg_graph <- renderDygraph({
      dygraph(
        t6b_rg_wide[,c(1, as.numeric(input$t6b_rg_input_time_series))]
      ) %>%
        dyGroup(names(t6b_rg_wide)[c(1, as.numeric(input$t6b_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned enterprises (EU)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t6b_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t7a_rg_graph <- renderDygraph({
      dygraph(
        t7a_rg_wide[,c(1, as.numeric(input$t7a_rg_input_time_series))]
      ) %>%
        dyGroup(names(t7a_rg_wide)[c(1, as.numeric(input$t7a_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned business jobs") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t7a_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t7b_rg_graph <- renderDygraph({
      dygraph(
        t7b_rg_wide[,c(1, as.numeric(input$t7b_rg_input_time_series))]
      ) %>%
        dyGroup(names(t7b_rg_wide)[c(1, as.numeric(input$t7b_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned business jobs (EU)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t7b_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t8a_rg_graph <- renderDygraph({
      dygraph(
        t8a_rg_wide[,c(1, as.numeric(input$t8a_rg_input_time_series))]
      ) %>%
        dyGroup(names(t8a_rg_wide)[c(1, as.numeric(input$t8a_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "International Exports Value") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t8a_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t8b_rg_graph <- renderDygraph({
      dygraph(
        t8b_rg_wide[,c(1, as.numeric(input$t8b_rg_input_time_series))]
      ) %>%
        dyGroup(names(t8b_rg_wide)[c(1, as.numeric(input$t8b_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "International Exports Value (EU)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t8b_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t9_rg_graph <- renderDygraph({
      dygraph(
        t9_rg_wide[,c(1, as.numeric(input$t9_rg_input_time_series))]
      ) %>%
        dyGroup(names(t9_rg_wide)[c(1, as.numeric(input$t9_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "BERD") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t9_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t10_rg_graph <- renderDygraph({
      dygraph(
        t10_rg_wide[,c(1, as.numeric(input$t10_rg_input_time_series))]
      ) %>%
        dyGroup(names(t10_rg_wide)[c(1, as.numeric(input$t10_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "BERD Jobs") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t10_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t11_rg_graph <- renderDygraph({
      dygraph(
        t11_rg_wide[,c(1, as.numeric(input$t11_rg_input_time_series))]
      ) %>%
        dyGroup(names(t11_rg_wide)[c(1, as.numeric(input$t11_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "High growth enterprises") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t11_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t12_rg_graph <- renderDygraph({
      dygraph(
        t12_rg_wide[,c(1, as.numeric(input$t12_rg_input_time_series))]
      ) %>%
        dyGroup(names(t12_rg_wide)[c(1, as.numeric(input$t12_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE Businesses Jobs") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t12_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t13_rg_graph <- renderDygraph({
      dygraph(
        t13_rg_wide[,c(1, as.numeric(input$t13_rg_input_time_series))]
      ) %>%
        dyGroup(names(t13_rg_wide)[c(1, as.numeric(input$t13_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Employment") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t13_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t14a_rg_graph <- renderDygraph({
      dygraph(
        t14a_rg_wide[,c(1, as.numeric(input$t14a_rg_input_time_series))]
      ) %>%
        dyGroup(names(t14a_rg_wide)[c(1, as.numeric(input$t14a_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Working age employment rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t14a_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t14b_rg_graph <- renderDygraph({
      dygraph(
        t14b_rg_wide[,c(1, as.numeric(input$t14b_rg_input_time_series))]
      ) %>%
        dyGroup(names(t14b_rg_wide)[c(1, as.numeric(input$t14b_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Working age employment rate (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t14b_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t15a_rg_graph <- renderDygraph({
      dygraph(
        t15a_rg_wide[,c(1, as.numeric(input$t15a_rg_input_time_series))]
      ) %>%
        dyGroup(names(t15a_rg_wide)[c(1, as.numeric(input$t15a_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t15a_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t15b_rg_graph <- renderDygraph({
      dygraph(
        t15b_rg_wide[,c(1, as.numeric(input$t15b_rg_input_time_series))]
      ) %>%
        dyGroup(names(t15b_rg_wide)[c(1, as.numeric(input$t15b_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t15b_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t15c_rg_graph <- renderDygraph({
      dygraph(
        t15c_rg_wide[,c(1, as.numeric(input$t15c_rg_input_time_series))]
      ) %>%
        dyGroup(names(t15c_rg_wide)[c(1, as.numeric(input$t15c_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t15c_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t16a_rg_graph <- renderDygraph({
      dygraph(
        t16a_rg_wide[,c(1, as.numeric(input$t16a_rg_input_time_series))]
      ) %>%
        dyGroup(names(t16a_rg_wide)[c(1, as.numeric(input$t16a_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed Females") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t16a_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t16b_rg_graph <- renderDygraph({
      dygraph(
        t16b_rg_wide[,c(1, as.numeric(input$t16b_rg_input_time_series))]
      ) %>%
        dyGroup(names(t16b_rg_wide)[c(1, as.numeric(input$t16b_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed Females rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t16b_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t16c_rg_graph <- renderDygraph({
      dygraph(
        t16c_rg_wide[,c(1, as.numeric(input$t16c_rg_input_time_series))]
      ) %>%
        dyGroup(names(t16c_rg_wide)[c(1, as.numeric(input$t16c_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed Females rate (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t16c_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t17a_rg_graph <- renderDygraph({
      dygraph(
        t17a_rg_wide[,c(1, as.numeric(input$t17a_rg_input_time_series))]
      ) %>%
        dyGroup(names(t17a_rg_wide)[c(1, as.numeric(input$t17a_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Unemployment") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t17a_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t17b_rg_graph <- renderDygraph({
      dygraph(
        t17b_rg_wide[,c(1, as.numeric(input$t17b_rg_input_time_series))]
      ) %>%
        dyGroup(names(t17b_rg_wide)[c(1, as.numeric(input$t17b_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Unemployment rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t17b_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t17c_rg_graph <- renderDygraph({
      dygraph(
        t17c_rg_wide[,c(1, as.numeric(input$t17c_rg_input_time_series))]
      ) %>%
        dyGroup(names(t17c_rg_wide)[c(1, as.numeric(input$t17c_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Unemployment rate (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t17c_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t18a_rg_graph <- renderDygraph({
      dygraph(
        t18a_rg_wide[,c(1, as.numeric(input$t18a_rg_input_time_series))]
      ) %>%
        dyGroup(names(t18a_rg_wide)[c(1, as.numeric(input$t18a_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (RA)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t18a_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t18b_rg_graph <- renderDygraph({
      dygraph(
        t18b_rg_wide[,c(1, as.numeric(input$t18b_rg_input_time_series))]
      ) %>%
        dyGroup(names(t18b_rg_wide)[c(1, as.numeric(input$t18b_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (RA) (SE)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t18b_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t19a_rg_graph <- renderDygraph({
      dygraph(
        t19a_rg_wide[,c(1, as.numeric(input$t19a_rg_input_time_series))]
      ) %>%
        dyGroup(names(t19a_rg_wide)[c(1, as.numeric(input$t19a_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (WE)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t19a_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t19b_rg_graph <- renderDygraph({
      dygraph(
        t19b_rg_wide[,c(1, as.numeric(input$t19b_rg_input_time_series))]
      ) %>%
        dyGroup(names(t19b_rg_wide)[c(1, as.numeric(input$t19b_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (WE) (SE)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t19b_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t20_rg_graph <- renderDygraph({
      dygraph(
        t20_rg_wide[,c(1, as.numeric(input$t20_rg_input_time_series))]
      ) %>%
        dyGroup(names(t20_rg_wide)[c(1, as.numeric(input$t20_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Population") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t20_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t21_rg_graph <- renderDygraph({
      dygraph(
        t21_rg_wide[,c(1, as.numeric(input$t21_rg_input_time_series))]
      ) %>%
        dyGroup(names(t21_rg_wide)[c(1, as.numeric(input$t21_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Population (16+)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t21_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t22_rg_graph <- renderDygraph({
      dygraph(
        t22_rg_wide[,c(1, as.numeric(input$t22_rg_input_time_series))]
      ) %>%
        dyGroup(names(t22_rg_wide)[c(1, as.numeric(input$t22_rg_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Population (16-64)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t22_rg_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    # regions_table aka SUMMARY TOOL####
    output$regions_table <- DT::renderDT({
      
      data_rt <- summary_tool_r
      data_rt <- data_rt[, c("AREA", input$checked_indicators_r)]
      data_rt <- data_rt[data_rt$AREA %in% input$checked_areas_r,] 
      
      DT::datatable(
        data_rt,
        # summary_tool_la,
        # colnames = c("Sector", "Subsector", as.character(seq(St,Yr))),
        extensions = c("Buttons", "KeyTable", "Scroller", "FixedColumns"),
        rownames = FALSE,
        options = list(
          buttons = list(
            list(extend='excel', filename="Summary Tool"),
            list(extend='csv', filename="Summary Tool"),
            list(extend='pdf', filename="Summary Tool"),
            list(extend='copy', filename="Summary Tool"),
            list(extend='print', filename="Summary Tool")
          ),
          dom = 'frtBip',
          scrollX = TRUE,
          paging = TRUE,
          searching = TRUE,
          keys = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE,
          fixedColumns = list(leftColumns = 1)
        )
      ) #  %>%
      # formatCurrency(columns = seq(1, ncol(TABLE_ALL)), currency = "", interval = 3, mark = ",", digits=0) %>% 
      #  formatStyle("Subsector", target = 'row', fontWeight = styleEqual(c("Total"), c("bold"))
      # )
    })
    
    # RURAL AND URBAN ####
    
    # t1_ru_barchart
    output$t1_ru_barchart <- renderPlot({
      DD <- t1_ru_long
      DD <- subset(DD, DD$Year == input$t1_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t1_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Value (£)") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 50, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t3b_ru_barchart
    output$t3b_ru_barchart <- renderPlot({
      DD <- t3b_ru_long
      DD <- subset(DD, DD$Year == input$t3b_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t3b_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Business registrations per 10,000 adults") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 1, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t4b_ru_barchart
    output$t4b_ru_barchart <- renderPlot({
      DD <- t4b_ru_long
      DD <- subset(DD, DD$Year == input$t4b_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t4b_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Business de-registrations per 10,000 adults") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 1, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t5b_ru_barchart
    output$t5b_ru_barchart <- renderPlot({
      DD <- t5b_ru_long
      DD <- subset(DD, DD$Year == input$t5b_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t5b_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Business 3-year survival rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.01, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t8a_ru_barchart
    output$t8a_ru_barchart <- renderPlot({
      DD <- t8a_ru_long
      DD <- subset(DD, DD$Year == input$t8a_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t8a_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "International exports per head") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 100, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t9_ru_barchart
    output$t9_ru_barchart <- renderPlot({
      DD <- t9_ru_long
      DD <- subset(DD, DD$Year == input$t9_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t9_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "BERD per head") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 10, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t14a_ru_barchart
    output$t14a_ru_barchart <- renderPlot({
      DD <- t14a_ru_long
      DD <- subset(DD, DD$Year == input$t14a_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t14a_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Employment rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.001, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t15b_ru_barchart
    output$t15b_ru_barchart <- renderPlot({
      DD <- t15b_ru_long
      DD <- subset(DD, DD$Year == input$t15b_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t15b_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Self-employment rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.001, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t17b_ru_barchart
    output$t17b_ru_barchart <- renderPlot({
      DD <- t17b_ru_long
      DD <- subset(DD, DD$Year == input$t17b_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t17b_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Unemployment rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.001, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # # t18a_ru_barchart no data available
    # output$t18a_ru_barchart <- renderPlot({
    #   DD <- t18a_ru_long
    #   DD <- subset(DD, DD$Year == input$t18a_ru_input)
    #   order.scores <- order(-DD$Value, DD$NAME)
    #   DD$Rank <- NA
    #   DD$Rank[order.scores] <- 1:nrow(DD)
    #   ggplot(subset(DD, DD$Year == input$t18a_ru_input)) + 
    #     geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
    #     coord_flip(clip = "off", expand = FALSE) + # Flip
    #     labs(x = "", y = "Median gross weekly pay") + # Labels
    #     theme_minimal() + # Theme
    #     geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
    #     geom_text(aes(x = Rank*120, y = Value + 0.1, label = as.character(Value)), hjust = 0, color = "black") + # Values
    #     scale_y_continuous(labels = scales::comma) + # Format y-axis values
    #     scale_x_reverse() + # Highest values on top
    #     theme(
    #       plot.margin = margin(0,2,0,7,"cm"),
    #       axis.text.y  = element_blank()
    #     )
    # })
    
    # TIME SERIES ####
    
    output$t1_ru_graph <- renderDygraph({
      dygraph(
        t1_ru_wide[,c(1, as.numeric(input$t1_ru_input_time_series))]
      ) %>%
        dyGroup(names(t1_ru_wide)[c(1, as.numeric(input$t1_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "GDP Growth Rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t1_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t2a_ru_graph <- renderDygraph({
      dygraph(
        t2a_ru_wide[,c(1, as.numeric(input$t2a_ru_input_time_series))]
      ) %>%
        dyGroup(names(t2a_ru_wide)[c(1, as.numeric(input$t2a_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE registered enterprises") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t2a_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t2b_ru_graph <- renderDygraph({
      dygraph(
        t2b_ru_wide[,c(1, as.numeric(input$t2b_ru_input_time_series))]
      ) %>%
        dyGroup(names(t2b_ru_wide)[c(1, as.numeric(input$t2b_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Small businesses") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t2b_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t3a_ru_graph <- renderDygraph({
      dygraph(
        t3a_ru_wide[,c(1, as.numeric(input$t3a_ru_input_time_series))]
      ) %>%
        dyGroup(names(t3a_ru_wide)[c(1, as.numeric(input$t3a_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Births)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t3a_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t3b_ru_graph <- renderDygraph({
      dygraph(
        t3b_ru_wide[,c(1, as.numeric(input$t3b_ru_input_time_series))]
      ) %>%
        dyGroup(names(t3b_ru_wide)[c(1, as.numeric(input$t3b_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Births) per 10,000 adults") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t3b_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t4a_ru_graph <- renderDygraph({
      dygraph(
        t4a_ru_wide[,c(1, as.numeric(input$t4a_ru_input_time_series))]
      ) %>%
        dyGroup(names(t4a_ru_wide)[c(1, as.numeric(input$t4a_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Deaths)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t4a_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t4b_ru_graph <- renderDygraph({
      dygraph(
        t4b_ru_wide[,c(1, as.numeric(input$t4b_ru_input_time_series))]
      ) %>%
        dyGroup(names(t4b_ru_wide)[c(1, as.numeric(input$t4b_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Deaths) per 10,000 adults") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t4b_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t5a_ru_graph <- renderDygraph({
      dygraph(
        t5a_ru_wide[,c(1, as.numeric(input$t5a_ru_input_time_series))]
      ) %>%
        dyGroup(names(t5a_ru_wide)[c(1, as.numeric(input$t5a_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "3-year business survival") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t5a_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t5b_ru_graph <- renderDygraph({
      dygraph(
        t5b_ru_wide[,c(1, as.numeric(input$t5b_ru_input_time_series))]
      ) %>%
        dyGroup(names(t5b_ru_wide)[c(1, as.numeric(input$t5b_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "3-year business survival rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t5b_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t6a_ru_graph <- renderDygraph({
      dygraph(
        t6a_ru_wide[,c(1, as.numeric(input$t6a_ru_input_time_series))]
      ) %>%
        dyGroup(names(t6a_ru_wide)[c(1, as.numeric(input$t6a_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned enterprises") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t6a_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t6b_ru_graph <- renderDygraph({
      dygraph(
        t6b_ru_wide[,c(1, as.numeric(input$t6b_ru_input_time_series))]
      ) %>%
        dyGroup(names(t6b_ru_wide)[c(1, as.numeric(input$t6b_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned enterprises (EU)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t6b_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t7a_ru_graph <- renderDygraph({
      dygraph(
        t7a_ru_wide[,c(1, as.numeric(input$t7a_ru_input_time_series))]
      ) %>%
        dyGroup(names(t7a_ru_wide)[c(1, as.numeric(input$t7a_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned business jobs") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t7a_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t7b_ru_graph <- renderDygraph({
      dygraph(
        t7b_ru_wide[,c(1, as.numeric(input$t7b_ru_input_time_series))]
      ) %>%
        dyGroup(names(t7b_ru_wide)[c(1, as.numeric(input$t7b_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned business jobs (EU)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t7b_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t8a_ru_graph <- renderDygraph({
      dygraph(
        t8a_ru_wide[,c(1, as.numeric(input$t8a_ru_input_time_series))]
      ) %>%
        dyGroup(names(t8a_ru_wide)[c(1, as.numeric(input$t8a_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "International Exports Value") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t8a_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t8b_ru_graph <- renderDygraph({
      dygraph(
        t8b_ru_wide[,c(1, as.numeric(input$t8b_ru_input_time_series))]
      ) %>%
        dyGroup(names(t8b_ru_wide)[c(1, as.numeric(input$t8b_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "International Exports Value (EU)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t8b_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t9_ru_graph <- renderDygraph({
      dygraph(
        t9_ru_wide[,c(1, as.numeric(input$t9_ru_input_time_series))]
      ) %>%
        dyGroup(names(t9_ru_wide)[c(1, as.numeric(input$t9_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "BERD") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t9_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t10_ru_graph <- renderDygraph({
      dygraph(
        t10_ru_wide[,c(1, as.numeric(input$t10_ru_input_time_series))]
      ) %>%
        dyGroup(names(t10_ru_wide)[c(1, as.numeric(input$t10_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "BERD Jobs") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t10_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t11_ru_graph <- renderDygraph({
      dygraph(
        t11_ru_wide[,c(1, as.numeric(input$t11_ru_input_time_series))]
      ) %>%
        dyGroup(names(t11_ru_wide)[c(1, as.numeric(input$t11_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "High growth enterprises") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t11_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t12_ru_graph <- renderDygraph({
      dygraph(
        t12_ru_wide[,c(1, as.numeric(input$t12_ru_input_time_series))]
      ) %>%
        dyGroup(names(t12_ru_wide)[c(1, as.numeric(input$t12_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE Businesses Jobs") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t12_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t13_ru_graph <- renderDygraph({
      dygraph(
        t13_ru_wide[,c(1, as.numeric(input$t13_ru_input_time_series))]
      ) %>%
        dyGroup(names(t13_ru_wide)[c(1, as.numeric(input$t13_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Employment") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t13_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t14a_ru_graph <- renderDygraph({
      dygraph(
        t14a_ru_wide[,c(1, as.numeric(input$t14a_ru_input_time_series))]
      ) %>%
        dyGroup(names(t14a_ru_wide)[c(1, as.numeric(input$t14a_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Working age employment rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t14a_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t14b_ru_graph <- renderDygraph({
      dygraph(
        t14b_ru_wide[,c(1, as.numeric(input$t14b_ru_input_time_series))]
      ) %>%
        dyGroup(names(t14b_ru_wide)[c(1, as.numeric(input$t14b_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Working age employment rate (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t14b_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t15a_ru_graph <- renderDygraph({
      dygraph(
        t15a_ru_wide[,c(1, as.numeric(input$t15a_ru_input_time_series))]
      ) %>%
        dyGroup(names(t15a_ru_wide)[c(1, as.numeric(input$t15a_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t15a_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t15b_ru_graph <- renderDygraph({
      dygraph(
        t15b_ru_wide[,c(1, as.numeric(input$t15b_ru_input_time_series))]
      ) %>%
        dyGroup(names(t15b_ru_wide)[c(1, as.numeric(input$t15b_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t15b_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t15c_ru_graph <- renderDygraph({
      dygraph(
        t15c_ru_wide[,c(1, as.numeric(input$t15c_ru_input_time_series))]
      ) %>%
        dyGroup(names(t15c_ru_wide)[c(1, as.numeric(input$t15c_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t15c_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t16a_ru_graph <- renderDygraph({
      dygraph(
        t16a_ru_wide[,c(1, as.numeric(input$t16a_ru_input_time_series))]
      ) %>%
        dyGroup(names(t16a_ru_wide)[c(1, as.numeric(input$t16a_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed Females") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t16a_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t16b_ru_graph <- renderDygraph({
      dygraph(
        t16b_ru_wide[,c(1, as.numeric(input$t16b_ru_input_time_series))]
      ) %>%
        dyGroup(names(t16b_ru_wide)[c(1, as.numeric(input$t16b_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed Females rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t16b_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t16c_ru_graph <- renderDygraph({
      dygraph(
        t16c_ru_wide[,c(1, as.numeric(input$t16c_ru_input_time_series))]
      ) %>%
        dyGroup(names(t16c_ru_wide)[c(1, as.numeric(input$t16c_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed Females rate (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t16c_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t17a_ru_graph <- renderDygraph({
      dygraph(
        t17a_ru_wide[,c(1, as.numeric(input$t17a_ru_input_time_series))]
      ) %>%
        dyGroup(names(t17a_ru_wide)[c(1, as.numeric(input$t17a_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Unemployment") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t17a_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t17b_ru_graph <- renderDygraph({
      dygraph(
        t17b_ru_wide[,c(1, as.numeric(input$t17b_ru_input_time_series))]
      ) %>%
        dyGroup(names(t17b_ru_wide)[c(1, as.numeric(input$t17b_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Unemployment rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t17b_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t17c_ru_graph <- renderDygraph({
      dygraph(
        t17c_ru_wide[,c(1, as.numeric(input$t17c_ru_input_time_series))]
      ) %>%
        dyGroup(names(t17c_ru_wide)[c(1, as.numeric(input$t17c_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Unemployment rate (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t17c_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t18a_ru_graph <- renderDygraph({
      dygraph(
        t18a_ru_wide[,c(1, as.numeric(input$t18a_ru_input_time_series))]
      ) %>%
        dyGroup(names(t18a_ru_wide)[c(1, as.numeric(input$t18a_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (RA)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t18a_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t18b_ru_graph <- renderDygraph({
      dygraph(
        t18b_ru_wide[,c(1, as.numeric(input$t18b_ru_input_time_series))]
      ) %>%
        dyGroup(names(t18b_ru_wide)[c(1, as.numeric(input$t18b_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (RA) (SE)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t18b_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t19a_ru_graph <- renderDygraph({
      dygraph(
        t19a_ru_wide[,c(1, as.numeric(input$t19a_ru_input_time_series))]
      ) %>%
        dyGroup(names(t19a_ru_wide)[c(1, as.numeric(input$t19a_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (WE)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t19a_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t19b_ru_graph <- renderDygraph({
      dygraph(
        t19b_ru_wide[,c(1, as.numeric(input$t19b_ru_input_time_series))]
      ) %>%
        dyGroup(names(t19b_ru_wide)[c(1, as.numeric(input$t19b_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (WE) (SE)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t19b_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t20_ru_graph <- renderDygraph({
      dygraph(
        t20_ru_wide[,c(1, as.numeric(input$t20_ru_input_time_series))]
      ) %>%
        dyGroup(names(t20_ru_wide)[c(1, as.numeric(input$t20_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Population") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t20_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t21_ru_graph <- renderDygraph({
      dygraph(
        t21_ru_wide[,c(1, as.numeric(input$t21_ru_input_time_series))]
      ) %>%
        dyGroup(names(t21_ru_wide)[c(1, as.numeric(input$t21_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Population (16+)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t21_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t22_ru_graph <- renderDygraph({
      dygraph(
        t22_ru_wide[,c(1, as.numeric(input$t22_ru_input_time_series))]
      ) %>%
        dyGroup(names(t22_ru_wide)[c(1, as.numeric(input$t22_ru_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Population (16-64)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t22_ru_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    
    # ruandurb_table aka SUMMARY TOOL####
    output$ruandurb_table <- DT::renderDT({
      
      data_rut <- summary_tool_ru
      data_rut <- data_rut[, c("AREA", input$checked_indicators_ru)]
      data_rut <- data_rut[data_rut$AREA %in% input$checked_areas_ru,] 
      
      DT::datatable(
        data_rut,
        # summary_tool_la,
        # colnames = c("Sector", "Subsector", as.character(seq(St,Yr))),
        extensions = c("Buttons", "KeyTable", "Scroller", "FixedColumns"),
        rownames = FALSE,
        options = list(
          buttons = list(
            list(extend='excel', filename="Summary Tool"),
            list(extend='csv', filename="Summary Tool"),
            list(extend='pdf', filename="Summary Tool"),
            list(extend='copy', filename="Summary Tool"),
            list(extend='print', filename="Summary Tool")
          ),
          dom = 'frtBip',
          scrollX = TRUE,
          paging = TRUE,
          searching = TRUE,
          keys = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE,
          fixedColumns = list(leftColumns = 1)
        )
      ) #  %>%
      # formatCurrency(columns = seq(1, ncol(TABLE_ALL)), currency = "", interval = 3, mark = ",", digits=0) %>% 
      #  formatStyle("Subsector", target = 'row', fontWeight = styleEqual(c("Total"), c("bold"))
      # )
    })
    
    # ENTERPRISE ####
    
    
    # t1_e_barchart
    output$t1_e_barchart <- renderPlot({
      DD <- t1_e_long
      DD <- subset(DD, DD$Year == input$t1_e_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t1_e_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Value (£)") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 50, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t3b_e_barchart
    output$t3b_e_barchart <- renderPlot({
      DD <- t3b_e_long
      DD <- subset(DD, DD$Year == input$t3b_e_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t3b_e_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Business registrations per 10,000 adults") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 1, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t4b_e_barchart
    output$t4b_e_barchart <- renderPlot({
      DD <- t4b_e_long
      DD <- subset(DD, DD$Year == input$t4b_e_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t4b_e_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Business de-registrations per 10,000 adults") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 1, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t5b_e_barchart
    output$t5b_e_barchart <- renderPlot({
      DD <- t5b_e_long
      DD <- subset(DD, DD$Year == input$t5b_e_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t5b_e_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Business 3-year survival rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.01, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t8a_e_barchart
    output$t8a_e_barchart <- renderPlot({
      DD <- t8a_e_long
      DD <- subset(DD, DD$Year == input$t8a_e_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t8a_e_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "International exports per head") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 100, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # no data available
    # # t9_e_barchart
    # output$t9_e_barchart <- renderPlot({
    #   DD <- t9_e_long
    #   DD <- subset(DD, DD$Year == input$t9_e_input)
    #   order.scores <- order(-DD$Value, DD$NAME)
    #   DD$Rank <- NA
    #   DD$Rank[order.scores] <- 1:nrow(DD)
    #   ggplot(subset(DD, DD$Year == input$t9_e_input)) + 
    #     geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
    #     coord_flip(clip = "off", expand = FALSE) + # Flip
    #     labs(x = "", y = "BERD per head") + # Labels
    #     theme_minimal() + # Theme
    #     geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
    #     geom_text(aes(x = Rank*120, y = Value + 10, label = as.character(Value)), hjust = 0, color = "black") + # Values
    #     scale_y_continuous(labels = scales::comma) + # Format y-axis values
    #     scale_x_reverse() + # Highest values on top
    #     theme(
    #       plot.margin = margin(0,2,0,7,"cm"),
    #       axis.text.y  = element_blank()
    #     )
    # })
    
    # t14a_e_barchart
    output$t14a_e_barchart <- renderPlot({
      DD <- t14a_e_long
      DD <- subset(DD, DD$Year == input$t14a_e_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t14a_e_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Employment rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.001, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t15b_e_barchart
    output$t15b_e_barchart <- renderPlot({
      DD <- t15b_e_long
      DD <- subset(DD, DD$Year == input$t15b_e_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t15b_e_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Self-employment rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.001, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t17b_e_barchart
    output$t17b_e_barchart <- renderPlot({
      DD <- t17b_e_long
      DD <- subset(DD, DD$Year == input$t17b_e_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t17b_e_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Unemployment rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.001, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # # t18a_e_barchart no data available
    # output$t18a_e_barchart <- renderPlot({
    #   DD <- t18a_e_long
    #   DD <- subset(DD, DD$Year == input$t18a_e_input)
    #   order.scores <- order(-DD$Value, DD$NAME)
    #   DD$Rank <- NA
    #   DD$Rank[order.scores] <- 1:nrow(DD)
    #   ggplot(subset(DD, DD$Year == input$t18a_e_input)) + 
    #     geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
    #     coord_flip(clip = "off", expand = FALSE) + # Flip
    #     labs(x = "", y = "Median gross weekly pay") + # Labels
    #     theme_minimal() + # Theme
    #     geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
    #     geom_text(aes(x = Rank*120, y = Value + 0.1, label = as.character(Value)), hjust = 0, color = "black") + # Values
    #     scale_y_continuous(labels = scales::comma) + # Format y-axis values
    #     scale_x_reverse() + # Highest values on top
    #     theme(
    #       plot.margin = margin(0,2,0,7,"cm"),
    #       axis.text.y  = element_blank()
    #     )
    # })
    
    
    # TIME SERIES ####
    
    output$t1_e_graph <- renderDygraph({
      dygraph(
        t1_e_wide[,c(1, as.numeric(input$t1_e_input_time_series))]
      ) %>%
        dyGroup(names(t1_e_wide)[c(1, as.numeric(input$t1_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "GDP Growth Rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t1_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t2a_e_graph <- renderDygraph({
      dygraph(
        t2a_e_wide[,c(1, as.numeric(input$t2a_e_input_time_series))]
      ) %>%
        dyGroup(names(t2a_e_wide)[c(1, as.numeric(input$t2a_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE registered enterprises") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t2a_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t2b_e_graph <- renderDygraph({
      dygraph(
        t2b_e_wide[,c(1, as.numeric(input$t2b_e_input_time_series))]
      ) %>%
        dyGroup(names(t2b_e_wide)[c(1, as.numeric(input$t2b_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Small businesses") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t2b_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t3a_e_graph <- renderDygraph({
      dygraph(
        t3a_e_wide[,c(1, as.numeric(input$t3a_e_input_time_series))]
      ) %>%
        dyGroup(names(t3a_e_wide)[c(1, as.numeric(input$t3a_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Births)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t3a_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t3b_e_graph <- renderDygraph({
      dygraph(
        t3b_e_wide[,c(1, as.numeric(input$t3b_e_input_time_series))]
      ) %>%
        dyGroup(names(t3b_e_wide)[c(1, as.numeric(input$t3b_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Births) per 10,000 adults") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t3b_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t4a_e_graph <- renderDygraph({
      dygraph(
        t4a_e_wide[,c(1, as.numeric(input$t4a_e_input_time_series))]
      ) %>%
        dyGroup(names(t4a_e_wide)[c(1, as.numeric(input$t4a_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Deaths)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t4a_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t4b_e_graph <- renderDygraph({
      dygraph(
        t4b_e_wide[,c(1, as.numeric(input$t4b_e_input_time_series))]
      ) %>%
        dyGroup(names(t4b_e_wide)[c(1, as.numeric(input$t4b_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE (Business Deaths) per 10,000 adults") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t4b_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t5a_e_graph <- renderDygraph({
      dygraph(
        t5a_e_wide[,c(1, as.numeric(input$t5a_e_input_time_series))]
      ) %>%
        dyGroup(names(t5a_e_wide)[c(1, as.numeric(input$t5a_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "3-year business survival") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t5a_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t5b_e_graph <- renderDygraph({
      dygraph(
        t5b_e_wide[,c(1, as.numeric(input$t5b_e_input_time_series))]
      ) %>%
        dyGroup(names(t5b_e_wide)[c(1, as.numeric(input$t5b_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "3-year business survival rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t5b_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t6a_e_graph <- renderDygraph({
      dygraph(
        t6a_e_wide[,c(1, as.numeric(input$t6a_e_input_time_series))]
      ) %>%
        dyGroup(names(t6a_e_wide)[c(1, as.numeric(input$t6a_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned enterprises") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t6a_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t6b_e_graph <- renderDygraph({
      dygraph(
        t6b_e_wide[,c(1, as.numeric(input$t6b_e_input_time_series))]
      ) %>%
        dyGroup(names(t6b_e_wide)[c(1, as.numeric(input$t6b_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned enterprises (EU)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t6b_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t7a_e_graph <- renderDygraph({
      dygraph(
        t7a_e_wide[,c(1, as.numeric(input$t7a_e_input_time_series))]
      ) %>%
        dyGroup(names(t7a_e_wide)[c(1, as.numeric(input$t7a_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned business jobs") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t7a_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t7b_e_graph <- renderDygraph({
      dygraph(
        t7b_e_wide[,c(1, as.numeric(input$t7b_e_input_time_series))]
      ) %>%
        dyGroup(names(t7b_e_wide)[c(1, as.numeric(input$t7b_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Foreign-owned business jobs (EU)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t7b_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t8a_e_graph <- renderDygraph({
      dygraph(
        t8a_e_wide[,c(1, as.numeric(input$t8a_e_input_time_series))]
      ) %>%
        dyGroup(names(t8a_e_wide)[c(1, as.numeric(input$t8a_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "International Exports Value") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t8a_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t8b_e_graph <- renderDygraph({
      dygraph(
        t8b_e_wide[,c(1, as.numeric(input$t8b_e_input_time_series))]
      ) %>%
        dyGroup(names(t8b_e_wide)[c(1, as.numeric(input$t8b_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "International Exports Value (EU)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t8b_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t9_e_graph <- renderDygraph({
      dygraph(
        t9_e_wide[,c(1, as.numeric(input$t9_e_input_time_series))]
      ) %>%
        dyGroup(names(t9_e_wide)[c(1, as.numeric(input$t9_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "BERD") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t9_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t10_e_graph <- renderDygraph({
      dygraph(
        t10_e_wide[,c(1, as.numeric(input$t10_e_input_time_series))]
      ) %>%
        dyGroup(names(t10_e_wide)[c(1, as.numeric(input$t10_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "BERD Jobs") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t10_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t11_e_graph <- renderDygraph({
      dygraph(
        t11_e_wide[,c(1, as.numeric(input$t11_e_input_time_series))]
      ) %>%
        dyGroup(names(t11_e_wide)[c(1, as.numeric(input$t11_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "High growth enterprises") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t11_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t12_e_graph <- renderDygraph({
      dygraph(
        t12_e_wide[,c(1, as.numeric(input$t12_e_input_time_series))]
      ) %>%
        dyGroup(names(t12_e_wide)[c(1, as.numeric(input$t12_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "VAT/PAYE Businesses Jobs") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t12_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t13_e_graph <- renderDygraph({
      dygraph(
        t13_e_wide[,c(1, as.numeric(input$t13_e_input_time_series))]
      ) %>%
        dyGroup(names(t13_e_wide)[c(1, as.numeric(input$t13_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Employment") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t13_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t14a_e_graph <- renderDygraph({
      dygraph(
        t14a_e_wide[,c(1, as.numeric(input$t14a_e_input_time_series))]
      ) %>%
        dyGroup(names(t14a_e_wide)[c(1, as.numeric(input$t14a_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Working age employment rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t14a_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t14b_e_graph <- renderDygraph({
      dygraph(
        t14b_e_wide[,c(1, as.numeric(input$t14b_e_input_time_series))]
      ) %>%
        dyGroup(names(t14b_e_wide)[c(1, as.numeric(input$t14b_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Working age employment rate (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t14b_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t15a_e_graph <- renderDygraph({
      dygraph(
        t15a_e_wide[,c(1, as.numeric(input$t15a_e_input_time_series))]
      ) %>%
        dyGroup(names(t15a_e_wide)[c(1, as.numeric(input$t15a_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t15a_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t15b_e_graph <- renderDygraph({
      dygraph(
        t15b_e_wide[,c(1, as.numeric(input$t15b_e_input_time_series))]
      ) %>%
        dyGroup(names(t15b_e_wide)[c(1, as.numeric(input$t15b_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t15b_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t15c_e_graph <- renderDygraph({
      dygraph(
        t15c_e_wide[,c(1, as.numeric(input$t15c_e_input_time_series))]
      ) %>%
        dyGroup(names(t15c_e_wide)[c(1, as.numeric(input$t15c_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t15c_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t16a_e_graph <- renderDygraph({
      dygraph(
        t16a_e_wide[,c(1, as.numeric(input$t16a_e_input_time_series))]
      ) %>%
        dyGroup(names(t16a_e_wide)[c(1, as.numeric(input$t16a_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed Females") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t16a_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t16b_e_graph <- renderDygraph({
      dygraph(
        t16b_e_wide[,c(1, as.numeric(input$t16b_e_input_time_series))]
      ) %>%
        dyGroup(names(t16b_e_wide)[c(1, as.numeric(input$t16b_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed Females rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t16b_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t16c_e_graph <- renderDygraph({
      dygraph(
        t16c_e_wide[,c(1, as.numeric(input$t16c_e_input_time_series))]
      ) %>%
        dyGroup(names(t16c_e_wide)[c(1, as.numeric(input$t16c_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Self-employed Females rate (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t16c_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t17a_e_graph <- renderDygraph({
      dygraph(
        t17a_e_wide[,c(1, as.numeric(input$t17a_e_input_time_series))]
      ) %>%
        dyGroup(names(t17a_e_wide)[c(1, as.numeric(input$t17a_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Unemployment") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t17a_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t17b_e_graph <- renderDygraph({
      dygraph(
        t17b_e_wide[,c(1, as.numeric(input$t17b_e_input_time_series))]
      ) %>%
        dyGroup(names(t17b_e_wide)[c(1, as.numeric(input$t17b_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Unemployment rate") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t17b_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t17c_e_graph <- renderDygraph({
      dygraph(
        t17c_e_wide[,c(1, as.numeric(input$t17c_e_input_time_series))]
      ) %>%
        dyGroup(names(t17c_e_wide)[c(1, as.numeric(input$t17c_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Unemployment rate (95% CI)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t17c_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t18a_e_graph <- renderDygraph({
      dygraph(
        t18a_e_wide[,c(1, as.numeric(input$t18a_e_input_time_series))]
      ) %>%
        dyGroup(names(t18a_e_wide)[c(1, as.numeric(input$t18a_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (RA)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t18a_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t18b_e_graph <- renderDygraph({
      dygraph(
        t18b_e_wide[,c(1, as.numeric(input$t18b_e_input_time_series))]
      ) %>%
        dyGroup(names(t18b_e_wide)[c(1, as.numeric(input$t18b_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (RA) (SE)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t18b_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t19a_e_graph <- renderDygraph({
      dygraph(
        t19a_e_wide[,c(1, as.numeric(input$t19a_e_input_time_series))]
      ) %>%
        dyGroup(names(t19a_e_wide)[c(1, as.numeric(input$t19a_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (WE)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t19a_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t19b_e_graph <- renderDygraph({
      dygraph(
        t19b_e_wide[,c(1, as.numeric(input$t19b_e_input_time_series))]
      ) %>%
        dyGroup(names(t19b_e_wide)[c(1, as.numeric(input$t19b_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Median gross weekly pay (WE) (SE)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t19b_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t20_e_graph <- renderDygraph({
      dygraph(
        t20_e_wide[,c(1, as.numeric(input$t20_e_input_time_series))]
      ) %>%
        dyGroup(names(t20_e_wide)[c(1, as.numeric(input$t20_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Population") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t20_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t21_e_graph <- renderDygraph({
      dygraph(
        t21_e_wide[,c(1, as.numeric(input$t21_e_input_time_series))]
      ) %>%
        dyGroup(names(t21_e_wide)[c(1, as.numeric(input$t21_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Population (16+)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t21_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    output$t22_e_graph <- renderDygraph({
      dygraph(
        t22_e_wide[,c(1, as.numeric(input$t22_e_input_time_series))]
      ) %>%
        dyGroup(names(t22_e_wide)[c(1, as.numeric(input$t22_e_input_time_series))], strokeWidth = 2) %>%
        dyRangeSelector() %>%
        dyAxis("x", label = "Year", rangePad = 5) %>%
        dyAxis("y", label = "Population (16-64)") %>%
        dyHighlight(
          highlightCircleSize = 3,
          highlightSeriesBackgroundAlpha = 0.2,
          hideOnMouseOut = FALSE,
          highlightSeriesOpts = list(strokeWidth = 6)
        ) %>%
        dyOptions(gridLineColor = "lightgrey", digitsAfterDecimal = 2) %>%
        dyLegend(labelsDiv = "legendDivID_t22_e_graph", labelsSeparateLines = TRUE) %>%
        dyUnzoom() %>%
        dyCrosshair(direction = "vertical")
    })
    
    
    # regions_table aka SUMMARY TOOL####
    output$regions_table <- DT::renderDT({
      
      data_rt <- summary_tool_r
      data_rt <- data_rt[, c("AREA", input$checked_indicators_r)]
      data_rt <- data_rt[data_rt$AREA %in% input$checked_areas_r,] 
      
      DT::datatable(
        data_rt,
        # summary_tool_la,
        # colnames = c("Sector", "Subsector", as.character(seq(St,Yr))),
        extensions = c("Buttons", "KeyTable", "Scroller", "FixedColumns"),
        rownames = FALSE,
        options = list(
          buttons = list(
            list(extend='excel', filename="Summary Tool"),
            list(extend='csv', filename="Summary Tool"),
            list(extend='pdf', filename="Summary Tool"),
            list(extend='copy', filename="Summary Tool"),
            list(extend='print', filename="Summary Tool")
          ),
          dom = 'frtBip',
          scrollX = TRUE,
          paging = TRUE,
          searching = TRUE,
          keys = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE,
          fixedColumns = list(leftColumns = 1)
        )
      ) #  %>%
      # formatCurrency(columns = seq(1, ncol(TABLE_ALL)), currency = "", interval = 3, mark = ",", digits=0) %>% 
      #  formatStyle("Subsector", target = 'row', fontWeight = styleEqual(c("Total"), c("bold"))
      # )
    })
    
    # RURAL AND URBAN ####
    
    # t1_ru_barchart
    output$t1_ru_barchart <- renderPlot({
      DD <- t1_ru_long
      DD <- subset(DD, DD$Year == input$t1_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t1_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Value (£)") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 50, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t3b_ru_barchart
    output$t3b_ru_barchart <- renderPlot({
      DD <- t3b_ru_long
      DD <- subset(DD, DD$Year == input$t3b_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t3b_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Business registrations per 10,000 adults") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 1, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t4b_ru_barchart
    output$t4b_ru_barchart <- renderPlot({
      DD <- t4b_ru_long
      DD <- subset(DD, DD$Year == input$t4b_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t4b_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Business de-registrations per 10,000 adults") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 1, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t5b_ru_barchart
    output$t5b_ru_barchart <- renderPlot({
      DD <- t5b_ru_long
      DD <- subset(DD, DD$Year == input$t5b_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t5b_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Business 3-year survival rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.01, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t8a_ru_barchart
    output$t8a_ru_barchart <- renderPlot({
      DD <- t8a_ru_long
      DD <- subset(DD, DD$Year == input$t8a_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t8a_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "International exports per head") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 100, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t9_ru_barchart
    output$t9_ru_barchart <- renderPlot({
      DD <- t9_ru_long
      DD <- subset(DD, DD$Year == input$t9_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t9_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "BERD per head") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 10, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t14a_ru_barchart
    output$t14a_ru_barchart <- renderPlot({
      DD <- t14a_ru_long
      DD <- subset(DD, DD$Year == input$t14a_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t14a_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Employment rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.001, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t15b_ru_barchart
    output$t15b_ru_barchart <- renderPlot({
      DD <- t15b_ru_long
      DD <- subset(DD, DD$Year == input$t15b_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t15b_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Self-employment rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.001, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # t17b_ru_barchart
    output$t17b_ru_barchart <- renderPlot({
      DD <- t17b_ru_long
      DD <- subset(DD, DD$Year == input$t17b_ru_input)
      order.scores <- order(-DD$Value, DD$NAME)
      DD$Rank <- NA
      DD$Rank[order.scores] <- 1:nrow(DD)
      ggplot(subset(DD, DD$Year == input$t17b_ru_input)) + 
        geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(x = "", y = "Unemployment rate") + # Labels
        theme_minimal() + # Theme
        geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
        geom_text(aes(x = Rank*120, y = Value + 0.001, label = as.character(Value)), hjust = 0, color = "black") + # Values
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        theme(
          plot.margin = margin(0,2,0,7,"cm"),
          axis.text.y  = element_blank()
        )
    })
    
    # # t18a_ru_barchart no data available
    # output$t18a_ru_barchart <- renderPlot({
    #   DD <- t18a_ru_long
    #   DD <- subset(DD, DD$Year == input$t18a_ru_input)
    #   order.scores <- order(-DD$Value, DD$NAME)
    #   DD$Rank <- NA
    #   DD$Rank[order.scores] <- 1:nrow(DD)
    #   ggplot(subset(DD, DD$Year == input$t18a_ru_input)) + 
    #     geom_col(aes(x = Rank*120, y = Value), fill = "azure3", color = "black") + # Columns
    #     coord_flip(clip = "off", expand = FALSE) + # Flip
    #     labs(x = "", y = "Median gross weekly pay") + # Labels
    #     theme_minimal() + # Theme
    #     geom_text(aes(x = Rank*120, y = 0, label = NAME), hjust = 1) + # Names
    #     geom_text(aes(x = Rank*120, y = Value + 0.1, label = as.character(Value)), hjust = 0, color = "black") + # Values
    #     scale_y_continuous(labels = scales::comma) + # Format y-axis values
    #     scale_x_reverse() + # Highest values on top
    #     theme(
    #       plot.margin = margin(0,2,0,7,"cm"),
    #       axis.text.y  = element_blank()
    #     )
    # })
    
    
    
    
    # enterprise_table aka SUMMARY TOOL####
    output$enterprise_table <- DT::renderDT({
      
      data_et <- summary_tool_e
      data_et <- data_et[, c("AREA", input$checked_indicators_e)]
      data_et <- data_et[data_et$AREA %in% input$checked_areas_e,] 
      
      DT::datatable(
        data_et,
        # summary_tool_la,
        # colnames = c("Sector", "Subsector", as.character(seq(St,Yr))),
        extensions = c("Buttons", "KeyTable", "Scroller", "FixedColumns"),
        rownames = FALSE,
        options = list(
          buttons = list(
            list(extend='excel', filename="Summary Tool"),
            list(extend='csv', filename="Summary Tool"),
            list(extend='pdf', filename="Summary Tool"),
            list(extend='copy', filename="Summary Tool"),
            list(extend='print', filename="Summary Tool")
          ),
          dom = 'frtBip',
          scrollX = TRUE,
          paging = TRUE,
          searching = TRUE,
          keys = TRUE,
          deferRender = TRUE,
          scrollY = 500,
          scroller = TRUE,
          fixedColumns = list(leftColumns = 1)
        )
      ) #  %>%
      # formatCurrency(columns = seq(1, ncol(TABLE_ALL)), currency = "", interval = 3, mark = ",", digits=0) %>% 
      #  formatStyle("Subsector", target = 'row', fontWeight = styleEqual(c("Total"), c("bold"))
      # )
    })
    
  } # ... server ends here.
)
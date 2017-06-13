function(input, output, session) {
  
   selectedSchoolData <- reactive({
    
    if ( input$school == 'All Schools' ) 
      return(dataForCharts)

     dataForCharts %>%
        filter(School %in% input$school) 
  })
  

  observe(
    updateSelectizeInput(session, 
                         inputId = 'items', 
                         choices = if ( input$school == 'All Schools' ) dataForCharts$Title  else  selectedSchoolData()$Title 
    )
  )
  
  
  
  itemsData <- reactive({
    
    req(input$items)

    isolate(selectedSchoolData()[selectedSchoolData()$Title %in% input$items, ])
    
    })
  
  
  
  selectedheatmap <- reactive({
    
    req(input$school)
    
    isolate(ifelse(input$school == 'All Schools',
                   hm <- heatmap_all,
                   ifelse(input$school == 'ARTS',
                          hm <- heatmap_arts,
                          ifelse(input$school == 'BIZ',
                                 hm <- heatmap_biz,
                                 ifelse(input$school == 'CHEM',
                                        hm <- heatmap_chem,
                                        ifelse(input$school == 'ELEC',
                                               hm <- heatmap_elec,
                                               ifelse(input$school == 'ENG',
                                                      hm <- heatmp_eng,
                                                      hm <- heatmap_sci)))))))
    hm
    
  })
  

  output$chart <- renderChart({
    
    validate(
      need(!is.null(itemsData()), paste0("Please select some items from ", input$school))
    )
    
    dataC <- itemsData() 
    
    datatomelt <- dataC %>%
      mutate(id = Title10) %>%
      select(-DOI, 
             -Title, -Title10, -Title20,
             -Journal, -Journal10,
             -School,
             -keys, -Field, -Year, -Altmetric.com_URL,
             -OA, -X_best_open_url, -isoadoi, -Citations2017, -Mendeley2017, -Twitter2017,
             -twitter_series, -mendeley_series, -citations_series)
    
    dataM <- reshape2::melt(datatomelt, id.vars = "id")
    nplot <- nPlot(value ~ id, data = dataM, 
                   group = "variable", type = "multiBarChart")
    nplot$set(dom="chart")
    return(nplot)
    
  })
  
  
  
  ggvisdata <- reactive({
    
    show_title <- function(x=NULL) {
      if(is.null(x)) return(NULL)
      key <- x["keys"][[1]]
      selectedSchoolData()$Title[key]
    } 
    
    xvar_name <- input$xc 
    yvar_name <- input$yc
    
    xscale <- input$scale
    
    xc <- prop("x", as.symbol(input$xc))
    yc <- prop("y", as.symbol(input$yc))
    
    df <- selectedSchoolData()
    
    df$keys <- seq_len(nrow(df))
    
    df %>%
      ggvis(x = xc, 
            y = yc, 
            key := ~keys, 
            fill = ~School, 
            stroke = ~isoadoi,
            stroke := "gold",
            strokeWidth := ~isoadoi*3,
            opacity := 0.80,
            size.hover := 200) %>%
      layer_points() %>%
      add_axis("x", 
               title = xvar_name,
               title_offset = 50,
               tick_padding = 7,
               offset = 10,
               properties = axis_props(
                 labels=list(
                   angle = 50, 
                   fontSize = 10))) %>%
      add_axis("y", 
               title = yvar_name, 
               title_offset = 50) %>% 
      set_options(width = "100%", 
                  height = "600px",
                  renderer = "canvas") %>%
      add_tooltip(show_title) %>%
      scale_numeric(property = "x", 
                    trans = xscale,
                    expand = 0)
    
    
  })
  
  ggvisdata %>% bind_shiny("gv")
  

  output$nrofpubl <- renderValueBox({
    valueBox(
      "Publications", 
      ifelse(input$school == 'All Schools', 
             nrow_uniquedois,
             nrow(selectedSchoolData()[!duplicated(selectedSchoolData()[,1]), ])), 
      icon = icon("calculator"),
      color = "orange"
    )
  })
  
  output$nrofitemswithmetrics <- renderValueBox({
    valueBox(
      "With altmetrics", 
      ifelse(input$school == 'All Schools',
             with_altmetrics_uniquedois,
             paste0(nrow(selectedSchoolData()[!is.na(selectedSchoolData()$AltmetricScore) &
                                                !duplicated(selectedSchoolData()[,1]), ]
                         ),
                    " (",
                    floor(nrow(selectedSchoolData()[!is.na(selectedSchoolData()$AltmetricScore) &
                                                      !duplicated(selectedSchoolData()[,1]), ]
                               ) / nrow(selectedSchoolData()[!duplicated(selectedSchoolData()[,1]), ]
                                        ) * 100),
                    "% )")), 
      icon = icon("calculator"),
      color = "yellow"
    )
  })
  
  output$nrofoadoi <- renderValueBox({
    valueBox(
      "With OA full text",
      ifelse(input$school == "All Schools",
             with_oadoi_uniquedois,
             paste0(nrow(selectedSchoolData()[!is.na(selectedSchoolData()$X_best_open_url) &
                                                !duplicated(selectedSchoolData()[,1]), ]
             ),
             " (",
             floor(nrow(selectedSchoolData()[!is.na(selectedSchoolData()$X_best_open_url) &
                                               !duplicated(selectedSchoolData()[,1]), ]
             ) / nrow(selectedSchoolData()[!duplicated(selectedSchoolData()[,1]), ]
             ) * 100),
             "% )")), 
      icon = icon("calculator"),
      color = "yellow"
             )
  })
  
  output$maxaltmetrics <- renderValueBox({
    valueBox(
      "Top Altmetric", 
      max(selectedSchoolData()$AltmetricScore, na.rm=T), 
      icon = icon("spinner"),
      color = "green",
      href = selectedSchoolData()[which.max(selectedSchoolData()$AltmetricScore), "Altmetric.com_URL"]
    )
  })
  
  output$maxmendeley <- renderValueBox({
    valueBox(
      "Top Mendeley", 
      max(selectedSchoolData()$Mendeley, na.rm=T), 
      icon = icon("spinner"),
      color = "teal",
      href = selectedSchoolData()[which.max(selectedSchoolData()$Mendeley), "Altmetric.com_URL"]
    )
  })

  output$maxtwitter <- renderValueBox({
    valueBox(
      "Top Twitter", 
      max(selectedSchoolData()$Twitter, na.rm=T), 
      icon = icon("twitter"),
      color = "light-blue",
      href = selectedSchoolData()[which.max(selectedSchoolData()$Twitter), "Altmetric.com_URL"]
    )
  })
  

  
  makedata <- reactive({
    
    totable <- selectedSchoolData() %>%
      select(-keys, -Title10, -Title20, -Journal10, -isoadoi) 
    
    totable <- totable[!duplicated(totable[,1]), ]
    
    for (i in 1:nrow(totable)) {
      url <- ifelse(!is.na(totable$Altmetric.com_URL[i]), 
                    "Altmetric",
                    paste0("http://dx.doi.org/", totable[i, "DOI"]))
      urlstring <- ifelse(substr(url, 1, 4) == "http",
                          paste0("<a href='", url, "'>DOI</a>"),
                          paste0("<a href='", totable[i, c("Altmetric.com_URL")], "'>", url, "</a>"))
      totable[i, c("Altmetric.com_URL")] <- urlstring
      
      url2 <- ifelse(!is.na(totable$X_best_open_url[i]), 
                     "OAFulltext",
                      "N/A")
      urlstring2 <- ifelse(substr(url2, 1, 3) == "N/A",
                           "N/A",
                           paste0("<a href='", totable[i, c("X_best_open_url")], "'>", url2, "</a>"))
      totable[i, c("X_best_open_url")] <- urlstring2
    }
    
    totable <- totable %>%
      mutate(Link = Altmetric.com_URL) %>%
      mutate(OAFulltext = X_best_open_url) %>% 
      select(School, DOI, Title, Link, OAFulltext, Field, Year, Journal, NrOfAuthors, AltmetricScore, Citations, Citations2017, CitesChange, CitesSeries,
           Mendeley, Mendeley2017, MendeleyChange, MendeleySeries, Twitter, Twitter2017, TweetsChange, TweetsSeries, GooglePlus, RedditUsers, 
           Facebook, Blogs, YouTubeChannels, SinaWeiboUsers,
           CiteULike, DeliciousUsers, AnyTypeOfPosts,
           ForumUsers, StackExchange, ResearchHighlightPlatforms, NewsOutlets)
    
    totable
 
})
  
  
  output$datatable <- DT::renderDataTable({
    
    dat <- datatable(makedata(), 
                     escape = FALSE, 
                     rownames = FALSE, 
                     extensions = c('Buttons','FixedColumns'),
                     options = list(pageLength = 25,
                                    dom = 'Bfrtip', 
                                    buttons = I('colvis'), 
                                    scrollX = T, 
                                    columnDefs = list(list(targets = c(13,17,21), 
                                                           render = JS("function(data, type, full){ return '<span class=sparkSamples>' + data + '</span>' }")),
                                                      list(targets = 2,
                                                           render = JS(
                                                          "function(data, type, row, meta) {",
                                                          "return type === 'display' && data.length > 10 ?",
                                                          "'<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
                                                          "}")
                                                      )),
                                    fnDrawCallback = cb,
                                    dom = 't',
                                    fixedColumns = list(leftColumns = 3))) %>%
      formatStyle('Citations2017', 'CitesChange',  
                  color = styleInterval(c(-1, 0, 1), c('black', 'black', 'black', 'black')),
                  backgroundColor = styleInterval(c(-1, 0, 1), c('#fc8d59', '#ffffbf', '#91cf60', '#91cf60'))) %>% 
      formatStyle('Twitter2017', 'TweetsChange',  
                  color = styleInterval(c(-1, 0, 1), c('black', 'black', 'black', 'black')),
                  backgroundColor = styleInterval(c(-1, 0, 1), c('#fc8d59', '#ffffbf', '#91cf60', '#91cf60'))) %>% 
      formatStyle('Mendeley2017', 'MendeleyChange',  
                  color = styleInterval(c(-1, 0, 1), c('black', 'black', 'black', 'black')),
                  backgroundColor = styleInterval(c(-1, 0, 1), c('#fc8d59', '#ffffbf', '#91cf60', '#91cf60')))
    
    return(dat)
    
  })
  
  
  output$fields <- renderPlotly({
      
      # https://github.com/ropensci/plotly/issues/329
      plot_ly(z = data.matrix(selectedheatmap()), 
              x = colnames(selectedheatmap()), 
              y = row.names(selectedheatmap()), 
              type = "heatmap") %>%
        layout(xaxis = ax,
               yaxis = ay,
               showlegend = FALSE,
               autosize = F, 
               width = w, 
               height = h, 
               margin = m)
      })
  
}



server <- function(input, output, session) {
  

    #---------------------> PORT AND SHIP ---------------------------->

    observeEvent(input$ship_type, {
        
        x <- data %>%
            filter(ship_type == input$ship_type) %>% 
            distinct(FLAG,SHIPNAME) %>%
            mutate(flag_img = paste0('<i class="',tolower(FLAG),' flag"></i>'))

        update_dropdown_input(
            
            session,
            input_id = 'ship',
            choices = paste(x$flag_img,x$SHIPNAME),
            choices_value = x$SHIPNAME,
            value = x[1,2]
  
        )

    }, ignoreInit = FALSE, ignoreNULL = TRUE)
  

    #-------------------------> DATA --------------------------------->
  
  
  r <- reactiveValues(df = NULL)
  
  observeEvent(input$ship, {
    
    datos <- (data) %>% 
      filter(SHIPNAME == input$ship & is_parked == 0) %>%
      filter(date == max(date)) %>%
      arrange(DATETIME)

    r$df <- datos
    
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  

    #----------------------> MAP ------------------------------------->

  
    output$map_marker <- renderLeaflet({

      datos <- r$df
      
      validate(
        need(nrow(datos) != 0, "No data to show")
      )
      
      datos.df <- split(datos, datos[,22])
      
      
      datos_se <- datos %>% 
        filter(row_number()==1 | row_number()==n())%>%
        mutate(flag_img = paste0('<i class="',tolower(FLAG),' flag"></i>'))
      datos_se$startend <- ''
      datos_se$startend[1] <- '<a class="ui teal label">Start</a>'
      datos_se$startend[2] <- '<a class="ui red label">End</a>'

      
      l <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% addTiles()

        names(datos.df) %>%
            purrr::walk( function(df) {
              
              l <<- l %>%
                addPolylines(
                  data=datos.df[[df]],
                  lng=~LON, 
                  lat=~LAT,
                  group = df,
                  color = 'steelblue',
                  
                  weight = 5,
                  fillOpacity = 0.5, 
                  dashArray = TRUE, 
                  smoothFactor = 0.5
                  
                  )
              })

        
        l %>%
          
          addLayersControl(
            
            overlayGroups = names(datos.df),
            options = layersControlOptions(collapsed = FALSE),
            position = "bottomleft"
            
            )%>%
          
          addPopups(
            
            data=datos_se,
            lng=~LON, 
            lat=~LAT,
            
            popup = paste(sep = '</br>',
                          paste('<b><a>',datos_se$SHIPNAME,'</a></b>',datos_se$flag_img),
                          paste(datos_se$startend,':',datos_se$DATETIME),
                          paste('<b>Port</b>',":",datos_se$port),
                          paste('<b>Destination</b>',":",datos_se$DESTINATION)
                          ),
            
            options = popupOptions(closeButton = FALSE)
            
            ) %>%
          
          addProviderTiles(
            
            providers$CartoDB.DarkMatter
            
            ) %>%
          
          addMeasure()

    })
    
    
    #-----------------------> GAUGE --------------------------------->
    
    
    output$speed_gauge <- renderPlotly({
      
      datos <- r$df
      
      validate(
        need(nrow(datos) != 0, 'No data to show')
      )
      
        x <- mean(datos$SPEED)
        
        
        fig <- plot_ly(
            domain = list(x = c(0, 1), y = c(0, 1)),
            value = x,
            title = list(text = "Average speed"),
            type = "indicator",
            mode = "gauge+number") 
        fig <- fig %>%
            layout(margin = list(l=20,r=30))
        
        fig

    })

    
    #-------------------------> HIST ------------------------------>
    
    output$plot_hc <- renderHighchart({

      
      datos <- r$df
        
         validate(
           need(nrow(datos) != 0, 'No data to show')
         )
        
        hchart(datos$SPEED)%>% 
          hc_title(text = "Speed of ship",
                   margin = 20, align = "left") %>% 
          hc_credits(enabled = TRUE, 
                     text = "Appsilon") %>% 
          hc_exporting(enabled = TRUE) %>% 
          hc_legend(enabled = FALSE)

    })

    
    #----------------------> CARD 1 ------------------------------>

    output$card1 <- renderUI({
      
      datos <- r$df
      
      tagList(
        
        card(
          
          div(class = "content", 
              div(class = "header", h1(datos$SHIPNAME[1])), 
              div(class = "meta", a(class = "ui tag label", "Ship Name")), 
              div(class = "description", "Maritime Vessel Name.")
              
              )
          ))
      
    })
    

    #----------------------> CARD 2 ------------------------------>

    
    output$card2 <- renderUI({

      
      datos <- r$df
      
      tagList(
        
        card(
          
          div(class = "content", 
              div(class = "header", h1(datos$ship_type[1])), 
              div(class = "meta", a(class = "ui tag label", "Ship Type")), 
              div(class = "description", "Types of ships.")
              
          )
        ))

    })

    #----------------------> CARD 3 ------------------------------>

    output$card3 <- renderUI({
      
      datos <- r$df
      

      tagList(
        
        card(
          
          div(class = "content", 
              div(class = "header", h1(datos$FLAG[1], tags$i(class = paste(tolower(datos$FLAG[1]),"flag")))), 
              div(class = "meta", a(class = "ui tag label", "Ship Flag")), 
              div(class = "description", " The flag state of a merchant vessel.")
              
          )
        ))

    })
    

    #----------------------> CARD 4 ------------------------------>

    
    output$card4 <- renderUI({
      
      datos <- r$df
      
      
      tagList(
        
        card(
          
          div(class = "content", 
              div(class = "header", h1(datos$SHIP_ID[1])), 
              div(class = "meta", a(class = "ui teal tag label", "Ship ID")), 
              div(class = "description", "The ID in the database.")
              
          )
        ))

    })
    
    
    #--------------------> ABSOLUTE PANEL ------------------------->

    output$panel <- renderUI({
      
      datos <- r$df
      
      
      validate(
        need(nrow(datos) != 0, "No data to show")
      )
      
      
      datos_se <- datos %>% 
        filter(row_number()==1 | row_number()==n())
      
      x <- round(difftime(datos_se$DATETIME[2],datos_se$DATETIME[1], units = 'hours'),2)
      y <- round(distm(datos_se$LON, datos_se$LAT,fun = distHaversine)[,1]/1000, 2)
      z <- round(distm(datos_se$LON, datos_se$LAT,fun = distHaversine)[,1]/1609, 2)
      

    
      
      tagList(
        
        wellPanel(
          HTML(
            paste0(
              
              '<h2 class="ui teal header">Stats</h2>',
              '<h3 class="ui teal header">Time: ',x,' hours</h3>',
              '<h3 class="ui teal header">Dist: ',y,' km</h3>',
              '<h3 class="ui teal header">Dist: ',z,' Miles</h3>'
              
              )
            ),

        ))
      
    })

}

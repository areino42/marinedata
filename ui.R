

library(shiny)
library(shiny.semantic)

options(rsconnect.max.bundle.size="xxlarge") 

ui <- semanticPage(
    
    theme = "cosmo",

    div(class="ui grid",
        
        div(class="row",
            
            div(class="twelve wide column",
                
                div(class = "ui horizontal divider", 
                    h1("Marine Data Analytics")),

                div(class="ui four column grid",
                    
                    div(class="row",
                        div(class="column",
                            
                            uiOutput("card1")
                            
                        ),
                        div(class="column",
                            
                            uiOutput("card2")
                            
                        ),
                        div(class="column",
                            
                            uiOutput("card3")

                        ),

                        div(class="column",
                            
                            uiOutput("card4")

                        )
                    )

                ),
                
                div(class = "ui horizontal divider", icon("map"), 
                    "Map"),
                
                div(class="row",
                    
                    div(class="ui segment",
                        
                        leafletOutput("map_marker", width = '100%', height=630)
                        
                    )
                    
                )
                
                
                
 
            ),
            
            
            div(class="four wide column",

                div(class="row",
                    
                    div(class="ui segment",
                        
   
                        h2(class = "ui header", icon("settings"), 
                           div(class = "content", "Control Panel", 
                               div(class = "sub header", "Manage preferences"))),
                        
                        br(),
                        
                        div(class = "ui horizontal divider", icon("ship"), 
                            "SHIP TYPE"),
                        
                        div(class = "description", "Please select the type of ship."),
                        br(),
                        
                        dropdown_input(
                          
                          input_id = "ship_type", 
                          choices = df$ship_type,
                          choices_value = df$ship_type,
                          default_text = "Select",
                          value = df[1,1],
                          type = "selection fluid"
                          
                          ),

                        br(),
                        
                        div(class = "ui horizontal divider", icon("ship"), 
                            "SHIP NAME"),
                        
                        
                        div(class = "description", "Please select the ship name."),
                        br(),
                        
                        dropdown_input(
                          
                          input_id = "ship", 
                          choices = c(''),
                          choices_value = c(''), 
                          default_text = "Select",
                          type = "selection fluid"
                          
                        ),
                        
                        div(class = "ui horizontal divider", icon("dashboard"), 
                            "SPEED"),

                        
                        div(class="ui one column stackable center aligned page grid",
                            
                            plotlyOutput("speed_gauge", width = "100%", height=250)
                            
                            ),

                        highchartOutput("plot_hc", width = "100%", height=250)

                    ))

                ),
            
            absolutePanel(
              
              bottom = 400, 
              left = 50, 
              width = 300,
              draggable = FALSE,
              htmlOutput('panel')

              
              )

        ))

)



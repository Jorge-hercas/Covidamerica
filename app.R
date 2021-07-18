
library(dplyr)
library(spdplyr)
library(tidyr)
library(leaflet)
library(readxl)
library(scales)
library(echarts4r)
library(shiny)
library(bslib)
datoscovid <- read_excel("datacovid.xlsx")
coordenadas <- read_excel("world_countrys.xlsx")

datoscov1 <- datoscovid |> 
    left_join(coordenadas, by = c("location" = "name")) 

theme <- bs_theme(
    bg = "#000000", fg = "#B8BCC2",
    "input-border-color" = "#a6a6a6"
)






ui <- bootstrapPage(
    absolutePanel(
        top = 10, left = 50, style = "z-index:500; text-align: right;",
        tags$h2("El COVID en América")
    ),
    theme = theme,
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 10, right = 10, 
                  fluidRow(column(width = 5, dateInput("fecha", "Fecha a graficar (mapa):", width = 180,value = "2021-02-12") ),
                           selectInput("var", "Histórico del país:", width = 180,
                                       c("Antigua y Barbuda" = "Antigua and Barbuda",
                                         "Argentina" = "Argentina",
                                         "Bahamas" = "Bahamas",
                                         "Barbados" = "Barbados",
                                         "Belice" = "Belize",
                                         "Bolivia" = "Bolivia",
                                         "Brasil" = "Brazil",
                                         "Canada" = "Canada",
                                         "Chile" = "Chile",
                                         "Colombia" = "Colombia",
                                         "Costa Rica" = "Costa Rica",
                                         "Cuba" = "Cuba",
                                         "Dominica" = "Dominica",
                                         "Republica Dominicana" = "Dominican Republic",
                                         "Ecuador" = "Ecuador",
                                         "El Salvador" = "El Salvador",
                                         "Groenlandia" = "Greenland",
                                         "Grenada" = "Grenada",
                                         "Guatemala" = "Guatemala",
                                         "Guyana" = "Guyana",
                                         "Haiti" = "Haiti",
                                         "Honduras" = "Honduras",
                                         "Jamaica" = "Jamaica",
                                         "Mexico" = "Mexico",
                                         "Nicaragua" = "Nicaragua",
                                         "Panama" = "Panama",
                                         "Paraguay" = "Paraguay",
                                         "Peru" = "Peru",
                                         "Trinidad y Tobago" = "Trinidad and Tobago",
                                         "Estados Unidos" = "United States",
                                         "Uruguay" = "Uruguay",
                                         "Venezuela" = "Venezuela"
                                       ), selected = "Mexico" )),
                  echarts4rOutput("graf", height = '350px', width = '550px'),
                  echarts4rOutput("graf1", height = '350px', width = '550px')
    )
)




server <- function(input, output){
    
    datos <- reactive({
        datoscov1 |> 
            filter(date == input$fecha)
        
    })
    
    output$map <- renderLeaflet({
        mytext <- paste(
            "Fecha: ", datos()$date, "<br/>", 
            "País: ", datos()$location, "<br/>", 
            "Casos totales: ", comma(datos()$total_cases), "<br/>", 
            "Muertes totales: ", comma(datos()$total_deaths), sep="") %>%
            lapply(htmltools::HTML)
        
        paleta <- colorBin( palette="Reds", domain=datos()$new_cases, na.color="transparent", bins=5)
        
        leaflet() %>%
            addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
                     attribution = paste(
                         "&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors",
                         "&copy; <a href=\"http://cartodb.com/attributions\">CartoDB</a>"
                     ))  %>%
            setView( lat=23, lng=-50 , zoom=3) %>%
            addCircleMarkers(~longitude, ~latitude, 
                             data = datos(),
                             fillColor = ~paleta(new_cases), fillOpacity = 0.7, color="white", radius=~new_cases^(1/3), stroke=FALSE,
                             label = mytext,
                             labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
            ) %>%
            addLegend( pal=paleta, values=~new_cases, opacity=0.9, title = "Magnitude", position = "bottomleft", data = datos() )
        
    })
    
    
    dato <- reactive({
        
        datoscovid |> 
            filter(location == input$var)
    })
    
    
    output$graf <- renderEcharts4r({
        
        dato() |> 
            e_charts(date) |> 
            e_line(new_cases, name = "casos nuevos", symbol = "none") |> 
            e_line(new_cases_smoothed, name = "Media movil", symbol = "none") |> 
            e_tooltip(trigger = "axis") |> 
            e_theme("inspired") |> 
            e_title("Evolución del COVID") |> 
            e_legend(right = 50) |> 
            e_color(color = c("#853c3c","#3c6885"))
        
        
    })
    
    output$graf1 <- renderEcharts4r({
        
        datos() |>   
            arrange(new_cases ) |> 
            top_n(5, new_cases) |> 
            e_charts(location) |> 
            e_bar(new_cases, name = "Casos nuevos") |> 
            e_flip_coords() |> 
            e_tooltip(trigger = "axis") |> 
            e_color(color = "#853c3c") |> 
            e_theme("inspired") |> 
            e_title(  "Top 5 países" )
        
        
    })
    
    
}

shinyApp(ui, server)





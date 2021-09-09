#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(htmltools)
library(rgdal)
library(sf)
library(ggplot2)
library(highcharter)
library(dplyr)
library(flexdashboard)

###

ui <- shinyUI(
    navbarPage("Exploring the association between Covid-19 Incidence and Demographics", 
               tabPanel(
                   "London Map",
                   sidebarLayout(
                       sidebarPanel(
                           tags$b("About this Study", tags$br(), tags$br()),
                           tags$p("This dashboard presents the findings from a spatial analysis of Covid-19 incidence
                                  data, within Greater London", 
                                  tags$br(), tags$br(),
                                  "The study looked at the data of Covid-19 incidence, as well as demographic data from across London. 
                                  Models were built to find out which demogrpahic measures were linked to Covid-19.",
                                  tags$br(), tags$br()),
                           tags$p("Use the drop down to select a demographic measure to map and see how it varies across the capital.", tags$br(), tags$br()),
                           selectInput("metric",
                                       "Metric",
                                       c("Incidence Rate", "Relative Risk", "Aged", "Lone Parent", "Race", "Belief", "Income", "Health", "Education")),
                           tags$b("Data Sources", tags$br(), tags$br()),
                           tags$p("Data for this study was downloaded from the following sources:"),
                           fluidRow(column(12, tags$b(uiOutput("datasource1")))),
                           fluidRow(column(12, tags$b(uiOutput("datasource2")))),
                           fluidRow(column(12, tags$b(uiOutput("datasource3")))),
                       ),
                       mainPanel(                            
                           fluidRow(
                           column(12, h3("Greater London Covid-19 Summary Statistics"))
                                    ),
                           fluidRow(column(4, h4("Total Cases")),
                                    column(4, h4("MSOA with highest rate")),
                                    column(4, h4("MSOA with lowest rate")),
                           ),
                           
                           fluidRow(
                               column(4, wellPanel(div(textOutput("london"),style = "font-size:125%"))),
                               column(4, wellPanel(div(textOutput("london2"),style = "font-size:125%"))),
                               column(4, wellPanel(div(textOutput("london3"),style = "font-size:125%"))),

                           ),
                           fluidRow(
                               column(12, h3(textOutput("maptitles")))
                           ),
                           leafletOutput("mainmap", height="80vh")  )
                   )
               ),
               tabPanel(
                   "Explore your Borough",
                   sidebarLayout(
                       sidebarPanel(
                           tags$b("Use this tab to explore one of the London Boroughs in more detail.",
                                  tags$br(), tags$br(),
                                  "Select a borough using the drop down menu and then select which demographic measure 
                                  you would like to explore further", 
                                  tags$br(), tags$br()),
                           selectInput("borough",
                                       "London Borough",
                                       c("City of London","Barking and Dagenham", "Barnet", 
                                         "Bexley","Brent", "Bromley", "Camden", "Croydon", 
                                         "Ealing", "Enfield", "Greenwich", "Hackney", 
                                         "Hammersmith and Fulham", "Haringey", "Harrow", 
                                         "Havering", "Hillingdon", "Hounslow", "Islington",
                                         "Kensington and Chelsea", "Kingston upon Thames",
                                         "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge",
                                         "Richmond upon Thames", "Southwark", "Sutton",
                                         "Tower Hamlets", "Waltham Forest", "Wandsworth","Westminster")),
                           selectInput("boroughmetric",
                                       "Demographic",
                                       c("Aged", "Lone Parent", "Race", "Belief", "Income", "Health", "Education")),
                           fluidRow(
                               column(12, highchartOutput("histogramleft"))
                           ),
                       ),
                       mainPanel(fluidRow(
                                        column(12, h3(textOutput("boroughmaptitle")))
                                    ),
                                  leafletOutput("boroughmap"),
                                  br(),
                                  highchartOutput("histogramtest"))
                   )
               ),
               tabPanel(
                   "Counterfactuals",
                   sidebarLayout(
                       sidebarPanel(
                           tags$b("Both education level and income level have been shown to be associated with incidence rates
                                             of Covid-19. This study has shown that that areas with higher proportions of the population 
                                             educated to degree level is linked to lower rates of Covid-19. The study has also shown that
                                             wealthier areas i.e. those with a higher mean income are linked with lower rates of Covid-19.",
                                  tags$br(), tags$br(),
                                    "Use the sliders to see what would happen to the incidence rates of Covid-19 
                                  when the level of education and/or income are changed...", tags$br(), tags$br()),
                           
                           sliderInput("educf",
                                       "Change Education (+/- pp):",
                                       min = -30,
                                       max = 30,
                                       value = 0),
                           tags$p("Note: This input corresponds to the percentage point change in the population 
                                        that is educated to degree level. "),
                           sliderInput("incomecf",
                                       "Change Income (+/- \u00A3k):",
                                       min = -30,
                                       max = 30,
                                       value = 0),
                           tags$p("Note: This input corresponds to the change in the average household income in \u00A3k"),
                       ),
                       mainPanel(
                           fluidRow(
                               column(12, h4("Proportion of population currently educated to degree level: 37.4%"))
                            ),
                           fluidRow(
                               column(12, h4("Current existing mean household income across Greater London: \u00A345,800"))
                           ),
                           fluidRow(
                               column(4, h4("Proposed Education Level")),
                               column(4, h4("Proposed Mean Household Income")),
                               column(4, h4("Change in Cases"))
                             ),
                           
                           fluidRow(
                                column(4, wellPanel(div(textOutput("cf_1"),style = "font-size:125%"))),
                                column(4, wellPanel(div(textOutput("cf_2"),style = "font-size:125%"))),
                                column(4, wellPanel(div(textOutput("cf_3"),style = "font-size:125%")))
                            ),
                           leafletOutput("cfmap", height="80vh"))
                   )
                )
    )
)

###
# Define server logic required to draw a histogram
server <- function(input, output) {
    #C:\Users\anama\Documents\Dissertation\shiny\example\layout_tester
    #read_in_tester <- readOGR("C:/Users/anama/Documents/Dissertation/shiny/example/layout_tester/tester3.shp", verbose = FALSE)
    read_in_tester <- readOGR("tester3.shp", verbose = FALSE)
    #colnames_for_app <- read.csv("C:/Users/anama/Documents/Dissertation/shiny/example/layout_tester/colnames_for_app.csv")
    colnames_for_app <- read.csv("colnames_for_app.csv")
    colnames_for_app_vector <- colnames_for_app$x
    shortened_names <- c("Aged", "Lone Parent", "Race", "Belief", "Income", "Health", "Education")
    full_titles <- c("Percentage of Population which are over 65 years (%)", 
                     "Percentage of Households in Population which are Lone Parent Households (%)", 
                     "Percentage of Population of White Ethnicity(%)", 
                     "Percentage of Population which are Muslim (%)", 
                     "Mean Household Income (\u00A3000s)", 
                     "Percentage of Population who Self-assess as being in 'Very Good Health' (%)", 
                     "Percentage of Population with a Degree (%)")
    colnames_for_app_vector[26:32] <- shortened_names
    colnames(read_in_tester@data) <- colnames_for_app_vector
    tester_sf <- st_as_sf(read_in_tester)
    tester_sf <- st_transform(tester_sf, 4326)
    
    titles_df <- as.data.frame(shortened_names)
    titles_df <- cbind(titles_df, full_titles)
    colnames(titles_df) <- c("short", "long")
    
    beta_1 <- -0.005
    beta_2 <- -0.003
    
    input_name <- c("Incidence Rate", "Relative Risk",  "Aged", "Lone Parent", "Race", "Belief", "Income", "Health", "Education")
    map_titles <- as.data.frame(c("Covid-19 Incidence Rate", "Covid-19 Relative Risk", 
                                  "Age Map: Percentage aged 65 and over in the population", 
                                  "Lone Parent Map: Percentage of households which are lone parent households", 
                                  "Ethnicity Map: Percentage of population which are white", 
                                  "Belief Map: Percentage of population of Muslim faith", 
                                  "Income Map: Mean household income", 
                                  "Health Map: Percentage of population which consider themselves 'in very good health'", 
                                  "Education Map: Percentage of population educated to degree level"))
    graph_titles <- c("Incidence Rate",
                      "Relative Risk",
                      "Percentage of Population which are over 65 years (%)", 
                      "Percentage of Households in Population which are Lone Parent Households (%)", 
                      "Percentage of Population of White Ethnicity(%)", 
                      "Percentage of Population which are Muslim (%)", 
                      "Mean Household Income (\u00A3000s)", 
                      "Percentage of Population who Self-assess as being in 'Very Good Health' (%)", 
                      "Percentage of Population with a Degree (%)")
    change_title_df <- cbind(input_name, map_titles, graph_titles)
    names(change_title_df) <- c("input_name", "map_title", "graph_title")
    
    output$maptitle1 <- renderText({ "This is the map title" })

    output$boroughmap <- renderLeaflet({
        
        selection <- input$borough
        if(input$borough == "Incidence Rate") {selection <- "SIR"}
        if(input$borough == "Relative Risk") {selection <- "RR"}
        
        pal <- colorNumeric(palette = "YlOrRd", domain = read_in_tester$RR)
        
        read_in_tester_lb <- read_in_tester@data[read_in_tester@data$LAD11NM == selection, ]
        
        labels_app <- sprintf("<strong> %s </strong> <br/>
                  <strong> Observed: </strong> %s <br/> 
                  <strong> Expected: </strong> %s <br/>
                  <strong> Mean Income: </strong> \u00A3%sk <br/>
                  <strong> IR: </strong> %s <br/> 
                  <strong> RR: </strong> %s (%s, %s)
                  ",
                              read_in_tester_lb$areaName, read_in_tester_lb$Y, round(read_in_tester_lb$E, 0), round(read_in_tester_lb$Income, 0),
                              round(read_in_tester_lb$SIR, 2), round(read_in_tester_lb$RR, 2),
                              round(read_in_tester_lb$LL, 2), round(read_in_tester_lb$UL, 2)) %>% 
            lapply(htmltools::HTML)
        
        leaflet(tester_sf[tester_sf$LAD11NM == selection, ]) %>%
            leaflet::addProviderTiles("CartoDB.Positron") %>%
            leaflet::addPolygons(
                color = "grey", weight = 1, fillColor = ~ pal(RR),
                fillOpacity = 0.5,
                highlightOptions = highlightOptions(weight = 4),
                label = labels_app,
                labelOptions = labelOptions(
                    style =
                        list(
                            "font-weight" = "normal",
                            padding = "3px 8px"
                        ),
                    textsize = "15px", direction = "auto"
                )
            ) %>%
            leaflet::addLegend(
                pal = pal, values = c(min(read_in_tester$RR), max(read_in_tester$RR)), opacity = 0.5, title = "RR", 
                #labFormat = labelFormat(prefix = "$"),
                position = "bottomright"
            )
        
    })
    

    
    output$mainmap <- renderLeaflet({
        

        selection <- input$metric
        if(input$metric == "Incidence Rate"){selection <- "SIR"}
        if(input$metric == "Relative Risk"){selection <- "RR"}
        print(selection)
        
        pal_main <- colorNumeric(palette = "viridis", domain = read_in_tester@data[[selection]])
        
        gbp <- ""
        num_dig <- 0
        
        if (selection %in% c("SIR", "RR")){
            symbols <- ""
            num_dig <- 2}
        else{if(selection == "Income"){
            symbols <- "k"
            gbp <- "\u00A3"}
            else{symbols <- "%"}}
        
        labels_app_demo <- sprintf("<strong> %s </strong> <br/>
                  <strong> Observed: </strong> %s <br/> 
                  <strong> Expected: </strong> %s <br/>
                  <strong> %s: </strong> %s%s%s <br/>
                  <strong> IR: </strong> %s <br/> 
                  <strong> RR: </strong> %s (%s, %s)
                  ",
                                   read_in_tester$areaName, read_in_tester$Y, round(read_in_tester$E, 0), 
                                   input$metric, gbp, round(read_in_tester@data[[selection]], num_dig), symbols,
                                   round(read_in_tester$SIR, 2), round(read_in_tester$RR, 2),
                                   round(read_in_tester$LL, 2), round(read_in_tester$UL, 2)) %>% 
            lapply(htmltools::HTML)
        
        leaflet(tester_sf) %>%
            leaflet::addProviderTiles("CartoDB.Positron") %>%
            leaflet::addPolygons(
                color = "grey", weight = 1, fillColor = ~ pal_main(read_in_tester@data[[selection]]),
                fillOpacity = 0.5,
                highlightOptions = highlightOptions(weight = 4),
                label = labels_app_demo,
                labelOptions = labelOptions(
                    style =
                        list(
                            "font-weight" = "normal",
                            padding = "3px 8px"
                        ),
                    textsize = "15px", direction = "auto"
                )
            ) %>%
            leaflet::addLegend(
                pal = pal_main, values = ~read_in_tester@data[[selection]], opacity = 0.5, title = input$metric,
                position = "bottomright"
            )
        
    })
    
    output$distPlot2 <- renderPlot({
        # generate bins based on input$bins from ui.R
        # the dataframe is called 'faithful' and 'waiting' is a column name...
        # so basically, the dataframe containing all the results needs to be here I think...
        # should I have results for all the different sliders?
        
        x    <- read_in_tester@data$RR
        bins <- seq(min(x), max(x), length.out = 50)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "test title")
        
    })
    
    
    output$distPlot3 <- renderPlot({
        
        selection2 <- input$boroughmetric
        print(selection2)
        
        #x    <- read_in_tester@data$Income #[[input$boroughmetric]]
        #test1 <- input$boroughmetric
        #print("test = ", test1)
        #bins <- seq(min(x), max(x), length.out = 50)
        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white', main = titles_df$long[titles_df$short == input$boroughmetric])
        borough_ave <- mean(read_in_tester@data[[input$boroughmetric]][read_in_tester@data$LAD11NM == input$borough])
        ggplot(read_in_tester@data, aes(x = read_in_tester@data[[input$boroughmetric]])) + 
            geom_histogram(binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
            theme_classic() +
            theme(plot.title = element_text(size=20), axis.title = element_text(size = 18), line = element_blank()) +
            ggtitle(titles_df$long[titles_df$short == input$boroughmetric]) +
            xlab("Dose (mg)") + 
            ylab("Count (No. MSOAs)")
            #geom_vline(data=read_in_tester@data, aes(xintercept=mean(read_in_tester@data[[input$boroughmetric]])),
                       #linetype="dashed")
    })
    
    output$histogramtest <-renderHighchart({
        selected <- input$boroughmetric
        # return graph titles when input name is input...
        graph_title <- change_title_df$graph_title[change_title_df$input_name == selected]
        if(input$boroughmetric == "Incidence Rate") {selected <- "SIR"}
        if(input$boroughmetric == "Relative Risk") {selected <- "RR"}
        borough_ave <- mean(read_in_tester@data[[selected]][read_in_tester@data$LAD11NM == input$borough])
        hchart(
            read_in_tester@data[[selected]], 
            color = "#AEC4CE", name = graph_title, bins = 25, 
        ) %>%
            hc_xAxis(
                     plotLines = list(list(
                         value = borough_ave,
                         color = '#FFBE33',
                         width = 2,
                         zIndex = 4,
                         label = list(text = input$borough, #"Borough Average",
                                      style = list( color = '#FFBE33', fontWeight = 'normal'   )
                         )))) %>%
            hc_yAxis(
                title = list(text = "Frequency",
                             style = list(color = "#7B7D7E", size = 22, useHTML = TRUE))) #%>%
            #hc_title(
            #    text = "Histogram of SIR values across all London MSOAs",
            #    margin = 20,
            #    align = "left",
            #    style = list(color = "#7B7D7E", useHTML = TRUE)
            #)
    })
    
    output$histogramleft <-renderHighchart({

        borough_ave <- mean(read_in_tester@data$RR[read_in_tester@data$LAD11NM == input$borough])
        hchart(
            read_in_tester@data$RR, 
            color = "#AEC4CE", name = "Relative Risk", bins = 25, 
        ) %>%
            hc_xAxis(
                plotLines = list(list(
                    value = borough_ave,
                    color = '#FFBE33',
                    width = 2,
                    zIndex = 4,
                    label = list(text = input$borough, #"Borough Average",
                                 style = list( color = '#FFBE33', fontWeight = 'normal'   )
                    )))) %>%
            hc_yAxis(
                title = list(text = "Frequency",
                             style = list(color = "#7B7D7E", size = 22, useHTML = TRUE))) #%>%
        #hc_title(
        #    text = "Histogram of SIR values across all London MSOAs",
        #    margin = 20,
        #    align = "left",
        #    style = list(color = "#7B7D7E", useHTML = TRUE)
        #)
    })
    
    output$histogramtwos <-renderHighchart({
        highchart() %>%
            hc_add_series(read_in_tester@data[[input$boroughmetric]], type = "column", name = "SPY") %>%
            hc_add_series(read_in_tester@data[[input$boroughmetric]], type = "column", name = "EFA") 
            #, 
            #color = "darkslateblue", name = input$boroughmetric,
        #) 
    })
    
    output$GSTotalBalanceLineHc <-renderHighchart({
        highchart() %>% 
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'Trade balance', data = c(1,2,3,4,5,6,7,8,9), color='brown' , marker = list(enabled = F), lineWidth = 3 )
                      )%>%
            hc_xAxis( categories = c(2, 4, 6, 8, 10) ) %>%
            hc_yAxis( title = list(text = "$ million, NZD"),
                      labels = list( format = "${value:,.0f} m"),
                      plotLines = list(
                          list(#label = list(text = "This is a plotLine"),
                              color = "#ff0000",
                              #dashStyle = 'shortDot',
                              width = 2,
                              value = 0 ) )
            ) %>%
            hc_plotOptions(column = list(
                dataLabels = list(enabled = F),
                #stacking = "normal",
                enableMouseTracking = T ) 
            )%>%
            hc_tooltip(table = TRUE,
                       sort = TRUE,
                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                             " {series.name}: ${point.y} m"),
                       headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
            ) %>%
            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
    })
    
    output$cfmap <- renderLeaflet({
        
        pal_main <- colorNumeric(palette = "YlOrRd", domain = c(0.3, 2.5)) 
        
        #cf_df <- read_in_tester@data
        read_in_tester@data$logRR <- log(read_in_tester@data$RR)
        cf_alpha <- input$educf
        cf_gamma <- input$incomecf
        beta_1 <- -0.005
        beta_2 <- -0.003

        read_in_tester@data <- read_in_tester@data %>% mutate(edu_plus_alpha = pmax(pmin((Education + cf_alpha), 100), 0),
                                  income_plus_gamma = pmax((Income + cf_gamma), 10)) %>%
            mutate(alpha_2 = edu_plus_alpha - Education,
                   gamma_2 = income_plus_gamma - Income) %>%
            mutate(RR_cf = exp(logRR + beta_1*alpha_2 + beta_2*gamma_2)) %>%
            mutate(Y_exist = E * RR) %>%
            mutate(Y_cf = E * RR_cf)
        
        total_y <- sum(read_in_tester@data$Y_exist)
        total_y_cf <- sum(read_in_tester@data$Y_cf)
        cases_diff <- total_y_cf - total_y
        print(cases_diff)
        prop_mean_edu <- mean(read_in_tester@data$Education) + cf_alpha
        prop_mean_income <- mean(read_in_tester@data$Income) + cf_gamma
        print(prop_mean_edu)
        print(prop_mean_income)
        
        
        labels_app_demo <- sprintf("<strong> %s </strong> <br/>
                  <strong> Observed: </strong> %s <br/> 
                  <strong> Expected: </strong> %s <br/>
                  <strong> IR: </strong> %s <br/> 
                  <strong> RR_cf: </strong> %s (%s, %s) 
                  ",
                                   read_in_tester@data$areaName, read_in_tester@data$Y, round(read_in_tester@data$E, 0), 
                                   round(read_in_tester@data$SIR, 2), round(read_in_tester@data$RR_cf, 2),
                                   round(read_in_tester@data$LL, 2), round(read_in_tester@data$UL, 2)) %>% 
            lapply(htmltools::HTML)
        
        #print(class(labels_app_demo))
        
        leaflet(tester_sf) %>%
            leaflet::addProviderTiles("CartoDB.Positron") %>%
            leaflet::addPolygons(
                color = "grey", weight = 1, fillColor = ~ pal_main(read_in_tester@data$RR_cf),
                fillOpacity = 0.5,
                highlightOptions = highlightOptions(weight = 4),
                label = labels_app_demo,
                labelOptions = labelOptions(
                    style =
                        list(
                            "font-weight" = "normal",
                            padding = "3px 8px"
                        ),
                    textsize = "15px", direction = "auto"
                )
            ) %>%
            leaflet::addLegend(
                pal = pal_main, values = ~read_in_tester@data$RR, opacity = 0.5, title = "RR_cf",
                position = "bottomright"
            )
        
    })
    

    
    output$calories <- renderValueBox({
        valueBox(
            formatC(100, format="d", big.mark=',')
            ,paste('Top Account:',"jimjams")
            ,icon = icon("stats",lib='glyphicon')
            ,color = "purple")  
    })

    output$box1 <- renderValueBox({
        valueBox(paste0(100, "kcal"), 
                 "Calories", icon = icon("fire"), color = "yellow")
    })
    output$box2 <- renderValueBox({
        valueBox(paste0(100, "kcal"), 
                 "Calories", icon = icon("fire"), color = "yellow")
    })
    
    output$datasources2 <- renderText({ "Data on Covid-19 cases was taken from " })
    
    url1 <- a("www.coronavirus.data.gov.uk", href = "https://coronavirus.data.gov.uk/details/download")
    output$datasource1 <- renderUI({
        tagList("Covid-19 Data:", url1)
    })
    
    url2 <- a("www.data.gov.uk", href = "https://data.gov.uk/dataset/d91901cb-13ee-4d00-906c-1ab36588943b/msoa-atlas")
    output$datasource2 <- renderUI({
        tagList("Demographics Data:", url2)
    })
    
    url3 <- a("www.data.london.gov.uk", href = "https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london")
    output$datasource3 <- renderUI({
        tagList("Shapefiles:", url3)
    })

    output$maptitles <- renderText({
        selected <- input$metric
        # return map titles when input name is input...
        new_title <- change_title_df$map_title[change_title_df$input_name == selected]
        paste(new_title)
        })
    
    output$boroughmaptitle <- renderText({
        selected <- input$borough
        # return map titles when input name is input...
        #new_title <- change_title_df$map_title[change_title_df$input_name == selected]
        paste("Relative Risk in", selected)
    })
    
    
    output$london <- renderText(paste({"702,936"}))
    output$london2 <- renderText(paste({"Olympic Park & Mill Meads"}))
    output$london3 <- renderText(paste({"Notting Hill West"}))
    
    output$test2 <- renderText(paste({"100"}))
    output$cf_1 <- renderText(paste(round(mean(read_in_tester@data$Education + input$educf),1), "%", sep = ""))
    output$cf_2 <- renderText(paste("\u00A3", round(mean(read_in_tester@data$Income + input$incomecf)*1000 ,0), sep = ""))
    
    output$cf_3 <- renderText({
        read_in_tester@data$logRR <- log(read_in_tester@data$RR)
        cf_alpha <- input$educf
        cf_gamma <- input$incomecf

        read_in_tester@data <- read_in_tester@data %>% mutate(edu_plus_alpha = pmax(pmin((Education + cf_alpha), 100), 0),
                                                              income_plus_gamma = pmax((Income + cf_gamma), 10)) %>%
            mutate(alpha_2 = edu_plus_alpha - Education,
                   gamma_2 = income_plus_gamma - Income) %>%
            mutate(RR_cf = exp(logRR + beta_1*alpha_2 + beta_2*gamma_2)) %>%
            mutate(Y_exist = E * RR) %>%
            mutate(Y_cf = E * RR_cf)
        
        total_y <- sum(read_in_tester@data$Y_exist)
        total_y_cf <- sum(read_in_tester@data$Y_cf)
        print(total_y)
        print(total_y_cf)
        cases_diff <- total_y_cf - total_y
        paste(round(cases_diff,0))
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

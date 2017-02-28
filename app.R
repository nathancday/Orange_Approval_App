library(forcats)
library(rvest)
library(plotly)
library(lubridate)
library(tidyverse)
library(magrittr)
library(shiny)
library(shinythemes)
library(ggplot2)

theme_set(ggplot2::theme_classic(base_size = 18) +
              ggplot2::theme(panel.border = element_rect(colour = "grey90", fill=NA, size=1),
                    plot.background = element_rect(fill = "#4e5d6c"),
                    strip.background = element_rect(linetype = "blank", fill = "#3e444c"),
                    text = element_text(color = "white", size = rel(.75)),
                    plot.margin = margin(1,1,1,1, "cm"),
                    axis.text = element_text(color = "white"),
                    axis.ticks = element_line(color = "white"),
                    panel.background = element_rect(fill = "lightgrey"),
                    panel.grid.minor = element_line(colour="white", size=0.5),
                    panel.grid.major = element_line(colour="white", size=0.5),
                    legend.position = "none")
)

# setwd("~/future/President_Approval/Pres_Approval_App/")
#### UI ####
ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel(title = "Red, White and Boo",
                           tags$head(tags$link(rel = "shortcut icon", href = "www/favicon.png"))
                           ),
                fluidRow(column(12,
                         p("A loess look comparing the 🍊's historic approval poll to past Presidents"))
                         ),
                fluidRow(column(12,
                                sidebarLayout(sidebarPanel(width = 3,
                                    h4("Time Scale:"),
                                    checkboxInput("don_bool", "Focus on first 100 days?", T),
                                    checkboxInput("don_predict", "Plot projections for 🍊's first 100 days?", F),
                                    conditionalPanel(condition = "input.don_bool == false",
                                        sliderInput("time_int", "Select Days in Office:", 100, 2922, value = 1461)
                                        ),
                                    br(),
                                    h4("Presidents Included:"),
                                    sliderInput("term_int", "Years of Service", as.Date("1940-01-01"), as.Date("2017-12-31"),
                                                                                       c(as.Date("1940-01-01"), as.Date("2017-12-31")),
                                                                                       timeFormat = "%Y"),
                                        
                                    br(),
                                    checkboxInput("more_bool", "Show more filters"),
                                    conditionalPanel(condition = "input.more_bool == true",
                                        h5("Still working on this, sorry : (")
                                        
                                        # selectInput("term_select", "Select # of terms", c(1,2)),
                                        # checkboxInput("party_bool", "Filter by party:"),
                                        # selectInput("party_select", "Party:", c("Republican", "Democrat"))
                                    )
                                ),
                                mainPanel(width = 9,
                                    tabsetPanel(id = "main_tabs",
                                        tabPanel("", icon = icon("thumbs-up"), value = "approve",
                                                 plotlyOutput("main_approval_plot", height = "400px"),
                                                 br(),
                                                 fluidRow(
                                                     column(9,
                                                           tags$body(p("A second plot by Individual Presidents"))
                                                           )
                                                     ),
                                                 plotlyOutput("second_approval_plot", height = "400px")
                                                 ),
                                        tabPanel("",icon = icon("thumbs-down"), value = "disapprove",
                                                 plotlyOutput("main_disapproval_plot", height = "400px"),
                                                 br(),
                                                 fluidRow(
                                                     column(9,
                                                            tags$body(p("A second plot by Individual Presidents"))
                                                     )
                                                 ),
                                                 plotlyOutput("second_disapproval_plot", height = "400px")
                                                 ),
                                        tabPanel("", icon = icon("question"), value = "unsure",
                                                 plotlyOutput("main_unsure_plot", height = "400px"),
                                                 br(),
                                                 fluidRow(
                                                     column(9,
                                                            tags$body(p("A second plot by Individual Presidents"))
                                                     )
                                                 ),
                                                 plotlyOutput("second_unsure_plot", height = "400px")
                                                 )
                                )
                                )
                )
                )
                ),
                br(),
                fluidRow(column(12,
                        tabsetPanel(
                            tabPanel("Data", icon = icon("folder-open"),
                                     tags$div(class = "info",
                                              tags$h4("Data"),
                                              tags$p("The data displayed here is sourced form UC Santa Barbara's",
                                                     tags$a(href = "http://www.presidency.ucsb.edu/data/popularity.php", "American Presidency Project"), "."),
                                              tags$p("It contains Gallup's approval poll ratings since Harry S. Truman. Polls have increase in their frequency
                                                     since Truman and Barack Obama's preseidency ave 2,000+ recorded obeservations, compared to 260 for George W. Bush.
                                                     Obama's polling obervations have been filterd, by selecting every 10th one, to reduce his impact on the historical averages.")
                                              )
                                              ),
                            tabPanel("Code", icon = icon("github"),
                                     p("Coming soon...")
                                     )
                            )
                            )
                                     
                )
                )

#### Server ####
server <- function(input, output) {
    data <- readRDS("www/data.RDS")
    trump_proj <- readRDS("www/projected_trump.RDS")
    trump <- filter(data, president == "Donald J. Trump")
    data %<>% filter(president != "Donald J. Trump")
    
    in_trump <- reactive({
        if(input$don_predict) {
            return(trump_proj)
        }
        else {
            return(trump)
        }
    })
    
    in_data <- reactive({
        # Don time axis react
        if(input$don_bool) {
            data <- filter(data, days_in_office <= 100)
            }
        
        data %<>% filter(days_in_office <= input$time_int)
        
        data %<>% filter(as.Date(end_date) > input$term_int[1],
                         as.Date(end_date) < input$term_int[2])
        
        return(data)
     })
    
    proj <- readRDS("www/proj.RDS")
    proj %<>% filter(president != "Donald J. Trump")
    in_proj <- reactive({
        # Don time axis react
        if(input$don_bool) {
            proj <- filter(proj, days_in_office <= 100)
        }
        
        proj %<>% filter(days_in_office <= input$time_int)
        
        proj %<>% filter(as.Date(end_date) > input$term_int[1],
                         as.Date(end_date) < input$term_int[2])
        
        return(proj)
    })
    
    output$main_approval_plot <- renderPlotly({
        p <- ggplot(in_data(), aes(x = days_in_office, y = approval, color = party, text = end_date)) +
            stat_smooth(se = F, size = 3, n = max(trump$days_in_office)) +
            stat_smooth(data = in_trump(), size = 3, se = F) +
            scale_color_manual(values = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange")) +
            ggplot2::theme(legend.position = "none") +
            labs(x = "Days in Office",
                 y = "Approval Rating")
        
        ggplotly(p, originalData = T, tooltip = c("approval", "party"))
    })
    
    output$second_approval_plot <- renderPlotly({
        
        p <- ggplot(in_proj(), aes(x = days_in_office, y = approval, group = president, color = party)) +
            stat_smooth(se = F, aes(linetype = line_type)) +
            stat_smooth(data = in_trump(), se = F, size = 1, color = "orange") +
            scale_color_manual(values = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange")) +
            ggplot2::theme(legend.position = "none") +
            labs(x = "Days in Office",
                 y = "Approval Rating")
        
        ggplotly(p,originalData = T, tooltip = c("approval", "president", "party"), layer = 1)
    })
    
    output$main_disapproval_plot <- renderPlotly({
        p <- ggplot(in_data(), aes(x = days_in_office, y = disapproval, color = party)) +
            stat_smooth(se = F, size = 3) +
            stat_smooth(data = in_trump(), size = 3, se = F) +
            scale_color_manual(values = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange")) +
            ggplot2::theme(legend.position = "none") +
            labs(x = "Days in Office",
                 y = "Disapproval Rating")
        ggplotly(p,originalData = T, tooltip = c("disapproval", "party"), layer = 1)
    })
    
    output$second_disapproval_plot <- renderPlotly({
        p <- ggplot(in_proj(), aes(x = days_in_office, y = disapproval, group = president, color = party)) +
            stat_smooth(se = F, aes(linetype = line_type)) +
            stat_smooth(data = in_trump(), se = F, size = 1, color = "orange") +
            scale_color_manual(values = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange")) +
            ggplot2::theme(legend.position = "none") +
            labs(x = "Days in Office",
                 y = "Disapproval Rating")
        ggplotly(p,originalData = T, tooltip = c("disapproval", "president", "party"), layer = 1)
    })
    
    output$main_unsure_plot <- renderPlotly({
        p <- ggplot(in_data(), aes(x = days_in_office, y = unsure, color = party)) +
            stat_smooth(se = F, size = 3) +
            stat_smooth(data = in_trump(), size = 3, se = F) +
            scale_color_manual(values = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange")) +
            ggplot2::theme(legend.position = "none") +
            labs(x = "Days in Office",
                 y = "Unsure Rating")
        ggplotly(p,originalData = T, tooltip = c("unsure", "party"), layer = 1)
    })
    
    output$second_unsure_plot <- renderPlotly({
        p <- ggplot(in_proj(), aes(x = days_in_office, y = unsure, group = president, color = party)) +
            stat_smooth(se = F, aes(linetype = line_type)) +
            stat_smooth(data = in_trump(), se = F, size = 1, color = "orange") +
            scale_color_manual(values = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange")) +
            ggplot2::theme(legend.position = "none") +
            labs(x = "Days in Office",
                 y = "Unsure Rating")
        ggplotly(p,originalData = T, tooltip = c("unsure", "president", "party"), layer = 1)
    })
}

shinyApp(ui = ui, server = server)


library(forcats)
library(rvest)
library(plotly)
library(lubridate)
library(tidyverse)
library(magrittr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(mgcv)


fillNAs <- function (vector, reverse = F) 
{
    if (reverse) {
        seq <- length(vector):1
    }
    if (!reverse) {
        seq <- 1:length(vector)
    }
    for (i in seq) {
        if (!is.na(vector[i])) {
            j <- vector[i]
        }
        if (is.na(vector[i])) {
            vector[i] <- j
        }
    }
    return(vector)
}

>>>>>>> final changes for new theme push live
theme_set(ggplot2::theme_bw(base_size = 18) +
              ggplot2::theme(panel.border = element_rect(colour = "#f5f5f5", fill=NA, size=1),
                    plot.background = element_rect(fill = "#f5f5f5"),
                    text = element_text(color = "white", size = rel(.75)),
                    plot.margin = margin(1,1,1,1, "cm"),
                    axis.text = element_text(color = "#317eac"),
                    axis.ticks = element_blank(),
                    axis.title = element_blank(),
                    panel.background = element_rect(fill = "#f5f5f5"),
                    panel.grid.minor = element_line(colour="#317eac", size=0.5),
                    panel.grid.major = element_line(colour="#317eac", size=0.5),
                    legend.position = "none")
)

# setwd("~/future/President_Approval/Pres_Approval_App/")
#### UI ####

ui <- fluidPage(theme = shinytheme("cerulean"),
#### UI-Title ####
titlePanel(title = "Red, White and Boo",
           tags$head(tags$link(rel = "shortcut icon", href = "www/favicon.png"))
           ),
fluidRow(column(12,
         p("A loess look at the 🍊's historic poll ratings ratings against the back drop of history."),
         p("Updated : 2017-08-29")
         )
         ),
#### UI-Main ####
fluidRow(column(12,
                #### UI-Controls ####
                sidebarLayout(sidebarPanel(width = 3,
                    h4("Time Scale:"),
                    sliderInput("term_int", "Years of Service", as.Date("1940-01-01"), as.Date("2017-12-31"),
                                c(as.Date("1940-01-01"), as.Date("2017-12-31")),
                                timeFormat = "%Y"),
                    checkboxInput("don_predict", "Plot an linear model for 🍊's future? ", F),
                    br(),
                    h4("Presidents Included:"),
                    
                        
                    br(),
                    checkboxInput("more_bool", "Show more filters"),
                    conditionalPanel(condition = "input.more_bool == true",
                        h5("Still working on this, sorry : (")
                        
                        # selectInput("term_select", "Select # of terms", c(1,2)),
                        # checkboxInput("party_bool", "Filter by party:"),
                        # selectInput("party_select", "Party:", c("Republican", "Democrat"))
                    )
                ),
                #### UI-Output ####
                mainPanel(width = 9,
                    tabsetPanel(id = "main_tabs",
                        tabPanel("", icon = icon("thumbs-up"), value = "approve",
                                 tags$h3("% Approval:"),
                                 fluidRow(
                                     column(9,
                                            tags$body(p("Compared to historical party averages"))
                                     )
                                 ),
                                 plotlyOutput("main_approval_plot", height = "400px"),
                                 br()
                        ),
                        tabPanel("", icon = icon("thumbs-down"), value = "approve",
                                 tags$h3("% Disapproval:"),
                                 fluidRow(
                                     column(9,
                                            tags$body(p("Compared to historical party averages"))
                                     )
                                 ),
                                 plotlyOutput("main_disapproval_plot", height = "400px"),
                                 br()
                        ),
                        tabPanel("", icon = icon("question"), value = "approve",
                                 tags$h3("% Approval:"),
                                 fluidRow(
                                     column(9,
                                            tags$body(p("Compared to historical party averages"))
                                     )
                                 ),
                                 plotlyOutput("main_unsure_plot", height = "400px")
                                 )
                )
                )
)
)
),
br(),
#### UI-Footer ####
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
                     tags$div(class = "info",
                              tags$h4("Code"),
                              tags$p("All of the code written to generate this Shiny app, including a bunch that doesn't show up in the rendered verion here
                                     is available and easier to see on",
                                     tags$a(href = "https://github.com/NathanCDay/Orange_Approval_App", "my Github repo"), "."),
                              tags$p("I built this app as an execise in web development with",
                                     tags$a(href = "https://github.com/rstudio/htmltools",
                                            tags$code("library(htmltools)")), ", webscrapping with",
                                     tags$a(href = "https://www.r-bloggers.com/rvest-easy-web-scraping-with-r/",
                                            tags$code("library(rvest)") ),
                                     ", and interactive graphics with",
                                     tags$a(href = "https://plot.ly/r/",
                                            tags$code("library(plotly)")), "."),
                              tags$p("Currently this app is manually run to scrape new polling data, but my next step is to build a cron job to auto scrape fresh data weekly. 
                                     I also want to build one more selector input to allow specifc president comparisons by name, that is what should appear when
                                     the Show More Tools checkbox is toggled on."))
                      )#,
            # tabPanel("App.R", icon = icon("laptop"),
            #          includeHTML("app.html"))
            )
            )
                     
),
br(),
fluidRow(column(12,
                tags$div(class = "well well-sm",
                         "This project was coded by Nate Day 2017"
                )
            )
         )
)

#### Server ####
server <- function(input, output) {

data <- readRDS("www/data.RDS")

in_data <- reactive({
    
    data %<>% filter(number != 45) %>%
        filter(as.Date(end_date) > input$term_int[1],
               as.Date(end_date) < input$term_int[2]) %>%
        bind_rows(filter(data, number == 45))
    
    if (input$don_predict) {
        
        djt <- filter(added, number == 45)
        
        mods <- djt %>%
            gather("rating", "value", approval:unsure) %>%
            split(.$rating) %>%
            map(~ lm(value ~ end_date, data = .))
        
        new_d <- tibble(end_date = max(djt$end_date) + seq(1,720, 9))
        
        preds <- map(mods, ~ predict(., new_d)) %>%
            map(~ mutate(new_d, value = .)) %>%
            map2_dfr(., names(.), ~ mutate(.x, rating = .y)) %>%
            spread(rating, value)
<<<<<<< HEAD
        
        djt %<>% bind_rows(preds,.) %>%
            map_df(~ fillNAs(., T))
        
        data %<>% filter(number != 45) %>%
            bind_rows(djt, .)
        
        
=======
        
        djt %<>% bind_rows(preds,.) %>%
            map_df(~ fillNAs(., T))
        
        data %<>% filter(number != 45) %>%
            bind_rows(djt, .)
        
        
>>>>>>> final changes for new theme push live
        }
    
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
    # ggplot(mtcars, aes(mpg, cyl)) + geom_point()
    p <- ggplot(in_data(), aes(end_date, approval, color = party, group = initials)) +
        geom_point(alpha = .5) +
        scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
        theme(axis.text.x = element_text(angle = 45)) +
        scale_color_manual(values = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange"))
    ggplotly(p, originalData = T, tooltip = c("approval", "initials"), layer = 1)
})


output$main_disapproval_plot <- renderPlotly({
    p <- ggplot(in_data(), aes(end_date, disapproval, color = party, group = initials)) +
        geom_point(alpha = .5) +
        scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
        theme(axis.text.x = element_text(angle = 45)) +
        scale_color_manual(values = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange")) +
        ggplot2::theme(legend.position = "none") +
        labs(x = "Days in Office",
             y = "Disapproval Rating")
    ggplotly(p,originalData = T, tooltip = c("disapproval", "initials"), layer = 1)
})

output$main_unsure_plot <- renderPlotly({
    p <- ggplot(in_data(), aes(end_date, unsure, color = party, group = initials)) +
        geom_point(alpha = .5) +
        scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
        theme(axis.text.x = element_text(angle = 45)) +
        scale_color_manual(values = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange")) +
        ggplot2::theme(legend.position = "none") +
        labs(x = "Days in Office",
             y = "Unsure Rating")
    ggplotly(p,originalData = T, tooltip = c("unsure", "initials"), layer = 1)
})

}

shinyApp(ui = ui, server = server)


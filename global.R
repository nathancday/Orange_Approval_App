# Scraper for http://www.presidency.ucsb.edu/data/popularity.php
library(forcats)
library(rvest)
library(plotly)
library(lubridate)
library(tidyverse)
library(magrittr)

setwd("~/future/President_Approval/Pres_Approval_App/")

metaFiller <- function(vector) {
    for (i in 1:length(vector)) {
        if (!is.na(vector[i])) {
            j <- vector[i]
        }
        if (is.na(vector[i])) {
            vector[i] <- j
        }
    }
    return(vector)
}


#### Parser ####

# # works for a single webpage
# site <- read_html("http://www.presidency.ucsb.edu/data/popularity.php")
# pres_approval <- site %>% html_nodes("table") %>% extract2(11) %>%
#     html_table(fill = T)
# 
# site2 <- read_html("http://www.presidency.ucsb.edu/data/popularity.php?pres=32&sort=time&direct=DESC&Submit=DISPLAY")
# fdr_approval <- site2 %>% html_nodes("table") %>% extract2(11) %>%
#     html_table(fill = T)

# id'd presidential number in url, want to loop on that
results_list <- list()
for (i in 32:45) {
    url <- paste0("http://www.presidency.ucsb.edu/data/popularity.php?pres=", i, "&sort=time&direct=DESC&Submit=DISPLAY")
    result <- url %>% read_html %>% html_nodes("table") %>% extract2(11) %>% html_table(fill = T)
    results_list[[i]] <- result
}

# # results grabbed 2017/2/18
saveRDS(results_list, "www/results.RDS")

#### Scrubber ####
results_list <- readRDS("www/results.RDS")

parsed <- results_list[32:45] %>% map(~ select(., 1:3,5:7)) %>%
    map(~ magrittr::extract(., -c(1:6), )) %>%
    map(~ set_colnames(., c("president", "start_date", "end_date", "approval", "disapproval", "unsure"))) %>%
    bind_rows()

parsed$president %<>% ifelse(nchar(.) < 2, NA, .) %>% metaFiller()

cleaned <- parsed %>% mutate_at(vars(contains("date")), "mdy") %>%
    mutate_at(vars(4:6), "as.numeric") %>%
    mutate(president = as.factor(president) %>% fct_inorder)

# natural time course
ggplot(cleaned, aes(end_date, approval, color = president)) +
    geom_path()


#### Adder ####
# add a scaled time variable for days in office
added <- cleaned %>% group_by(president) %>%
    mutate(days_in_office = end_date - min(end_date))

# add party variable manually
party_key <- c("d","d", "r", "d", "d", "r", "r", "d", "r", "r", "d", "r", "d", "o") %>%
    set_names(unique(added$president))
party_key %<>% gsub("r","Republican", .) %>% gsub("d", "Democrat", .) %>% gsub("^o", "Orange", .)
added %<>% mutate(party = party_key[president])

# need to account for Obama weight; 2800 obs vs 270 for GWB
# o <- filter(added, president == "Barack Obama") %>% arrange(days_in_office)
# added %<>% filter(president != "Barack Obama")
# o <- filter(o, rep(c(T,rep(F, 9)), length.out = nrow(o)))
# added %<>% rbind(o)

# need to get year out for filter
added$year <- strftime(added$end_date, "%Y")

# add unique id by party for linetype
added %<>% group_by(party) %>% mutate(line_type = as.character(as.numeric(factor(president)) + 1))


# adjust days_in_office to numeric

added$days_in_office %<>% as.numeric()
added %<>% as.data.frame()
# shiny app storage
saveRDS(added, "www/data.RDS")



#### Play ####
# plots for play
ggplot(added, aes(days_in_office, approval, color = president)) +
    geom_path(alpha = .5)

ggplot(added, aes(days_in_office, approval, color = president)) +
    geom_path(alpha = .5) +
    facet_wrap(~president)

ggplot(added, aes(days_in_office, approval, color = president)) +
    stat_smooth(se = F)





# map out new data frame with loess projects values to alleviate stat_smooth low n woes
test <- added %>% gather(measure, value, approval:unsure) %>%
    mutate(pres_mes = paste(president, measure, sep = "_")) %>%
    as.data.frame()

test_mods <- test %>% split(.$pres_mes) %>%
    map(~ loess(value ~ as.numeric(days_in_office), data = .))
dummy_data <- test %>% split(.$pres_mes) %>%
    map(~ max(as.numeric(.$days_in_office))) %>%
    map(~ seq(0, ., by = 1))
response_data <- map2(test_mods, dummy_data, predict) %>%
    map(as.data.frame) %>% map(~mutate(., days_in = seq(0, by = 1, length.out = nrow(.)))) %>%
    bind_rows(.id = "pres_mes") %>%
    set_colnames(c("pres_mes", "value", "days_in")) %>%
    separate(pres_mes, c("president", "measure"), sep = "_") %>%
    group_by(president) %>%
    spread(measure, value) %>%
    set_colnames(c("president", "days_in_office", "approval", "disapproval", "unsure"))

start_date <- added %>% group_by(president, party, line_type) %>%
    slice(which.min(start_date)) %>%
    select(1:2)

projected <- full_join(response_data, start_date) %>%
    mutate(end_date = start_date + days(days_in_office),
           year = strftime(end_date, "%Y"),
           start_date = lag(end_date)) %>%
    mutate_at(vars(3:5), funs(round(., 2)))

saveRDS(projected, "www/proj.RDS")

trump_mods <- filter(test, president == "Donald J. Trump") %>%
    split(.$pres_mes) %>%
    map(~ lm(value ~ as.numeric(days_in_office), data = .))
trump_data <- filter(test, president == "Donald J. Trump") %>%
    split(.$pres_mes) %>%
    map(~ max(.$days_in_office)) %>%
    map(~ seq(0, 100, by = 1)) %>%
    map(as.data.frame) %>% map(~ set_colnames(.,"days_in_office"))
response_trump <- map2(trump_mods, trump_data, predict) %>%
    map(as.data.frame) %>% map(~mutate(., days_in = seq(0, by = 1, length.out = nrow(.)))) %>%
    bind_rows(.id = "pres_mes") %>%
    set_colnames(c("pres_mes", "value", "days_in")) %>%
    separate(pres_mes, c("president", "measure"), sep = "_") %>%
    group_by(president) %>%
    spread(measure, value) %>%
    set_colnames(c("president", "days_in_office", "approval", "disapproval", "unsure"))

start_date2 <- filter(added, president == "Donald J. Trump") %>%
    group_by(president, party, line_type) %>%
    slice(which.min(start_date)) %>%
    select(1:2)

projected_trump <- full_join(response_trump, start_date2) %>%
    mutate(end_date = start_date + days(days_in_office),
           year = strftime(end_date, "%Y"),
           start_date = lag(end_date)) %>%
    mutate_at(vars(3:5), funs(round(., 2)))

saveRDS(projected_trump, "www/projected_trump.RDS")


#### GGTern ####
ggplot(data = projected_trump, aes(x = approval, y = disapproval, z = unsure)) +
    geom_text(aes(label = days_in_office)) +
    coord_tern() +
    theme_bw() +
    theme_showarrows()
    
ggplotly()


# from help page
label <- function(txt) {
    list(
        text = txt, 
        x = 0.1, y = 1,
        ax = 0, ay = 0,
        xref = "paper", yref = "paper", 
        align = "center",
        font = list(family = "serif", size = 15, color = "white"),
        bgcolor = "#b3b3b3", bordercolor = "black", borderwidth = 2
    )
}

# reusable function for axis formatting
axis <- function(txt) {
    list(
        title = txt, tickformat = ".0%", tickfont = list(size = 10)
    )
}

ternaryAxes <- list(
    aaxis = axis("Approval"), 
    baxis = axis("Disapproval"), 
    caxis = axis("Unsure")
)

plot_ly(projected,
        a = ~approval, b = ~disapproval, c = ~unsure, color = ~party,
        hoverinfo = "text",
        text = ~paste(president,
                      "</br>+:", approval,
                      "</br>-", disapproval,
                      "</br>?", unsure,
                      "</br>", party),
        colors = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange"),
        type = "scatterternary", mode = "markers",
        marker = list(symbol = "circle", size = 10)) %>%
    layout(
        ternary = ternaryAxes, xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, reversescale = F),
        yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F)
    )

plot_ly(projected_trump,
        a = ~approval, b = ~disapproval, c = ~100 -unsure, color = ~party,
        hoverinfo = "text",
        text = ~paste(president,
                      "</br>+:", approval,
                      "</br>-", disapproval,
                      "</br>?", unsure,
                      "</br>", party),
        colors = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange"),
        type = "scatterternary", mode = "markers") %>%
    layout(
        ternary = ternaryAxes, xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F),
        yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F)
    )

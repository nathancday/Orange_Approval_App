# Scraper for http://www.presidency.ucsb.edu/data/popularity.php
library(forcats)
library(rvest)
library(plotly)
library(lubridate)
library(tidyverse)
library(magrittr)

setwd("~/future/President_Approval/Pres_Approval_App/")

# useful function for filling tables
na_filler <- function(vector) {
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
scale_color_discrete <- function(...) ggplot2::scale_color_manual(..., values = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange"))

#### Scraper -------------------------------------------------------------------
# let's get our hands dirty

# id is presidential # in url, want to loop on that
ucsb_scraper <- function(id) {
    
    result <- paste0("http://www.presidency.ucsb.edu/data/popularity.php?pres=",
                     id,
                     "&sort=time&direct=DESC&Submit=DISPLAY") %>%
        read_html() %>% 
        html_nodes("table") %>% 
        extract2(11) %>% 
        html_table(fill = T)

}

# # get historical data once for Roosevelt - Obama
# past_presidents <- map(32:44, ucsb_scraper)
# # save my scraper traffic
# saveRDS(past_presidents, "www/past_presidents.RDS")
past_presidents <- readRDS("www/past_presidents.RDS")

# get current data for the orange
current_president <- map(45, ucsb_scraper)
# combine into one
results_list <- c(past_presidents, current_president) %>%
    set_names(32:45)

#### Scrubber ------------------------------------------------------------------
# clean up the wild and unruly data from the scrap

# tidy up
parsed <- results_list %>%
    map(~ select(., 1:3,5:7)) %>%
    map(~ magrittr::extract(., -c(1:6), )) %>%
    map_dfr(~ set_colnames(., c("president", "start_date", "end_date",
                            "approval", "disapproval", "unsure")), .id = "number")

# handle subsequent empty rows in president var with na_filler()
parsed$president %<>% ifelse(nchar(.) < 2, NA, .) %>%
    na_filler() %>%
    fct_inorder()
    
# recode date vars and numeric vars from chr
parsed %<>% mutate_at(vars(contains("date")), mdy) %>%
    mutate_if(is.character, as.numeric)

# capture initials for plotting (kinda sloppy)
parsed$initials <- gsub("^(\\D).* (\\D)\\. (\\D).*", "\\1\\2\\3", parsed$president) %>%
    gsub("^(\\D).* (\\D).*", "\\1\\2", .) %>%
    fct_inorder()

# viz check
ggplot(parsed, aes(end_date, approval, color = initials)) +
    geom_point(alpha = .5)
# whoa there modern polling frequency

# see how over plotted BO and DJT are
parsed %>%
    group_by(initials) %>%
    summarise(count = n(),
              range = max(end_date) - min(start_date),
              days_per_poll = range / count) %>%
    ggplot(aes(initials, days_per_poll, fill = initials)) +
    geom_col() +
    labs(title = "BO and DJT need to be adjusted to one every 9.1 days")

parsed %<>% filter(number %in% 44:45) %>%
    split(.$initials) %>%
    map_df(~ .[c(rep(F,8), T),]) %>%
    filter(!is.na(initials)) %>%
    bind_rows(filter(parsed, !(number %in% 44:45)))

ggplot(parsed, aes(end_date, approval, color = initials)) +
    geom_point(alpha = .5)
    

#### Adder --------------------------------------------------------------------
# build in useful display features

# add a scaled time variable for days in office
added <- parsed %>% group_by(president) %>%
    mutate(days_in_office = end_date - min(end_date))

# add party variable manually
party_key <- c("d","d", "r", "d", "d", "r", "r", "d", "r", "r", "d", "r", "d", "o") %>%
    set_names(unique(added$president))
party_key %<>% gsub("r","Republican", .) %>% gsub("d", "Democrat", .) %>% gsub("^o", "Orange", .)
added %<>% mutate(party = party_key[president])

# need to get year out for filter
added$year <- strftime(added$end_date, "%Y")

# adjust days_in_office to numeric

added$days_in_office %<>% as.numeric()
added %<>% as.tibble()


ggplot(added, aes(end_date, approval, color = party)) +
    geom_point(alpha = .5) +
    ggplot2::scale_color_manual(values = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange"))

# shiny app storage
saveRDS(added, "www/data.RDS")



# #### Play ####
# # plots for play
# ggplot(added, aes(days_in_office, approval, color = president)) +
#     geom_path(alpha = .5)
# 
# ggplot(added, aes(days_in_office, approval, color = president)) +
#     geom_path(alpha = .5) +
#     facet_wrap(~president)
# 
# ggplot(added, aes(days_in_office, approval, color = president)) +
#     stat_smooth(se = F)
# 
# 
# 
# 
# 
# # map out new data frame with loess projects values to alleviate stat_smooth low n woes
# test <- added %>% gather(measure, value, approval:unsure) %>%
#     mutate(pres_mes = paste(president, measure, sep = "_")) %>%
#     as.data.frame()
# 
# test_mods <- test %>% split(.$pres_mes) %>%
#     map(~ loess(value ~ as.numeric(days_in_office), data = .))
# dummy_data <- test %>% split(.$pres_mes) %>%
#     map(~ max(as.numeric(.$days_in_office))) %>%
#     map(~ seq(0, ., by = 1))
# response_data <- map2(test_mods, dummy_data, predict) %>%
#     map(as.data.frame) %>% map(~mutate(., days_in = seq(0, by = 1, length.out = nrow(.)))) %>%
#     bind_rows(.id = "pres_mes") %>%
#     set_colnames(c("pres_mes", "value", "days_in")) %>%
#     separate(pres_mes, c("president", "measure"), sep = "_") %>%
#     group_by(president) %>%
#     spread(measure, value) %>%
#     set_colnames(c("president", "days_in_office", "approval", "disapproval", "unsure"))
# 
# start_date <- added %>% group_by(president, party, line_type) %>%
#     slice(which.min(start_date)) %>%
#     select(1:2)
# 
# projected <- full_join(response_data, start_date) %>%
#     mutate(end_date = start_date + days(days_in_office),
#            year = strftime(end_date, "%Y"),
#            start_date = lag(end_date)) %>%
#     mutate_if(is.numeric, funs(round(., 2)))
# 
# saveRDS(projected, "www/proj.RDS")
# 
# 
# 
# trump_mods <- filter(test, president == "Donald J. Trump") %>%
#     split(.$pres_mes) %>%
#     map(~ lm(value ~ as.numeric(days_in_office), data = .))
# trump_data <- filter(test, president == "Donald J. Trump") %>%
#     split(.$pres_mes) %>%
#     map(~ max(.$days_in_office)) %>%
#     map(~ seq(0, 100, by = 1)) %>%
#     map(as.data.frame) %>% map(~ set_colnames(.,"days_in_office"))
# response_trump <- map2(trump_mods, trump_data, predict) %>%
#     map(as.data.frame) %>% map(~mutate(., days_in = seq(0, by = 1, length.out = nrow(.)))) %>%
#     bind_rows(.id = "pres_mes") %>%
#     set_colnames(c("pres_mes", "value", "days_in")) %>%
#     separate(pres_mes, c("president", "measure"), sep = "_") %>%
#     group_by(president) %>%
#     spread(measure, value) %>%
#     set_colnames(c("president", "days_in_office", "approval", "disapproval", "unsure"))
# 
# start_date2 <- filter(added, president == "Donald J. Trump") %>%
#     group_by(president, party, line_type) %>%
#     slice(which.min(start_date)) %>%
#     select(1:2)
# 
# projected_trump <- full_join(response_trump, start_date2) %>%
#     mutate(end_date = start_date + days(days_in_office),
#            year = strftime(end_date, "%Y"),
#            start_date = lag(end_date)) %>% 
#     mutate_if(is.numeric, funs(round(., 2)))
# 
# saveRDS(projected_trump, "www/projected_trump.RDS")
# 
# 
# ### rework preds
# data <- readRDS("www/data.RDS") %>%
#     filter(president == "Donald J. Trump")
# 
# mods <- data %>%
#     gather(key, value, approval:unsure) %>%
#     split(.$key) %>%
#     map(~ lm(value ~ days_in_office, data = .))
# 
# new_data <- tibble(days_in_office = seq(max(gathered$days_in_office), 500))
# 
# new_data <- map_df(mods, ~ predict(., newdata = new_data)) %>%
#     mutate_all(round,1) %>%
#     bind_cols(new_data)
# 
# 
# # re-attach to data
# data2 <- bind_rows(new_data, data)
# # need to fill in NAs
# 
# 
# 
# 
# 
# 
# 
# #### GGTern ####
# # ggplot(data = projected_trump, aes(x = approval, y = disapproval, z = unsure)) +
# #     geom_text(aes(label = days_in_office)) +
# #     coord_tern() +
# #     theme_bw() +
# #     theme_showarrows()
# #     
# # ggplotly()
# # 
# # 
# # # from help page
# # label <- function(txt) {
# #     list(
# #         text = txt, 
# #         x = 0.1, y = 1,
# #         ax = 0, ay = 0,
# #         xref = "paper", yref = "paper", 
# #         align = "center",
# #         font = list(family = "serif", size = 15, color = "white"),
# #         bgcolor = "#b3b3b3", bordercolor = "black", borderwidth = 2
# #     )
# # }
# # 
# # # reusable function for axis formatting
# # axis <- function(txt) {
# #     list(
# #         title = txt, tickformat = ".0%", tickfont = list(size = 10)
# #     )
# # }
# # 
# # ternaryAxes <- list(
# #     aaxis = axis("Approval"), 
# #     baxis = axis("Disapproval"), 
# #     caxis = axis("Unsure")
# # )
# # 
# # plot_ly(projected,
# #         a = ~approval, b = ~disapproval, c = ~unsure, color = ~party,
# #         hoverinfo = "text",
# #         text = ~paste(president,
# #                       "</br>+:", approval,
# #                       "</br>-", disapproval,
# #                       "</br>?", unsure,
# #                       "</br>", party),
# #         colors = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange"),
# #         type = "scatterternary", mode = "markers",
# #         marker = list(symbol = "circle", size = 10)) %>%
# #     layout(
# #         ternary = ternaryAxes, xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, reversescale = F),
# #         yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F)
# #     )
# # 
# # plot_ly(projected_trump,
# #         a = ~approval, b = ~disapproval, c = ~100 -unsure, color = ~party,
# #         hoverinfo = "text",
# #         text = ~paste(president,
# #                       "</br>+:", approval,
# #                       "</br>-", disapproval,
# #                       "</br>?", unsure,
# #                       "</br>", party),
# #         colors = c("Democrat" = "#232066", "Republican" = "#e91d0e", "Orange" = "orange"),
# #         type = "scatterternary", mode = "markers") %>%
# #     layout(
# #         ternary = ternaryAxes, xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F),
# #         yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F)
# #     )

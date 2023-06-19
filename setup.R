library(shiny)
library(tidyverse)
library(rvest)
library(RSelenium)


# Setup ---------------------------

avp <- read_html("https://avpamerica.com/VA-Beach-Volleyball-Schedule.aspx")

avp_df <- avp %>% 
  html_element(css = "#tblEvent") %>% 
  html_table()

# this only gets us the default HTML, which is 1 month of tourneys. 
# gotta use {RSelenium} !!

ff_instance <- rsDriver(browser = "firefox", port = as.integer(paste(sample(1:9, 4, replace = TRUE), collapse = "")))
ff_instance_go <- ff_instance[["client"]]

ff_instance_go$navigate("https://avpamerica.com/VA-Beach-Volleyball-Schedule.aspx")
date_accordion <- ff_instance_go$findElement(using = "id", value = "ddDates")
date_accordion$clickElement()

ff_instance_go$buttondown()




page_html <- ff_instance_go$getPageSource()[[1]] %>% read_html()

# get all events
x <- page_html %>% 
  html_nodes(xpath = '//*[@id="tblEvent"]') %>% 
  html_table()

df_raw <- x[[1]]

# clean up df

# goal is to get a df with cols date (YYYY-MM-DD), city, lat, lon, surface, 
# division (from the actual event page), anything else from the event page i can grab

df <- df_raw
colnames(df) <- c("date_city", "event_name", "surface", "notes")

# get links for each event
event_links_raw <- page_html %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  str_subset(pattern = "EventID")

df %>% 
  # remove rows that are just month headers (July '23, August '23)
  filter(!(date_city %in% c("June '23", "July '23", "August '23", 
                            "September '23", "October '23", "November '23", 
                            "December '23", "January '24", "April '24"))) %>% 
  separate(date_city, into = c("date", "city"), sep = "(?<=[0-9]{4})") %>% 
  # clean up date_city and event_name
  separate(city, into = c("city", "state"), sep = ", ") %>%
  # account for multi-day tourneys (e.g. July 20 - 21, 2023)
  separate(date, into = c("start_date", "end_date"), sep = "-") %>%
  mutate(
    # add event links
    event_link = event_links_raw,
    event_link = str_c("https://avpamerica.com/", event_link),
    # clean up start & end date columns
    start_date = case_when(
      # if end_date isn't NA, then it's a multi-day tourney, was split, and i need 
      # to add ", {year}" back into the start_date (the last 4 characters of end_date)
      !(is.na(end_date)) ~ str_c(start_date, str_sub(end_date, -4, -1)),
      TRUE ~ start_date
    ),
    end_date = case_when(
      is.na(end_date) ~ start_date,
      TRUE ~ end_date
    )
    ) %>% 
  View()



# add links in
df <- df %>% 
  mutate()

event_links_clean <- df %>% pull(event_link)

# loop through event links to get the Divisions for each event
event_titles <- c()
event_hosts <- c()
event_divs <- c()

for (i in 1:length(event_links_clean)) {
  
  # don't need RSelenium, just grab html from the page :)
  
  curr_page_html <- read_html(event_links_clean[i])
  
  curr_title <- curr_page_html %>% 
    html_nodes(css = ".MainStoryTitleGray") %>% 
    html_text()
  
  curr_host <- curr_page_html %>% 
    html_nodes(css = "#lblName") %>% 
    html_text()
  
  curr_divs <- curr_page_html %>% 
    html_nodes(css = ".EventGrayText") %>% 
    html_text() %>% 
    # remove additional information chunk, since it can include "Divisions:" text
    str_subset(pattern = "Additional Information", negate = TRUE) %>% 
    str_subset(pattern = "Divisions:")
  
  event_titles <- append(event_titles, curr_title)
  event_hosts <- append(event_hosts, curr_host)
  event_divs <- append(event_divs, curr_divs)
  
}

# add titles, hosts, divisions to the main df, clean them up
df <- df %>% 
  mutate(
    event_titles = event_titles,
    event_hosts = event_hosts,
    # remove "[event_num] - Event Details"which is the last ~25 characters
    event_hosts = str_sub(event_hosts, start = 1L, end = -25L),
    event_divisions = event_divs,
    # remove "Divisons: "
    event_divisions = str_sub(event_divisions, start = 12),
    event_divisions = str_trim(event_divisions)
    )









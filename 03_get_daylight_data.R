library(rvest)
library(stringr)

get_dalyight_data <- function(year = 2016) {
  
  data <- data.frame(stringsAsFactors = F)
  
  for(i in 1:12) {
    
    url <- paste0("https://www.timeanddate.com/sun/@690791?month=",
                  i, "&year=", year)
    
    message(url)
    
    page <- html_session(url)
    
    day <- page %>% 
      html_nodes('tbody th') %>% 
      html_text()
    
    start <- page %>% 
      html_nodes('th+ td') %>% 
      html_text()
    
    end <- page %>% 
      html_nodes('td:nth-child(3)') %>% 
      html_text()
    
    length <- page %>% 
      html_nodes('td.tr.sep-l') %>% 
      html_text()
    
    df <- data.frame(year = year, month = i, day, start, end, length)
    
    data <- rbind.data.frame(data, df)
    
    Sys.sleep(5)
    
  }
  
  return(data)
  
}

df <- get_dalyight_data(year = 2016)

parse_data <- function(df) {
  
  df$date <- as.Date(paste(df$year, df$month, df$day, sep = "-"))
  
  df$start <- df$start %>% 
    str_extract("\\d+?:\\d+") %>% 
    strptime(format = "%H:%M", tz = "UTC")
  
  df$end <- df$end %>% 
    str_extract("\\d+?:\\d+") %>% 
    strptime(format = "%H:%M", tz = "UTC")
  
  return(df)
  
}

df <- parse_data(df)

write.csv(df, "data/daylight.csv", row.names = F)

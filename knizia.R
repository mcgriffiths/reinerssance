library(rvest)
library(stringr)
library(dplyr)
library(purrr)
library(readr)
library(ggplot2)
library(plotly)

get_ids <- function(page_num){
  games_page <- paste0("https://boardgamegeek.com/search/boardgame/page/",
                       page_num,
                       "?advsearch=1&q=&include%5Bdesignerid%5D=2&include%5Bpublisherid%5D=&geekitemname=&range%5Byearpublished%5D%5Bmin%5D=&range%5Byearpublished%5D%5Bmax%5D=&range%5Bminage%5D%5Bmax%5D=&range%5Bnumvoters%5D%5Bmin%5D=&range%5Bnumweights%5D%5Bmin%5D=&range%5Bminplayers%5D%5Bmax%5D=&range%5Bmaxplayers%5D%5Bmin%5D=&range%5Bleastplaytime%5D%5Bmin%5D=&range%5Bplaytime%5D%5Bmax%5D=&floatrange%5Bavgrating%5D%5Bmin%5D=&floatrange%5Bavgrating%5D%5Bmax%5D=&floatrange%5Bavgweight%5D%5Bmin%5D=&floatrange%5Bavgweight%5D%5Bmax%5D=&colfiltertype=&searchuser=qwertymartin&nosubtypes%5B0%5D=boardgameexpansion&playerrangetype=normal&B1=Submit"
  )

  games <- read_html(games_page)
 
  games %>%
    html_nodes(".collection_thumbnail a") %>%
    html_attr("href") %>%
    str_replace("/boardgame/", "") %>%
    str_replace("/.+", "")
  }

game_ids <- unlist(map(1:6, get_ids))

get_info <- function(game_id){
  Sys.sleep(2)
  print(game_id)
  info_page <- paste0("https://boardgamegeek.com/xmlapi2/thing?id=",game_id,"&stats=1")
  game_stats <- read_xml(info_page)

  attrs <- c('name',
             'yearpublished', 
             'minplayers',
             'maxplayers',
             'playingtime',
             'minplaytime',
             'maxplaytime', 
             'minage', 
             'statistics/ratings/usersrated', 
             'statistics/ratings/average',
             'statistics/ratings/bayesaverage',
             'statistics/ratings/ranks/rank',
             'statistics/ratings/owned',
             'statistics/ratings/averageweight',
             'statistics/ratings/numweights'
             )

  get_attr <- function(attr){
    game_stats %>%
      xml_find_all(paste0('.//item/',attr)) %>%
      xml_attr('value') %>%
      .[1]
  }

  
  map_chr(attrs, get_attr) %>%
    set_names(str_replace(attrs, 'statistics/ratings/', ''))

}

game_df <- map(game_ids, get_info) %>%
  map_df(bind_rows)

write_csv(game_df, 'knizia_full.csv')

game_df <- read_csv('knizia_full.csv')

game_df <- game_df %>% 
  filter(yearpublished >= 1990, `ranks/rank` != 'Not Ranked')

gg <- game_df %>%
  ggplot(aes(x=yearpublished, y=average, 
             size = usersrated, text = name)) +
  geom_point(alpha = 0.5, color = 'blue') +
  theme_minimal() +
  labs(y = "Average user rating",
       x = "Year published",
       title = "BGG-ranked Knizia games")

ggplotly(gg, tooltip = 'text')

game_df %>%
  filter(average > 7.0) %>%
  ggplot(aes(x=yearpublished)) +
  geom_bar()

game_df %>%
  filter(usersrated > 5000) %>%
  ggplot(aes(x=yearpublished)) +
  geom_bar()

gg <- game_df %>%
  filter(averageweight > 0.5, numweights > 5) %>%
  ggplot(aes(y=average, x=averageweight, text=name)) +
  geom_point() +
  geom_smooth()


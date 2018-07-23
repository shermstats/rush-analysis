library(readr)
library(dplyr)
library(spotifyr)
library(purrr)
library(tidyr)
library(jsonlite)
library(lubridate)
library(here)

source(here("lib", "vars.R"))

rush_studio_album_names <- c("rush", "fly by night",
"caress of steel", "2112", "a farewell to kings", "hemispheres",
"permanent waves", "moving pictures", "signals",
"grace under pressure", "power windows", "hold your fire", "presto",
"roll the bones", "counterparts", "test for echo", "vapor trails",
"snakes & arrows", "clockwork angels")

get_rush_dat <- function(ORIGINAL_DAT) {
    ## Look for the cached file because this takes a really long time to query
    
    if (file.exists(ORIGINAL_DAT)) {
        cat("Reading in file...\n")
        dat <- readRDS(ORIGINAL_DAT)
    } else {
        cat("Getting album data from Spotify...\n")
        dat <- get_discography("Rush") %>%
            ungroup()
        
        saveRDS(dat, ORIGINAL_DAT)
    }

    return(dat)
}    

original_rush_dat <- get_rush_dat(ORIGINAL_DAT)

rush_dat <- original_rush_dat %>% 
    filter(tolower(album_name) %in% rush_studio_album_names) %>% 
    filter(!grepl("- Live", track_name)) ## Remove bonus tracks

## Album-level info
rush_albums <- rush_dat %>%
    select(artist_name, artist_uri, starts_with("album"), track_name, track_uri, track_n, danceability:key_mode, lyrics) %>%
    select(-album_img, -album_type, -liveness) %>% ## remove redundant columns
    mutate(album_release_date = if_else(!is.na(ymd(album_release_date)), ymd(album_release_date), ymd(album_release_year))) %>% 
    select(-album_release_year) %>% 
    mutate(track_name = gsub(" - Remastered", "", .$track_name),
           track_name = if_else(grepl("^2112", .$track_name), "2112", track_name))

get_audio_analysis_df <- function(dat) {

    audio_analysis_df <- map_df(dat$track_uri, function(x) {
        audio_analysis <- get_track_audio_analysis(x)    
        return(tibble(track_uri = x,
                      audio_analysis = audio_analysis,
                      content_type = c("meta", "track", "bars", "beats",
                                       "tatums", "sections", "segments")))
    })

    album_track_info <- dat %>%
        select(album_uri, album_name, track_name, track_uri, track_n)

    audio_analysis_col <- audio_analysis_df %>% select(audio_analysis)

    audio_analysis_df <- audio_analysis_df %>%
        select(-audio_analysis) %>%  ## to avoid issues with distinct
        inner_join(album_track_info, by = "track_uri") %>%
        distinct() %>% 
        bind_cols(audio_analysis_col)

    audio_analysis_df
    
}

rush_audio_analysis <- get_audio_analysis_df(rush_albums)

rush_albums %>%
    saveRDS(OUTPUT_FEATURES)

rush_audio_analysis %>%
    saveRDS(OUTPUT_AUDIO_ANALYSIS)

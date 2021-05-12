#' ---
#' titile: Movies passing or failing the Bechdel Test
#' subtitle: R-Ladies Edinburgh 
#' date: 2021 May 12th
#' author: Athanasia Monika Mowinckel
#' ---

library(tidyverse)
library(extrafont)
# extrafont::font_import()
# loadfonts(device = "postscript")

# Set some global vars
bin_breaks <- 10
base_fontsize <- 3
margins <- .3


## Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 11)

## Merge imdb and bechdel data
## Bin years according to bin_breaks variable
bechdel <- tuesdata$raw_bechdel %>% 
  as_tibble() %>% 
  right_join(tuesdata$movies) %>% 
  mutate(
    y = ifelse(binary == "PASS", 1L, 0L),
    year_bin = cut(year, 
                   breaks = seq(1970, 2020, bin_breaks),
                   labels = seq(1970, 2020 - bin_breaks, bin_breaks),
                   include.lowest = TRUE)
  )

# Base palette
palette <- c("#cfafd0", "#b0d0af")

# Separate genre so that we get extra long and tidy data
# Remove NA genre and Docs (too few obs)
# Group by and count number of movies by genre, decase and pass/fall
bechdel_bd <- bechdel %>% 
  separate_rows(genre, sep = ",") %>%
  mutate(genre = str_trim(genre)) %>% 
  filter(genre != "Documentary", !is.na(genre)) %>% 
  group_by(genre, year_bin, binary) %>% 
  tally() %>% 
  mutate(
    N = sum(n),
    pc = n/N,
    pc = ifelse(binary == "FAIL", pc*-1, pc),
    n = ifelse(binary == "FAIL", n*-1, n),
    year_bin_num = as.numeric(year_bin)) %>% 
  ungroup()

# function to neatly display the absolute percent
abs_percent <- function(x){
  scales::percent(abs(x))
}

# Rescale a variable from 0 - 1 to -1 - 1
rescale <- function(x){
  ((x - 0) * (1 - -1) / (1 - 0)) + -1
}

# Create a data set with one row per genre in the end
# With number of movies passing the test, and total number of movies
# Calculate the percent that pass the test,
# Create a variable categorising the genre's Bechdel status
# in the categories.
# Rescale percent to out y-axis so that 0-1, goes from -1 to 1.
bechdel_bd_mean <- bechdel_bd %>% 
  group_by(genre, year_bin) %>%
  summarise(
    N = unique(N),
    pass = sum(ifelse(binary == "PASS", n, 0))
  ) %>% 
  summarise(
    pass = sum(pass),
    n_full = sum(abs(N))
  ) %>% 
  mutate(
    pc_mean = pass/n_full,
    pc_pass = case_when(
      pc_mean > .55 ~ "More pass than fail",
      pc_mean < .45 ~ "More fail than pass",
      TRUE ~ "About equal amounts"
    ),
    pc_pass = factor(pc_pass, 
                     levels = c("More fail than pass",
                                "More pass than fail",
                                "About equal amounts")),
    pc_new = rescale(pc_mean)
  )

# Add to the summary data columns with fonts for each
# genre (ff) and scaling of the font size so they appear
# more or less the same.
bechdel_bd_mean <- bechdel_bd_mean %>% 
  mutate(
    ff = case_when(
      genre == "Romance"   ~ "Great Vibes",
      genre == "Biography" ~ "Great Vibes",
      genre == "History"   ~ "Great Vibes",
      genre == "Comedy"    ~ "Comic Sans MS",
      genre == "Animation" ~ "Pixel Coleco",
      genre == "Sci-Fi"    ~ "Earth Orbiter",
      genre == "Musical"   ~ "RitzFLFCond",
      genre == "Western"   ~ "Carnivalee Freakshow",
      genre == "Horror"    ~ "Zombie Holocaust",
      genre == "Thriller"  ~ "Zombie Holocaust",
      genre == "Sport"     ~ "OLD SPORT 01 COLLEGE NCV",
      genre == "Adventure" ~ "Adventure",
      genre == "Crime"     ~ "The Godfather",
      genre == "War"       ~ "Angkatan Bersenjata",
      genre == "Action"    ~ "Night Traveler Wide Italic",
      genre == "Fantasy"   ~ "Ace Records",
      TRUE  ~  "Helvetica Neue"
    ),
    fs = case_when(
      ff == "Great Vibes"      ~ base_fontsize + 1.5,
      ff == "The Godfather"    ~ base_fontsize + 2.5,
      ff == "Zombie Holocaust" ~ base_fontsize + 1,
      ff == "Carnivalee Freakshow" ~ base_fontsize + 1,
      ff == "RitzFLFCond"       ~ base_fontsize + 1,
      ff == "Ace Records" ~ base_fontsize + 1,
      TRUE ~ base_fontsize
    )
  )

full_plot <- function(){
  ggplot(data = bechdel_bd_mean) + 
    # panel background of genre Bechdel status
    geom_rect(
      alpha = .4,
      aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = -Inf,
        ymax = Inf,
        fill = pc_pass
      )
    ) +
    # Mirrored bar chart
    geom_bar(
      data = bechdel_bd,
      stat = "identity",
      aes(fill = binary,
          x = year_bin, 
          y = pc)
    ) +
    # Genre label above panel
    geom_text(
      aes(label = genre,
          x = max(bechdel_bd$year_bin_num) + 2,
          y = 0,
          family = ff,
          size = I(fs)),
      colour = "white") +
    
    # Horizontal (vertical since flipped) line of mean
    geom_hline(
      aes(yintercept = pc_new)
    ) + 
    
    # Pass/Fail text within plot
    geom_text(
      data = select(bechdel_bd, genre, binary) %>% distinct(),
      size = base_fontsize - 1,
      colour = "grey70",
      vjust = 1,
      aes(
        x = 1990,
        angle = ifelse(binary == "PASS", -90, 90),
        y = ifelse(binary == "PASS", Inf, -Inf),
        label = binary
      )
    ) +
    # Make panels by genre
    facet_wrap(~genre) +
    
    # Flip x and y coordinates, and limit the x-axis
    coord_flip(clip = "off",
               xlim = c(.7, 5.3)) +
    # Scale y to absolute percent
    scale_y_continuous(
      labels = abs_percent
    ) +
    # Define fill colours.
    scale_fill_manual(
      values = c(palette, "grey50","grey70", "white"),
      breaks = c(levels(bechdel_bd_mean$pc_pass), "FAIL", "PASS")
    ) +
    # Add annotations to plot
    labs(
      title = "Movies that pass or fail the Bechdel test\n\n",
      subtitle = "By genre and decade",
      x = "", 
      y = "",
      fill = "",
      caption = stringr::str_wrap("Movies by genre that pass or fail the Bechdel test. Here displayed as the percent of movies that fail or pass for each decade between 1970 and 2020. The vertical line indicates the mean across all decades for each genre, and each genre's background colour indicates whether the genre has more movies that pass the Bechel test than fail (green), more that fail than pass (pink), or have roughly the same amount that pass or fail (grey) across the time span measured.", 100)
    ) +
    theme(
      text = element_text(family = "Helvetica Neue",
                          size = base_fontsize,
                          colour = "grey80"),
      
      plot.title = element_text(size = base_fontsize * 4),
      plot.subtitle = element_text(
        size = base_fontsize * 3,
        vjust = 15,
        face = "italic"),
      plot.caption = element_text(size = base_fontsize+2),
      plot.background = element_rect(fill = "black"),
      plot.margin = unit(c(margins, margins*2, 
                           margins, margins*2), "cm"),
      
      panel.background = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(margins*2, "cm"),
      
      strip.background = element_blank(),
      strip.text = element_blank(),
      
      legend.position = c(0.09, -0.15),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(margins, "cm"),
      legend.text = element_text(size = base_fontsize+2),
      
      axis.ticks = element_blank(),
      axis.text = element_text(colour = "grey70",
                               size = base_fontsize)
    ) +
    guides(fill = guide_legend(ncol = 2))
}

full_plot()

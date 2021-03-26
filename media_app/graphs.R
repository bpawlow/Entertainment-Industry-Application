# Loaded Packages & Other R Scripts----

library(tidyverse)
library(magick)
library(ggthemes)
library(dplyr)
library(viridis)  
library(textclean)

### Loading Data Cleaning R Script

source("data_cleaning.R")

# Movie Graphs ---- 

# Gross revenue for all years 
image1 <- image_read("www/top_movies.jpg")

plot_1 <- movies %>% 
  group_by(year) %>% 
  summarize(
    total_rev = sum(gross)/1000000000
  ) %>% 
  ggplot(aes(x = year, y = total_rev)) +
    geom_bar(stat = 'identity', fill = "darkorchid1", 
             color = 'black') +
    geom_line(color = 'red', size = 1.5) +
    geom_abline(intercept = -244.22292, 
                slope = 0.12455333333, 
                linetype = 'dashed', 
                color = 'black', size = 2) +
    geom_abline(intercept = -87.6706002674, 
              slope = 0.04572537777, 
              linetype = 'dashed', 
              color = 'yellow', size = 2) +
    annotate(geom = "text", label = 'Average Inflation Rate', x = 2008,
           y = 6.75, size = 8, color = 'black', angle = 10) +
    annotate(geom = "text", label = 'Average World Population Growth Rate', x = 2008,
           y = 3.5, size = 8, color = 'yellow', angle = 3) +
    xlab("Year") +
    ylab("Total Gross Revenue (In Billions)") +
    labs(
    title = "Total Gross Revenue of Released Movies from 1986-2016"
    ) +
    scale_x_continuous(
      name = "Year",
      breaks = 1986:2016,
      expand = c(0,0.25)
    ) +
    theme_dark() +
    theme(
      plot.title = element_text(face = "bold", size = 18) 
    ) 
    #Possibly fix dimensions of image
    grid::grid.raster(image1, x = 0.075, y = 0.5, just = c('left', 'bottom'), width = unit(2.75, 'inches'))

# Top ten movies for specific years (animated on RShiny)
    
# Based on gross revenue 
    
   inp_2 <- movies %>% 
      arrange(desc(gross)) %>% 
      group_by(year) %>%
      slice(1:10) %>% 
      mutate(
        gross_mil = gross/1000000,
      ) %>% 
     filter(country == "USA", year == 2008)
   
    plot_2 <- ggplot(inp_2, aes(x = reorder(name, -gross_mil), y = gross_mil)) +
      geom_bar(aes(fill = genre), stat = 'identity', 
               color = 'black') +
      geom_text(aes(label = round(gross_mil)), vjust = 1.5, size = 6) +
      scale_fill_discrete(name = "Genre") + 
      xlab("Year") +
      ylab("Gross Revenue (In Millions)") +
      labs(
        title = "Top Ten Movies with Largest Gross Revenue For the Given Year"
      ) +
      theme_dark() +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        axis.text.x = element_text(angle = 25, vjust = 0.60, size = 10), 
        axis.title = element_text(size = 14),
        legend.title = element_text(face = "bold")
      ) 
    
    # Based on rating scores
    
   inp_3 <- movies %>% 
      arrange(desc(score)) %>% 
      group_by(year) %>%
      slice(1:10)
      #filter(year == "1986") %>% #need to change when animated
   
    plot_3 <- ggplot(inp_3, aes(x = reorder(name, -score), y = score)) +
      geom_bar(aes(fill = genre), stat = 'identity', 
               color = 'black') +
      geom_text(aes(label = score), vjust = 1.5, size = 7) +
      scale_fill_discrete(name = "Genre") + 
      xlab("Year") +
      ylab("IMDb User Rating") +
      labs(
        title = "Top Ten Movies Based on IMDb User Rating For the Given Year"
      ) +
      theme_dark() +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        axis.text.x = element_text(angle = 25, vjust = 0.60, size = 10), 
        axis.title = element_text(size = 14),
        legend.title = element_text(face = "bold")
      ) 

## Correlation with budget and revenue and score rating 
    
   plot_4 <- movies %>% 
      mutate(
        gross_mil = gross/1000000,
        budget_mil = budget/1000000
      ) %>%
      ggplot(aes(x = budget_mil, y = gross_mil)) + # change y variable based on 
      #animation selection
      geom_point(color = 'springgreen4', shape = 3, size = 1.5) +
      geom_smooth() +
      xlab("Budget (In Millions)") +
      ylab("Gross Revenue (In Millions)") +
      labs(
        title = "Correlation Between Budget of Movie and Gross Revenue" 
        # change title based on animation selection
      ) + 
      theme_classic() +
      theme(
        plot.title = element_text(face = "bold", size = 18), 
        axis.title = element_text(size = 14)
      ) 
    
   plot_5 <- movies %>% 
      mutate(
        budget_mil = budget/1000000
      ) %>%
      ggplot(aes(x = budget_mil, y = score)) + # change y variable based on 
      #animation selection
      geom_point(color = "red", shape = 8, size = 1.75) +
      xlab("Budget (In Millions)") +
      ylab("IMDb User Rating (1-10)") +
      labs(
        title = "Correlation Between Budget of Movie and IMDb User Rating" 
        # change title based on animation selection
      ) + 
      theme_classic() +
      theme(
        plot.title = element_text(face = "bold", size = 18), 
        axis.title = element_text(size = 14)
      ) 

# Pie chart of film countries of production
    
    #distributions of movies by country 
   inp_6 <- movies %>% 
      group_by(country) %>% 
      summarize(movies = n()) %>% 
      arrange(desc(movies))
    
    plot_6 <- ggplot(movies, aes(x=factor(1), fill=country)) +
      geom_bar(width=1, color="white") +
      coord_polar("y") + # remove background, grid, numeric labels
      annotate(geom = "text", label = 'atop(bold("USA"))', x = 0.9,
               y = 2000, size = 12, parse = TRUE) +
      annotate(geom = "text", label = "4872 Movies", 
               x = 0.8, y = 3000, size = 8, color = 'gray30') +
      labs(fill = "Countries") +
      theme_void() + 
      annotate(geom = "text", label = 'atop(bold("USA"))', x = 0.9,
               y = 2000, size = 12, parse = TRUE) +
      theme(
        plot.title = element_text(face = "bold", size = 20),
        plot.background = element_rect(fill = "lightsteelblue1"),
        legend.title = element_text(face = "bold", size = 14)
      ) +
      labs(
        title = "Distribution of Movies by Country in the Dataset (1986-2016)" 
      )
    
# Different genres of movies by year
  inp_7 <- movies %>% 
      group_by(year, genre) %>% 
      summarize(movies = n()) %>% 
      arrange(desc(movies))
  
  # movies %>% 
  # filter (year == 2010) %>% # change based on input 
    
  plot_7 <-   
    ggplot(inp_7, aes(x=factor(1), y = movies, fill=genre)) +
      geom_bar(stat = 'identity', color="white") +
      coord_polar("y") + # remove background, grid, numeric labels
      labs(fill = "Movie Genres") +
      theme_void() + 
      theme(
        legend.title = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 21),
        plot.background = element_rect(fill = "lightsteelblue1")
      ) +
      labs(
        title = "Proportion of Movie Genres By Year" 
      ) +
    annotate(geom = "text", label = "2010", 
             x = 0.2, y = 1, size = 8) #adjust based on input year
      
# Frequency of Budgets/length of movies
    
    plot_8 <- movies %>% 
      mutate(
        budget_mil = budget/1000000
      ) %>%
      filter(budget > 0) %>%  # missing data likely comes from 0 for budget
      ggplot(aes(x = budget_mil)) +
      #animation selection
      geom_freqpoly(size = 1.25) +
      xlim(c(0, 200)) +
      xlab("Budget (In Millions)") +
      ylab("Number of Movies") +
      labs(
        title = "Most Frequent Budget Allocation Amounts for Movies" 
        # change title based on animation selection
      ) + 
      theme_economist() +
      theme(
        plot.title = element_text(face = "bold.italic", size = 18, 
                                  color = 'blue'), 
        axis.title = element_text(size = 14, face = 'bold.italic')
      )
    
    plot_9 <- movies %>% 
      mutate(
        budget_mil = budget/1000000
      ) %>%
      filter(runtime > 50) %>%  # missing data likely comes from 0 for budget
      ggplot(aes(x = runtime)) +
      #animation selection
      geom_freqpoly(size = 1.25) +
      xlim(c(50, 200)) +
      xlab("Runtime (In Minutes)") +
      ylab("Number of Movies") +
      labs(
        title = "Most Frequent Runtimes in Movies" 
        # change title based on animation selection
      ) + 
      theme_economist() +
      theme(
        plot.title = element_text(face = "bold", size = 21, color = 'blue'), 
        axis.title = element_text(size = 14, face = 'bold.italic')
      )
    
# Music Graphs ---- 
    
    #sales by volume over time 
   plot_10 <- music %>% 
      ggplot(aes(x = year, y = value_actual, fill = format)) +
      geom_bar(stat = 'identity') +
      xlab("Year") +
      ylab("Music Sales by Volume (in millions)") +
      labs(
        title = "Music Sales by Volume Over Time (1973-2019)" 
      ) +
      scale_fill_viridis(discrete = TRUE, name = 'Music Format') +
      scale_x_continuous(breaks = 1973:2019,
                         expand = c(0,0.25)) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_text(face = 'bold', size = 18), 
        plot.background = element_rect(fill = "palegoldenrod", size = 2), 
        plot.title = element_text(face = "bold", size = 20), 
        axis.title = element_text(face = "bold.italic", size = 13),
        axis.text.x = element_text(angle = 25, vjust = 0.60, face = 'bold')
      ) 
    
  format_yearly <- music %>% 
      group_by(year, format) %>% 
      summarize(music_sales = sum(value_actual)) %>% 
     # filter(year == 1999) %>% # change with animations 
      arrange(desc(music_sales))
    
  plot_11 <- format_yearly %>% 
      ggplot(aes(x=factor(1), y = music_sales, fill=format)) +
      geom_bar(stat = 'identity', width=1, color="black") +
      coord_polar("y") + # remove background, grid, numeric labels
      labs(fill = "Music Format") +
      theme_void() +
      theme(
        legend.title = element_text(face = 'bold', size = 18), 
        plot.background = element_rect(fill = "palegoldenrod", size = 2), 
        plot.title = element_text(face = "bold", size = 20), 
        axis.title = element_blank()
      )  +
      labs(
        title = "Proportion of Movie Genres By Year" 
      ) +
      annotate(geom = "text", label = "1999", 
               x = 0.2, y = 1, size = 6) #adjust based on input year
    
### Spotify rankings 
    
# Top ten popular songs (1970-2021)
    
    spotify_summary <- spotify_old %>% 
      arrange(desc(popularity)) %>% 
      group_by(year) %>%
      slice(1:10) %>% 
      mutate(
        name = toupper(name)
      ) %>% 
      filter(year == 2021) %>% #need to change when animated
      mutate(
        label = 1:10
      )
    
      legend_ord <- levels(with(spotify_summary, reorder(label, -popularity)))
     
      ggplot(spotify_summary, aes(x = reorder(label, -popularity), 
                                  y = popularity, order = label)) +
      geom_bar(aes(fill = name), 
               stat = 'identity', 
               color = 'black') +
      geom_text(aes(label = popularity, vjust = 1.5, size = 6)) +
      annotate(geom = "text", label = toString(spotify_summary$year[1]), x = 9.5,
                 y = 92, size = 12) +
      xlab("Song Labels") +
      ylab("Popularity Rating (1-100 Scale)") +
      labs(
        title = "Top Ten Songs Based on Popularity Rating For Given Release Year",
        fill = "Song Titles"
      ) +
      theme_stata() +
      theme(
        plot.title = element_text(face = "bold", size = 24),
        axis.text.x = element_blank(), 
        axis.title = element_text(face = "bold", size = 14),
        legend.title = element_text(face = "bold"), 
        axis.title.x = element_blank(), 
        axis.ticks.x = element_blank()
      )  +
        scale_fill_discrete(breaks = spotify_summary[['name']]) +
        guides(fill = guide_legend(ncol = 2))
      
  # Difference in danceability/loudness/energy of songs based on release date
      #(1920 - 2021)
      
      
     spotify_prop <- spotify_old %>%
        arrange(desc(popularity)) %>% 
        group_by(year) %>%
        slice(1:50)
     
    plot_14 <- spotify_prop %>% 
        ggplot(aes(x = year, y = danceability)) + # change y variable based on 
        #animation selection
        geom_point(shape = 18, color = 'mediumslateblue', size = 3) +
        geom_smooth(color = 'black', size = 1.25) +
        xlab("Year") +
        ylab("Danceability (0-1 Scale)") + # change based on input 
        labs(
          title = "Change in Danceability of Top 50 Songs Over Time" 
          # change title based on animation selection
        ) + 
        theme_stata() +
        theme(
          plot.title = element_text(face = "bold", size = 18), 
          axis.title = element_text(size = 14)
        )
        
    
    plot_15 <- spotify_prop %>% 
      ggplot(aes(x = year, y = energy)) + # change y variable based on 
      #animation selection
      geom_point(shape = 18, color = 'mediumslateblue', size = 3) +
      geom_smooth(color = 'black', size = 1.25) +
      xlab("Year") +
      ylab("Energy (0-1 Scale)") + # change based on input 
      labs(
        title = "Change in Energy of Top 50 Songs Over Time" 
        # change title based on animation selection
      ) + 
      theme_stata() +
      theme(
        plot.title = element_text(face = "bold", size = 18), 
        axis.title = element_text(size = 14)
      )
        
    plot_16 <- spotify_prop %>% 
      ggplot(aes(x = year, y = loudness)) + # change y variable based on 
      #animation selection
      geom_point(shape = 18, color = 'mediumslateblue', size = 3) +
      geom_smooth(color = 'black', size = 1.25) +
      xlab("Year") +
      ylab("Loudness (dB)") + # change based on input 
      labs(
        title = "Change in Loudness of Top 50 Songs Over Time" 
        # change title based on animation selection
      ) + 
      theme_stata() +
      theme(
        plot.title = element_text(face = "bold", size = 18), 
        axis.title = element_text(size = 14)
      )

          ### Different graphs will be shown in animation of RShiny ###
      
# Video Games Graphs ----  
      
      #Cumulative sales (sum over time)
      
      summary_vg_sales <- vg_sales %>% 
        group_by(platform) %>% 
        summarize(
          tot_global_sales = sum(global_sales),
        ) %>% 
        arrange(platform) %>% 
        filter(tot_global_sales > 2)
      
      plot_17 <- summary_vg_sales %>% 
        ggplot(aes(x=platform, y = tot_global_sales)) +
        geom_bar(stat = 'identity', width=0.8, fill="brown") +
        geom_text(aes(label = round(tot_global_sales)), hjust = -0.5, size = 5) +
        theme_wsj() +
        theme(
          plot.title = element_text(size = 21),
          axis.title = element_text(),
          axis.title.y = element_text(size = 14, face = 'bold.italic'),
          axis.title.x = element_text(size = 14, face = 'bold.italic')
        ) +
        coord_flip() + 
        xlab("Gaming Consoles") +
        ylab("Game Copies Sold (In Millions)") + 
        labs(
          title = "Total Video Game Copies Sold By Gaming Console (1980 - 2016)" 
          # change title based on animation selection
        )
  
      #Sales grouped by console each year (ANIMATED)
      
      year_sales_console <- vg_sales %>% 
        filter(year == 2005) %>%  #change in animation based on the input 
        group_by(platform) %>% 
        summarize(
          year = year,
          year_global_sales = sum(global_sales),
        ) %>% 
        unique() %>% 
        arrange(platform) %>% 
        filter(year_global_sales > 2)
       
    
      year_sales_console %>% 
        ggplot(aes(x=platform, y = year_global_sales)) +
        geom_bar(stat = 'identity', width=0.8, fill="brown") +
        geom_text(aes(label = round(year_global_sales)), hjust = -0.5, size = 5) +
        ylim(0,250) +
        theme_wsj() +
        theme(
          plot.title = element_text(size = 21),
          axis.title = element_text(),
          axis.title.y = element_text(size = 14, face = 'bold.italic'),
          axis.title.x = element_text(size = 14, face = 'bold.italic')
        ) +
        coord_flip() + 
        annotate(geom = "text", label = toString(year_sales_console$year[1]), x = 0.75,
                 y = 245, size = 10, color = 'blue') +
        xlab("Gaming Consoles") +
        ylab("Game Copies Sold Globally (In Millions)") + 
        labs(
          title = "Total Video Game Copies Sold Globally By Gaming Console" 
          # change title based on animation selection
        ) 
   
      ## Increase in sales for specific platforms 
      
      vg_sales %>% 
        group_by(platform, year) %>% 
        summarize(
          global_sales = sum(global_sales)
        ) %>% 
        filter(year >= 2000, global_sales > 1) %>%  #Change year based on slider inputs
        ggplot(aes(x = year, y = global_sales, group = platform)) +
          geom_line(aes(color = platform), size = 1.25) +
        theme_economist_white() +
        theme(
          plot.title = element_text(size = 21),
          axis.title.y = element_text(size = 14, face = 'bold.italic'),
          axis.title.x = element_text(size = 14, face = 'bold.italic'),
          legend.title = element_text(size = 14, face = 'bold.italic')
        ) +
        xlab("Year") +
        ylab("Game Copies Sold Globally (In Millions)") + 
        labs(
          title = "Video Game Copies Sold Globally Over Time (1980-2016)",
          color = "Console Platform"
        ) 
      
      ## Top Games Every Year 
      
      vg_sales %>% 
        group_by(year, name) %>%
        summarize(
           global_sales = sum(global_sales),
           genre = genre
        ) %>% 
        unique() %>% 
        arrange(desc(global_sales)) %>% 
        filter(year == 2016) %>% #need to change when animated
        head(10) %>% 
        ggplot(aes(x = reorder(name, -global_sales), y = global_sales)) +
        geom_bar(aes(fill = genre), stat = 'identity', 
                 color = 'black') +
        geom_text(aes(label = global_sales), vjust = 1.5, size = 6) +
        scale_fill_discrete(name = "Genre") + 
        xlab("Year") +
        ylab("Gross Revenue (In Millions)") +
        labs(
          title = "Top Ten Movies with Largest Gross Revenue For the Given Year"
        ) +
        theme_par() +
        theme(
          plot.title = element_text(vjust = 4, face = "bold", size = 21),
          plot.background = element_rect(fill = "lightgoldenrodyellow", size = 2), 
          axis.text.x = element_text(angle = 25, vjust = 0.60, size = 10), 
          axis.title = element_text(size = 14, face = 'bold'),
          legend.title = element_text(face = "bold")
        ) 

      
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
# library(DT)
library(ggplot2)
# library(car)
# library(nortest)
# library(tseries)
# library(RcmdrMisc)
# library(lmtest)

# Importing Graphs R-Script

source('graphs.R')

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),

    # Application title
    titlePanel(h1("A Deep Dive Into The Entertainment Industry", align = "center")),
    
    navbarPage("Three Changing Components of Entertainment Industry",
               tabPanel(icon("home"),
                        
                        fluidRow(HTML('<center><img src="main_entertainment.jpg"
                                      width="1000"></center>'),
                                 br(),
                                 column(
                          
                                    br(),
                                    p(strong("\"You and I and everybody in show business and the entertainment industry fly by the seat of our pants. 
                                             We don't know quite what is going to happen.\" - William Shatner"), 
                                              style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px; font-size:32px"),
                                     br(),
                                    
                                     h2("Why this Topic?", align = 'center'), 
                          
                                      p("The main reason I chose this data is because I am fascinated with how the media has influenced my childhood and has affected the lives of 
                                        many Americans over the years. I also wanted to take a fiscal 
                                        approach and compare different revenues and sales for specific products over the course of multiple decades.
                                        By combining these different topics for my data analysis and visualization I can further understand 
                                        how our entertainment has shifted to fit a more digitized world. 
                                        Here are some additional questions of interests that I would consider exploring:
                                    ",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px; font-size:21px"),
                                     br(),
                                     p('How have the sales of different entertainment products changed over time?', 
                                       align = 'center',
                                       style='font-size: 32px; color:black;background-color:papayawhip;padding:15px;border-radius:10px;'), 
                                     br(),
                                     p('What are some common themes/genres/topics that fuel success in sales in the entertainmnet industry?', 
                                       align = 'center',
                                       style='font-size: 32px; color:black;background-color:papayawhip;padding:15px;border-radius:10px;'),
                                    width=12)
                        ), 
                        hr(),
                        p(em("Developed by"),br("Bradley Pawlow"),style="text-align:center; font-family: times"),
                        class = 'flex-center'
               ),
    tabPanel("Movies",
                        tags$style(
                          "li a {
        font-size: 32px;
        font-weight: bold;
      }
    "), 
            fluidRow(column(width=2),
             column(
               h4(p("The movie industry has been pivotal in the growth of the entertainment industry 
                    dating back to the 1970s and 1980s. During this time there was an introduction to
                    more 'high-concept' films in which writers would develop more complex plotlines and conflict 
                    points within their films than before. Moreover, filming cameras and equipment became more 
                    technologically advanced to the point where visual graphics were more realistic than decades 
                    prior. As we have seen today, movie access has shifted more to on-demand streaming services 
                    such as Netflix, Hulu, Disney+, etc. ",style="color:black;text-align:center")),
               width=8,style="background-color:lavender;border-radius: 10px")
    ),      
              br(),
    h1(p("Dataset (Scraped from IMDb, Imported from Kaggle)"), align = "center"), 
              DT::dataTableOutput("movietable"),
    br(),
              plotOutput("firstPlot"),
    br(),
    plotOutput("fourthPlot"),
    br(),
    plotOutput("fifthPlot"),
    br(),
    br(),
    h1(p("Distribution of Movies by Country and Genre"), align = "center"), 
    br(), 
    sidebarLayout(
      position = "right",
      sidebarPanel(
        img(src = "top_movies.jpg", height = 350, width = 400),
      ),
      mainPanel(
        plotOutput("sixthPlot")
      )
    ),
    br(),
    sidebarLayout(
      position = "right",
      sidebarPanel(
        sliderInput("year",
                    "Select Year:",
                    min = 1986,
                    max = 2016,
                    value = 2016,
                    animate = TRUE,
                    format = "####",
                    sep = "")
      ),
      mainPanel(
        plotOutput("seventhPlot")
      )
    ),
    br(),
    br(),
    h1(p("Budget and Runtime of Movies"), align = "center"), 
    plotOutput("eighthPlot"),
    br(), 
    plotOutput("ninthPlot")
  ),
    tabPanel("Music",
             fluidRow(column(width=2),
                      column(
                        h4(p("The music industry has evolved drastically as cultures 
                             around the world have changed. What was considered popular music to our 
                             parents and grandparents back then is certainly not the same as 
                             popular music now. With the continual improvements in music technology, including
                             electronic beat production, autotune, and music streaming services, we are seeing
                             a new era of musical entertainment where it is much easier to access and produce 
                             our favorite songs.",style="color:black;text-align:center")),
                    width=8,style="background-color:lavender;border-radius: 10px")
             ),      
             br(),
             h1(p("Spotify Dataset (Scraped from Spotify APIs, Imported from Kaggle)"), align = "center"), 
             DT::dataTableOutput("spotify"),
             br(),
             h1(p("Music Industry Sales Dataset (Scraped from US Sales Database RIAA, Imported from Kaggle)"), align = "center"), 
             DT::dataTableOutput("musicsales"),
             br(), 
             plotOutput("tenthPlot"), 
             br(),
             sidebarLayout(
               position = "left",
               sidebarPanel(
                 sliderInput("year2",
                             "Select Year:",
                             min = 1973,
                             max = 2019,
                             value = 2019,
                             animate = TRUE,
                             format = "####",
                             sep = ""), 
                 br(),
                 img(src = "vinyl.jpg", height = 350, width = 400)
               ),
               mainPanel(
                 plotOutput("eleventhPlot")
               )
             ),
             br(),
             h1(p("It's All About Spotify Now"), align = "center"), 
             br(), 
             # sidebarLayout(
             #   position = "right",
             #   sidebarPanel(
             #     #drop down widget to select x variable
             #     selectInput(
             #       inputId = "measure",
             #       label = "Select Track Property:",
             #       choices = list(
             #         "Danceability" = "danceability",
             #         "Energy" = "energy",
             #         "Loudness" = "loudness"
             #       )
             #     )
             #   ),
             #   mainPanel(
             #     plotOutput("thirteenthPlot")
             #   )
             # ),
             plotOutput("fourteenthPlot"),
             br(),
             plotOutput("fifteenthPlot"),
             br(),
             plotOutput("sixteenthPlot")
             ),
    tabPanel("Video Games",
             fluidRow(column(width=2),
                      column(
                        h4(p("The video game industry has taken the younger generation by
                             storm. Video games have become a popular hobby for many teenagers, especially
                             during a pandemic that left everyone inside their homes. Since the early video games designed 
                             (such as Pac-Man), developers 
                             have been able to create more difficult and complex video games. Moreover,
                             gaming consoles have improved drastically to run these high-memory video games 
                             with better processors and memory storage properties. Let's see how the sales faired.", style="color:black;text-align:center")),
                        width=8,style="background-color:lavender;border-radius: 10px")
             ), 
             br(),
             h1(p("Video Game Sales Dataset (Scraped from VGChartz, Imported from Kaggle)"), align = "center"), 
             DT::dataTableOutput("videogamesales"),
             br(),
             plotOutput("seventeenthPlot")
             )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$movietable = DT::renderDataTable({
    movies
  })
  
  output$spotify = DT::renderDataTable({
    spotify_old
  })
  
  output$musicsales = DT::renderDataTable({
    music
  })

  output$videogamesales = DT::renderDataTable({
    vg_sales
  })
  
  output$firstPlot <- renderPlot({
    plot_1
  })
  
  # inp_3 <- reactive({
  #   inp_3 %>%
  #    filter(year == input$year)
  # })

  output$secondPlot <- renderPlot({
    plot_3
  })
  
  output$fourthPlot <- renderPlot({
    plot_4
  })
  
  output$fifthPlot <- renderPlot({
    plot_5
  })
  
  output$sixthPlot <- renderPlot({
    plot_6
  })
  
  
  output$seventhPlot <- renderPlot({
    inp_7 <- inp_7 %>%
      filter(year == input$year)
    
    ggplot(inp_7, aes(x=factor(1), y = movies, fill=genre)) +
      geom_bar(stat = 'identity', width=1, color="white") +
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
      annotate(geom = "text", label = toString(input$year), 
               x = 0.2, y = 1, size = 8) #adjust based on input year
  })
  
  output$eighthPlot <- renderPlot({
    plot_8
  })
  
  output$ninthPlot <- renderPlot({
    plot_9
  })
  
  output$tenthPlot <- renderPlot({
    plot_10
  })
  
  output$eleventhPlot <- renderPlot({
    format_yearly <- format_yearly %>%
      filter(year == input$year2)
    
    format_yearly %>% 
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
        title = "Proportion of Music Formats By Year" 
      ) +
      annotate(geom = "text", label = toString(input$year2), 
               x = 0.2, y = 1, size = 6) #adjust based on input year

  }
)
  output$thirteenthPlot <- renderPlot({
    
    # #y-axis label
    # y_label <- switch(input$measure,
    #                   "danceability" = "Danceability (0-1 Scale)",
    #                   "energy" = "Energy (0-1 Scale)",
    #                   "loudness" = "Loudness (dB)"
    # )
    # 
    # input_y <- case_when(
    #   input$measure == "danceability" ~ pull(spotify_prop, danceability),
    #   input$measure == "energy" ~ pull(spotify_prop, energy),
    #   input$measure == "loudness" ~ pull(spotify_prop, loudness)
    # )
    # 
    # y <- input_y
    # 
    #   ggplot(spotify_prop, aes(x = year, y = y)) + # change y variable based on 
    #   #animation selection
    #   geom_point(shape = 18, color = 'mediumslateblue', size = 3) +
    #   geom_smooth(color = 'black', size = 1.25) +
    #   xlab("Year") +
    #   ylab(y_label) + # change based on input 
    #   labs(
    #     title = "Change in Track Properties Over Time For Top 50 Popular Songs on Spotify" 
    #     # change title based on animation selection
    #   ) + 
    #   theme_stata() +
    #   theme(
    #     plot.title = element_text(face = "bold", size = 18), 
    #     axis.title = element_text(size = 14)
    #   )
  })
  
  output$fourteenthPlot <- renderPlot({
    plot_14
  })
  
  output$fifteenthPlot <- renderPlot({
    plot_15
  })
  
  output$sixteenthPlot <- renderPlot({
    plot_16
  })
  output$seventeenthPlot <- renderPlot({
    plot_17
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

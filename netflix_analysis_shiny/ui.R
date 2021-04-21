# Define UI for application that draws a histogram
shinyUI(
  
  dashboardPage(
    
    dashboardHeader(title = "Netflix Analysis"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = "Content Amount",
                 icon = icon("chart-line"),
                 tabName = "Content_Amount"),
        menuItem(text = "Top 20 Charts", 
                 icon = icon("dna"), 
                 tabName = "Top_20",
                 menuSubItem(text = "Genres", 
                             tabName = "Genres"),
                 menuSubItem(text = "Countries",
                             tabName = "Countries")),
        menuItem(text = "Movie Duration",
                 icon = icon("clock"),
                 tabName = "Movie_Duration"),
        menuItem(text = "Raw Data", 
                 icon = icon("table"),
                 tabName = "Raw_Data"),
        menuItem(text = "About Me",
                 icon = icon("child"),
                 tabName = "About_Me",
                 menuSubItem("LinkedIn",
                             icon = icon("linkedin"),
                             href = "https://www.linkedin.com/in/trianinarita/"),
                 menuSubItem("Github",
                             icon = icon("github"),
                             href = "https://www.github.com/trianinarita"),
                 menuSubItem("E-mail",
                             icon = icon("envelope"),
                             href = "mailto:trianinarita@gmail.com"),
                 menuSubItem("Twitter",
                             icon = icon("twitter"),
                             href = "https://twitter.com/trianinarita"))
        )
      ),
    
    body = shinydashboard::dashboardBody(
      
      shinyDashboardThemes("purple_gradient"),
      
       tabItems(
        
        tabItem(tabName = "Content_Amount",
                align = "center",
                h2("Content Amount in Each Year"),
                selectInput(
                            inputId = "content_country", label = "Select Country",
                            choices = unique(amount_by_country$country),
                            selected = c("Indonesia", "United States"),
                            multiple = T),
                selectInput(
                            inputId = "content_types", label = "Select Category",
                            choices = unique(amount_by_country$type),
                            selected = c("Movie"),
                            multiple = F),
                plotlyOutput("plot_contentyear"),
                br(),
                fluidRow(
                  column(width = 6, 
                         valueBox(subtitle = "Total Movies on Netflix", 
                                  color = "teal",
                                  value = number((type_movies$n), accuracy = 1, big.mark = ","),
                                  icon = icon("film"), 
                                  width = 12)
                  ),  
                  column(width = 6, 
                         valueBox(subtitle = "Total TV Shows on Netflix", 
                                  color = "light-blue",
                                  value = number((type_tvshows$n), accuracy = 1, big.mark = ","),
                                  icon = icon("tv"),
                                  width = 12)
                  ))
        ),
  
        tabItem(tabName = "Genres",
                align = "center",
                h2("Top 20 Genres"),
                # p("Plot ini menggambarkan top 20 genres yang ada pada Netflix."),
                plotlyOutput("plot_20genres")
        ),
        
        tabItem(tabName = "Countries",
                align = "center",
                h2("Top 20 Countries"),
                # p("Plot ini menggambarkan top 20 countries yang ada pada Netflix."),
                plotlyOutput("plot_20countries")
        ),
        
        tabItem(tabName = "Movie_Duration",
                align = "center",
                h2("Movie Duration in Top 10 Countries"),
                plotlyOutput("plot_movieduration")
        ),
        
        tabItem(tabName = "Raw_Data",
              tags$h4("Data Source"),
              "Kaggle:",
               tags$a(href="https://www.kaggle.com/shivamb/netflix-shows",
                      "https://www.kaggle.com/shivamb/netflix-shows"),
              br(),
              p("Data Update: January 2021"),
              downloadButton(outputId = "download", label = "Download Data"),
              br(),
              br(),
              DT::dataTableOutput("data_table")
        )
      )
    ),
    # rightsidebar = NULL,
    # enable_preloader = TRUE,
    # loading_duration = 1,
    footer = shinydashboardPlus::dashboardFooter(
      left = tagList(
        tags$b("Created By:"),
        br(),
        tags$i("Triani Narita, 2021")
      )
    )
  )
)
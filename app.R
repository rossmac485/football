# Load packages ----
library(shiny)
library(readr)
library(tidyverse)
library(data.table)

# Source helpers ----

# Load data ----
datalist = list()
for (i in 2012:2020) {
  dat <- read_csv(paste("https://raw.githubusercontent.com/ewenme/transfers/master/data/", i,"/english_premier_league.csv",sep = ""))
  datalist[[i]] <- dat
}
df <- dplyr::bind_rows(datalist)

# Filter out free transfers and missing fee data, summer transfers only
# Simplify position field
df_fee = df %>% filter(!is.na(fee_cleaned) & fee_cleaned > 0 & transfer_period == 'Summer') %>%
  mutate(position_simple = case_when(
    grepl("Midfield",position) ~ "Midfielder",
    grepl("Back",position) ~ "Defender",
    grepl("Goalkeeper",position) ~ "Keeper",
    TRUE ~ "Forward"))

# Prep color palette
color.function <- colorRampPalette( c( "#CCCCCC" , "#104E8B" ) )
color.ramp <- color.function( n = length(unique(df_fee$year)) )
palette(color.ramp)
# df_fee$color <- as.factor(df_fee$year)
# color_easy = c(color.ramp)[df_fee$color]

# User interface ----
ui <- fluidPage(
  titlePanel(title = h1("Premier League Transfers", align="center")),
  sidebarPanel(
    # img(src = "football.png", height = "10%", width = "10%", align = "center"),
    
# Top X slider 
    sliderInput("x", label = h3("Show me the top X players..."), min = 1, 
                max = 20, value = 5),

# Season selector  
    checkboxGroupInput("season", label = h4("...transferred in season Y"),
                       choices = sort(unique(df_fee$year)),
                       selected = unique(df_fee$year)),

# Transfer type selector    
    checkboxGroupInput("transfer", label = h4("...with transfer type"),
                 choices = c("in", "out"),
                 selected = "in"),

# Position selector  
    checkboxGroupInput("position", label = h4("...playing in position"),
                 # choices = sort(unique(df_fee$position_simple)),
                 # selected = unique(df_fee$position_simple)),
                 choices = c("Keeper", "Defender", "Midfielder", "Forward"),
                 selected = c("Keeper", "Defender", "Midfielder", "Forward")),

# Age selector
    sliderInput("age", label = h4("...aged between"), min = min(df_fee$age), 
                max = max(df_fee$age), value = c(min(df_fee$age),max(df_fee$age)))
    
    ),
  
  mainPanel(
    # img(src = "football.png", height = "25%", width = "25%"),
    plotOutput("bar",height = 500))
)

# Server logic
server <- function(input,output){
  
  reactive_data <- reactive({
    choice_number <- input$x
    choice_season <- input$season
    choice_transfer <- input$transfer
    choice_position <- input$position
    choice_age <- input$age
    return(
      df_fee %>% filter(
          transfer_movement %in% choice_transfer 
            & position_simple %in% choice_position
            & year %in% choice_season
            & between(age,choice_age[1],choice_age[2])
          ) %>%
        slice_max(fee_cleaned, n = choice_number)
      )
  })
  
  output$bar <- renderPlot({
    
    our_data <- reactive_data()
    
    chart <- barplot(as.numeric(our_data$fee_cleaned),names.arg = our_data$player_name,
            # xlab = "Player",
            ylab = "Fee, GBPm",
            # col = our_data$year,
            # main = "Top transfers, summer 2020",
            # border = "red",
            # las = 2,
            # mar=100,
            xaxt ="n"
            ) 
    labs <- paste(our_data$player_name,
                  our_data$club_name,
                  our_data$club_involved_name,
                  sep="\n")
    text(cex=1, x=chart, y=max(our_data$fee_cleaned)/50, labs, xpd=TRUE, srt=90, adj=0)
  })
}

# Run the app
shinyApp(ui, server)

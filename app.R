
#setwd("~/node-test/WZ_Stats")

#rsconnect::deployApp('~/node-test/WZ_Stats')

# Shiny web application for Warzone stats

library(shiny)
require(shinydashboard)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(reshape2)
library(rvest)
library(plotly)

api_data <- read_csv("api_data.csv")
last_week <- read_csv("last_week.csv")
last_week <- filter(last_week,is.na(kills)==F)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Warzone stats")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
    menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Player - All time", tabName = "player", icon = icon("user", lib='glyphicon')),
    menuItem("Player - Latest week", tabName = "latest", icon=icon("flash")),
    menuItem("Github", icon = icon("send",lib='glyphicon'), 
             href = "https://github.com/wejm")
    
  )
)
#Overview page set up

#KPI boxes
frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
  
  box(
    title = "KD"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("kdbyplayer", height = "300px")
  )
  
  ,box(
    title = "Win percentage"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("winpcbyplayer", height = "300px")
  ) 
  
)

#Player page set up

selector <- selectInput("playerid","Select player",sort(api_data$player))

gaugerow <- fluidRow(
  
  box(
    title = "KD"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("gauge")
  )
  
  ,box(
    title = "Win percentage"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("gauge2")
  ) 
  
)

gaugerow2 <- fluidRow(
  
  box(
    title = "Top 5 conversion %"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("gauge3")
  )
  
  ,box(
    title = "Average score"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("gauge4")
  ) 
  
)

gaugerow3 <- fluidRow(
  
  box(
    title = "Days played"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("gauge5")
  )

  ,box(
    title = "Revives per game"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("gauge6")
  )
  
)

#Player - latest week page set up

selector2 <- selectInput("playerid2","Select player",sort(last_week$player))

l_gaugerow <- fluidRow(
  
  box(
    title = "KD"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("l_gauge")
  )
  
  ,box(
    title = "Damage ratio"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("l_gauge2")
  ) 
  
)

l_gaugerow2 <- fluidRow(
  
  box(
    title = "Gulag win %"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("l_gauge3")
  )
  
  ,box(
    title = "Gulag KD"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("l_gauge4")
  ) 
  
)
tab <- actionButton("switch_tab", "See player details")

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",frow1,frow2,tab
    ),
    
    tabItem(tabName = "player",
            h1("Player - All time"),
            selector,
            h4("Code: Player value in black. Red line indicates group average. Arrow and green/red value indicates difference from average value. ★ indicates top player for that metric."), 
            gaugerow,
    gaugerow2, gaugerow3),
    
    tabItem(tabName = "latest",
            h1("Player - Latest week"),
            selector2,
            h4(textOutput("matches_played")),
            h5("Code: Player value in black. Red line indicates group average. Arrow and green/red value indicates difference from average value. ★ indicates top player for that metric."), 
            l_gaugerow,
            l_gaugerow2)
  )
)


#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Warzone stats dashboard', header, sidebar, body, skin='red')




############### Server logic ###################
# Define server logic

server <- function(input, output, session) {
  
  observeEvent(input$switch_tab, {
    updateTabsetPanel(session, "tabs", "player")
  })
  
  #Some data manipulation for KPI boxes
  
  top_kd <- filter(api_data,kdRatio==max(api_data$kdRatio))
  top_win <- filter(api_data,win_pc==max(api_data$win_pc))
  top_5conversion <- filter(api_data,top5conversion==max(api_data$top5conversion))
  np <- length(api_data$player)
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(top_kd$kdRatio, format="g", big.mark=',')
      ,paste('Highest KD:',top_kd$player)
      ,icon = icon("star",lib='glyphicon')
      ,color = "purple")
    
  })
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(top_win$win_pc, format="g", big.mark=',')
      ,paste('Highest win %:',top_win$player)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
    
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(top_5conversion$top5conversion, format="g", big.mark=',')
      ,paste('Highest top 5 conversion %:',top_5conversion$player)
      ,icon = icon("record",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  #creating the plotOutput content
  
  output$kdbyplayer <- renderPlot({
    p <- ggplot(api_data, aes(x = reorder(player, -kill_ratio),kill_ratio))
    p <- p + geom_col() + theme_bw() + geom_text(aes(label=kill_ratio), position=position_dodge(width=0.9), vjust=-0.25)
    p <- p + xlab("Player") + geom_hline(yintercept = mean(api_data$kill_ratio),linetype="dashed", color = "red")
    p <- p+ ggtitle("Kill death ratio")
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    print(p)
  })
  
  
  output$winpcbyplayer <- renderPlot({
    p <- ggplot(api_data, aes(x = reorder(player, -win_pc),win_pc))
    p <- p + geom_col() + theme_bw() + geom_text(aes(label=win_pc), position=position_dodge(width=0.9), vjust=-0.25)
    p <- p + xlab("Player") + geom_hline(yintercept = mean(api_data$win_pc), linetype="dashed", color = "red")
    p <- p+ ggtitle("Win %")
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
    print(p)
  })
  
  #Manipulation for gauge chart
  
  
  #KD gauge chart
  output$gauge <- renderPlotly({
    selected <- filter(api_data,player==input$playerid)
    if(selected$kdRatio==max(api_data$kdRatio)){title <- list(text = '★', font = list(outline = 'black', color = 'orange'))} else{title <- ""}
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = selected$kdRatio,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = mean(api_data$kdRatio)),
      title=title,
      gauge = list(
        axis =list(range = list(NULL, 2)),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = mean(api_data$kdRatio)))) 
    fig <- fig %>%
      layout(margin = list(l=20,r=30))
    
    fig
  })
  
  #Win % gauge chart
  output$gauge2 <- renderPlotly({
    selected <- filter(api_data,player==input$playerid)
    if(selected$win_pc==max(api_data$win_pc)){title <- list(text = '★', font = list(outline = 'black', color = 'orange'))} else{title <- ""}
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = selected$win_pc,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = mean(api_data$win_pc)),
      title=title,
      gauge = list(
        axis =list(range = list(NULL, 15)),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = mean(api_data$win_pc)))) 
    fig <- fig %>%
      layout(margin = list(l=20,r=30))
    
    fig
  })
  
  #Top 5 conversions gauge chart
  output$gauge3 <- renderPlotly({
    selected <- filter(api_data,player==input$playerid)
    if(selected$top5conversion==max(api_data$top5conversion)){title <- list(text = '★', font = list(outline = 'black', color = 'orange'))} else{title <- ""}
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = selected$top5conversion,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = mean(api_data$top5conversion)),
      title=title,
      gauge = list(
        axis =list(range = list(NULL, 40)),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = mean(api_data$top5conversion)))) 
    fig <- fig %>%
      layout(margin = list(l=20,r=30))
    
    fig
  })
  
  #Average score gauge chart
  output$gauge4 <- renderPlotly({
    selected <- filter(api_data,player==input$playerid)
    if(selected$avg_score==max(api_data$avg_score)){title <- list(text = '★', font = list(outline = 'black', color = 'orange'))} else{title <- ""}
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = selected$avg_score,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = mean(api_data$avg_score)),
      title=title,
      gauge = list(
        axis =list(range = list(NULL, 5000)),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = mean(api_data$avg_score)))) 
    fig <- fig %>%
      layout(margin = list(l=20,r=30))
    
    fig
  })
  
  #Days played gauge chart
  output$gauge5 <- renderPlotly({
    selected <- filter(api_data,player==input$playerid)
    if(selected$minutes==max(api_data$minutes)){title <- list(text = '★', font = list(outline = 'black', color = 'orange'))} else{title <- ""}
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = selected$minutes/(60*24),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = mean(api_data$minutes/(60*24))),
      title=title,
      gauge = list(
        axis =list(range = list(NULL, 40)),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = mean(api_data$minutes/(60*24))))) 
    fig <- fig %>%
      layout(margin = list(l=20,r=30))
    fig
  })
  
  #Revives per game
  output$gauge6 <- renderPlotly({
    selected <- filter(api_data,player==input$playerid)
    if(selected$revives/selected$played==max(api_data$revives/api_data$played)){title <- list(text = '★', font = list(outline = 'black', color = 'orange'))} else{title <- ""}
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = selected$revives/selected$played,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = mean(api_data$revives/api_data$played)),
      title=title,
      gauge = list(
        axis =list(range = list(NULL, 1)),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = mean(api_data$revives/api_data$played)))) 
    fig <- fig %>%
      layout(margin = list(l=20,r=30))
    fig
  })
  
  ##### Latest data gauge charts ######
  
  output$matches_played <- renderText({
    selected <- filter(last_week,player==input$playerid2)
    paste("Number of matches played during latest week:", selected$matchesPlayed)
  })
  
  #KD gauge chart
  output$l_gauge <- renderPlotly({
    selected <- filter(last_week,player==input$playerid2)
    if(selected$kdRatio==max(last_week$kdRatio)){title <- list(text = '★', font = list(outline = 'black', color = 'orange'))} else{title <- ""}
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = selected$kdRatio,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = mean(last_week$kdRatio)),
      title=title,
      gauge = list(
        axis =list(range = list(NULL, 3)),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = mean(last_week$kdRatio)))) 
    fig <- fig %>%
      layout(margin = list(l=20,r=30))
    
    fig
  })
  
  #Damage ratio chart
  output$l_gauge2 <- renderPlotly({
    selected <- filter(last_week,player==input$playerid2)
    if(selected$damageRatio==max(last_week$damageRatio)){title <- list(text = '★', font = list(outline = 'black', color = 'orange'))} else{title <- ""}
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = selected$damageRatio,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = mean(last_week$damageRatio)),
      title=title,
      gauge = list(
        axis =list(range = list(NULL, 4)),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = mean(last_week$damageRatio)))) 
    fig <- fig %>%
      layout(margin = list(l=20,r=30))
    
    fig
  })
  
  #gulag win pc
  output$l_gauge3 <- renderPlotly({
    selected <- filter(last_week,player==input$playerid2)
    if(selected$gulag_win_pc==max(last_week$gulag_win_pc)){title <- list(text = '★', font = list(outline = 'black', color = 'orange'))} else{title <- ""}
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = selected$gulag_win_pc,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = mean(last_week$gulag_win_pc)),
      title=title,
      gauge = list(
        axis =list(range = list(NULL, 100)),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = mean(last_week$gulag_win_pc)))) 
    fig <- fig %>%
      layout(margin = list(l=20,r=30))
    
    fig
  })
  
  
  #gulag kd
  output$l_gauge4 <- renderPlotly({
    selected <- filter(last_week,player==input$playerid2)
    if(selected$gulag_kd==max(last_week$gulag_kd)){title <- list(text = '★', font = list(outline = 'black', color = 'orange'))} else{title <- ""}
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = selected$gulag_kd,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = mean(last_week$gulag_kd)),
      title=title,
      gauge = list(
        axis =list(range = list(NULL, 1.5)),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = mean(last_week$gulag_kd)))) 
    fig <- fig %>%
      layout(margin = list(l=20,r=30))
    
    fig
  })

}



# Run the application 
shinyApp(ui = ui, server = server)


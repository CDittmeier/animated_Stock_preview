
## app.R ##

library(shinydashboard)
library(tidyverse)
library(tidyquant)
library(timetk)
library(tibbletime)
library(scales)
library(broom)
library(gganimate)
library(transformr)
library(ggthemes)
library(gifski)
library(udunits2)


ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Stock Animation"),
  dashboardSidebar(sidebarMenu(
         menuItem("Animated Stock Price", tabName = "dashboard", icon = icon("dashboard"),
                  badgeLabel = "new", badgeColor = "green")
       )),
  dashboardBody(
    # Boxes need to be put in a row (or column)
      fluidRow(
      box(width = 4,
        title = "Animated Stock Preview",
        textInput("sel_security", "Enter ticker:", value = "GOOG"), submitButton("Go", icon("refresh")
        )
      ), valueBoxOutput("nameBox")  ),
    fluidRow(
      box(width = 12, height = 500, imageOutput("plot1"))
    
    
  )
))


server <- function(input, output, session) {
  
  output$nameBox <- renderValueBox({
    symbolInfo <- read.csv(
      "ftp://ftp.nasdaqtrader.com/SymbolDirectory/nasdaqlisted.txt",
      sep="|")
    
    symbolShiny <- symbolInfo %>%
      filter(Symbol == input$sel_security) %>%
      select(Symbol, Security.Name)
    
    valueBox(
      input$sel_security, symbolShiny[1,2], icon = icon("dollar"),
      color = "olive"
    )
  })
  
  output$plot1 <- renderImage({

    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Creating stock chart animation',
                 detail = 'May take up to 30 seconds after fully loaded')
    
    for (i in 1:15) {
      progress$set(value = i)
      Sys.sleep(0.1)
    }
    
    outfile <- tempfile(fileext='.gif')
    
    data <- tq_get(input$sel_security, 
             get = "stock.prices",
             from = "2017-01-01")
      
    p = ggplot(data = data, aes(x=date, y=adjusted)) +
    geom_line(col = "cornflowerblue", size = 1.5) +
    scale_x_date(breaks = pretty_breaks(n = 12)) +
    scale_y_continuous(name="price", labels = scales::dollar) +
    scale_color_continuous(low = "red", high = "green")+
    theme_dark() +
    transition_time(date) +
    ease_aes('linear') +
    labs(title = paste(as.character(input$sel_security), 'Price - Date: {frame_time}'), x = 'Date', y = 'Adjusted Price') +
    shadow_mark(size = 1, colour = 'white')
    #anim_save("outfile.gif", animate(p,renderer = gifski_renderer(loop = T))) # New
    
    q = ggplot(data = data, aes(x=date, y=volume)) +
    geom_bar(fill = "cyan", stat = "identity") +
    scale_x_date(breaks = pretty_breaks(n = 12)) +
    scale_y_continuous(labels = scales::comma, breaks = pretty_breaks(n = 6)) + 
    theme_minimal() + 
    labs(title = 'Volume - Date: {frame_time}', x = 'Date', y = 'Volume') +
    transition_time(date) +
    shadow_mark(size = 1, colour = 'grey')
    #anim_save("outfile.gif", animate(q,renderer = gifski_renderer(loop = T))) # New
    
p_gif <- animate(p,renderer = gifski_renderer(loop = T))
q_gif <- animate(q,renderer = gifski_renderer(loop = T))

p_mgif <- image_read(p_gif)
q_mgif <- image_read(q_gif)


new_gif <- image_append(c(p_mgif[1], q_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(p_mgif[i], q_mgif[i]))
  new_gif <- c(new_gif, combined)
}

anim_save("outfile.gif", new_gif)    
    
list(src = "outfile.gif",
         contentType = 'image/gif'
     # ,
     #     width = 716,
     #     height = 368
         # alt = "This is alternate text"
    )
    
    
  })
  deleteFile = TRUE}

shinyApp(ui, server, options = list(height = 1080, width = 2060))




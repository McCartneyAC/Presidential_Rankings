library(DT)
library(ggmap)
library(shiny)
library(readxl)
library(forcats)
library(ggthemes)
library(tidyverse)
library(lubridate)
library(rsconnect)
library(shinythemes)
library(fiftystater)

ui <- fluidPage( 
  
  
 # Basic Page Information 
  theme = shinytheme ("flatly"), 
  titlePanel("US Presidential Rankings"),
  
  # Select Features
  sidebarLayout(
    sidebarPanel(
      
      
      selectInput("valueselect", "Select Value to Graph", 
                  choices = list(
                    "Rating" = "Rating",
                    "Polarization Score (smaller is more polarized)" = "Polarization"),
                  selected = "Rating"
                  ),
      
      
      selectInput("yearselect", "Select Polling Year",
                  choices = list(
                    "2014" = 2014,
                    "2018" = 2018
                    ),
                  selected = 2018
                  ),
      
      
      selectInput("colorselect", "Color by",
                    list(
                    "Political Party" = "Party", 
                    "Home State" = "HomeState"),
                  selected = "Party"
                  ),
     
       
      selectInput("orderby", "Order Chart by",
                  choices = list(
                    "Rating" = "Rating",
                    "Presidency" = "Presidency",
                    "Polarization" = "Polarization"
                  ),
                  selected = "Presidency"
                  )
      ),
                   
                   
                   
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel("About",
                                tags$div(class="header", checked=NA,
                                         tags$h3("Rottinghaus and Vaughn's Presidential Data"),
                                         tags$p("In 2014 and again in March of 2018, Brandon Rottinghaus and Justin S. Vaughn polled a group of
                                                presidential scholars, political theorists, and poli-sci researchers, professors, and academics
                                                and asked them various questions about the 44, and now 45, presidents. One of the bigly publicized
                                                products of this survey are the presidential rankings, which have recently", tags$a(href="https://www.nytimes.com/interactive/2018/02/19/opinion/how-does-trump-stack-up-against-the-best-and-worst-presidents.html?smid=tw-share", "made news.")),
                                         tags$p(tags$a(href="https://fivethirtyeight.com/features/presidential-ratings-are-flawed-which-makes-it-hard-to-assess-trump/", "Five Thirty Eight"), 
                                                " has a pretty good write up on the perils of over-interpreting 
                                                the data, which I suggest you read. The purpose of this Shiny app is to allow users 
                                                to explore the data a bit more than is possible in news reports and other online articles,
                                                and to see the data themselves via an interactive data table."),
                                         tags$p("Please enjoy!")
                                         )
                                ),
                       tabPanel("Bump Plot",
                                plotOutput(outputId = "bumpplot", height=1000)),
                       tabPanel("Browse the Data",
                                DTOutput('dt')
                                ),
                       tabPanel("Make More Plots", 
                                plotOutput(outputId="plotid")
                                )
                       )
                     )
                   )
                 )



server<-function(input, output){

  my_theme <- function() {
    
    # Colors
    color.background = "white"
    color.text = "#22211d"
    
    # Begin construction of chart
    theme_bw(base_size=15) +
      
      # Format background colors
      theme(panel.background = element_rect(fill=color.background, color=color.background)) +
      theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
      theme(panel.border     = element_rect(color=color.background)) +
      theme(strip.background = element_rect(fill=color.background, color=color.background)) +
      
      # Format the grid
      theme(panel.grid.major.y = element_blank()) +
      theme(panel.grid.minor.y = element_blank()) +
      theme(panel.grid.minor.x = element_blank()) +
      theme(axis.ticks       = element_blank()) +
      
      # Format the legend
      theme(legend.position = "none") +
      
      # Format title and axis labels
      theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
      theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
      theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
      theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
      theme(axis.text.y      = element_text(size=10, color = color.text)) +
      theme(strip.text       = element_text(face = "bold")) +
      
      # Plot margins
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
  }
  
  
  
  presidents<-readxl::read_xlsx("presidents.xlsx")
  
  pal<-c("cornflowerblue",
         "darkorchid4",
         "goldenrod4",
         "dimgray",
         "firebrick",  # five
         "darkolivegreen",
         "slateblue",
         "sienna4",
         "coral1",
         "green1", # ten
         "palegreen4",
         "khaki4",
         "darkorchid",
         "turquoise4",
         "darkorange4", #fifteen
         "aquamarine4",
         "peru",
         "grey15",
         "cyan4",
         "hotpink4" #twenty
  )
  
  
  #map <- reactive({})

  
  output$dt <- renderDT(
    presidents, options = list(lengthChange = FALSE,rownames=FALSE)
  )
  
  
  output$plotid<-renderPlot({
    # current error: length(f) == length(x) is not TRUE
    # need to fix plot so data length matches between ggplot() element and geom_col() element
    # perhaps add multiple geom_col elements overtop of a base that is consistent? 
      ggplot(
        data=dplyr::filter(presidents, Year == input$yearselect), 
        aes(x = fct_reorder(President, input$orderby), y = input$valueselect, fill = input$colorselect)) +
      geom_col() + 
      coord_flip() + 
      scale_fill_manual(values=pal) + 
      labs(
        title="Rankings of U.S. Presidents",
        x = "President",
        y = "Rating",
        caption = "Rottinghaus and Vaughn's Presidential Data"
        ) + 
      theme_hc()
    }, height = 850)
  
  output$plotid<-renderPlot({
    if (input$orderby == "Rating") {
      ggplot(
        data=dplyr::filter(presidents, Year == input$yearselect), 
        aes(x = fct_reorder(President, Rating))) +
        geom_col(data = dplyr::filter(presidents, Year == input$yearselect),
                 aes_string(y=input$valueselect, fill = input$colorselect)) + 
        coord_flip() +
        scale_fill_manual(values=pal) + 
        labs(
          title="Rankings of U.S. Presidents",
          x = "President",
          y = "Selected Value",
          caption = "Rottinghaus and Vaughn's Presidential Data"
        ) + 
        theme_hc()
    }
    else if (input$orderby == "Presidency"){
      ggplot(
        data=dplyr::filter(presidents, Year == input$yearselect), 
        aes(x = fct_reorder(President, Presidency))) +
        geom_col(data = dplyr::filter(presidents, Year == input$yearselect),
                 aes_string(y=input$valueselect, fill = input$colorselect)) + 
        coord_flip() +
        scale_fill_manual(values=pal) + 
        labs(
          title="Rankings of U.S. Presidents",
          x = "President",
          y = "Selected Value",
          caption = "Rottinghaus and Vaughn's Presidential Data"
        ) + 
        theme_hc()
    }
    else if (input$orderby == "Polarization"){
      ggplot(
        data=dplyr::filter(presidents, Year == input$yearselect), 
        aes(x = fct_reorder(President, Polarization))) +
        geom_col(data = dplyr::filter(presidents, Year == input$yearselect),
                 aes_string(y=input$valueselect, fill = input$colorselect)) + 
        coord_flip() +
        scale_fill_manual(values=pal) + 
        labs(
          title="Rankings of U.S. Presidents",
          x = "President",
          y = "Selected Value",
          caption = "Rottinghaus and Vaughn's Presidential Data"
        ) + 
        theme_hc()
    }
    else ggplot()
  }, height = 850)
  
  
  
  show_top_n <- 45
  
  output$bumpplot<-renderPlot({
      ggplot(data=presidents, aes(x = Year, y = Rank, group = President)) +
      geom_line(aes(color = Party, alpha = 1), size = 2) +
      geom_point(aes(color = Party, alpha = 1), size = 4) +
      scale_y_reverse(breaks = 1:44,expand = c(.05, .05))+
      scale_x_continuous(breaks = 2014:2018, minor_breaks = 2014:2018) +
      geom_text(data = presidents %>% filter(Year == "2018"),
                aes(label = President, x = 2018.5) ,  fontface = "bold", color = "#888888", size = 4) +
      coord_cartesian(ylim = c(1,44)) + 
      my_theme()+ 
      scale_color_manual(values=pal)+
      labs(
        title="How did Ratings Change over Four Years?"
      )
  })
  
  
  
  
}


shinyApp(ui, server)

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

ui <- fluidPage( 
  
  
 # Basic Page Information 
  theme = shinytheme ("sandstone"), 
  titlePanel("US Presidential Rankings"),
  
  # Select Features
  sidebarLayout(
    sidebarPanel(
      
      
      selectInput("valueselect", "Select Value to Graph", 
                  choices = list(
                    "Rating" = "inputrating",
                    "Polarization Score \n (smaller is more polarized)" = "inputpolar"),
                  selected = "inputrating"
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
                    "Home State" = "`Home State`"),
                  selected = "Party"
                  ),
      
      selectInput("orderby", "Order Chart by",
                  choices = list(
                    "Rank" = "inputorder_rank",
                    "Rating" = "inputorder_rating",
                    "Presidency" = "inputorder_presidency",
                    "Polarization" = "inputorder_polarization"
                  ),
                  selected = "inputorder_presidency"
                  )
      ),
                   
                   
                   
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel("plots", plotOutput(outputId="plotid")),
                       tabPanel("maps", plotOutput(outputId="map")), 
                       tabPanel("about",
                                tags$div(class="header", checked=NA,
                                         tags$h3("Rottinghaus and Vaughn's Presidential Data"),
                                         tags$p("In 2014 and again in March of 2018, Brandon Rottinghaus and Justin S. Vaughn polled a group of
                                                presidential scholars, political theories, and poli-sci researchers, professors, and academics
                                                and asked them various questions about the 44, and now 45, presidents. One of the bigly publicized
                                                products of this survey are the presidential rankings, which have recently", tags$a(href="https://www.nytimes.com/interactive/2018/02/19/opinion/how-does-trump-stack-up-against-the-best-and-worst-presidents.html?smid=tw-share", "made news.")),
                                         tags$p(tags$a(href="https://fivethirtyeight.com/features/presidential-ratings-are-flawed-which-makes-it-hard-to-assess-trump/", "Five Thirty Eight"), 
                                                " has a pretty good write up on the perils of over-interpreting 
                                                the data, which I suggest you read. The purpose of this Shiny app is to allow users 
                                                to explore the data a bit more than is possible in news reports and other online articles,
                                                and to see the data themselves via an interactive data table."),
                                         tags$p("Please enjoy!")
                                         # tags$p("What if you don't have ciphertext? Try", tags$a(href="http://rot13.com/", "Rot13."),
                                         )
                                ),
                       tabPanel("data dable",
                                DTOutput('dt')
                                )
                       )
                     )
                   )
                 )



server<-function(input, output){
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
  
  presidents<-readxl::read_xlsx("presidents.xlsx")
  
  #map <- reactive({})
  #output$<-renderPlot({})
  #output$<-renderPlot({})
  
  output$dt <- renderDT(
    presidents, options = list(lengthChange = FALSE,rownames=FALSE)
  )
  
  
  output$plotid<-renderPlot({
    
      ggplot(
        data=dplyr::filter(presidents, Year == input$yearselect), 
          aes(x = factor(President, levels = President[order(presidents$Presidency)]), y = Rating, fill = Party))+
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
  
  
  
  
  
  
}


shinyApp(ui, server)









##############################################################################################################
# practice and set up



setwd("C:\\Users\\Andrew\\Documents\\GitHub\\Presidential_Rankings")
presidents<-readxl::read_xlsx("presidents.xlsx")
datatable(presidents, rownames=F)
presidents

yr <- 2018
clr <- presidents$Party
plt <- presidents$Rating


pal<-c("cornflowerblue","darkorchid4","goldenrod4","dimgray","firebrick","darkolivegreen")

presidents %>%
  filter(Year == yr) %>%
  ggplot(aes(x = factor(President, levels = President[order(presidents$Presidency)]), y = Rating, fill = Party)) + 
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
presidents

shinyApp(
  ui = fluidPage(DTOutput('tbl')),
  server = function(input, output) {

    output$tbl = renderDT(
      presidents, options = list(lengthChange = FALSE,rownames=FALSE)
    )
  }
)

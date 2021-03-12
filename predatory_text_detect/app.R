
library(shiny)
library(tidyverse)

# load lexicon once 

pred_lex <- read_csv("predatory_lexicon_updated.csv")

ui <- fluidPage(

    # Application title
    titlePanel("Predatory Text Detect from Literature / Papers"),

    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #        
    #     ),

        # Show a plot of the generated distribution
        mainPanel(
           h3("Enter your Input Text here:"),
           textAreaInput("input_text", label = ''),
           h3("Output Score"),
           htmlOutput("score"),
           h3("Matching Keywords from lexicon"),
           dataTableOutput("matching") #using htmloutput for colors
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$score <- renderText({
        
        df <- pred_lex
        
        input_text <- tolower(input$input_text)
        
        print(input_text)
        
        df$match <- tolower(input_text) %>%
            str_detect(df$word)
        
        score <- as.integer(df %>%
                                filter(match) %>%
                                summarise(value))
        
        ifelse(sum(df$match)>0,
            ifelse(score < 0, 
               paste('<span style=\"color:red\">',score,
                     '</span>', sep = ""),
               paste('<span style=\"color:green\">',score,
                     '</span>', sep = "")),      
               paste('<span style=\"color:blue\">',0,
                     '</span>', sep = ""))
         
    })
    
    output$matching <- renderDataTable({
        
        df <- pred_lex
        
        input_text <- tolower(input$input_text)
        
        df$match <- tolower(input_text) %>%
            str_detect(df$word)
        
        df %>% filter(match)
            
    })
    
}    

# Run the application 
shinyApp(ui = ui, server = server)

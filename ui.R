#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
        
        # Application title
        titlePanel("Gerard Dion's word suggestion Application"),
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
                sidebarPanel(
                        textInput("sentence", "Sentence", value = "", width = NULL, placeholder = NULL),
                        submitButton("Submit")
                        #sliderInput("bins",
                        #            "Number of bins:",
                        #            min = 1,
                        #            max = 50,
                        #            value = 30)
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        h1("The suggestions are as follows:"),
                        textOutput("suggestion1"),
                        h1(" "),
                        textOutput("suggestion2"),
                        h1(" "),
                        textOutput("suggestion3")
                        #plotOutput("distPlot")
                )
        )
))

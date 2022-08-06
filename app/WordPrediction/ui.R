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
    titlePanel("N-Gram Model: Next Word Prediction"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("nPreds",
                        "Number of predictions",
                        min = 1,
                        max = 4,
                        value = 3),
            p("The number of predictions outputted may be less then the number used. This is due to how the model makes predictions. If the phrase you used is not seen in the model, there is limited information the model is able to predict from.")
        ),
        mainPanel(
            textInput(
                "phraseIn", 
                "Input a phrase for next word prediction",
                width="100%",
                placeholder = "Your phrase here..."
            ),
            actionButton(
                "goPred", 
                label = div("Predict", icon("arrow-right", lib="font-awesome")),
                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
            ),
            actionButton(
                "babble", 
                label = div("Babble", icon("baby", lib="font-awesome"))
            ),
            p(tags$small("What is babble? The Babble button will take the top prediction, add it your phrase, and then make a new prediction based on this new sentence. The sentence will become more inchorenent the longer you go, but is a fun way to show how the model makes predictions.")
              , style = "padding-top:8px;"),
            h2("Predictions"),
            tags$div(
                tags$p(
                    "Input a phrase above to get the predictions",
                    style = "color:#999999",
                    id = "placeholder-text"
                ), 
                id = 'placeholder'
            ),
            h4("More Information", style = "padding-top:24px"),
            p("For more information on how the model works and how it was built check out: ", tags$a("https://github.com/tinman8/NextWordPrediction", href="https://github.com/tinman8/NextWordPrediction"))
    ))
))

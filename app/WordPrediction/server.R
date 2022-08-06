library(shiny)
library(scales)
library(tictoc)
source("predict.R")

shinyServer(function(input, output, session) {
    x <- function(phraseIn) {
        removeUI(selector = "#predictions")
        removeUI(selector = "#placeholder-text")
        tic(quiet = TRUE)
        predictionsVals <- nextWord(phraseIn, n = input$nPreds)
        toc(quiet = TRUE, log= TRUE)
        
        t <- tic.log(format = TRUE)
        tic.clearlog()
        
        predictionsVals <- predictionsVals[!which(is.na(predictionsVals$y)),]
        n <- nrow(predictionsVals)
        
        predictions <- lapply(1:n, function(x) {
            tags$p(
                x, 
                "-", 
                phraseIn, 
                strong(predictionsVals[x]$y),
                " -- Probability Score: ",
                label_percent(accuracy = 0.001)(10^predictionsVals[x]$GT)
            )
        })
        
        ui_out <- tags$div(
            predictions, 
            tags$p("Server Prediction Speed: ", t[1]),
            id="predictions"
        )
        
        insertUI(
            selector = "#placeholder",
            where = "afterEnd",
            ui = ui_out
            
        )
        
        predictionsVals[1]$y
    }
    
    prevPred <- ""
    
    observeEvent(input$goPred, {
        prevPred <<- x(input$phraseIn)
    })
    
    observeEvent(input$babble, {
        newPhrase <- paste(input$phraseIn, prevPred)
        prevPred <<- x(newPhrase)
        updateTextInput(session, "phraseIn", value = newPhrase)
    })
})

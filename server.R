##############################
# Shiny App: ExploratR - Exploration uni- bi- et multi-variée avec R
# User interface
##############################


shinyServer(function(input, output, session) {
  
  # DYNAMIC UI ----
  
  # dynamic slider
  output$slideruni <- renderUI({
    if(!is.null(input$uniquanti)){
      minMax <- range(baseData$df[, input$uniquanti], na.rm = TRUE)
      varRange <- minMax[2] - minMax[1]
      sliderInput(inputId = "nbins", 
                  label = "Index of sequences", 
                  min = 0, 
                  max = 30, 
                  value = 10,
                  step = 1)
    } else{
      return()
    }
  })
  

  # update and choose columns
  output$coluniquanti <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "uniquanti", 
                label = "Choisir une variable quanti", 
                choices = colNames, 
                selected = "", 
                multiple = TRUE, 
                selectize = TRUE)
  })
  
  output$coluniquali <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "uniquali", 
                label = "Choisir une variable quali", 
                choices = colNames, 
                selected = "", 
                multiple = TRUE, 
                selectize = TRUE)
  })

  output$colqualidep <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "qualidep", 
                label = "Choisir la variable à expliquer", 
                choices = colNames, 
                selected = "", 
                multiple = TRUE, 
                selectize = TRUE)
  })
  
  output$colqualiindep <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "qualiindep", 
                label = "Choisir la variable explicative", 
                choices = colNames, 
                selected = "", 
                multiple = TRUE, 
                selectize = TRUE)
  })
  
  output$colquantidep <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "quantidep", 
                label = "Choisir la variable à expliquer", 
                choices = colNames, 
                selected = "", 
                multiple = TRUE, 
                selectize = TRUE)
  })  
  
  output$colquantiindep <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "quantiindep", 
                label = "Choisir la variable explicative", 
                choices = colNames, 
                selected = "", 
                multiple = TRUE, 
                selectize = TRUE)
  })  
  
  output$colquanlidep <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "quanlidep", 
                label = "Choisir la variable à expliquer", 
                choices = colNames, 
                selected = "", 
                multiple = TRUE, 
                selectize = TRUE)
  })    
  
  output$colquanliindep <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "quanliindep", 
                label = "Choisir la variable explicative", 
                choices = colNames, 
                selected = "", 
                multiple = TRUE, 
                selectize = TRUE)
  })  

  
  # DONNEES ----
  
  # show table
  output$contentstable <- renderDataTable({
    baseData$df
  })

  output$description <- renderText("<strong>Projet ANR Cartelec</strong> <br/> Résultats des présidentielles 2007 et données socio-économiques à l'échelle du bureau de vote")

  
  
  # UNIVARIE ----
  
  # summary
  output$unisummary <- renderText({
    if (!is.null(input$uniquanti) & is.null(input$uniquali)){
      textResult <- paste("Moyenne = ", round(mean(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2), "<br/>",
                          "Médiane = ", round(median(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2), "<br/>",
                          "Écart-type = ", round(sd(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2), 
                          sep = "")
      return(textResult)

    } else if (is.null(input$uniquanti) & !is.null(input$uniquali)) {
      textResult <- paste("Nb. obs. = ", nrow(baseData$df), "<br/>",
                          "Valeurs manquantes = ", anyNA(baseData$df[, input$uniquali]),
                          sep = "")
      return(textResult)
    } else {
      return()
    }
  })
  

  # plot univariate
  output$uniplot <- renderPlot({
    if (!is.null(input$uniquanti) & is.null(input$uniquali)){
      Histogram(df = baseData$df, varquanti = input$uniquanti, nbins = input$nbins, drawsummary = input$drawsummary)
    } else if (is.null(input$uniquanti) & !is.null(input$uniquali)) {
        Barplot(df = baseData$df, varquali = input$uniquali)
    } else {
      return()
    }
  })
  
  
  # BIVARIE : QUALI-QUALI ----
  
  # Print mosaic plot
  output$mosaic <- renderPlot({
    if (!is.null(input$qualiindep) & !is.null(input$qualidep)){
      Mosaicplot(df = baseData$df, varx = input$qualiindep, vary = input$qualidep)
    } else {
      return()
    }
  })
  
  # contingency table
  output$contingtab <- renderTable({
    if (!is.null(input$qualidep) & !is.null(input$qualiindep)){
      chiResults <- chisq.test(baseData$df[, input$qualidep], baseData$df[, input$qualiindep])
      if(input$contcont == "obsfreq"){
        return(chiResults$observed)
      }
      else if(input$contcont == "rowpct"){
        return(100 * prop.table(table(baseData$df[, input$qualidep], baseData$df[, input$qualiindep]), margin = 1))
      }
      else if(input$contcont == "expfreq"){
        return(round(chiResults$expected, digits = 0))
      }
      else if(input$contcont == "rawresid"){
        return(chiResults$observed - chiResults$expected)
      }
      else if(input$contcont == "stdresid"){
        return(chiResults$residuals)
      }
    } else {
      return()
    }
  })
  
  # contingency text
  output$contingtext <- renderText({
    if (!is.null(input$qualidep) & !is.null(input$qualiindep)){
      chiResults <- chisq.test(baseData$df[, input$qualidep], baseData$df[, input$qualiindep])
      textResult <- paste("Chi2 = ", round(chiResults$statistic, 2), "<br/>",
                          "Phi = ", round(sqrt(chiResults$statistic / nrow(baseData$df)), 2), "<br/>",
                          "V de Cramer = ", round(sqrt(chiResults$statistic / (nrow(baseData$df) * min(dim(chiResults$observed)))), 2), 
                          sep = "")
      return(textResult)
    } else {
      return()
    }
  })
  
  
  
  # BIVARIE : QUANTI-QUANTI ----
  
  # Print scatter plot
  
  output$scatterplot <- renderPlot({
    if (!is.null(input$quantiindep) & !is.null(input$quantidep)){
      ScatterPlot(df = baseData$df, varx = input$quantiindep, vary = input$quantidep)
    } else {
      return()
    }
  })

  # Compute linear regression
  
  regMod <- reactive({
    if (!is.null(input$quantidep) & !is.null(input$quantiindep)){
      ComputeRegression(df = baseData$df, vardep = input$quantidep, varindep = input$quantiindep)
    } else {
      return()
    }
  })
  
  # Print coefficients
  
  output$coefreg <- renderText({
    if (!is.null(input$quantidep) & !is.null(input$quantiindep)){
      print.xtable(xtable(regMod()$TABCOEF), type = "HTML", include.rownames = FALSE, html.table.attributes = "frame = border")
    } else {
      return()
    }
  })
  
  # BIVARIE : QUALI-QUANTI ----
  
  # Print boxplot
  
  output$boxes <- renderPlot({
    if (!is.null(input$quanliindep) & !is.null(input$quanlidep)){
      Boxplot(df = baseData$df, varx = input$quanliindep, vary = input$quanlidep)
    } else {
      return()
    }
  })
  
  # Compute linear regression
  
  aovMod <- reactive({
    if (!is.null(input$quanlidep) & !is.null(input$quanliindep)){
      ComputeRegression(df = baseData$df, vardep = input$quanlidep, varindep = input$quanliindep)
    } else {
      return()
    }
  })
  
  # Print coefficients
  
  output$coefanova <- renderText({
    if (!is.null(input$quanlidep) & !is.null(input$quanliindep)){
      print.xtable(xtable(aovMod()$TABCOEF), type = "HTML", include.rownames = FALSE, html.table.attributes = "frame = border")
    } else {
      return()
    }
  })
  
  
  
})

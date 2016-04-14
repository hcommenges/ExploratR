##############################
# Shiny App: ExploratR - Exploration uni- bi- et multi-variée avec R
# User interface
##############################


shinyServer(function(input, output, session) {
  
  observe({
    if (!is.null(input$fileInput)){
      baseData$df <- read.csv(file=input$fileInput$datapath, header=input$header, sep=input$sep, quote=input$quote, dec=input$dec, stringsAsFactor=FALSE, check.names=FALSE)
    }
  })
  
  observeEvent(input$loadExData, {
    baseData$df <- tabFinal
  })
  
  # DYNAMIC UI ----
  
  # dynamic slider
  output$slideruni <- renderUI({
    if(!is.null(input$uniquanti)){
      minMax <- range(baseData$df[, input$uniquanti], na.rm = TRUE)
      varRange <- minMax[2] - minMax[1]
      sliderInput(inputId = "nbins", 
                  label = "Nombre de classes", 
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
    colNames <- c("", colnames(baseData$df))
    selectInput(inputId = "uniquanti", 
                label = "Choisir une variable quanti", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  })
  
  output$coluniquali <- renderUI({
    colNames <- c("", colnames(baseData$df))
    selectInput(inputId = "uniquali", 
                label = "Choisir une variable quali", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  })
  
  output$colqualidep <- renderUI({
    colNames <- c("", colnames(baseData$df))
    selectInput(inputId = "qualidep", 
                label = "Choisir la variable à expliquer", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  })
  
  output$colqualiindep <- renderUI({
    colNames <- c("", colnames(baseData$df))
    selectInput(inputId = "qualiindep", 
                label = "Choisir la variable explicative", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  })
  
  output$colquantidep <- renderUI({
    colNames <- c("", colnames(baseData$df))
    selectInput(inputId = "quantidep", 
                label = "Choisir la variable à expliquer", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  })  
  
  output$colquantiindep <- renderUI({
    colNames <- c("", colnames(baseData$df))
    selectInput(inputId = "quantiindep", 
                label = "Choisir la variable explicative", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  })  
  
  output$colquanlidep <- renderUI({
    colNames <- c("", colnames(baseData$df))
    selectInput(inputId = "quanlidep", 
                label = "Choisir la variable à expliquer (quanti)", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  })    
  
  output$colquanliindep <- renderUI({
    colNames <- c("", colnames(baseData$df))
    selectInput(inputId = "quanliindep", 
                label = "Choisir la variable explicative (quali)", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  })  
  
  output$colaovdep <- renderUI({
    colNames <- c("", colnames(baseData$df))
    selectInput(inputId = "aovdep", 
                label = "Choisir la variable à expliquer (quanti)", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  })    
  
  output$colaovindep <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "aovindep", 
                label = "Choisir deux variables explicatives (quali)", 
                choices = colNames, 
                selected = "", 
                multiple = TRUE, 
                selectize = TRUE)
  })
  
  output$colancovdep <- renderUI({
    colNames <- c("", colnames(baseData$df))
    selectInput(inputId = "ancovdep", 
                label = "Choisir la variable à expliquer (quanti)", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  })    
  
  output$colancovindep <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "ancovindep", 
                label = "Choisir deux variables explicatives (1 quanti puis 1 quali)", 
                choices = colNames, 
                selected = "", 
                multiple = TRUE, 
                selectize = TRUE)
  }) 
  
  output$colquanti2dep <- renderUI({
    colNames <- c("", colnames(baseData$df))
    selectInput(inputId = "quanti2dep", 
                label = "Choisir la variable à expliquer (quanti)", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  })    
  
  output$colquanti2indep <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "quanti2indep", 
                label = "Choisir deux variables explicatives (quanti)", 
                choices = colNames, 
                selected = "", 
                multiple = TRUE, 
                selectize = TRUE)
  }) 
  
  output$colregmultdep <- renderUI({
    colNames <- c("", colnames(baseData$df))
    selectInput(inputId = "regmultdep", 
                label = "Choisir une variable à expliquer (quanti)", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  }) 
  
  output$colregmultindep <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "regmultindep", 
                label = "Choisir plusieurs variables explicatives (quanti)", 
                choices = colNames, 
                selected = "", 
                multiple = TRUE, 
                selectize = TRUE)
  })
  
  output$colfacto <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "factovar", 
                label = "Choisir plusieurs variables quantitatives", 
                choices = colNames, 
                selected = "", 
                multiple = TRUE, 
                selectize = TRUE)
  })
  
  output$colid <- renderUI({
    colNames <- colnames(baseData$df)
    selectInput(inputId = "idvar", 
                label = "Choisir la variable identifiant", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  })
  
  output$colcartovar <- renderUI({
    colNames <- c("", colnames(baseData$df))
    selectInput(inputId = "cartovar", 
                label = "Choisir la variable", 
                choices = colNames, 
                selected = "", 
                multiple = FALSE, 
                selectize = TRUE)
  })
  
  
  # ADD COLUMNS ----
  
  # Add regression residuals (simple)
  
  observeEvent(input$addreg1resid, {
    if (input$reg1prefix != ""){
      absName <- paste(input$reg1prefix, "AbsResid", sep = "_")
      relName <- paste(input$reg1prefix, "RelResid", sep = "_")
    } else {
      absName <- paste("lm", "AbsResid", sep = "_")
      relName <- paste("lm", "RelResid", sep = "_")
    }
    
    baseData$df[, c(absName, relName)] <- regMod()$TABRESID
  })
  
  # Add regression residuals (ancova)
  
  observeEvent(input$addreg2resid, {
    if (input$reg2prefix != ""){
      absName <- paste(input$reg2prefix, "AbsResid", sep = "_")
      relName <- paste(input$reg2prefix, "RelResid", sep = "_")
    } else {
      absName <- paste("lm", "AbsResid", sep = "_")
      relName <- paste("lm", "RelResid", sep = "_")
    }
    
    baseData$df[, c(absName, relName)] <- ancovMod()$TABRESID
  })
  
  # Add regression residuals (2 quanti)
  
  observeEvent(input$addreg3resid, {
    if (input$reg1prefix != ""){
      absName <- paste(input$reg3prefix, "AbsResid", sep = "_")
      relName <- paste(input$reg3prefix, "RelResid", sep = "_")
    } else {
      absName <- paste("lm", "AbsResid", sep = "_")
      relName <- paste("lm", "RelResid", sep = "_")
    }
    
    baseData$df[, c(absName, relName)] <- reg2Mod()$TABRESID
  })
  
  # Add regression residuals (multiple)
  
  observeEvent(input$addreg4resid, {
    if (input$reg1prefix != ""){
      absName <- paste(input$reg1prefix, "AbsResid", sep = "_")
      relName <- paste(input$reg1prefix, "RelResid", sep = "_")
    } else {
      absName <- paste("lm", "AbsResid", sep = "_")
      relName <- paste("lm", "RelResid", sep = "_")
    }
    
    baseData$df[, c(absName, relName)] <- regmultMod()$TABRESID
  })
  
  
  # Add factorial coordinates
  
  observeEvent(input$addfaccoord, {
    if (input$facprefix != ""){
      compNames <- paste(input$facprefix, c("C1", "C2", "C3", "C4"), sep = "_")
    } else {
      compNames <- c("C1", "C2", "C3", "C4")
    }
    
    baseData$df[, compNames] <- principalComp()$li
  })
  
  
  
  # GUIDE  ----
  
  output$citation <- renderText("<strong>Once upon a time, statisticians only explored</strong> (John Tukey)")
  
  
  # DONNEES ----
  
  # show table
  output$contentstable <- renderDataTable({
    baseData$df
  })
  
  output$description <- renderText("<strong>Projet ANR Cartelec</strong> <br/> Résultats des présidentielles 2007 et données socio-économiques à l'échelle du bureau de vote (cf. Guide d'utilisation).")
  
  # download data
  output$downloaddata <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      if (input$csvtype == "anglo"){
        write.csv(x = baseData$df, file = file, row.names = FALSE)
      } else {
        write.csv2(x = baseData$df, file = file, row.names = FALSE)
      }
    })
  
  
  # UNIVARIE ----
  
  # summary
  output$unisummary <- renderText({
    #if (!is.null(input$uniquanti) & is.null(input$uniquali)){
    if (input$uniquanti != "" & input$uniquali == ""){
      textResult <- paste("Nb. obs. = ", nrow(baseData$df), "<br/>",
                          "Valeurs manquantes = ", anyNA(baseData$df[, input$uniquanti]), "<br/>",
                          "Moyenne = ", round(mean(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2), "<br/>",
                          "Médiane = ", round(median(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2), "<br/>",
                          "Écart-type = ", round(sd(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2), "<br/>",
                          "Coef. de variation = ", round(sd(baseData$df[, input$uniquanti], na.rm = TRUE) / mean(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2),
                          sep = "")
      return(textResult)
      
    } else if (input$uniquanti == "" & input$uniquali != "") {
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
    if (input$uniquanti != "" & input$uniquali == ""){
      Histogram(df = baseData$df, varquanti = input$uniquanti, nbins = input$nbins, drawsummary = input$drawsummary)
    } else if (input$uniquanti == "" & input$uniquali != "") {
      Barplot(df = baseData$df, varquali = input$uniquali)
    } else {
      return()
    }
  })
  
  # download plot
  output$downloaduniplot <- downloadHandler(
    filename = "uniplot.svg",
    content = function(file) {
      svg(file, width = input$widthuni / 2.54, height = input$heightuni / 2.54, pointsize = 8)
      if (input$uniquanti != "" & input$uniquali == ""){
        print(Histogram(df = baseData$df, varquanti = input$uniquanti, nbins = input$nbins, drawsummary = input$drawsummary))
      } else if (input$uniquanti == "" & input$uniquali != "") {
        print(Barplot(df = baseData$df, varquali = input$uniquali))
      } else {
        return()
      }
      dev.off()
    })
  
  
  
  # BIVARIE : QUALI-QUALI ----
  
  # Print mosaic plot
  output$mosaic <- renderPlot({
    if (input$qualiindep != "" & input$qualidep != ""){
      Mosaicplot(df = baseData$df, varx = input$qualiindep, vary = input$qualidep)
    } else {
      return()
    }
  })
  
  # contingency table
  output$contingtab <- renderTable({
    if (input$qualiindep != "" & input$qualidep != ""){
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
    if (input$qualiindep != "" & input$qualidep != ""){
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
  
  # download plot
  output$downloadmosaicplot <- downloadHandler(
    filename = "mosaicplot.svg",
    content = function(file) {
      svg(file, width = input$widthmosaic / 2.54, height = input$heightmosaic / 2.54, pointsize = 8)
      if (input$qualiindep != "" & input$qualidep != ""){
        print(Mosaicplot(df = baseData$df, varx = input$qualiindep, vary = input$qualidep))
      } else {
        return()
      }
      dev.off()
    })
  
  
  # BIVARIE : QUANTI-QUANTI ----
  
  # print scatter plot
  output$scatterplot <- renderPlot({
    if (input$quantiindep != "" & input$quantidep != ""){
      ScatterPlot(df = baseData$df, varx = input$quantiindep, vary = input$quantidep)
    } else {
      return()
    }
  })
  
  # compute linear regression
  regMod <- reactive({
    if (input$quantiindep != "" & input$quantidep != ""){
      ComputeRegression(df = baseData$df, vardep = input$quantidep, varindep = input$quantiindep)
    } else {
      return()
    }
  })
  
  # print coefficients
  output$coefreg <- renderTable(include.rownames = FALSE, expr = {
    if (input$quantiindep != "" & input$quantidep != ""){
      regMod()$TABCOEF
    } else {
      return()
    }
  })
  
  # download plot
  output$downloadreg1 <- downloadHandler(
    filename = "regressionplot.svg",
    content = function(file) {
      svg(file, width = input$widthreg1 / 2.54, height = input$heightreg1 / 2.54, pointsize = 8)
      if (input$quantiindep != "" & input$quantidep != ""){
        print(ScatterPlot(df = baseData$df, varx = input$quantiindep, vary = input$quantidep))
      } else {
        return()
      }
      dev.off()
    })
  
  
  # BIVARIE : QUALI-QUANTI ----
  
  # print boxplot
  output$boxes <- renderPlot({
    if (input$quanliindep != "" & input$quanlidep != ""){
      Boxplot(df = baseData$df, varx = input$quanliindep, vary = input$quanlidep)
    } else {
      return()
    }
  })
  
  # compute linear regression
  aovMod <- reactive({
    if (input$quanliindep != "" & input$quanlidep != ""){
      ComputeRegression(df = baseData$df, vardep = input$quanlidep, varindep = input$quanliindep)
    } else {
      return()
    }
  })
  
  # print coefficients
  output$coefanova <- renderTable(include.rownames = FALSE, expr = {
    if (input$quanliindep != "" & input$quanlidep != ""){
      rbind(aovMod()$TABVAR, aovMod()$TABCOEF)
    } else {
      return()
    }
  })
  
  # download plot
  output$downloadanova <- downloadHandler(
    filename = "anovaplot.svg",
    content = function(file) {
      svg(file, width = input$widthanova1 / 2.54, height = input$heightanova1 / 2.54, pointsize = 8)
      if (input$quanliindep != "" & input$quanlidep != ""){
        print(Boxplot(df = baseData$df, varx = input$quanliindep, vary = input$quanlidep))
      } else {
        return()
      }
      dev.off()
    })
  
  
  # TRIVARIE : ANOVA 2 FACTORS ----
  
  # print boxplot
  output$boxes2 <- renderPlot({
    if (!is.null(input$aovindep) & input$aovdep != ""){
      Boxplot2(df = baseData$df, varx = input$aovindep[1], vary = input$aovdep, groupx = input$aovindep[2])
    } else {
      return()
    }
  })
  
  # compute linear regression
  aov2Mod <- reactive({
    if (input$aovdep != "" & !is.null(input$aovindep)){
      ComputeRegression(df = baseData$df, vardep = input$aovdep, varindep = input$aovindep)
    } else {
      return()
    }
  })
  
  # print coefficients
  output$coefanova2 <- renderTable(include.rownames = FALSE, expr = {
    if (input$aovdep != "" & !is.null(input$aovindep)){
      aov2Mod()$TABCOEF
    } else {
      return()
    }
  })
  
  # download plot
  output$downloadanova2 <- downloadHandler(
    filename = "anovaplot.svg",
    content = function(file) {
      svg(file, width = input$widthanova2 / 2.54, height = input$heightanova2 / 2.54, pointsize = 8)
      if (!is.null(input$aovindep) & input$aovdep != ""){
        print(Boxplot2(df = baseData$df, varx = input$aovindep[1], vary = input$aovdep, groupx = input$aovindep[2]))
      } else {
        return()
      }
      dev.off()
    })
  
  
  # TRIVARIE : ANCOVA ----
  
  # print scatter
  output$scatterancov <- renderPlot({
    if (input$ancovdep != "" & !is.null(input$ancovindep)){
      ScatterPlotAncov(df = baseData$df, varx = input$ancovindep[1], vary = input$ancovdep, groupx = input$ancovindep[2])
    } else {
      return()
    }
  })
  
  # compute linear regression
  ancovMod <- reactive({
    if (input$ancovdep != "" & !is.null(input$ancovindep)){
      ComputeRegression(df = baseData$df, vardep = input$ancovdep, varindep = input$ancovindep, interact = input$interactancov)
    } else {
      return()
    }
  })
  
  # print coefficients
  output$coefancov <- renderTable(include.rownames = FALSE, expr = {
    if (input$ancovdep != "" & !is.null(input$ancovindep)){
      ancovMod()$TABCOEF
    } else {
      return()
    }
  })
  
  # download plot
  output$downloadancova <- downloadHandler(
    filename = "ancovaplot.svg",
    content = function(file) {
      svg(file, width = input$widthancova / 2.54, height = input$heightancova / 2.54, pointsize = 8)
      if (input$ancovindep != "" & !is.null(input$ancovdep)){
        print(ScatterPlotAncov(df = baseData$df, varx = input$ancovindep[1], vary = input$ancovdep, groupx = input$ancovindep[2]))
      } else {
        return()
      }
      dev.off()
    })
  
  
  # TRIVARIE : REGRESSION ----
  
  # print scatter
  output$scatterreg2 <- renderPlot({
    if (input$quanti2dep != "" & !is.null(input$quanti2indep)){
      ScatterPlot3D(df = baseData$df, varx = input$quanti2indep[1], vary = input$quanti2indep[2], varz = input$quanti2dep)
    } else {
      return()
    }
  })
  
  # compute linear regression
  reg2Mod <- reactive({
    if (input$quanti2dep != "" & !is.null(input$quanti2indep)){
      ComputeRegression(df = baseData$df, vardep = input$quanti2dep, varindep = input$quanti2indep, interact = input$interactreg)
    } else {
      return()
    }
  })
  
  # print coefficients
  output$coefreg2 <- renderTable(include.rownames = FALSE, expr = {
    if (input$quanti2dep != "" & !is.null(input$quanti2indep)){
      reg2Mod()$TABCOEF
    } else {
      return()
    }
  })
  
  # download plot
  output$downloadreg2 <- downloadHandler(
    filename = "regressionplot.svg",
    content = function(file) {
      svg(file, width = input$widthreg2 / 2.54, height = input$heightreg2 / 2.54, pointsize = 8)
      if (input$quanti2dep != "" & !is.null(input$quanti2indep)){
        print(ScatterPlot3D(df = baseData$df, varx = input$quanti2indep[1], vary = input$quanti2indep[2], varz = input$quanti2dep))
      } else {
        return()
      }
      dev.off()
    })
  
  
  
  # MULTIVARIE : REGRESSION ----
  
  # Compute linear regression
  
  regmultMod <- reactive({
    if (input$regmultdep != "" & !is.null(input$regmultindep)){
      ComputeRegressionMult(df = baseData$df, vardep = input$regmultdep, varindep = input$regmultindep)
    } else {
      return()
    }
  })
  
  # Print matrix
  output$matcor <- renderTable(include.rownames = TRUE, expr = {
    if (input$regmultdep != "" & !is.null(input$regmultindep)){
      regmultMod()$MATCOR
    } else {
      return()
    }
  })
  
  # Print coefficients
  
  output$coefregmult <- renderTable(include.rownames = FALSE, expr = {
    if (input$regmultdep != "" & !is.null(input$regmultindep)){
      regmultMod()$TABCOEF
    } else {
      return()
    }
  })
  
  
  # MULTIVARIE : ANALYSE FACTORIELLE ----
  
  # Compute factorial analysis
  
  principalComp <- reactive({
    if (!is.null(input$factovar)){
      ComputePrincipalComp(df = baseData$df, varquanti = input$factovar, ident = input$idvar)
    } else {
      return()
    }
  })
  
  # Correlation matrix
  
  output$facmatcor <- renderTable({
    if (!is.null(input$factovar)){
      CorCompMat(dudiobj = principalComp(), xaxis = input$xaxis, yaxis = input$yaxis)
    } else {
      return()
    }
  })
  
  
  # Plot components
  
  output$compinert <- renderPlot({
    if (!is.null(input$factovar)){
      DecompInertia(dudiobj = principalComp())
    } else {
      return()
    }
  })
  
  # Plot circle of correlations
  
  output$corcircle <- renderPlot({
    if (!is.null(input$factovar)){
      CorCircle(dudiobj = principalComp(), xaxis = input$xaxis, yaxis = input$yaxis)
    } else {
      return()
    }
  })
  
  # Table of contributions (variables and observations)
  
  output$contribvar <- renderTable(digits = 0, expr = {
    if (!is.null(input$factovar)){
      ContribVarIndiv(dudiobj = principalComp())$CTRVAR
    } else {
      return()
    }
  })
  
  output$contribind <- renderDataTable(options = list(pageLength = 10), expr = {
    if (!is.null(input$factovar)){
      ContribVarIndiv(dudiobj = principalComp())$CTRIND
    } else {
      return()
    }
  })
  
  
  # CARTOGRAPHIE ----
  
  output$carto <- renderPlot({
    if (input$cartovar != ""){
      choroLayer(spdf = baseData$spdf, df = baseData$df, spdfid = "BUREAU", dfid = "BUREAU", var = input$cartovar, method = input$cartomethod, nclass = input$cartoclass)
    } else {
      return()
    }
  })
  
})

##############################
# Shiny App: ExploratR - Exploration uni- bi- et multi-variée avec R
# User interface
##############################


shinyServer(function(input, output, session) {
  
  baseData <- reactiveValues(spdf = bureauxParis, df = tabFinal)
  
  observe({
    req(input$fileInput$datapath)
    oriData <- read.csv(file = input$fileInput$datapath,
                        sep = input$sepcol,
                        quote = input$quote,
                        dec = input$sepdec,
                        encoding = input$encodtab,
                        stringsAsFactor = FALSE,
                        check.names = FALSE)
    baseData$df <- oriData
  })
  
  observe({
    req(input$shapeInput)
    oriDir <- getwd()
    setwd(tempdir())
    unzip(zipfile = input$shapeInput$datapath, overwrite = TRUE, exdir = "shpdir")
    fileName <- list.files("shpdir")[1]
    layerName <- substr(fileName, start = 1, stop = nchar(fileName) - 4)
    spObject <- readOGR(dsn = "shpdir", layer = layerName, stringsAsFactors = FALSE)
    setwd(oriDir)
    baseData$spdf <- spObject
  })
  
  
  observeEvent(input$loadExData, {
    baseData$df <- tabFinal
    baseData$spdf <- bureauxParis
  })
  
  # DYNAMIC UI ----
  
  
  observe({
    columnList <- c("", colnames(baseData$df))
    
    updateSelectInput(session = session,
                      inputId = "idtab",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "idshape",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "uniquanti",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "uniquali",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "qualidep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "qualiindep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "quantidep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "quantiindep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "quanlidep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "quanliindep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "aovdep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "aovindep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "ancovdep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "ancovindep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "quanti2dep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "quanti2indep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "regmultdep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "regmultindep",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "factovar",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "cahvar",
                      choices = columnList)
    updateSelectInput(session = session,
                      inputId = "cartovar",
                      choices = columnList)
  })
  
  
  
  # FILTER ROWS ----
  
  observeEvent(input$addfilter, {
    tempTab <- try(baseData$df %>% filter_(input$filterrow))
    if(is.data.frame(tempTab)){
      baseData$df <- tempTab
    } else {
      baseData$df <- baseData$df
    }
  })
  
  observeEvent(input$delfilter, {
    if(colnames(baseData$df)[1] == "BUREAU"){
      baseData$df <- tabFinal
    } else {
      baseData$df <- oriData
    }
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
  
  # Add regression residuals (anova)
  
  observeEvent(input$addaov1resid, {
    if (input$aov1prefix != ""){
      absName <- paste(input$aov1prefix, "AbsResid", sep = "_")
      relName <- paste(input$aov1prefix, "RelResid", sep = "_")
    } else {
      absName <- paste("anova1", "AbsResid", sep = "_")
      relName <- paste("anova1", "RelResid", sep = "_")
    }
    
    baseData$df[, c(absName, relName)] <- aovMod()$TABRESID
  })
  
  # Add regression residuals (anova 2)
  
  observeEvent(input$addaov2resid, {
    if (input$aov2prefix != ""){
      absName <- paste(input$aov2prefix, "AbsResid", sep = "_")
      relName <- paste(input$aov2prefix, "RelResid", sep = "_")
    } else {
      absName <- paste("anova2", "AbsResid", sep = "_")
      relName <- paste("anova2", "RelResid", sep = "_")
    }
    
    baseData$df[, c(absName, relName)] <- aov2Mod()$TABRESID
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
    if (input$reg3prefix != ""){
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
  
  # Add factorial coordinates
  
  observeEvent(input$addcahclass, {
    if (input$cahprefix != ""){
      className <- paste(input$cahprefix, "CLASSES", sep = "_")
    } else {
      className <- "CLASSES"
    }
    
    baseData$df[, className] <- PlotProfile(classifobj = clusterComp(), nbclus = input$cahnclass)$CLUSID
  })
  
  
  # GUIDE  ----
  
  output$citation <- renderText("<strong>Once upon a time, statisticians only explored</strong> (John Tukey)")
  
  
  # DONNEES ----
  
  # show table
  output$contentstable <- renderDataTable(options = list(pageLength = 10), expr = {
    baseData$df
  })
  
  output$description <- renderText("<strong>Projet ANR Cartelec</strong> (cf. Guide d'utilisation).")
  
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
    if (input$uniquanti != "" & input$uniquali == ""){
      textResult <- paste("Nb. obs. = ", nrow(baseData$df), "<br/>",
                          "Valeurs manquantes = ", anyNA(baseData$df[, input$uniquanti]), "<br/>",
                          "Moyenne = ", round(mean(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2), "<br/>",
                          "Médiane = ", round(median(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2), "<br/>",
                          "Variance = ", round(var(baseData$df[, input$uniquanti], na.rm = TRUE), digits = 2), "<br/>",
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
  output$unitab <- renderTable(include.rownames = FALSE, expr = {
    if (input$uniquanti != "" & input$uniquali == ""){
      return()
    } else if (input$uniquanti == "" & input$uniquali != "") {
      if (length(unique(baseData$df[, input$uniquali])) > 30){
        stop("La variable sélectionnée n'est probablement pas qualitative")
      } else {
        tabFreq <- as.data.frame(table(baseData$df[, input$uniquali]))
        tabFreq$Perc <- round(100 * tabFreq$Freq / sum(tabFreq$Freq), digits = 2)
        colnames(tabFreq) <- c("Modalité", "Freq. absolue", "Freq. relative")
        return(tabFreq)
      }
    } else if (input$uniquanti == "" & input$uniquali == ""){
      return()
    } else {
      stop("Sélectionner une seule variable")
    }
  })
  
  # plot univariate
  output$uniplot <- renderPlot({
    if (input$uniquanti != "" & input$uniquali == ""){
      Histogram(df = baseData$df, varquanti = input$uniquanti, nbins = input$nbins, drawsummary = input$drawsummary)
    } else if (input$uniquanti == "" & input$uniquali != "") {
      if (length(unique(baseData$df[, input$uniquali])) > 30){
        stop("La variable sélectionnée n'est probablement pas qualitative")
      } else {
        Barplot(df = baseData$df, varquali = input$uniquali)
      }
    } else if (input$uniquanti == "" & input$uniquali == ""){
      return()
    } else {
      stop("Sélectionner une seule variable")
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
    req(input$qualiindep, input$qualidep)
    if((length(unique(baseData$df[, input$qualidep])) - 1) * (length(unique(baseData$df[, input$qualiindep])) - 1) > 150){
      stop("La ou les variables sélectionnées ne sont probablement pas qualitatives (ddl > 150)")
    } else {
      Mosaicplot(df = baseData$df, varx = input$qualiindep, vary = input$qualidep)
    }
  })
  
  # contingency table
  output$contingtab <- renderTable({
    req(input$qualiindep, input$qualidep)
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
  })
  
  # contingency text
  output$contingtext <- renderText({
    req(input$qualiindep, input$qualidep)
    chiResults <- chisq.test(baseData$df[, input$qualidep], baseData$df[, input$qualiindep])
    textResult <- paste("Chi2 = ", round(chiResults$statistic, 2), "<br/>",
                        "Phi = ", round(sqrt(chiResults$statistic / nrow(baseData$df)), 2), "<br/>",
                        "V de Cramer = ", round(sqrt(chiResults$statistic / (nrow(baseData$df) * min(dim(chiResults$observed)))), 2), 
                        sep = "")
    return(textResult)
  })
  
  # download plot
  output$downloadmosaicplot <- downloadHandler(
    filename = "mosaicplot.svg",
    content = function(file) {
      svg(file, width = input$widthmosaic / 2.54, height = input$heightmosaic / 2.54, pointsize = 8)
      req(input$qualiindep, input$qualidep)
      print(Mosaicplot(df = baseData$df, varx = input$qualiindep, vary = input$qualidep))
      dev.off()
    })
  
  
  # BIVARIE : QUANTI-QUANTI ----
  
  # print scatter plot
  output$scatterplot <- renderPlot({
    req(input$quantiindep, input$quantidep)
    ScatterPlot(df = baseData$df, varx = input$quantiindep, vary = input$quantidep)
  })
  
  # compute linear regression
  regMod <- reactive({
    req(input$quantiindep, input$quantidep)
    ComputeRegression(df = baseData$df, vardep = input$quantidep, varindep = input$quantiindep)
  })
  
  # print coefficients
  output$coefreg <- renderTable(include.rownames = FALSE, expr = {
    req(input$quantiindep, input$quantidep)
    regMod()$TABCOEF
  })
  
  # download plot
  output$downloadreg1 <- downloadHandler(
    filename = "regressionplot.svg",
    content = function(file) {
      svg(file, width = input$widthreg1 / 2.54, height = input$heightreg1 / 2.54, pointsize = 8)
      req(input$quantiindep, input$quantidep)
      print(ScatterPlot(df = baseData$df, varx = input$quantiindep, vary = input$quantidep))
      dev.off()
    })
  
  
  # BIVARIE : QUALI-QUANTI ----
  
  # print boxplot
  output$boxes <- renderPlot({
    req(input$quanliindep, input$quanlidep)
    Boxplot(df = baseData$df, varx = input$quanliindep, vary = input$quanlidep, jit = input$bpjitter)
  })
  
  # print aovplot
  output$aovplot <- renderPlot({
    req(input$quanliindep, input$quanlidep)
    AnovaPlot(df = baseData$df, varx = input$quanliindep, vary = input$quanlidep)
  })
  
  # compute linear regression
  aovMod <- reactive({
    req(input$quanliindep, input$quanlidep)
    ComputeRegression(df = baseData$df, vardep = input$quanlidep, varindep = input$quanliindep)
  })
  
  # print coefficients
  output$coefanova <- renderTable(include.rownames = FALSE, expr = {
    req(input$quanliindep, input$quanlidep)
    rbind(aovMod()$TABVAR, aovMod()$TABCOEF)
  })
  
  # print mean and variance
  output$tabanova <- renderTable(include.rownames = FALSE, expr = {
    req(input$quanliindep, input$quanlidep)
    AnovaTab(df = baseData$df, varx = input$quanliindep, vary = input$quanlidep)
  })
  
  # download plot
  output$downloadanova <- downloadHandler(
    filename = "anovaplot.svg",
    content = function(file) {
      svg(file, width = input$widthanova1 / 2.54, height = input$heightanova1 / 2.54, pointsize = 8)
      req(input$quanliindep, input$quanlidep)
      print(grid.arrange(Boxplot(df = baseData$df, varx = input$quanliindep, vary = input$quanlidep, jit = input$bpjitter),
                         AnovaPlot(df = baseData$df, varx = input$quanliindep, vary = input$quanlidep),
                         nrow = 2))
      dev.off()
    })
  
  
  # TRIVARIE : ANOVA 2 FACTORS ----
  
  # print boxplot
  output$boxes2 <- renderPlot({
    req(input$aovindep, input$aovdep)
    Boxplot2(df = baseData$df, 
             varx = input$aovindep[1], 
             vary = input$aovdep, 
             groupx = input$aovindep[2])
  })
  
  # print anova plot 
  output$anovaplot2 <- renderPlot({
    req(input$aovdep, input$aovindep)
    AnovaPlot2(df = baseData$df, varx = input$aovindep[1], vary = input$aovdep, groupx = input$aovindep[2], interact = input$interactaov2)
  })
  
  # compute linear regression
  aov2Mod <- reactive({
    req(input$aovdep, input$aovindep)
    ComputeRegression(df = baseData$df, vardep = input$aovdep, varindep = input$aovindep, interact = input$interactaov2)
  })
  
  # print coefficients
  output$coefanova2 <- renderTable(include.rownames = FALSE, expr = {
    req(input$aovdep, input$aovindep)
    aov2Mod()$TABCOEF
  })
  
  # print mean and variance 
  output$tabanova2 <- renderTable(include.rownames = FALSE, expr = {
    req(input$aovdep, input$aovindep)
    AnovaTab2(df = baseData$df, varx = input$aovindep[1], vary = input$aovdep, groupx = input$aovindep[2])
  })
  
  # print mean and variance 
  output$tabanova2interact <- renderTable(include.rownames = FALSE, expr = {
    req(input$aovdep, input$aovindep)
    AnovaTab2Interact(df = baseData$df, varx = input$aovindep[1], vary = input$aovdep, groupx = input$aovindep[2])
  })
  
  # download plot
  output$downloadanova2 <- downloadHandler(
    filename = "anovaplot.svg",
    content = function(file) {
      svg(file, width = input$widthanova2 / 2.54, height = input$heightanova2 / 2.54, pointsize = 8)
      req(input$aovindep, input$aovdep)
      print(grid.arrange(Boxplot2(df = baseData$df, varx = input$aovindep[1], vary = input$aovdep, groupx = input$aovindep[2]),
                         AnovaPlot2(df = baseData$df, varx = input$aovindep[1], vary = input$aovdep, groupx = input$aovindep[2], interact = input$interactaov2),
                         nrow = 2))
      dev.off()
    })
  
  
  # TRIVARIE : ANCOVA ----
  
  # print scatter
  output$scatterancov <- renderPlot({
    req(input$ancovdep, input$ancovindep)
    if(input$interactancov == FALSE){
      DrawRegressionLines(df = baseData$df, varx = input$ancovindep[1], vary = input$ancovdep, groupx = input$ancovindep[2])
    } else {
      ScatterPlotAncov(df = baseData$df, varx = input$ancovindep[1], vary = input$ancovdep, groupx = input$ancovindep[2])
    }
  })
  
  # compute linear regression
  ancovMod <- reactive({
    req(input$ancovdep, input$ancovindep)
    ComputeRegression(df = baseData$df, vardep = input$ancovdep, varindep = input$ancovindep, interact = input$interactancov)
  })
  
  # print coefficients
  output$coefancov <- renderTable(include.rownames = FALSE, expr = {
    req(input$ancovdep, input$ancovindep)
    ancovMod()$TABCOEF
  })
  
  # download plot
  output$downloadancova <- downloadHandler(
    filename = "ancovaplot.svg",
    content = function(file) {
      svg(file, width = input$widthancova / 2.54, height = input$heightancova / 2.54, pointsize = 8)
      req(input$ancovdep, input$ancovindep)
      print(ScatterPlotAncov(df = baseData$df, varx = input$ancovindep[1], vary = input$ancovdep, groupx = input$ancovindep[2]))
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
  
  
  principalComp <- eventReactive(input$buttonpca, {
    if(colnames(baseData$df)[1] == "BUREAU"){
      ComputePrincipalComp(df = baseData$df, varquanti = input$factovar, ident = "BUREAU")
    } else {
      if(input$idtab == ""){
        stop("Sélectionner une variable identifiant (onglet Données)")
      } else {
        ComputePrincipalComp(df = baseData$df, varquanti = input$factovar, ident = input$idtab)
      }
    }
  })
  
  
  
  # Correlation matrix
  
  output$facmatcor <- renderTable({
    req(principalComp)
    CorCompMat(dudiobj = principalComp(), xaxis = input$xaxis, yaxis = input$yaxis)
  })
  
  
  # Plot components
  
  output$compinert <- renderPlot({
    req(input$factovar)
    DecompInertia(dudiobj = principalComp())
  })
  
  # Plot individuals
  
  output$indivpca <- renderPlot({
    req(input$factovar)
    PlotIndiv(dudiobj = principalComp(), xaxis = as.integer(input$xaxis), yaxis = as.integer(input$yaxis), printlabel = input$labelindiv)
  })
  
  # Plot circle of correlations
  
  output$corcircle <- renderPlot({
    req(input$factovar)
    CorCircle(dudiobj = principalComp(), xaxis = as.integer(input$xaxis), yaxis = as.integer(input$yaxis))
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
  
  # download plots
  output$downloadpca <- downloadHandler(
    filename = "pcaplot.svg",
    content = function(file) {
      svg(file, width = input$widthpca / 2.54, height = input$heightpca / 2.54, pointsize = 8)
      req(input$factovar)
      print(grid.arrange(DecompInertia(dudiobj = principalComp()),
                         CorCircle(dudiobj = principalComp(), xaxis = as.integer(input$xaxis), yaxis = as.integer(input$yaxis)),
                         PlotIndiv(dudiobj = principalComp(), xaxis = as.integer(input$xaxis), yaxis = as.integer(input$yaxis), printlabel = input$labelindiv),
                         nrow = 3))
      dev.off()
    })
  
  
  # MULTIVARIE : CLASSIFICATION ----
  
  # Compute hierarchical clustering
  
  clusterComp <- eventReactive(input$buttoncah, {
    ComputeClassif(df = baseData$df, varquanti = input$cahvar, stand = input$cahstandardize, method = input$cahmethod)
  })
  
  # Plot dendrogram
  
  output$cahdendro <- renderPlot({
    req(clusterComp)
    withProgress(min = 0, message = "Calcul en cours", expr = {
      PlotDendro(classifobj = clusterComp())})
  })
  
  # Plot heigth
  
  output$cahheight <- renderPlot({
    req(clusterComp)
    PlotHeight(classifobj = clusterComp())
  })
  
  # Plot profile
  
  output$cahprofile <- renderPlot({
    req(clusterComp)
    PlotProfile(classifobj = clusterComp(), nbclus = input$cahnclass)$PROFILE
  })
  
  
  # download plots
  output$downloadclus <- downloadHandler(
    filename = "clusplot.svg",
    content = function(file) {
      svg(file, width = input$widthclus / 2.54, height = input$heightclus / 2.54, pointsize = 8)
      req(clusterComp)
      print(grid.arrange(PlotDendro(classifobj = clusterComp()),
                         PlotHeight(classifobj = clusterComp()),
                         PlotProfile(classifobj = clusterComp(), nbclus = input$cahnclass)$PROFILE,
                         nrow = 3))
      dev.off()
    })
  
  
  
  # CARTOGRAPHIE ----
  
  output$carto <- renderPlot({
    req(input$cartovar)
    if(colnames(baseData$df)[1] == "BUREAU"){
      CartoVar(spdf = baseData$spdf, df = baseData$df, idshape = "BUREAU", idtab = "BUREAU", 
               varquanti = input$cartovar, paltype = input$colpal, discret = input$cartomethod, nbcl = input$cartoclass)
    } else {
      CartoVar(spdf = baseData$spdf, df = baseData$df, idshape = input$idshape, idtab = input$idtab, 
               varquanti = input$cartovar, paltype = input$colpal, discret = input$cartomethod, nbcl = input$cartoclass)
    }
  })
  
  # download plots
  output$downloadcarto <- downloadHandler(
    filename = "cartoplot.svg",
    content = function(file) {
      svg(file, width = input$widthpca / 2.54, height = input$heightpca / 2.54, pointsize = 8)
      req(input$cartovar)
      if(colnames(baseData$df)[1] == "BUREAU"){
        CartoVar(spdf = baseData$spdf, df = baseData$df, idshape = "BUREAU", idtab = "BUREAU", 
                 varquanti = input$cartovar, paltype = input$colpal, discret = input$cartomethod, nbcl = input$cartoclass)
      } else {
        CartoVar(spdf = baseData$spdf, df = baseData$df, idshape = input$idshape, idtab = input$idtab, 
                 varquanti = input$cartovar, paltype = input$colpal, discret = input$cartomethod, nbcl = input$cartoclass)
      }
      dev.off()
    })
  
})

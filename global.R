##############################
# Shiny App: ExploratR - Exploration uni- bi- et multi-variée avec R
# Packages and fuctions
##############################


# load packages ----

require(RColorBrewer)
require(ggplot2)
library(scatterplot3d)
library(xtable)


# load data ----

load("data/ExploratR.RData")

baseData <- reactiveValues(spdf = bureauxParis, df = tabFinal)


# Draw histogram ----

Histogram <- function(df, varquanti, nbins = 15, drawsummary = FALSE){
  caseNumber <- nrow(df)
  myPlot <- ggplot(df) + 
    geom_histogram(aes_string(x = varquanti), color = "white", fill = "grey30", bins = nbins) +
    scale_y_continuous(paste("Fréquence (n = ", caseNumber, ")", sep = "")) + theme_bw()
  
  if (isTRUE(drawsummary)){
    myPlot <- myPlot +  
      geom_vline(xintercept = mean(df[[varquanti]], na.rm = TRUE), color = "chocolate4") +
      geom_vline(xintercept = quantile(df[[varquanti]], probs = seq(0, 1, 0.25), na.rm = TRUE)[2:4], color = "chartreuse4")
  }
  return(myPlot)
}


# Draw barplot ----

Barplot <- function(df, varquali){
  caseNumber <- nrow(df)
  myPlot <- ggplot(df) + 
    geom_bar(aes_string(x = varquali), color = "grey30", fill = "grey30") +
    scale_x_discrete("Modalités") +  scale_y_continuous(paste("Fréquence (n = ", caseNumber, ")", sep = "")) + 
    theme_bw()
  return(myPlot)
}


# Draw mosaic plot ----

Mosaicplot <- function(df, varx, vary){
  mosaicPlot <- mosaicplot(table(df[, varx], df[, vary]), main = paste("Tri croisé : ", varx, " - ", vary, sep = ""))
  return(mosaicPlot)
}


# Draw scatter plot ----

ScatterPlot <- function(df, varx, vary){
  scatPlot <- ggplot(df) + 
    geom_point(aes_string(x = varx, y = vary), color = "grey60") + 
    geom_smooth(aes_string(x = varx, y = vary), method = "lm", se = FALSE, color = "chocolate") +
    theme_bw()
  
  return(scatPlot)
}


# Draw boxplot ----

Boxplot <- function(df, varx, vary){
  boxPlot <- ggplot(df) + 
    geom_boxplot(aes_string(x = varx, y = vary), color = "grey20", fill = "grey70") +
    theme_bw()
  
  return(boxPlot)
}


# Draw boxplot 2 factors ----

Boxplot2 <- function(df, varx, vary, groupx){
  boxPlot <- ggplot(df) + 
    geom_boxplot(aes_string(x = varx, y = vary, fill = groupx), color = "grey20") +
    theme_bw()
  
  return(boxPlot)
}


# Draw scatter plot ----

ScatterPlotAncov <- function(df, varx, vary, groupx){
  scatPlot <- ggplot(df) + 
    geom_point(aes_string(x = varx, y = vary, group = groupx, color = groupx)) + 
    geom_smooth(aes_string(x = varx, y = vary, group = groupx, color = groupx), method = "lm", se = FALSE) +
    theme_bw()
  
  return(scatPlot)
}

# Draw scatter plot 3D ----

ScatterPlot3D <- function(df, varx, vary, varz){
  scatPlot <- scatterplot3d(x = df[, varx], y = df[, vary], z = df[, varz], pch = 20, highlight.3d = FALSE, angle = 35,
                            xlab = varx, ylab = vary, zlab = varz)
  fitMod <- lm(formula = formula(eval(paste(varz, "~", varx, "+", vary, sep = " "))), data = df)
  scatPlot$plane3d(fitMod, lty.box = "solid")
  
  return(scatPlot)
}


# Compute linear model ----

ComputeRegression <- function(df, vardep, varindep, interact = FALSE){
  if(interact == FALSE){
    linMod <- summary(lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "+")))), data = df))
  } else {
    linMod <- summary(lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "*")))), data = df))
  }
  coefReg <- round(linMod$coefficients, digits = 2)[, 1:2]
  rawR2 <- round(linMod$r.squared, digits = 2)
  adjR2 <- round(linMod$adj.r.squared, digits = 2)
  # matCor <- round(cor(df[, c(vardep, varindep)], use = "complete.obs", method = "pearson"), digits = 3)
  matCor <- 1
  tabResid <- data.frame(ABSRESID = round(linMod$residuals, digits = 3), 
                         RELRESID = round(linMod$residuals / (df[, vardep] - linMod$residuals), digits = 4))
  
  tabResults <- data.frame(CONCEPT = c("Coef. de détermination",
                                       "Coef. de détermination multiple",
                                       row.names(coefReg)),
                           VALEUR = c(rawR2, adjR2, coefReg[, 1]),
                           stringsAsFactors = FALSE)
  return(list(TABCOEF = tabResults, TABRESID = tabResid, MATCOR = matCor))
}


ComputeRegressionMult <- function(df, vardep, varindep, interact = FALSE){
  if(interact == FALSE){
    linMod <- summary(lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "+")))), data = df))
  } else {
    linMod <- summary(lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "*")))), data = df))
  }
  coefReg <- round(linMod$coefficients, digits = 2)[, 1:2]
  rawR2 <- round(linMod$r.squared, digits = 2)
  adjR2 <- round(linMod$adj.r.squared, digits = 2)
  matCor <- round(cor(df[, c(vardep, varindep)], use = "complete.obs", method = "pearson"), digits = 3)
  tabResid <- data.frame(ABSRESID = round(linMod$residuals, digits = 3), 
                         RELRESID = round(linMod$residuals / (df[, vardep] - linMod$residuals), digits = 4))
  
  tabResults <- data.frame(CONCEPT = c("Coef. de détermination",
                                       "Coef. de détermination multiple",
                                       row.names(coefReg)),
                           VALEUR = c(rawR2, adjR2, coefReg[, 1]),
                           stringsAsFactors = FALSE)
  return(list(TABCOEF = tabResults, TABRESID = tabResid, MATCOR = matCor))
}



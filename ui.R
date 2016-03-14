##############################
# Shiny App: ExploratR - Exploration uni- bi- et multi-variée avec R
# User interface
##############################

shinyUI(fluidPage(
  titlePanel("ExploratR - Exploration uni- bi- et multivariée avec R",
             tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
                       tags$title("ExploratR - Exploration uni- bi- et multivariée avec R"))
  ),
  
  tabsetPanel(
    tabPanel("Données",
             fluidRow(
               column(3, wellPanel(
                 tags$h4("Données d'exemple"),
                 htmlOutput("description")
               )),
               
               column(9, wellPanel(
                 tags$hr(),
                 dataTableOutput("contentstable")
               )
               )
             )
    ),
    
    tabPanel("Univarié",
             fluidRow(
               column(3, wellPanel(
                 tags$h4("Choisir la variable à explorer"),
                 uiOutput("coluniquanti"),
                 uiOutput("coluniquali"),
                 checkboxInput("uniset", "Personnaliser l'histogramme"),
                 conditionalPanel(condition = "input.uniset == true",
                                  uiOutput("slideruni"),
                                  checkboxInput("drawsummary", 
                                                label = "Tracer les résumés (Q1, Q2, Q3, Moyenne)", 
                                                value = FALSE)
                 ))),
               column(5, 
                      tags$h4("Résumé graphique"),
                      plotOutput("uniplot")),
               column(4, 
                      tags$h4("Résumé numérique"),
                      htmlOutput("unisummary"))
             )
    ),
    
    tabPanel("Bivarié",
             tabsetPanel(
               tabPanel("CONTNGENCE (quali-quali)",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            uiOutput("colqualidep"),
                            uiOutput("colqualiindep"),
                            
                            radioButtons(inputId = "contcont", label = "Contenu du tableau",
                                         c("Effectifs observés" = "obsfreq",
                                           "Pourcentages en ligne" = "rowpct",
                                           "Effectifs espérés" = "expfreq",
                                           "Résidus bruts" = "rawresid",
                                           "Résidus standardisés" = "stdresid"),
                                         selected = "obsfreq")
                          )),
                          column(5,
                                 tags$h4("Résumé graphique"),
                                 plotOutput("mosaic")
                          ),
                          column(4, 
                                 tags$h4("Résumé numérique"),
                                 tags$h5("Mesures locales"),
                                 tableOutput("contingtab"),
                                 tags$h5("Mesures globales"),
                                 htmlOutput("contingtext")
                          )
                        )
               ),
               
               
               tabPanel("RÉGRESSION (quanti-quanti)",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            uiOutput("colquantidep"),
                            uiOutput("colquantiindep")
                          )),
                          column(5, 
                                 tags$h4("Résumé graphique"),
                                 plotOutput("scatterplot")),
                          column(4,
                                 tags$h4("Résumé numérique"),
                                 htmlOutput("coefreg"))
                        )
               ),
               
               tabPanel("ANOVA (quali-quanti)",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            uiOutput("colquanlidep"),
                            uiOutput("colquanliindep")
                          )),
                          column(5, 
                                 tags$h4("Résumé graphique"),
                                 plotOutput("boxes")),
                          column(4,
                                 tags$h4("Résumé numérique"),
                                 htmlOutput("coefanova")
                          )
                        )
               )
             )
    ),
    
    tabPanel("Trivarié",
             tabsetPanel(
               tabPanel("ANOVA (2 quali)",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            uiOutput("colaovdep"),
                            uiOutput("colaovindep")
                          )),
                          column(5,
                                 tags$h4("Résumé graphique"),
                                 plotOutput("boxes2")
                          ),
                          column(4, 
                                 tags$h4("Résumé numérique"),
                                 htmlOutput("coefanova2")
                          )
                        )
               ),
               
               tabPanel("ANCOVA (1 quali / 1 quanti)",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            uiOutput("colancovdep"),
                            uiOutput("colancovindep"),
                            checkboxInput(inputId = "interactancov", 
                                          label = "Interaction entre les variables explicatives",
                                          value = FALSE)
                          )),
                          column(5,
                                 tags$h4("Résumé graphique"),
                                 plotOutput("scatterancov")
                          ),
                          column(4, 
                                 tags$h4("Résumé numérique"),
                                 htmlOutput("coefancov")
                          )
                        )
               ),
               
               tabPanel("RÉGRESSION (2 quanti)",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            uiOutput("colquanti2dep"),
                            uiOutput("colquanti2indep"),
                            checkboxInput(inputId = "interactreg", 
                                          label = "Interaction entre les variables explicatives",
                                          value = FALSE)
                          )),
                          column(5,
                                 tags$h4("Résumé graphique"),
                                 plotOutput("scatterreg2")
                          ),
                          column(4, 
                                 tags$h4("Résumé numérique"),
                                 htmlOutput("coefreg2")
                          )
                        )
               )
             )
    ),
    
    
    tabPanel("Multivarié",
             tabsetPanel(
               tabPanel("RÉGRESSION",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            uiOutput("colregmultdep"),
                            uiOutput("colregmultindep")
                          )),
                          column(5,
                                 tags$h4("Matrice de corrélation"),
                                 htmlOutput("matcor")
                          ),
                          column(4, 
                                 tags$h4("Résumé numérique du modèle"),
                                 htmlOutput("coefregmult")
                          )
                        )
               ),
               
               tabPanel("ANALYSE FACTORIELLE")
               )
             ),
    
    tabPanel("Guide d'utilisation", 
             fluidRow(
               column(2, wellPanel()),
               column(10, includeMarkdown("README.md"))))
  )
)
)



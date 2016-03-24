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
                                 tags$h5(HTML("<strong>Mesures locales</strong>")),
                                 tableOutput("contingtab"),
                                 tags$h5(HTML("<strong>Mesures globales</strong>")),
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
                          ),
                          checkboxInput("reg1save", "Enregistrer les résidus"),
                          conditionalPanel(condition = "input.reg1save == true",
                                           fluidRow(
                                             column(2, textInput(inputId = "reg1prefix", label = "Préfixe", value = "")),
                                             column(1, actionButton(inputId = "addreg1resid", label = "Ajouter les résidus"))
                                           )
                          )),
                          column(5, 
                                 tags$h4("Résumé graphique"),
                                 plotOutput("scatterplot")),
                          column(4,
                                 tags$h4("Résumé numérique"),
                                 tableOutput("coefreg"))
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
                                 tableOutput("coefanova")
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
                                 tableOutput("coefanova2")
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
                                 tableOutput("coefancov")
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
                                 tableOutput("coefreg2")
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
                                 tableOutput("matcor")
                          ),
                          column(4, 
                                 tags$h4("Résumé numérique du modèle"),
                                 tableOutput("coefregmult")
                          )
                        )
               ),
               
               tabPanel("ANALYSE FACTORIELLE",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            uiOutput("colfacto"),
                            uiOutput("colid"),
                            selectInput("xaxis", label = "Axe des abscisses (x)", choices = 1:4, selected = 1, multiple = FALSE, selectize = TRUE),
                            selectInput("yaxis", label = "Axe des ordonnées (y)", choices = 1:4, selected = 2, multiple = FALSE, selectize = TRUE))
                          ),
                          column(9,
                                 tags$h4("Matrice de corrélation"),
                                 tableOutput("facmatcor"))
                        ),
                        fluidRow(
                          column(3, wellPanel()),
                          column(4,
                                 tags$h4("Cercle des corrélations"),
                                 plotOutput("corcircle")),
                          column(5,
                                 tags$h4("Décomposition de l'inertie"),
                                 plotOutput("compinert")
                          )
                        ),
                        fluidRow(
                          column(3, wellPanel()),
                          column(4,
                                 tags$h4("Contribution des variables (somme = 1000)"),
                                 tableOutput("contribvar")),
                          column(5,
                                 tags$h4("Contribution des individus (somme = 1000)"),
                                 dataTableOutput("contribind")
                          )
                        )
               )
             )
    ),
    
    tabPanel("Cartographie", 
             fluidRow(
               column(3, wellPanel(
                 tags$h4("Choisir la variable"),
                 uiOutput("colcartovar"),
                 selectInput("cartomethod", label = "Méthode de discrétisation", choices = c("sd", "equal", "quantile", "jenks"), selected = "quantile", multiple = FALSE, selectize = TRUE),
                 sliderInput("cartoclass", label = "Nombre de classes", min = 1, max = 8, value = 4)
               )),
               column(9, 
                      plotOutput("carto"))
             )
    ),
    
    tabPanel("Guide d'utilisation", 
             fluidRow(
               column(2, wellPanel()),
               column(10, includeMarkdown("README.md"))))
  )
)
)



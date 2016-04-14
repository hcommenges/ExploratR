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
    
    # Guide ----
    
    tabPanel("Guide d'utilisation", 
             fluidRow(
               column(2, wellPanel(
                 htmlOutput("citation")
               )),
               column(10, includeMarkdown("README.md")))),
    
    # Données ----
    
    tabPanel("Données",
             fluidRow(
               column(3, wellPanel(
                 fluidRow(
                   tags$h4("Charger les données d'exemple"),
                   actionButton(inputId = "loadExData", label = "Chargement"),
                   htmlOutput("description"),
                   tags$hr(),
                   tags$h4("Charger ses propres données"),
                   checkboxInput(inputId = "showsettings", value = FALSE, label = "Options de chargement"),
                   conditionalPanel(
                     condition = "input.showsettings == true",
                     fileInput("fileInput", "Charger fichier CSV", multiple = FALSE),
                     checkboxInput("header", "Header", TRUE),
                     radioButtons("sep", "Separator", c(Comma = ',',Semicolon = ';',Tab='\t'),','),
                     radioButtons("dec", "Decimal", c(Comma = ',', Point = '.'), '.'),
                     radioButtons("quote", "Quote", c(None = '','Double Quote'='"','Single Quote'="'"),'"')
                   ),
                   tags$hr(),
                   tags$h4("Récupérer le tableau"),
                   radioButtons("csvtype", "Options du CSV", c("Norme anglo (virgule et point)" = "anglo",
                                                               "Norme franco (point-virgule et virgule)" = "franco")),
                   downloadButton("downloaddata", "Télécharger")
                 )
               )),
               
               column(9, wellPanel(
                 tags$hr(),
                 dataTableOutput("contentstable")
               )
               )
             )
    ),
    
    # Univarié ----
    
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
                 ),
                 tags$br(),
                 tags$h4("Récupérer le graphique"),
                 numericInput(inputId = "widthuni", label = "Width (cm)", value = 20, min = 1, max = 30),
                 numericInput(inputId = "heightuni", label = "Height (cm)", value = 15, min = 1, max = 30),
                 downloadButton("downloaduniplot", "Télécharger")
               )),
               column(5, 
                      tags$h4("Résumé graphique"),
                      plotOutput("uniplot")),
               column(4, 
                      tags$h4("Résumé numérique"),
                      htmlOutput("unisummary"))
             )
    ),
    
    # Bivarié ----
    
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
                                         selected = "obsfreq"),
                            tags$br(),
                            tags$h4("Récupérer le graphique"),
                            numericInput(inputId = "widthmosaic", label = "Width (cm)", value = 20, min = 1, max = 30),
                            numericInput(inputId = "heightmosaic", label = "Height (cm)", value = 15, min = 1, max = 30),
                            downloadButton("downloadmosaicplot", "Télécharger")
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
                            uiOutput("colquantiindep"),
                            checkboxInput("reg1save", "Enregistrer les résidus"),
                            conditionalPanel(condition = "input.reg1save == true",
                                             textInput(inputId = "reg1prefix", label = "Préfixe", value = ""),
                                             actionButton(inputId = "addreg1resid", label = "Ajouter les résidus")),
                            tags$br(),
                            tags$h4("Récupérer le graphique"),
                            numericInput(inputId = "widthreg1", label = "Width (cm)", value = 20, min = 1, max = 30),
                            numericInput(inputId = "heightreg1", label = "Height (cm)", value = 15, min = 1, max = 30),
                            downloadButton("downloadreg1", "Télécharger")
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
                            uiOutput("colquanliindep"),
                            tags$br(),
                            tags$h4("Récupérer le graphique"),
                            numericInput(inputId = "widthanova1", label = "Width (cm)", value = 20, min = 1, max = 30),
                            numericInput(inputId = "heightanova1", label = "Height (cm)", value = 15, min = 1, max = 30),
                            downloadButton("downloadanova", "Télécharger")
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
    
    # Trivarié ----
    
    tabPanel("Trivarié",
             tabsetPanel(
               tabPanel("ANOVA (2 quali)",
                        fluidRow(
                          column(3, wellPanel(
                            tags$h4("Choisir les variables"),
                            uiOutput("colaovdep"),
                            uiOutput("colaovindep"),
                            tags$br(),
                            tags$h4("Récupérer le graphique"),
                            numericInput(inputId = "widthanova2", label = "Width (cm)", value = 20, min = 1, max = 30),
                            numericInput(inputId = "heightanova2", label = "Height (cm)", value = 15, min = 1, max = 30),
                            downloadButton("downloadanova2", "Télécharger")
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
                                          value = FALSE),
                            checkboxInput("reg2save", "Enregistrer les résidus"),
                            conditionalPanel(condition = "input.reg2save == true",
                                             textInput(inputId = "reg2prefix", label = "Préfixe", value = ""),
                                             actionButton(inputId = "addreg2resid", label = "Ajouter les résidus"),
                                             tags$br(),
                                             tags$h4("Récupérer le graphique"),
                                             numericInput(inputId = "widthancova", label = "Width (cm)", value = 20, min = 1, max = 30),
                                             numericInput(inputId = "heightancova", label = "Height (cm)", value = 15, min = 1, max = 30),
                                             downloadButton("downloadancova", "Télécharger")
                            )
                          )
                          ),
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
                                          value = FALSE),
                            checkboxInput("reg3save", "Enregistrer les résidus"),
                            conditionalPanel(condition = "input.reg3save == true",
                                             textInput(inputId = "reg3prefix", label = "Préfixe", value = ""),
                                             actionButton(inputId = "addreg3resid", label = "Ajouter les résidus")),
                            tags$br(),
                            tags$h4("Récupérer le graphique"),
                            numericInput(inputId = "widthreg2", label = "Width (cm)", value = 20, min = 1, max = 30),
                            numericInput(inputId = "heightreg2", label = "Height (cm)", value = 15, min = 1, max = 30),
                            downloadButton("downloadreg2", "Télécharger")
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
  
  # Multivarié ----
  
  tabPanel("Multivarié",
           tabsetPanel(
             tabPanel("RÉGRESSION",
                      fluidRow(
                        column(3, wellPanel(
                          tags$h4("Choisir les variables"),
                          uiOutput("colregmultdep"),
                          uiOutput("colregmultindep"),
                          checkboxInput("reg4save", "Enregistrer les résidus"),
                          conditionalPanel(condition = "input.reg4save == true",
                                           textInput(inputId = "reg4prefix", label = "Préfixe", value = ""),
                                           actionButton(inputId = "addreg4resid", label = "Ajouter les résidus")
                          )
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
                        column(3, wellPanel(checkboxInput("facsave", "Enregistrer les coordonnées"),
                                            conditionalPanel(condition = "input.facsave == true",
                                                             textInput(inputId = "facprefix", label = "Préfixe", value = ""),
                                                             actionButton(inputId = "addfaccoord", label = "Ajouter les coordonnées factorielles")
                                            ))),
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
  
  # Cartographie ----
  
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
  )
)
)
)



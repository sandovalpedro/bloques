#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
library(tidyverse)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
  navbarPage("Randomized Block Design (RBD)", inverse=T,
             
             tabPanel("Home",
                      column(5,br(),br(),br(),br(),
                             h2(HTML("Randomized Block Design (RBD)")),
                             h3(HTML("A shiny app for understanding the use, 
                                     interpretation, and limitations of complete 
                                     randomized block design in biology science."))
                      ),
                      column(7,
                             img(src = "figura1.PNG", width=600)
                      )
             ),
             
             
             
             navbarMenu(HTML("Goals and Statistical <br>Background"),
                        
                        tabPanel("Goals",
                                 withMathJax(),
                                 hr(),
                                 h3(HTML("What are we interested in?")),
                                 hr(),
                                 
                                 column(7,
                                        h4(HTML("The RBD is carried out when there is evidence of inhomogeneity 
                                          of the experimental units. The inclusion of blocks in the 
                                          design increases the sensitivity of the experiment.
                                     <br><br>
                                     The hypotheses to be tested are:
                                     ")),
                                        p(HTML("$$H_{0}: \\alpha_{i}=0$$")),
                                        p(HTML("$$H_{a}: \\alpha_{i} \\neq 0$$")),
                                        
                                        h4(HTML("Note that there is no hypothesis test for block.<br><br>")),
                                        
                                        h4(HTML("These hypotheses are formulated in terms of effects, 
                             then \\(H_{0}\\) implies that the effect of the ith treatment on \\(\\mu\\) 
                             is \\(0\\), no effect of factor or treatment.
                               On the other hand \\(H_{a}\\) implies that at least one of the treatments 
                               differs from the others, implies that there is a treatment effect. 
                               "))
                                 )
                        ),
                        
                        tabPanel(HTML("The model"),
                                 
                                 column(7,hr(),
                                        h3(HTML("Linear Model for RBD")),
                                        hr(),
                                        
                                        h5(HTML("The mathematical model to fit data to a random block  
                                     design (RBD), with the same number of replicates for each level of the 
                                     factor is:
                                     <br>
                                     
                                     $$y_{ij} = \\mu + b_i + \\alpha_j + \\epsilon_{ij}$$
                                     
                                     where \\(y_{ij}\\) is the response variable for the ith block and jth treatment,
                                     \\(\\mu\\) is the global mean of the combined populations,
                                     \\(b_i\\) is the effect produced by the ith block,
                                     \\(\\alpha_j\\) is the effect produced by the jth treatment, and finally 
                                     \\(\\epsilon_{ij}\\) is the random experimental error associated with each observation 
                                     \\(y_{ij}\\).<br><br>
                                     To make estimates and hypothesis tests on the parameters \\(\\mu\\) and 
                                     \\(\\alpha_{j}\\) is necessary to establish some conditions: <br><br>
                                     1. The experimental errors \\(\\epsilon_{ij}\\) are independent and normally distributed 
                                     \\(N(0,\\sigma^2)\\)<br>
                                     2. Each treatment defines a population with normal distribution \\(N(\\mu_j,\\sigma^2)\\) 
                                     and equal variance (homoscedasticity).
                                     
                                     
                                     "))
                                        
                                        
                                 )
                                 
                        ),
                        
                        tabPanel("Fitting Model",
                                 hr(),
                                 h3(HTML("How to fit a CRD model?")),
                                 hr(),
                                 column(8,
                                        h4(HTML("Usin matrix notations we can write the effect model as:
                                    
                                    $$Y = X\\beta+\\epsilon$$
                                    
                                    where \\(X\\) is called matrix design.<br><br>
                                    
                                    The least squares estimators for \\(\\beta\\) are 
                                    obtained by solving the matrix equationn:
                                    
                                    $$Y'Y - \\hat \\beta^{'} X'Y = Y'(I-X(X'X)^{-}X')Y$$
                                    
                                    where:
                                    
                                    $$(X'X)^{-1}X'Y = \\hat \\beta $$
                                    
                                    
                                    "))
                                        
                                 )
                                 
                        ),
                        
                        tabPanel(HTML("Verifying Assumptions<br> of the model"),hr(),
                                 h3(HTML("Is the fitted linear model valid?")),hr(),
                                 
                                 column(10,
                                        h5(HTML("It is important to check the assumptions when applying a statistical technique, 
                                      and in the case of model fit 
                                     \\(y_{ij} = \\mu+b_i+\\alpha_j+\\epsilon_{ij}\\), is no exception.
                                     <br><br>
                                     Three assumptions must be verified to trust the model estimators: <br>
                                     
                                     1. The uncorrelation of the residuals <br>
                                     2. Constant variance in residuals <br>
                                     3. Normality of the residuals <br><br>
                                     
                                     The verification of the first assumption is made when the observations are 
                                     not taken sequentially in time or space and do not have a cluster-like 
                                     structure.<br><br>
                                     
                                     The assumption of equal variances (homoscedasticity) is not satisfied when 
                                     the response variable of the linear model comes from counts or binary data, 
                                     it can even be affected by the presence of atypical data..<br><br>
                                     
                                     To verify this assumption there are two ways, graphically or statistical 
                                     tests. Graphically, a graph of residuals against predicted is made, it 
                                     is common that it is made with the standardized residuals, these being: 
                                     
                                     $$r_{i0} = \\frac{\\hat \\epsilon_i}{\\sqrt(1-x_i(X'X)^-x'_i)}$$
                                     
                                     It is important that the chart does not show funnel-like behavior or some other kind of pattern..<br><br>
                                     
                                     On the other hand, a Bartlett test can be performed whose hypotheses are given by:
                                     
                                     $$H_0:\\sigma^2_1=\\sigma^2_2=\\cdots=\\sigma^2_k$$
                                     $$H_1:\\sigma^2_i\\neq \\sigma^2_j \\hspace{1cm}i\\neq j$$
                                     
                                     The assumption of normality in some cases is related to that of homoscedasticity. <br><br>
                                     
                                     Graphically, the normal probabilistic graph or QQ-Plot can be used. This type of graph 
                                     looks for the points to fall on the line of the theoretical quantiles of the normal 
                                     distribution.<br><br>
                                     
                                     A statistical test used to check for normality on the residuals is the Shapiro-Wilks test. 
                                     The hypothesis to be tested is: <br>
                                     $$H_0:\\textit{The sample comes from a normal population.}$$
                                     
                                     This hypothesis is contrasted with the statistic:
                                     
                                     $$W_c = \\frac{1}{ns^2}\\left(\\sum_{i=1}^h a_{in}(w_{n-i+1}-w_i)\\right)^2$$

                                     
                                     ")),
                                 )
                                 
                                 
                        ),
                        
                        tabPanel("Power",hr(),
                                 
                                 h3(HTML("What is the power in a randomized block design?")),hr(),
                                 column(8,
                                        
                                        h4(HTML("In the case of RBD, the objective is to determine 
                                                the number of blocks associated with the probability 
                                                of detecting significant differences in the treatments.
                                                <br><br>
                                                
                                                The power associated with the RBD has a 
                                                theoretical probability distribution given by:
                                                
                                                $$F_{t-1,(b-1)(t-1),\\lambda}$$
                                                
                                                Where \\(t\\) is the number of treatments and \\(b\\) is the 
                                                number of blocks.<br><br>
                                                
                                                The non-centrality parameter for the F test associated 
                                                with detecting significant differences in treatments 
                                                for RBD is:
                                                
                                                $$\\lambda = \\displaystyle\\frac{b}{\\sigma^2}\\displaystyle\\sum_{j=1}^{t}(\\mu_j-\\bar \\mu)^2$$
                                              
                                              ")),
                                 )
                        ),
                        
                        tabPanel("Results",hr(),
                                 h3(HTML("ANOVA table elements for randomized block design")),hr(),
                                 
                                 column(7,
                                        
                                        h5(HTML("When a researcher applies a randomized block design, the goal is to 
                                              check if there are differences between the levels 
                                              of treatments.<br><br>
                                              
                                              The ANOVA table for a randomized block design is:
                                              
                                              ")),
                                        
                                        img(src = "figura1.JPG", width=500),
                                        
                                        h5(HTML("Note that there is no F ratio for the block, since the block 
                                                is included to homogenize the experimental units..<br><br>
                                              
                                              The elements of the sum of squares for ANOVA table, are given by:
                                              
                                              $$ssT=q\\displaystyle\\sum_{j=1}^{p}(\\bar y_{j}-\\bar y)^{2}
                                              \\hspace{1cm}
                                              ssBlock=p\\displaystyle\\sum_{i=1}^{q}(\\bar y_{i}-\\bar y)^{2}
                                              \\hspace{1cm}
                                              ssR = \\displaystyle\\sum_{i=1}^{p}\\displaystyle\\sum_{j=1}^q(\\bar y_{ij}-\\bar y_i-\\bar y_j + \\bar y)^2
                                              $$
                                              
                                              $$ssTotal = \\displaystyle\\sum_{i=1}^p\\displaystyle\\sum_{j=1}^q(y_{ij}-\\bar y)^2
                                              $$
                                              
                                              On the other hand, the elements that make up the Mean Square are given by:
                                              
                                              $$msT = \\displaystyle\\frac{ssT}{p-1}
                                              \\hspace{0.5cm}
                                              msBlock = \\displaystyle\\frac{ssBlock}{q-1}
                                              \\hspace{0.5cm}
                                               msE = \\displaystyle\\frac{ssE}{(p-1)(q-1)}
                                              $$
                                              
                                              ")),
                                        h5(HTML("The last column shows the ratio between \\(msT\\) and \\(msE\\), if this result is \\(\\gg 1\\), 
                                     this implies that there is greater variability between treatments than within treatments, 
                                     this generates significant differences between treatments.<br><br>
                                     In \\(R\\), the ANOVA table has a column associated with the p-value, which represents the 
                                     probability associated with the value of the F-ratio of a probability distribution \\(F\\). 
                                     If the p-value is less than alpha, then there are significant differences in the treatments.
                                     "))
                                        
                                 )
                                 
                        )
                        
                      ),
             
             tabPanel(HTML("Parameter's <br> settings"), 
                      fluidPage(
                        column(3,style = "background-color:#d4af37;",
                               br(),
                               h4("Parameters' settings"),
                               hr(),
                               numericInput('num_sim',withMathJax('Simulations'),value=500),
                               numericInput('mu',withMathJax('Mean in absence of effects (\\(\\mu\\))'),value=100),
                               numericInput('sigma',withMathJax('Common sigma (\\(\\sigma\\))'),value=3),
                               numericInput('n','Sample size per cell (\\(n\\))',value=1),
                               numericInput('alfa','Significance level (\\(\\alpha\\))',value=0.05),
                               hr(),
                               numericInput('rows',withMathJax('Levels of factor F (rows: \\(\\alpha_j\\) )'),value=4),
                               h5(withMathJax('Effects of \\(F\\) (\\(\\alpha_i\\))')),
                               rHandsontableOutput("input_table_F1"),
                               br(),
                               numericInput('cols',withMathJax('Levels of Blocks F2 (cols: \\(b_i\\))'),value=8),
                               h5(withMathJax('Blocks of \\(b\\) (\\(b_i\\))')),
                               rHandsontableOutput("input_table_F2"),
                               hr(),
                               #h5(withMathJax('Interaction parameters (\\(\\gamma_{ij}\\))')),
                               #rHandsontableOutput("input_table"),
                               #hr(),
                               fluidPage(
                                 column(3,actionButton('go','New data')),
                                 br(),br(),
                                 column(8,sliderInput('yRange','Range for response axes',value=c(90,120),min=0,max=200))
                               )
                        ),
                        
                        column(9,
                               tabsetPanel(
                                 tabPanel('Heat Map',
                                          br(),br(),
                                          h3('Heat Map'),
                                          hr(),
                                          withMathJax("The heat map for levels of factor \\(\\alpha_i\\) and blokcs \\(b_j\\)"),
                                          #withMathJax("\\(\\mu_{ij}=\\mu + b_j + \\alpha_i \\)"),
                                          hr(),
                                          plotOutput('PlotCleanMeans'),
                                          hr()
                                          #h4(HTML("Tabla de medias entre celdas")),
                                          #tableOutput('TableCleanMeans')
                                          ),
                                 tabPanel('Observed means',
                                          column(7,
                                                 br(),br(),
                                                 h3('Observed means'),
                                                 hr(),
                                                 h3(HTML("Summary by block")),
                                                 tableOutput('TableMeans'),
                                                 hr(),
                                                 h3(HTML("Summary by treatment")),
                                                 tableOutput('TableMeans1'),hr(),
                                                 h3("Descriptive Plot"),
                                                 hr(),
                                                 plotOutput('Means')
                                                 ),
                                          column(3,
                                                 h3(HTML("Simulation Data")),
                                                 tableOutput('datos')
                                                 ),
                                          column(2,br(),br(),br(),br(),
                                                 downloadButton('downloadData', 'Save data as *.csv', 
                                                 style = "color:white;background-color:black;")
                                                 )
                                          
                                          ),
                                 tabPanel('Check assumptions',
                                          br(),
                                          hr(),
                                          column(6,
                                                 plotOutput('homocedas'),
                                                 hr(),
                                                 h3(HTML("Bartlet test")),
                                                 verbatimTextOutput('Barlett')
                                                 ),
                                          column(6,
                                                 plotOutput('normali'),
                                                 hr(),
                                                 h3(HTML("Shapiro test")),
                                                 verbatimTextOutput('shapiro')
                                                 )),
                                 tabPanel('ANOVA',
                                          column(6,
                                                 #checkboxInput('optAnova','Test without interactions?',value=FALSE),
                                                 h3('ANOVA table with blocks'),
                                                 hr(),
                                                 verbatimTextOutput('ANOVA'),
                                                 hr(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                                 h3('ANOVA table without block'),
                                                 hr(),
                                                 verbatimTextOutput('ANOVA1'),
                                                 ),
                                          column(6,
                                                 h3(HTML("Pairwise comparison with blocks")),
                                                 hr(),
                                                 plotOutput('plot_emmeans2'),hr(),
                                                 h3(HTML("Pairwise comparison without blocks")),
                                                 hr(),
                                                 plotOutput('plot_emmeans31')
                                                 )
                                         
                                          #h3('Estimated effects and 95% CI'),
                                          #hr(),
                                          #fluidPage(
                                          #  column(8,verbatimTextOutput('TableEstimatedEffects'))
                                          #)
                               ),
                               # tabPanel('Pairwise comparisons',
                               #          fluidPage(
                               #            br(),
                               #            h3('Effects'),
                               #            hr(),
                               #            column(6 ,
                               #                   h3(HTML("Compración por pares para \\(F_1\\)")),
                               #                   plotOutput('plot_emmeans2')),
                               #            column(6,
                               #                   h3(HTML("Compración por pares para \\(F_2\\)")),
                               #                   plotOutput('plot_emmeans3'))
                               #          )
                               #          ),
                               
                               tabPanel(HTML("Function <br>power"),
                                        column(1,
                                               ),
                                        column(10,
                                               h3(HTML("Power function for \\(\\alpha_i\\)")),
                                               plotlyOutput("curve_powera"),hr(),br(),br(),
                                               ),
                                        # h3(HTML("Función de potencia para factor \\(\\beta_j\\)")),
                                        # plotlyOutput("curve_powerb"),hr(),br(),br(),
                                        # h3(HTML("Función de potencia para interacción \\(\\gamma_{ij}\\)")),
                                        # plotlyOutput("curve_powerc"),br()
                               ),
                               
                               tabPanel('Power',
                                        column(2,),
                                        column(8,
                                               h3(HTML("Power for factor \\(\\alpha_i\\)")),
                                               #actionButton('go_a', 'Potencia \\(F_1(\\alpha_i)\\)', class = "btn-warning"),
                                               plotOutput('f_nocentrada_a'),
                                               #tableOutput("algo")
                                        )
                                        # column(6,
                                        #        h3(HTML("Potencia para factor \\(B\\)")),
                                        #        #actionButton('go_b','Potencia \\(F_2(\\beta_j)\\)', class = "btn-warning"),
                                        #        plotOutput('f_nocentrada_b'),
                                        #        h3(HTML("Potencia para factor \\(\\gamma_{ij}\\)")),
                                        #        #actionButton('go_int','Potencia \\(\\gamma_{ij}\\)', class = "btn-warning"),
                                        #        plotOutput('f_nocentrada_int'),
                                        #        #actionButton('go_tab','Table', class = "btn-warning"),
                                        #        #tableOutput('css'),
                                        #        #verbatimTextOutput('mm')
                                        # )
                                        
                                        
                                        
                                        
                                        )
                               )
                               
                      ))
                      ),
             
             # tabPanel(HTML("Analizando <br>datos propios"),
             #          tabsetPanel(
             #            tabPanel("Cargando datos",
             #                     column(4,
             #                            fileInput('target_upload1', 'Choose file to upload',
             #                                      accept = c(
             #                                        'text/csv',
             #                                        'text/comma-separated-values',
             #                                        '.csv'
             #                                      )),
             #                            radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=";",inline=TRUE),
             #                            dataTableOutput("sample_table")
             #                     )
             #            ),
             #            tabPanel("Descriptiva",br(),br(),br(),
             #                     column(6,
             #                            h3(HTML("Descripción de los datos en forma tabular")),
             #                            tableOutput("descc_plot")
             #                     ),
             #                     column(6,
             #                            h3(HTML("Descripción de los datos en forma gráfica")),
             #                            plotOutput("plot_plot")
             #                     )
             #            ),
             #            tabPanel("Supuestos",br(),br(),
             #                     column(6,
             #                            h3(HTML("Normalidad")),
             #                            plotOutput("norm_norm"),
             #                            verbatimTextOutput("shapiro_norm")
             #                     ),
             #                     column(6,
             #                            h3(HTML("Homocedasticidad")),
             #                            plotOutput("homo_homo"),
             #                            verbatimTextOutput("bartlet_homo")
             #                     )
             #                     
             #                     
             #            ),
             #            tabPanel("Resultados",
             #                     column(6,br(),br(),br(),
             #                            h3(HTML("Tabla ANOVA")),
             #                            verbatimTextOutput("table_anovaa")
             #                     ),
             #                     column(6,br(),br(),br(),
             #                            h3(HTML("Gráfico de comparaciones Múltiples")),
             #                            plotOutput("comp_usuari1"),
             #                            plotOutput("comp_usuari2")
             #                     )
             #            ),
             #            tabPanel("Potencia",br(),br(),
             #                     column(6,
             #                            h3(HTML("Potencia que presentan los datos")),
             #                            verbatimTextOutput("power_power")
             #                     )
             #            )
             #          )
             #          
             #          
             #          
             # )
             
             # tabPanel("Planning",
             #          column(2,style = "background-color:#d4af37;",
             #                 br(),
             #                 h4("Parameters' settings for Power"),
             #                 hr(),
             #                 numericInput('rows1',withMathJax('Number of levels of factor F1 (rows: \\(\\alpha_i\\) )'),value=4),
             #                 numericInput('cols1',withMathJax('Number of levels of factor F2 (cols: \\(\\beta_j\\))'),value=4),
             #                 numericInput('n1','Sample size per cell',value = 3),
             #                 numericInput('sigma1',withMathJax('Common \\(\\sigma\\)'),value=3),
             #                 numericInput('alfa',withMathJax('Significance level \\(\\alpha\\)'),value=0.05),
             #                 numericInput('efecto',withMathJax('Minimal practical difference'),value=1),
             #                 hr(),
             #                 
             #                 ),
             #          column(5,
             #                 h4(HTML("Calculando la potencia para un diseño completamente al azar de dos 
             #                        factores.")),
             #                 textOutput('text_power_a'),
             #                 textOutput('text_power_b'),
             #                 textOutput('text_power_int'),
             #                 h4(HTML("Curva de potencia para factor \\(A\\)")),
             #                 plotlyOutput('power_func_a'),
             #                 br(),br(),
             #                 h4(HTML("Curva de potencia para factor \\(B\\)")),
             #                 plotlyOutput('power_func_b'),
             #                 br(),br(),br(),
             #                 h4(HTML("Curva de potencia para interacción \\(\\gamma_{ij}\\)")),
             #                 plotlyOutput('power_func_int')
             #                 
             #                 
             #                 ),
             #          column(5,
             #                 br(),
             #                 br(),
             #                 br(),
             #                 br(),
             #                 plotOutput('potencia_plot_a'),
             #                 sliderInput('xRange2','X scale',value=c(0,15),min=0,max=30),
             #                 br(),
             #                 plotOutput('potencia_plot_b'),
             #                 sliderInput('xRange3','X scale',value=c(0,15),min=0,max=30),
             #                 br(),
             #                 plotOutput('potencia_plot_fac'),
             #                 sliderInput('xRange1','X scale',value=c(0,15),min=0,max=30)
             #              
             #                 )
             #          )
             
  ),

  hr(),
  column(5,
         h6(HTML('<b>Project: PI20/00377</b> <br>
         Albert Sorribas, Ester Vilaprinyo, Rui Alves, Pedro Sandoval <br>
         Miguel Ángel Escobar, Jose Serrano, Xavier Gómez <br>
         Biomodels Group <br>
         University of Lleida - Institute of Biomedical Research (IRBLleida)'))
  ),
  column(1),
  column(4,
         img(src = "Institutions.png", width=400),
  ),br(),br(),br()
  )
)

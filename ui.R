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
  navbarPage("Factorial design (two factors)", inverse=T,
             navbarMenu("Theory",
                        tabPanel("Description",
                                 
                                 p(HTML("El diseño completamente al azar de dos o más factores, 
                                        tamibién llamado diseño factorial, tiene la característica 
                                        de ser utilizado cuando el investigador está interesado 
                                        en determinar el efecto de dos o más factores por separado 
                                        y el efecto de cada nivel de un factor evaluado en cada 
                                        nivel de los otros factores. <br><br>
                                        El modelo lineal que describe este diseño viene dado por:"),
                                        
                                   hr(),
                                   
                                   p(HTML("$$y_{ijk} = \\mu + \\alpha_i + \\beta_j + (\\alpha\\beta)_{ij} 
                                        + \\epsilon_{ijk}$$")),
                                   
                                   hr(),
                                   
                                   p(HTML("
                                        
                                        <br><br>
                                        Donde \\(y_{ijk}\\) es la variable respuesta, \\(\\alpha_i\\) 
                                        es el efecto del factor \\(\\alpha\\) en el nivel \\(i\\),
                                        \\(\\beta_j\\) es el efecto del factor \\(\\beta\\) en el nivel 
                                        \\(j\\), \\((\\alpha\\beta)_{ij}\\) es el efecto de interacción 
                                        que viene dado por \\(\\mu_{ij}-\\mu+\\alpha_i+\\beta_j\\), donde 
                                        \\(\\mu_{ij}\\) es la media de la celda de fila \\(i\\) y columna 
                                        \\(j\\). Por último \\(\\epsilon_{ijk}\\) es el término residual.
                                        <br><br>
                                        Las pruebas de hipótesis a poner a prueba en un diseño completamente 
                                        al azar de dos factores son:<br><br>")),
                                  hr(),
                                        
                                  p(HTML("$$H_0:\\alpha_i=0$$ 
                                        $$H_1:\\alpha_i \\neq 0$$ <br>")),
                                  hr(),
                                   
                                  p(HTML("$$H_0:\\beta_j=0$$ 
                                        $$H_1:\\beta_j \\neq 0$$ <br>")),
                                  hr(),
                                  
                                  p(HTML("$$H_0:(\\alpha\\beta)_{ij}=0$$ 
                                           $$H_1:(\\alpha\\beta)_{ij} \\neq 0$$"))
                                
                                 
                             )),
                        tabPanel(p(HTML("Supuestos y <br> Objetivos")),
                                 h3(HTML("Supuestos")),
                                 p(HTML("Al igual que en el diseño completamente al azar de un factor, 
                                        el cumplimiento de los supuestos del modelo lineal es de vital 
                                        importancia para los diseños factoriales. Esto garantiza que los 
                                        cocientes \\(F\\) se aproximen a la distribución teórica planteada.<br>
                                        <br>
                                        Para estos diseño la independencia de las unidesdes experimentales es de 
                                        vital importancia para garantizar que no se presenten estructuras de correlación 
                                        en los datos que pueden invalidar los resultados.<br><br>
                                        En resumen, los residuales del modelo deben cumplir:")),
                                 hr(),
                                 
                                 p(HTML("$$\\epsilon_{ijk}\\approx NID(0,\\sigma^2)$$")),
                                 
                                 hr(),
                                
                                 p(HTML("
                                        Como se mencionó para el diseño completamente al azar de un factor, el supuesto 
                                        de independencia se debe comprobar desde la génesis del diseño, esto quiere decir 
                                        que no se cuenta con una prueba o gráfico para cumplir con este supuesto 
                                        en estos diseños. <br><br>
                                        Por otro lado, el supuesto de normalidad podría ser analizado de dos formas:<br><br>
                                        1. De forma gráfica, mediante el gráfico qq-plot <br>
                                        2. Mediante una prueba estadística (por ejemplo la prueba de Shapiro-Wilk) <br><br>
                                        
                                        La homocedasticidad también podría ser probada de forma gráfica (mediante el gráfico de 
                                        residuales contra predichos) y mediante una prueba estadística, por ejemplo 
                                        la prueba de Battlet.
                                        ")),
                                 hr(),
                                 h3(HTML("Objetivos")),
                                 p(HTML("Uno de los principales objetivos de un diseño factorial es determinar el comportamiento 
                                        que presenta la variable respuesta cuando dos o más variables explicatorias (factores) 
                                        interarctúan entre si. En ocaciones es de vital importancia para la investigadora o 
                                        investigador conocer si la variable respuesta en los niveles de uno de los factores tiene 
                                        el mismo comportamiento en los niveles de los otros factores. <br><br>
                                        En caso que esta sea la situación, se podrán concluir que no existe interacción entre los 
                                        factores, y con ello se podrían analizar los efectos principales de forma separada.
                                        En caso contrario, si el comportamiento de la variable respuesta en uno de los niveles no 
                                        presenta el mismo comportamiento en los niveles de los otros factores, entonces se estará en 
                                        presencia de interacción, generando una restricción en el análisis de los factores principales.
                                        "))
                                 ),
                        tabPanel("Power",
                                 p(HTML("El concepto de potencia está asociado al error tipo II, como se conversó en el diseño 
                                        completamente al azar de un factor. En el caso de un diseño de dos factores (dos variables 
                                        cualitativas independientes) la potencia se puede determinar mediante la utilización de dos 
                                        métodos: <br><br>
                                        El primero de los métodos es determinar la potencia de la interacción, para este caso la 
                                        distribución de probabilidad teórica vendrá dada por:
                                        $$F_{t-1,n-t,\\lambda}$$
                                        Donde \\(t\\) representa la cantidad de celdas en el modelo, que normalmente viene dado por 
                                        (\\(n_A \\cdot n_B\\)) donde \\(n_A\\) representa los niveles del factora \\(A\\) y \\(n_B\\) 
                                        representa los niveles del factor \\(B\\).<br><br>
                                        Por otro lado, \\(n\\) representa la cantidad de réplicas en el diseño. El parámetro de no 
                                        centralidad \\(\\lambda\\), que es utilizado cuando no se cumple \\(H_0\\), viene dado por:
                                        
                                        $$\\lambda = \\frac{r}{\\sigma^2} \\sum_i\\sum_j(\\bar \\mu_{ij}-\\bar \\mu_{..})^2$$
                                        
                                        El segundo de los métodos es determinar la potencia de los factores principales, en el caso 
                                        un diseño de dos factores se tienen dos valores de \\(\\lambda\\), uno para el factor \\(A\\) 
                                        y otro para el factor \\(B\\), entonces:
                                        
                                        $$\\lambda_A = \\frac{br}{\\sigma^2}\\sum_i(\\bar \\mu_{i.}-\\bar\\mu_{..})^2$$
                                        $$\\lambda_B = \\frac{ar}{\\sigma^2}\\sum_j(\\bar \\mu_{.j}-\\bar\\mu_{..})^2$$
                                        
                                        En estas expresiones tememos que \\(r\\) son las réplicas, \\(b\\) son los niveles 
                                        del factor \\(B\\) y \\(a\\) son los niveles del factor \\(A\\).
                                        
                                        "))
                                 ),
                        tabPanel("Resultados",
                                 p(HTML("El principal resultado de un análisis que involucre un diseño completamente al azar de 
                                        dos factores es TABLA ANOVA. Para el caso de dos factores la tabla estará compuesta por 
                                        por tres filas a evaluar: <br><br>
                                        1. El efecto del primer factor (\\(\\alpha_i\\))<br>
                                        2. El efecto del segundo factor (\\(\\beta_j\\))<br>
                                        3. El efecto de la interacción (\\(\\gamma_{ij}\\))<br><br>
                                        
                                        El primer efecto que el investigador deberá revisar es el de la interacción. Existen dos 
                                        posibles opciones, que el efecto de interacción sea significatio o que no lo sea. En caso 
                                        de que el efecto de interacción no se significativo, se podrán interpretar los efectos 
                                        principales de forma separada, esto quiere decir realizando comparaciones múltiples 
                                        para cada efecto principal, por el contrario si la interacción es significativa se deberán 
                                        realizar comparaciones múltiples tomando en cuenta esta realidad (la realidad de que la 
                                        variable respuesta de un factor varía con respecto a los niveles del otro factor).<br><br>
                                        
                                        ")),
                                 h4(HTML("Posibles resultados en un diseño de dos factores")),
                                 p(HTML("Existen 5 posibles combinaciones de resultados para un diseño factorial de dos factores: <br><br>
                                        a. Que no existan efectos principales de los dos factores ni interacción <br>
                                        b. Que exista efecto del factor principal \\(F_1\\) pero que no exista efecto del factor \\(F_2\\) 
                                        ni que exista interacción. <br>
                                        c. Que exista efecto del factor principal \\(F_2\\) pero que no exista efecto del factor \\(F_1\\) 
                                        ni que exista interacción. <br>
                                        d. Que exista efecto de ambos factores \\(F_1\\) y \\(F_2\\) y que no exista efecto de interacción. <br>
                                        e. Que exista efecto de ambos factores \\(F_1\\) y \\(F_2\\) y que también exista efecto de interacción.
                                        <br><br>")),
                                 column(6,
                                        h3(HTML("Caso \\(a\\)")),
                                        img(src = "1.jpg", height = 300, width = 600),
                                        
                                        h3(HTML("Caso \\(c\\)")),
                                        img(src = "3.jpg", height = 300, width = 600),
                                        
                                        h3(HTML("Caso \\(e\\)")),
                                        img(src = "5.jpg", height = 300, width = 600)
                                        ),
                                 column(6,
                                        h3(HTML("Caso \\(b\\)")),
                                        img(src = "2.jpg", height = 300, width = 600),
                                        
                                        h3(HTML("Caso \\(d\\)")),
                                        img(src = "4.jpg", height = 300, width = 600)
                                        ),
                                 
                                 imageOutput("1"),
                                 imageOutput("2"),
                                 imageOutput("3"),
                                 imageOutput("4")
                                 
                                 
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
                               numericInput('sigma',withMathJax('Common \\(\\sigma\\)'),value=3),
                               numericInput('n','Sample size per cell',value=1),
                               numericInput('alfa','Significance level',value=0.05),
                               hr(),
                               numericInput('rows',withMathJax('Levels of factor F (rows: \\(\\alpha_i\\) )'),value=4),
                               h5(withMathJax('Effects of \\(F\\) (\\(\\alpha_i\\))')),
                               rHandsontableOutput("input_table_F1"),
                               br(),br(),
                               numericInput('cols',withMathJax('Levels of factor F2 (cols: \\(\\beta_j\\))'),value=8),
                               h5(withMathJax('Blocks of \\(b\\) (\\(b_j\\))')),
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
                                                 h3(HTML("Tabla resumen por bloque")),
                                                 tableOutput('TableMeans'),
                                                 hr(),
                                                 h3(HTML("Tabla resumen por Tratamiento")),
                                                 tableOutput('TableMeans1'),
                                                 h3("Comportamiento del factor en el bloque"),
                                                 hr(),
                                                 plotOutput('Means')
                                                 ),
                                          column(3,
                                                 h3(HTML("Datos simulados")),
                                                 tableOutput('datos')
                                                 ),
                                          column(2,br(),br(),br(),br(),
                                                 downloadButton('downloadData', 'Salvar datos *.csv', 
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
                                                 br(),
                                                 #checkboxInput('optAnova','Test without interactions?',value=FALSE),
                                                 h3('ANOVA table for the generated example'),
                                                 hr(),
                                                 verbatimTextOutput('ANOVA'),
                                                 hr(),
                                                 h3('ANOVA sin efecto del bloque'),
                                                 hr(),
                                                 verbatimTextOutput('ANOVA1'),
                                                 ),
                                          column(6,
                                                 h3(HTML("Compración por pares para \\(F_1\\)")),
                                                 hr(),
                                                 plotOutput('plot_emmeans2'),hr()
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
                               tabPanel('Power',
                                        
                                        column(6,
                                               h3(HTML("Potencia para factor \\(A\\)")),
                                               #actionButton('go_a', 'Potencia \\(F_1(\\alpha_i)\\)', class = "btn-warning"),
                                               plotOutput('f_nocentrada_a'),
                                               tableOutput("algo")
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
                                        
                                        
                                        
                                        
                                        ),
                               tabPanel(HTML("Function <br>power"),
                                        h3(HTML("Función de potencia para factor \\(\\alpha_i\\)")),
                                        plotlyOutput("curve_powera"),hr(),br(),br(),
                                        h3(HTML("Función de potencia para factor \\(\\beta_j\\)")),
                                        plotlyOutput("curve_powerb"),hr(),br(),br(),
                                        h3(HTML("Función de potencia para interacción \\(\\gamma_{ij}\\)")),
                                        plotlyOutput("curve_powerc"),br()
                                        )
                               )
                               
                      ))
                      ),
             
             tabPanel(HTML("Analizando <br>datos propios"),
                      tabsetPanel(
                        tabPanel("Cargando datos",
                                 column(4,
                                        fileInput('target_upload1', 'Choose file to upload',
                                                  accept = c(
                                                    'text/csv',
                                                    'text/comma-separated-values',
                                                    '.csv'
                                                  )),
                                        radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=";",inline=TRUE),
                                        dataTableOutput("sample_table")
                                 )
                        ),
                        tabPanel("Descriptiva",br(),br(),br(),
                                 column(6,
                                        h3(HTML("Descripción de los datos en forma tabular")),
                                        tableOutput("descc_plot")
                                 ),
                                 column(6,
                                        h3(HTML("Descripción de los datos en forma gráfica")),
                                        plotOutput("plot_plot")
                                 )
                        ),
                        tabPanel("Supuestos",br(),br(),
                                 column(6,
                                        h3(HTML("Normalidad")),
                                        plotOutput("norm_norm"),
                                        verbatimTextOutput("shapiro_norm")
                                 ),
                                 column(6,
                                        h3(HTML("Homocedasticidad")),
                                        plotOutput("homo_homo"),
                                        verbatimTextOutput("bartlet_homo")
                                 )
                                 
                                 
                        ),
                        tabPanel("Resultados",
                                 column(6,br(),br(),br(),
                                        h3(HTML("Tabla ANOVA")),
                                        verbatimTextOutput("table_anovaa")
                                 ),
                                 column(6,br(),br(),br(),
                                        h3(HTML("Gráfico de comparaciones Múltiples")),
                                        plotOutput("comp_usuari1"),
                                        plotOutput("comp_usuari2")
                                 )
                        ),
                        tabPanel("Potencia",br(),br(),
                                 column(6,
                                        h3(HTML("Potencia que presentan los datos")),
                                        verbatimTextOutput("power_power")
                                 )
                        )
                      )
                      
                      
                      
             )
             
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
img(src = "Institutions.png", width=600),
hr(),
HTML('<b>Project: PI20/00377</b> <br>
         Albert Sorribas, Ester Vilaprinyo, Rui Alves, Pedro Sandoval <br>
         Miguel Ángel Escobar, Jose Serrano, Xavier Gómez <br>
         Biomodels Group <br>
         University of Lleida - Institute of Biomedical Research (IRBLleida)'),
hr()
  )
)

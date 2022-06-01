#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
library(tidyverse)
library(sjPlot)
library(ggplot2)
library(daewr)
library(emmeans)
library(plotly)
library(DT)

# Función para potencia teórica media entre celdas

plot_power_fac <- function(df_1,signi,replicas,sigma,diferencia){
  
#  critical_f <- qf(1-input$alfa, df1 = (input$rows1 * input$cols1)-1, 
#                   df2 = (input$n1 - 1)*(input$rows1 * input$cols1))
  
  critical_f <- qf(1-signi, df1 = df_1-1, df2 = (replicas-1)*df_1)
  lambda <- (replicas*(diferencia^2/2))/sigma^2
  
  grafico <- ggplot(data.frame(x = c(0,30)), aes(x)) +
    
    stat_function(
      fun = "df",
      geom = "area",
      fill = "orange",
      args = list(
        df1 = df_1-1,
        df2 = (replicas-1)*df_1,
        ncp = lambda
      ),
      xlim = c(critical_f, 30)
    ) +
    stat_function(
      fun = "df",
      geom = "area",
      linetype = 2,
      fill = "orange",
      alpha = .3,
      args = list(
        df1 = df_1-1,
        df2 = (replicas-1)*df_1,
        ncp = lambda
      ))+
    stat_function(
      fun = "df",
      geom = "line",
      linetype = 2,
      args = list(
        df1 = df_1-1,
        df2 = (replicas-1)*df_1
      )
    )+
    
    ggtitle("Potencia para la interacción") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(grafico)
}


# Función para potencia teórica efecto principal A

plot_power_fac_a <- function(df_1,signi,replicas,sigma,diferencia,a,b){
  
  critical_fa <- qf(1-signi, df1 = a-1, df2 = (replicas-1)*b*a)
  lambda <- b*(replicas*(diferencia^2/2))/sigma^2
  
  grafico1 <- ggplot(data.frame(x = c(0,100)), aes(x)) +
    
    stat_function(
      fun = "df",
      geom = "area",
      fill = "orange",
      args = list(
        df1 = a-1,
        df2 = (replicas-1)*b*a,
        ncp = lambda
      ),
      xlim = c(critical_fa, 100)
    ) +
    stat_function(
      fun = "df",
      geom = "area",
      linetype = 2,
      fill = "orange",
      alpha = .3,
      args = list(
        df1 = a-1,
        df2 = (replicas-1)*b*a,
        ncp = lambda
      ))+
    stat_function(
      fun = "df",
      geom = "line",
      linetype = 2,
      args = list(
        df1 = a-1,
        df2 = (replicas-1)*b*a
      )
    )+
    
    ggtitle("Potencia para el factor A") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(grafico1)
}


# Función para potencia teórica efecto principal B

plot_power_fac_b <- function(df_1,signi,replicas,sigma,diferencia,a,b){
  
  critical_fb <- qf(1-signi, df1 = b-1, df2 = (replicas-1)*b*a)
  lambda <- a*(replicas*(diferencia^2/2))/sigma^2
  
  grafico2 <- ggplot(data.frame(x = c(0,100)), aes(x)) +
    
    stat_function(
      fun = "df",
      geom = "area",
      fill = "orange",
      args = list(
        df1 = a-1,
        df2 = (replicas-1)*b*a,
        ncp = lambda
      ),
      xlim = c(critical_fb, 100)
    ) +
    stat_function(
      fun = "df",
      geom = "area",
      linetype = 2,
      fill = "orange",
      alpha = .3,
      args = list(
        df1 = a-1,
        df2 = (replicas-1)*b*a,
        ncp = lambda
      ))+
    stat_function(
      fun = "df",
      geom = "line",
      linetype = 2,
      args = list(
        df1 = a-1,
        df2 = (replicas-1)*b*a
      )
    )+
    
    ggtitle("Potencia para factor B") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  return(grafico2)
}


## Gráfico de homocedasticidad

homocedast <- function(datos){
  mod3 <-lm(response ~ F1*F2, datos)
  est <- mod3$fitted.values
  pred <- mod3$residuals
  bd1 <- data.frame(est, pred)
  p4 <- ggplot(bd1) +
    aes(x = est, y = pred) +
    geom_point(size= I(3), col='blue') +
    geom_hline(yintercept = 0, linetype ='dashed') +
    labs(x = "Fitted values", y = "Residuals") +
    ggtitle('Revision homocedasticidad') +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.5)))
  return(p4)
}

## Gráfico de normalidad para supuestos

# Supuesto de normalidad para supuestos
normalidad <- function(datos){
  res <- lm(response~F1*F2,datos)
  p2 <- ggplot(datos, aes(sample = res$residuals))+ 
    stat_qq(size= I(3)) + stat_qq_line(size= I(0.6)) +
    ggtitle('Prueba de normalidad') +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.5))) +
    labs(y="Residuos Estandarizados", x="Quantiles teoricos")
  return(p2)
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ################ Factor 1 main effects 
  input_table_F1 <- reactive({
    
    rr <- rep(0,input$rows)
    mat <- matrix(rr, ncol = input$rows, nrow = 1)
    res <- c()
    for (j in 1:input$rows) {res <- c(res,paste("F1(",j,")",sep=''))}
    colnames(mat) <- res  
    
    res = as.data.frame(mat)
    res
    
  })
  
  output$input_table_F1 <- renderRHandsontable({
    
    rhandsontable(input_table_F1())
  })
  
  values_F1 <- reactiveValues(data=NULL)
  
  observe({
    values_F1$data <- hot_to_r(input$input_table_F1)
  })
  
  dF1 <- reactive({
    values_F1$data
  })
  
  ############## Factor 2 main effects

  input_table_F2 <- reactive({
    
    rr <- rep(0,input$rows)
    mat <- matrix(rr, ncol = input$cols, nrow = 1)
    res <- c()
    for (j in 1:input$cols) {res <- c(res,paste("F2(",j,")",sep=''))}
    colnames(mat) <- res  
    
    res = as.data.frame(mat)
    res
    
  })
  
  output$input_table_F2 <- renderRHandsontable({
    
    rhandsontable(input_table_F2())
  })
  
  values_F2 <- reactiveValues(data=NULL)
  
  observe({
    values_F2$data <- hot_to_r(input$input_table_F2)
  })
  
  dF2 <- reactive({
    values_F2$data
  })
  
  
  ################ Table for interactions
  input_table <- reactive({
    
    rr <- rep(0,input$cols * input$rows)
    mat <- matrix(rr, ncol = input$cols, nrow = input$rows)
    res <- c()
    for (j in 1:input$cols) {res <- c(res,paste("F2(",j,")",sep=''))}
    colnames(mat) <- res  
    
    res <- c()
    for (j in 1:input$rows)  {res <- c(res,paste("F1(",j,")",sep=''))}
    rownames(mat) <- res
    
    res = as.data.frame(mat)
    res
    
  })
  
  output$input_table <- renderRHandsontable({
    
    rhandsontable(input_table())
  })
  
  values <- reactiveValues(data=NULL)
  
  observe({
    values$data <- hot_to_r(input$input_table)
  })
  
  dinter <- reactive({
    values$data
  })
  
  #################################################

  dataF1F2 <- eventReactive(input$go,{
    eF1 <- t(dF1())
    eF2 <- t(dF2())
    eF1F2 <- dinter()
    
    data <- data.frame(
      F1=rep(rep(1:input$rows,input$n),input$cols) %>% sort,
      F2=rep(rep(1:input$cols,input$n),input$rows) 
    )
    ra <- input$rows
    ca <- input$cols
    n <- input$n
    k <- ra*ca*n
    
    ss <- input$sigma
    res <- rnorm(k,0,input$sigma)
    data <- data %>% mutate(response=input$mu+eF1[F1]+eF2[F2]+eF1F2[cbind(F1, F2)]+rnorm(k,0,input$sigma))
     
    data
  },ignoreNULL = T)
  
  
  
  cleanData <- eventReactive(input$go,{
    eF1 <- t(dF1())
    eF2 <- t(dF2())
    eF1F2 <- dinter()
    
    data <- data.frame(
      F1=rep(rep(1:input$rows,1),input$cols) %>% sort,
      F2=rep(rep(1:input$cols,1),input$rows) 
    )
    ra <- input$rows
    ca <- input$cols
    n <- 1
     
    data <- data %>% mutate(response=input$mu+eF1[F1]+eF2[F2]+eF1F2[cbind(F1,F2)])
    
    
    data
  })
  
#  output$dades <- renderTable({
 #   dataF1F2()
#  })
  
  ## Gráfica de medias 
  output$Means <- renderPlot({
    d <- dataF1F2() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    d
    d2 <- d %>% group_by(F1,F2) %>% summarise_all(.funs = c(m = mean,
      sd1 = sd))

    ggplot(d) +
      aes(x = F1, y = response,
          color = F2,
          group = F2) +
      stat_summary(fun.data = mean_se, 
                   geom = "pointrange") +
      geom_line(stat = "summary", fun = mean, size=.75)+
      ylim(input$yRange[1],input$yRange[2])
    
  })
  
  ## Tabla de medias observadas
  output$TableMeans <- renderTable({
    d <- dataF1F2() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    d2 <- d %>% group_by(F1,F2) %>% summarise(m=mean(response))
    spread(d2,key=F2,value=m) 
  })
  
  ## Tabla de ANOVA
  output$ANOVA <- renderPrint({
    d <- dataF1F2() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
     
    if(input$optAnova==FALSE) {res <- lm(response~F1*F2,d)}
      else {res <- lm(response~F1+F2,d)}
    
    anova(res)
  })
  
  ## Efectos estimados
  output$EstimatedEffects <- renderPlot({
    d <- dataF1F2() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    
    if(input$optAnova==FALSE) {res <- lm(response~F1*F2,d)}
    else {res <- lm(response~F1+F2,d)}
    #summary(res)
    #data.frame(effect=coefficients(res),lower=confint(res)[,1],upper=confint(res)[,2])
    plot_model(res)
  })
  
  
  output$TableEstimatedEffects <- renderPrint({
    d <- dataF1F2() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    if(input$optAnova==FALSE) {res <- lm(response~F1*F2,d)}
    else {res <- lm(response~F1+F2,d)}
    #summary(res)
    data.frame(effect=round(coefficients(res),2),lower=round(confint(res)[,1],2),upper=round(confint(res)[,2],2))
    #HTML(knitr::knit_print(tab_model(res)) )
  })
  
  ## Gráfico de medias sin desviación estándar
  output$PlotCleanMeans <- renderPlot({
    d <- cleanData()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    ggplot(d,aes(F1,response,group=F2))+
      geom_point(size=3,aes(color=F2))+
      geom_line(size=1,aes(color=F2))+
      ylim(input$yRange[1],input$yRange[2])
  })
  
  ## Tabla de medias sin desviación estándar
  output$TableCleanMeans <- renderTable({
    d <- cleanData() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    d2 <- d %>% group_by(F1,F2) %>% summarise(m=mean(response))
    d3 <- spread(d2,key=F2,value=m)
    d3 <- as.data.frame(d3)
    
  })
  
  
  ## Prueba Barlett
  output$Barlett <- renderPrint({
    d <- dataF1F2()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    bartlett.test(response~interaction(F1,F2),data=d)
  })
  
  
  
  # Plot emmeans para interacción
  output$plot_emmeans2 <- renderPlot({
    d <- dataF1F2() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    res1 <- anova(lm(response~F1*F2,d))
    res <- aov(response~F1*F2, d)
    lsm_int <- emmeans(res, ~ F1:F2)
    lsm_A <- emmeans(res, ~ F1)
    
    if(res1$`Pr(>F)`[3]<0.05) {contrastes <- contrast(lsm_int, method = "pairwise", by = "F1")
    plot(contrastes)+coord_flip()+geom_vline(xintercept = 0, linetype="dashed")}
    else {contrastes1 <- contrast(lsm_A, method = "pairwise")
    plot(contrastes1)+coord_flip()+geom_vline(xintercept = 0, linetype="dashed")}
    })
  
  # Plot emmeans para interacción
  output$plot_emmeans3 <- renderPlot({
    d <- dataF1F2() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    res1 <- anova(lm(response~F1*F2,d))
    res <- aov(response~F1*F2, d)
    lsm_int <- emmeans(res, ~ F1:F2)
    lsm_B <- emmeans(res, ~ F2)
    contrastes <- contrast(lsm_int, method = "pairwise", by = "F2")
    
    if(res1$`Pr(>F)`[3]<0.05) {contrastes <- contrast(lsm_int, method = "pairwise", by = "F2")
    plot(contrastes)+coord_flip()+geom_vline(xintercept = 0, linetype="dashed")}
    else {contrastes1 <- contrast(lsm_B, method = "pairwise")
    plot(contrastes1)+coord_flip()+geom_vline(xintercept = 0, linetype="dashed")}
  
  })
  
  
  # Texto para imprimir potencia entre medias de celdas
  
  output$text_power_int <- renderText({
    css <- input$efecto^2/2
    lambda <- (input$n1 * css)/input$sigma1^2
    critical_f <- qf(1-input$alfa, df1 = (input$rows1 * input$cols1)-1, 
                     df2 = (input$n1 - 1)*(input$rows1 * input$cols1))
    
    ppower1 <- 1-pf(q = critical_f, df1 = (input$rows1 * input$cols1)-1,
                    df2 = (input$n1 - 1)*(input$rows1 * input$cols1),
                    ncp = lambda)
    paste("La potencia para detectar diferencias entre celdas (interacción) es de:", round((x=ppower1),digits=3))
    
  })
  
  
  # Texto para imprimir potencia de factor A
  output$text_power_a <- renderText({
    
    a <- input$cols1
    b <- input$rows1
    css <- input$efecto^2/2
    lambda_a <- b * (input$n1 * css)/(input$sigma1^2) 
    lambda_b <- a * (input$n1 * css)/(input$sigma1^2)
    dfa <- a - 1
    dfb <- b - 1
    df2ab <- (input$n1 - 1) * b * a
    critical_fa <- qf(1-input$alfa, df1 = dfa, 
                     df2 = df2ab)
    critical_fb <- qf(1-input$alfa, df1 = dfb, 
                      df2 = df2ab)
    
    powera <- 1 - pf(critical_fa, dfa, df2ab, lambda_a)
    powerb <- 1 - pf(critical_fb, dfb, df2ab, lambda_b)
    
    paste("La potencia para detectar diferencias en el factor A es de:", round((x=min(powera)),digits=3))
    
  })
  
  # Texto para imprimir potencia de factor B
  output$text_power_b <- renderText({
    
    a <- input$cols1
    b <- input$rows1
    css <- input$efecto^2/2
    lambda_a <- b * (input$n1 * css)/(input$sigma1^2) 
    lambda_b <- a * (input$n1 * css)/(input$sigma1^2)
    dfa <- a - 1
    dfb <- b - 1
    df2ab <- (input$n1 - 1) * b * a
    critical_fa <- qf(1-input$alfa, df1 = dfa, 
                      df2 = df2ab)
    critical_fb <- qf(1-input$alfa, df1 = dfb, 
                      df2 = df2ab)
    
    powera <- 1 - pf(critical_fa, dfa, df2ab, lambda_a)
    powerb <- 1 - pf(critical_fb, dfb, df2ab, lambda_b)
    
    paste("La potencia para detectar diferencias en el factor B es de:", round((x=min(powerb)),digits=3))
    
  })
  
  
  ## Plot sobre distribución F no centrada diferencia de celdas
  output$potencia_plot_fac <- renderPlot({
    resultado_f_nc <-  plot_power_fac(df_1 = input$rows1 * input$cols1, signi = input$alfa, replicas = input$n1,
                                  sigma = input$sigma1, diferencia = input$efecto)
    plot_final  <- resultado_f_nc + xlim(input$xRange1[1],input$xRange1[2])
    
    plot_final
    
  })
  
  
  ## Plot sobre distribución F no centrada factor A
  output$potencia_plot_a <- renderPlot({
    resultado_f_nc_a <-  plot_power_fac_a(df_1=input$rows1 * input$cols1,signi=input$alfa,
                                        replicas=input$n1,sigma=input$sigma1,
                                        diferencia=input$efecto,a=input$cols1,b=input$rows1)
    plot_final1  <- resultado_f_nc_a + xlim(input$xRange2[1],input$xRange2[2])
    
    plot_final1
    
  })
  
  ## Plot sobre distribución F no centrada factor B
  output$potencia_plot_b <- renderPlot({
    resultado_f_nc_b <-  plot_power_fac_b(df_1=input$rows1 * input$cols1,signi=input$alfa,
                                        replicas=input$n1,sigma=input$sigma1,
                                        diferencia=input$efecto,a=input$cols1,b=input$rows1)
    plot_final2  <- resultado_f_nc_b + xlim(input$xRange3[1],input$xRange3[2])
    
    plot_final2
    
  })
  
  
  ############### Factor 1 main effects for simulation
  input_table_F1_sim <- reactive({
    
    rr_sim <- rep(0,input$rows_sim)
    mat_sim <- matrix(rr_sim, ncol = input$rows_sim, nrow = 1)
    res1 <- c()
    for (j in 1:input$rows_sim) {res1 <- c(res1,paste("F1(",j,")",sep=''))}
    colnames(mat_sim) <- res1  
    
    res1 = as.data.frame(mat_sim)
    res1
    
  })
  
  output$input_table_F1_sim <- renderRHandsontable({
    
    rhandsontable(input_table_F1_sim())
  })
  
  values_F1_sim <- reactiveValues(data=NULL)
  
  observe({
    values_F1_sim$data <- hot_to_r(input$input_table_F1_sim)
  })
  
  dF1_sim <- reactive({
    values_F1_sim$data
  })
  
  
  ################ Factor 2 main effects for simulation 
  input_table_F2_sim <- reactive({
    
    rr <- rep(0,input$rows_sim)
    mat <- matrix(rr, ncol = input$cols_sim, nrow = 1)
    res <- c()
    for (j in 1:input$cols_sim) {res <- c(res,paste("F2(",j,")",sep=''))}
    colnames(mat) <- res  
    
    res2 = as.data.frame(mat)
    res2
    
  })
  
  output$input_table_F2_sim <- renderRHandsontable({
    
    rhandsontable(input_table_F2_sim())
  })
  
  values_F2_sim <- reactiveValues(data=NULL)
  
  observe({
    values_F2_sim$data <- hot_to_r(input$input_table_F2_sim)
  })
  
  dF2_sim <- reactive({
    values_F2_sim$data
  })
  
  
  ################ Table interaction for simulations
  input_table_sim <- reactive({
    
    rr <- rep(0,input$cols_sim * input$rows_sim)
    mat <- matrix(rr, ncol = input$cols_sim, nrow = input$rows_sim)
    res <- c()
    for (j in 1:input$cols_sim) {res <- c(res,paste("F2(",j,")",sep=''))}
    colnames(mat) <- res  
    
    res <- c()
    for (j in 1:input$rows_sim)  {res <- c(res,paste("F1(",j,")",sep=''))}
    rownames(mat) <- res
    
    res3 = as.data.frame(mat)
    res3
    
  })
  
  output$input_table_sim <- renderRHandsontable({
    
    rhandsontable(input_table_sim())
  })
  
  values_sim <- reactiveValues(data=NULL)
  
  observe({
    values_sim$data <- hot_to_r(input$input_table_sim)
  })
  
  dinter_sim <- reactive({
    values_sim$data
  })
  

  #################### Data generation for simulation
  
  dataF1F2_sim_sim <- function(rows,n,cols,sigma){
    
    eF1 <- t(dF1())
    eF2 <- t(dF2())
    eF1F2 <- dinter()
    
    data1 <- data.frame(
      F1=rep(rep(1:rows,n),cols) %>% sort,
      F2=rep(rep(1:cols,n),rows) 
    )
    ra1 <- rows
    ca1 <- cols
    n1 <- n
    k1 <- ra1*ca1*n1
    
    ss1 <- sigma
    #res5 <- rnorm(k1,0,ss1)
    data1 <- data1 %>% mutate(response=input$mu+eF1[F1]+eF2[F2]+eF1F2[cbind(F1,F2)]+rnorm(k1,0,ss1))
    
    data1$F1 <- as.factor(data1$F1)
    data1$F2 <- as.factor(data1$F2)
    
    data1
    
    modd <- anova(lm(response ~ F1*F2,data1))
    r2 <- modd$`F value`[3]
    r3 <- modd$`F value`[2]
    r4 <- modd$`F value`[1]

    list(f_int=r2,f_b=r3,f_a=r4,data1=data1)
  }
  
  dataF1F2_sim <- eventReactive(input$go,{
    
  dataF1F2_sim_sim(rows=input$rows,n=input$n,
                 cols=input$cols,sigma=input$sigma)
    
  })
  
  ## Generar datos 
  datos11 <- eventReactive(input$go,{#if (input$fix==TRUE){set.seed(input$valfix)}
    #else set.seed(round(runif(1,1,10000),0))
    dataF1F2_sim_sim(rows=input$rows,n=input$n,cols=input$cols,sigma=input$sigma)$data1},
    ignoreNULL = F)
  

  # Simulación de una distribución F interacción
  
  simulation_f_int <- function(num_sim,mu_sim,sigma_sim,n_sim,rows_sim,cols_sim){

    fila <- rep(t(dF1()),length(t(dF1())))
    eff_i <- matrix(fila,nrow = input$rows, ncol=input$cols,byrow = F)
    colum <- rep(t(dF2()),length(t(dF2())))
    eff_j <- matrix(colum,nrow = input$rows, ncol = input$cols,byrow = T)
    interac <- as.matrix(dinter())
    cell_mean <- input$mu+eff_i+eff_j+interac
    cell_mean_int <- interac
   
    mm <- mean(cell_mean_int)
    mi <- rowMeans(cell_mean)
    mj <- colMeans(cell_mean)
    
    css <- sum((cell_mean_int-mm)^2)
    cssi <- sum((mi-mean(mi))^2)
    cssj <- sum((mj-mean(mj))^2)
    
    vect <- c(css,cssi,cssj)
    
    pnc <- (n_sim*css)/sigma_sim^2
    
    
    f <- replicate(num_sim, dataF1F2_sim_sim(rows_sim,n_sim,cols_sim,sigma_sim)$f_int) # cocientes F simulados
    data1 <- data.frame(f) # cocientes F en forma de tabla
    
    lambda <- pnc # parametro de no centralidad
    
    # Distribución F de simulación y la F teorica
    ua <- qf(p = 0.95, df1 = (rows_sim*cols_sim)-1, 
             df2 = (n_sim - 1)*(rows_sim*cols_sim))
    
    power <- sum(f>ua)/num_sim
    
    powert <- 1-pf(ua,(rows_sim * cols_sim)-1,
                   (n_sim - 1)*(rows_sim*cols_sim),lambda) %>% round(3)
    
    
    p4 <- ggplot(data1, aes(f))+
      geom_histogram(binwidth = 0.3,aes(y = ..density..), color="black", fill="yellow",
                     alpha = 0.5)+
      stat_function(
        fun = "df",
        geom = "area",
        linetype = 2,
        fill = "gray",
        alpha = .7,
        args = list(
          df1 = (rows_sim * cols_sim)-1,
          df2 = (n_sim - 1)*(rows_sim * cols_sim),
          ncp = lambda)) +
      
      stat_function(
        fun = "df",
        geom = "area",
        linetype = 2,
        fill = "red",
        alpha = .6,
        args = list(
          df1 = (rows_sim*cols_sim)-1,
          df2 = (n_sim - 1)*(rows_sim*cols_sim),
          ncp = lambda
        ), 
        xlim = c(ua, 30)
      )+
      
      stat_function(
        fun = df,
        geom = "line",
        linetype = 2,
        args = list(
          df1 = (rows_sim * cols_sim)-1,
          df2 = (n_sim - 1)*(rows_sim * cols_sim)
        )
      ) + 
      xlim(0,30)+
      #   geom_vline(xintercept = ua,color="red",linetype="dashed")+
      annotate(geom = "text",x=10, y=0.2, label = paste("Potencia por simulación: ",power))+
      annotate(geom = "text",x=10, y=0.25, label = paste("Potencia teórica: ",powert))
    
    #     power <- sum(f>qf(1-alfas,ks-1,(n_tot)-ks))/nsim
    list(f=f,p4=p4,mm=mm,mi=mi,mj=mj,css=css)
    
  }
  
  
  # Simulación de una distribución F factor A
  
  simulation_f_a <- function(num_sim,mu_sim, sigma_sim,n_sim,rows_sim,cols_sim){

    fila <- rep(t(dF1()),length(t(dF1())))
    eff_i <- matrix(fila,nrow = input$rows, ncol=input$cols,byrow = F)
    colum <- rep(t(dF2()),length(t(dF2())))
    eff_j <- matrix(colum,nrow = input$rows, ncol = input$cols,byrow = T)
    interac <- as.matrix(dinter())
    cell_mean <- input$mu+eff_i+eff_j+interac
    
    #mm <- mean(colMeans(cell_mean))
    mi <- rowMeans(cell_mean)
    #mj <- colMeans(cell_mean)
    
    #css <- sum((cell_mean-mm)^2)
    cssi <- sum((mi-mean(mi))^2)
    #cssj <- sum((mj-mean(mj))^2)
    
    pnc <- cols_sim*(n_sim*cssi)/sigma_sim^2
    
    
    f <- replicate(num_sim, dataF1F2_sim_sim(rows_sim,n_sim,cols_sim,sigma_sim)$f_a) # cocientes F simulados
    data1 <- data.frame(f) # cocientes F en forma de tabla
    
    lambda <- pnc # parametro de no centralidad
    
    # Distribución F de simulación y la F teorica
    ua <- qf(p = 0.95, df1 = rows_sim-1, 
             df2 = (n_sim - 1)*(rows_sim * cols_sim))
    
    power <- sum(f>ua)/num_sim
    
    powert <- 1-pf(ua,rows_sim-1,
                   (n_sim - 1)*(rows_sim * cols_sim),lambda)%>%round(2)

    
    p5 <- ggplot(data1, aes(f))+
      geom_histogram(binwidth = 0.3,aes(y = ..density..), color="black", fill="yellow",
                     alpha = 0.5)+
      stat_function(
        fun = "df",
        geom = "area",
        linetype = 2,
        fill = "gray",
        alpha = .7,
        args = list(
          df1 = rows_sim-1,
          df2 = (n_sim - 1)*(rows_sim * cols_sim),
          ncp = lambda)) +
      
      stat_function(
        fun = "df",
        geom = "area",
        linetype = 2,
        fill = "red",
        alpha = .6,
        args = list(
          df1 = rows_sim-1,
          df2 = (n_sim - 1)*(rows_sim * cols_sim),
          ncp = lambda
        ), 
        xlim = c(ua, 30)
      )+
      
      stat_function(
        fun = df,
        geom = "line",
        linetype = 2,
        args = list(
          df1 = rows_sim-1,
          df2 = (n_sim - 1)*(rows_sim * cols_sim)
        )
      ) + 
      xlim(0,30)+
      #   geom_vline(xintercept = ua,color="red",linetype="dashed")+
      annotate(geom = "text",x=10, y=0.2, label = paste("Potencia por simulación: ",power))+
      annotate(geom = "text",x=10, y=0.25, label = paste("Potencia teórica: ",powert))
    
    
    #     power <- sum(f>qf(1-alfas,ks-1,(n_tot)-ks))/nsim
    list(p5=p5,cssi=cssi)
    
  }
  
  
  # Simulación de una distribución F factor B
  
  simulation_f_b <- function(num_sim,mu_sim, sigma_sim,n_sim,rows_sim,cols_sim){
    
    fila <- rep(t(dF1()),length(t(dF1())))
    eff_i <- matrix(fila,nrow = input$rows, ncol=input$cols,byrow = F)
    colum <- rep(t(dF2()),length(t(dF2())))
    eff_j <- matrix(colum,nrow = input$rows,ncol = input$cols,byrow = T)
    interac <- as.matrix(dinter())
    cell_mean <- input$mu+eff_i+eff_j+interac
    
    #mm <- mean(colMeans(cell_mean))
    #mi <- rowMeans(cell_mean)
    mj <- colMeans(cell_mean)
    
    #css <- sum((cell_mean-mm)^2)
    #cssi <- sum((mi-mean(mi))^2)
    cssj <- sum((mj-mean(mj))^2)
    
    pnc <- rows_sim*(n_sim*cssj)/sigma_sim^2
    
    
    f <- replicate(num_sim, dataF1F2_sim_sim(rows_sim,n_sim,cols_sim,sigma_sim)$f_b) # cocientes F simulados
    data1 <- data.frame(f) # cocientes F en forma de tabla
    
    lambda <- pnc # parametro de no centralidad
    
    # Distribución F de simulación y la F teorica
    ua <- qf(p = 0.95, df1 = cols_sim-1, 
             df2 = (n_sim - 1)*(rows_sim * cols_sim))
    
    power <- sum(f>ua)/num_sim
    
    powert <- 1-pf(ua,cols_sim-1,
                   (n_sim - 1)*(rows_sim * cols_sim),lambda)%>%round(2)
    
    
    p6 <- ggplot(data1, aes(f))+
      geom_histogram(binwidth = 0.3,aes(y = ..density..), color="black", fill="yellow",
                     alpha = 0.5)+
      stat_function(
        fun = "df",
        geom = "area",
        linetype = 2,
        fill = "gray",
        alpha = .7,
        args = list(
          df1 = cols_sim-1,
          df2 = (n_sim - 1)*(rows_sim * cols_sim),
          ncp = lambda)) +
      
      stat_function(
        fun = "df",
        geom = "area",
        linetype = 2,
        fill = "red",
        alpha = .6,
        args = list(
          df1 = cols_sim-1,
          df2 = (n_sim - 1)*(rows_sim * cols_sim),
          ncp = lambda
        ), 
        xlim = c(ua, 30)
      )+
      
      stat_function(
        fun = df,
        geom = "line",
        linetype = 2,
        args = list(
          df1 = cols_sim-1,
          df2 = (n_sim - 1)*(rows_sim * cols_sim)
        )
      ) + 
      xlim(0,30)+
      #   geom_vline(xintercept = ua,color="red",linetype="dashed")+
      annotate(geom = "text",x=10, y=0.2, label = paste("Potencia por simulación: ",power))+
      annotate(geom = "text",x=10, y=0.25, label = paste("Potencia teórica: ",powert))
    
    
    #     power <- sum(f>qf(1-alfas,ks-1,(n_tot)-ks))/nsim
    list(p6=p6,cssj=cssj)
  }
  
  
  #Plot simulación distribución F no centrada con botón para activar simulación
  #para interacción
  
  plot1 <- eventReactive(input$go,{#if (input$fix==TRUE){set.seed(input$valfix)}
    #else set.seed(round(runif(1,1,10000),0))
    
    simulation_f_int(num_sim=input$num_sim,
                     mu_sim=input$mu, 
                     sigma_sim=input$sigma,
                     n_sim=input$n,
                     rows_sim=input$rows, 
                     cols_sim=input$cols)
    
    
  }, ignoreNULL = F)
  
  
  output$f_nocentrada_int <- renderPlot({
    
    plot1()
    #    plot_final1 <- dibujito1 + xlim(input$xRange[1],input$xRange[2])
    
    #    dibujito1
    
  })
  
  
  # Para que  el plot de efecto del factor A que se active con boton
  plot2 <- eventReactive(input$go,{#if (input$fix==TRUE){set.seed(input$valfix)}
    #else set.seed(round(runif(1,1,10000),0))
    
    simulation_f_a(num_sim=input$num_sim,
                     mu_sim=input$mu, 
                     sigma_sim=input$sigma,
                     n_sim=input$n,
                     rows_sim=input$rows, 
                     cols_sim=input$cols)
    
  }, ignoreNULL = F)
  

  #Plot simulación distribución F no centrada con botón para activar simulación
  #para interacción
  
  output$f_nocentrada_a <- renderPlot({
    
   plot2()
    
    #    plot_final1 <- dibujito1 + xlim(input$xRange[1],input$xRange[2])
    
    #    dibujito1
    
    
  })
  
 
  # Para que  el plot de efecto del factor B que se active con boton
  plot3 <- eventReactive(input$go,{#if (input$fix==TRUE){set.seed(input$valfix)}
    #else set.seed(round(runif(1,1,10000),0))
    
    simulation_f_b(num_sim=input$num_sim,
                   mu_sim=input$mu, 
                   sigma_sim=input$sigma,
                   n_sim=input$n,
                   rows_sim=input$rows, 
                   cols_sim=input$cols)
    
  }, ignoreNULL = F)
  
  
  #Plot simulación distribución F no centrada con botón para activar simulación
  #para interacción
  
  output$f_nocentrada_b <- renderPlot({
    
    plot3()
    
    #    plot_final1 <- dibujito1 + xlim(input$xRange[1],input$xRange[2])
    
    #    dibujito1
    
    
  })
  
  
  # Para que la matriz se active con boton
  matriz <- eventReactive(input$go,{
                          
          matrixx <- simulation_f_int(num_sim=input$num_sim,
                                           mu_sim=input$mu, 
                                           sigma_sim=input$sigma,
                                           n_sim=input$n,
                                           rows_sim=input$rows, 
                                           cols_sim=input$cols)
          #tablee <- cbind(matrixx$mi,matrixx$mj)
          #tablee
          delta <- sqrt(2*matrixx$css)
          delta
    
}, ignoreNULL = F)
                                           
  
  # #Imprime la matriz de efectos
  # output$css <- renderTable({
  #   
  #   matriz()
  #  
  #   })
  
  #output$mm <- renderPrint({
    
    #css <- simulation_f_int(num_sim=input$num_sim,
    #                      mu_sim=input$mu_sim, 
    #                      sigma_sim=input$sigma_sim,
    #                      n_sim=input$n_sim,
    #                      rows_sim=input$rows_sim, 
    #                      cols_sim=input$cols_sim)
  #  css$fila
    
  #})
  
  # ### Curva de potencia para interacción
  # output$power_func_int <- renderPlotly({
  #   
  #   x1 <- 2
  #   x2 <- 30
  #   nrep <- c(x1:x2)
  #   css <- input$efecto^2/2
  #   
  #   lambda_pow_func <- (nrep * css)/input$sigma1^2
  #   critical_f <- qf(1-input$alfa, df1 = (input$rows1 * input$cols1)-1, 
  #                    df2 = (nrep - 1)*(input$rows1 * input$cols1))
  #   
  #   ppower1 <- 1-pf(q = critical_f, df1 = (input$rows1 * input$cols1)-1,
  #                   df2 = (nrep - 1)*(input$rows1 * input$cols1),
  #                   ncp = lambda_pow_func)
  #   
  #   datoss <- data.frame(nrep,ppower1)
  #   
  #   p8 <- ggplot(datoss, aes(x = nrep, y = ppower1))+
  #         geom_line(size=0.5) + 
  #         theme(axis.text=element_text(size=10), 
  #         axis.title=element_text(size=10), 
  #         legend.text=element_text(size=14))+
  #     #ggtitle("Curva de potencia para interacción") + 
  #     theme(plot.title = element_text(hjust = 0.5))
  #   
  #         ggplotly(p8)
  # 
  # }) 
  # 
  # ## Curva de potencia para factor A
  # output$power_func_a <- renderPlotly({
  #   
  #   x1 <- 2
  #   x2 <- 30
  #   nrep <- c(x1:x2)
  #   
  #   a <- input$cols1
  #   b <- input$rows1
  #   css <- input$efecto^2/2
  #   lambda_a <- b * (nrep * css)/(input$sigma1^2) 
  #   lambda_b <- a * (nrep * css)/(input$sigma1^2)
  #   dfa <- a - 1
  #   dfb <- b - 1
  #   df2ab <- (nrep - 1) * b * a
  #   critical_fa <- qf(1-input$alfa, df1 = dfa, 
  #                     df2 = df2ab)
  #   critical_fb <- qf(1-input$alfa, df1 = dfb, 
  #                     df2 = df2ab)
  #   
  #   powera <- 1 - pf(critical_fa, dfa, df2ab, lambda_a)
  #   powerb <- 1 - pf(critical_fb, dfb, df2ab, lambda_b)
  #   
  #   dataa <- data.frame(nrep,powerb)
  #   
  #   p7 <- ggplot(dataa, aes(x = nrep, y = powera))+
  #     geom_line(size=0.5) + 
  #     theme(axis.text=element_text(size=10), 
  #           axis.title=element_text(size=10), 
  #           legend.text=element_text(size=14))+
  #     #ggtitle("Curva de potencia para factor B") + 
  #     theme(plot.title = element_text(hjust = 0.5))
  #   
  #   ggplotly(p7)
  #   
  # }) 
  # 
  # ## Curva de potencia para factor B
  # output$power_func_b <- renderPlotly({
  #   
  #   x1 <- 2
  #   x2 <- 30
  #   nrep <- c(x1:x2)
  #   
  #   a <- input$cols1
  #   b <- input$rows1
  #   css <- input$efecto^2/2
  #   lambda_a <- b * (nrep * css)/(input$sigma1^2) 
  #   lambda_b <- a * (nrep * css)/(input$sigma1^2)
  #   dfa <- a - 1
  #   dfb <- b - 1
  #   df2ab <- (nrep - 1) * b * a
  #   critical_fa <- qf(1-input$alfa, df1 = dfa, 
  #                     df2 = df2ab)
  #   critical_fb <- qf(1-input$alfa, df1 = dfb, 
  #                     df2 = df2ab)
  #   
  #   powera <- 1 - pf(critical_fa, dfa, df2ab, lambda_a)
  #   powerb <- 1 - pf(critical_fb, dfb, df2ab, lambda_b)
  #   
  #   dataa <- data.frame(nrep,powera)
  #   
  #   p6 <- ggplot(dataa, aes(x = nrep, y = powera))+
  #     geom_line(size=0.5) + 
  #     theme(axis.text=element_text(size=10), 
  #           axis.title=element_text(size=10), 
  #           legend.text=element_text(size=14))+
  #     #ggtitle("Curva de potencia para factor A") + 
  #     theme(plot.title = element_text(hjust = 0.5))
  #   
  #   ggplotly(p6)
  #   
  # }) 
  
  
  
  # Gráfico de homocedasticidad para simulación
  output$homocedas <- renderPlot({
    d = datos11()
    homocedast(d)
  })
  
  # qq-plot
  output$normali <- renderPlot({
    d = datos11()
    normalidad(d)
  })
  
  
  ## Datos de la simulación
  
  output$datos <- renderTable({
    d <- dataF1F2()
    d
    
  })
  
  ## Descargar datos de la simulación
  
  output$downloadData <- downloadHandler(
    filename = function(){"datos.csv"}, 
    content = function(fname){
      write.csv2(dataF1F2(), fname)
    }
  )
  
  
  # tabla de shapiro para datos simulados
  output$shapiro <- renderPrint({
    
    d = datos11() 
    shapi <- lm(response~F1*F2,d)
    shapiro.test(shapi$residuals)
    
  })
  
  ## Curva de potencia para factor A
  output$curve_powera <- renderPlotly({
    
    x1 <- 2
    x2 <- 30
    nrep <- c(x1:x2)
    
    a <- input$cols
    b <- input$rows
    css <- simulation_f_a(num_sim=input$num_sim,mu_sim=input$mu, 
                          sigma_sim=input$sigma,n_sim=input$n,
                          rows_sim=input$rows,cols_sim=input$cols)$cssi
    lambda_a <- b * (nrep * css)/(input$sigma^2) 
    lambda_b <- a * (nrep * css)/(input$sigma^2)
    dfa <- a - 1
    dfb <- b - 1
    df2ab <- (nrep - 1) * b * a
    critical_fa <- qf(1-input$alfa, df1 = dfa, 
                      df2 = df2ab)
    critical_fb <- qf(1-input$alfa, df1 = dfb, 
                      df2 = df2ab)
    
    powera <- 1 - pf(critical_fa, dfa, df2ab, lambda_a)
    powerb <- 1 - pf(critical_fb, dfb, df2ab, lambda_b)
    
    dataa <- data.frame(nrep,powerb)
    
    p17 <- ggplot(dataa, aes(x = nrep, y = powera))+
      geom_line(size=0.5) + 
      theme(axis.text=element_text(size=10), 
            axis.title=element_text(size=10), 
            legend.text=element_text(size=14))+
      #ggtitle("Curva de potencia para factor B") + 
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p17)
    
  }) 
  
  ## Curva de potencia para factor B 
  output$curve_powerb <- renderPlotly({
    
    x1 <- 2
    x2 <- 30
    nrep <- c(x1:x2)
    
    a <- input$cols
    b <- input$rows
    css <- simulation_f_b(num_sim=input$num_sim,mu_sim=input$mu, 
                          sigma_sim=input$sigma,n_sim=input$n,
                          rows_sim=input$rows,cols_sim=input$cols)$cssj
    lambda_a <- b * (nrep * css)/(input$sigma^2) 
    lambda_b <- a * (nrep * css)/(input$sigma^2)
    dfa <- a - 1
    dfb <- b - 1
    df2ab <- (nrep - 1) * b * a
    critical_fa <- qf(1-input$alfa, df1 = dfa, 
                      df2 = df2ab)
    critical_fb <- qf(1-input$alfa, df1 = dfb, 
                      df2 = df2ab)
    
    powera <- 1 - pf(critical_fa, dfa, df2ab, lambda_a)
    powerb <- 1 - pf(critical_fb, dfb, df2ab, lambda_b)
    
    dataa <- data.frame(nrep,powera)
    
    p16 <- ggplot(dataa, aes(x = nrep, y = powera))+
      geom_line(size=0.5) + 
      theme(axis.text=element_text(size=10), 
            axis.title=element_text(size=10), 
            legend.text=element_text(size=14))+
      #ggtitle("Curva de potencia para factor A") + 
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p16)
    
  }) 
  
  ### Curva de potencia para interacción
  output$curve_powerc <- renderPlotly({
    
    x1 <- 2
    x2 <- 30
    nrep <- c(x1:x2)
    css <- simulation_f_int(num_sim=input$num_sim,mu_sim=input$mu, 
                          sigma_sim=input$sigma,n_sim=input$n,
                          rows_sim=input$rows,cols_sim=input$cols)$css
    
    lambda_pow_func <- (nrep * css)/input$sigma^2
    critical_f <- qf(1-input$alfa, df1 = (input$rows * input$cols)-1, 
                     df2 = (nrep - 1)*(input$rows * input$cols))
    
    ppower1 <- 1-pf(q = critical_f, df1 = (input$rows * input$cols)-1,
                    df2 = (nrep - 1)*(input$rows * input$cols),
                    ncp = lambda_pow_func)
    
    datoss <- data.frame(nrep,ppower1)
    
    p18 <- ggplot(datoss, aes(x = nrep, y = ppower1))+
      geom_line(size=0.5) + 
      theme(axis.text=element_text(size=10), 
            axis.title=element_text(size=10), 
            legend.text=element_text(size=14))+
      #ggtitle("Curva de potencia para interacción") + 
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p18)
    
  }) 
  
  ## Para analizar los datos
  df_products_upload1 <- reactive({
    inFile <- input$target_upload1
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
    return(df)
  })
  
  ## Cargando los datos
  output$sample_table<- renderDataTable({
    df <- df_products_upload1()
    datatable(df)
  })
  
  
  ## Tabla de medias observadas para datos introducidos por usuario
  output$descc_plot <- renderTable({
    d <- df_products_upload1() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    d2 <- d %>% group_by(F1,F2) %>% summarise(m=mean(response))
    spread(d2,key=F2,value=m) 
  })
  
  ## Gráfica de medias para datos incluidos por el usuario
  output$plot_plot <- renderPlot({
    d <- df_products_upload1() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    d
    d2 <- d %>% group_by(F1,F2) %>% summarise_all(.funs = c(m = mean,
                                                            sd1 = sd))
    
    ggplot(d) +
      aes(x = F1, y = response,
          color = F2,
          group = F2) +
      stat_summary(fun.data = mean_se, 
                   geom = "pointrange") +
      geom_line(stat = "summary", fun = mean, size=.75)+
      ylim(input$yRange[1],input$yRange[2])
    
  })
  
  #qqplot para datos incluidos por el usuario
  output$norm_norm <- renderPlot({
    normalidad(df_products_upload1())
  })
  
  # Prueba shapiro para datos incluidos por el usuario
  
  output$shapiro_norm <- renderPrint({
    
    d = df_products_upload1()
    shapi <- lm(response~F1*F2,d)
    shapiro.test(shapi$residuals)
    
  })
  
  # Gráfico de homocedasticidad para datos incluidos por usuario
  
  output$homo_homo <- renderPlot({
    homocedast(df_products_upload1())
  })
  
  # Prueba de bartlet para datos incluidos por el usuario
  
  output$bartlet_homo <- renderPrint({
      d <- df_products_upload1()
      d$F1 <- factor(d$F1)
      d$F2 <- factor(d$F2)
      bartlett.test(response~interaction(F1,F2),data=d)
    })
  
  # Tabla ANOVA para datos incluidos por el usuario
  output$table_anovaa <- renderPrint({
    
    d <- df_products_upload1() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    res <- lm(response~F1*F2,d)
    anova(res)
    
  })
  
  # Gráfico de comparaciones múltiples para datos incluidos por el usuario
  output$comp_usuari1 <- renderPlot({
    d <- df_products_upload1() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    res1 <- anova(lm(response~F1*F2,d))
    res <- aov(response~F1*F2, d)
    lsm_int <- emmeans(res, ~ F1:F2)
    lsm_A <- emmeans(res, ~ F1)
    
    if(res1$`Pr(>F)`[3]<0.05) {contrastes <- contrast(lsm_int, method = "pairwise", by = "F1")
    plot(contrastes)+coord_flip()+geom_vline(xintercept = 0, linetype="dashed")}
    else {contrastes1 <- contrast(lsm_A, method = "pairwise")
    plot(contrastes1)+coord_flip()+geom_vline(xintercept = 0, linetype="dashed")}
  })
  
  # Plot emmeans para interacción para datos incluidos por el usuario
  output$comp_usuari2 <- renderPlot({
    d <- df_products_upload1() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    res1 <- anova(lm(response~F1*F2,d))
    res <- aov(response~F1*F2, d)
    lsm_int <- emmeans(res, ~ F1:F2)
    lsm_B <- emmeans(res, ~ F2)
    contrastes <- contrast(lsm_int, method = "pairwise", by = "F2")
    
    if(res1$`Pr(>F)`[3]<0.05) {contrastes <- contrast(lsm_int, method = "pairwise", by = "F2")
    plot(contrastes)+coord_flip()+geom_vline(xintercept = 0, linetype="dashed")}
    else {contrastes1 <- contrast(lsm_B, method = "pairwise")
    plot(contrastes1)+coord_flip()+geom_vline(xintercept = 0, linetype="dashed")}
    
  })
  
  #Potencia para datos incluidos por el usuario
  
  
  

  
  
  
})









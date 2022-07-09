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
library(hrbrthemes)
library(viridis)
library(ggpubr)


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


## Gráfico de homocedasticidad

homocedast <- function(datos){
  mod3 <-lm(response ~ F1+F2, datos)
  est <- mod3$fitted.values
  pred <- mod3$residuals
  bd1 <- data.frame(est, pred)
  p4 <- ggplot(bd1) +
    aes(x = est, y = pred) +
    geom_point(size= I(3), col='blue') +
    geom_hline(yintercept = 0, linetype ='dashed') +
    labs(x = "Fitted values", y = "Residuals") +
    ggtitle('Homocedasticity plot') +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.5)))
  return(p4)
}

# Supuesto de normalidad para supuestos de bloque
normalidad <- function(datos){
  res <- lm(response~F1+F2,datos)
  p2 <- ggplot(datos, aes(sample = res$residuals))+ 
    stat_qq(size= I(3)) + stat_qq_line(size= I(0.6)) +
    ggtitle('QQ-Plot') +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.5))) +
    labs(y="Residuals", x="quantils")
  return(p2)
}

# Estadistica descriptiva para bloques
stadis_desc <- function(df){
  
  bar_y <- mean(df$response)
  table <- df %>% 
    group_by(F2) %>%
    summarise(
      n = length(response), 
      media = mean(response), 
      sd = sd(response), 
      min = min(response), 
      max = max(response),
      cv = sd(response)/mean(response)*100)
  return(table)}

# Estadistica descriptiva
stadis_desc1 <- function(df){
  
  bar_y <- mean(df$response)
  table <- df %>% 
    group_by(F1) %>%
    summarise(
      n = length(response), 
      media = mean(response), 
      sd = sd(response), 
      min = min(response), 
      max = max(response),
      cv = sd(response)/mean(response)*100)
  return(table)}


# Para graficar comparacion de medias y p-valores
Tukey1 <- function(datos){
  
  datos$F1 <- factor(data$F1)
  datos$F2 <- factor(data$F2)
  res <- lm(response~F1+F2,datos)
  tky1 = as.data.frame(TukeyHSD(aov(res))$F1)
  tky1$pair = rownames(tky1) 
  
  # Plot pairwise TukeyHSD comparisons and color by significance level
  p10 <-  ggplot(tky1, aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), 
                                     label=c("p<0.01","p<0.05","Non-Sig")))) +
    geom_hline(yintercept=0, linetype ='dashed') +
    geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2,size=0.8) +
    geom_point(aes(pair, diff),size=2) +
    labs(colour="") +
    ggtitle(TeX('Model = respuesta ~ treat')) +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.5))) +
    labs(y="Diferencia", x="Pares")
  
  p10
  
}










##############################################################################
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
  
  
  ##############################
  ## Datos para crear el diseño en bloques
  dataF1F2 <- eventReactive(input$go,{
    eF1 <- t(dF1())
    eF2 <- t(dF2())
    
    data <- data.frame(
      F1=rep(rep(1:input$rows,input$n),input$cols),
      F2=rep(rep(1:input$cols),input$rows) %>% sort()
    )
    
    ra <- input$rows
    ca <- input$cols
    n <- input$n
    k <- ra*ca*n
    
    ss <- input$sigma
    res <- rnorm(k,0,input$sigma)
    data <- data %>% mutate(response=input$mu+eF1[F1]+eF2[F2]+rnorm(k,0,input$sigma))
     
    data
  },ignoreNULL = T)
  

  ## Gráfica de medias para bloques
  output$Means <- renderPlot({
    d <- dataF1F2() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    d
    d2 <- d %>% group_by(F1,F2) %>% summarise_all(.funs = c(m = mean,
      sd1 = sd))

    ggplot(d) +
      aes(x = F2, y = response,
          color = F1,
          group = F1) +
      stat_summary(fun.data = mean_se, 
                   geom = "pointrange") +
      geom_line(stat = "summary", fun = mean, size=.75)+
      labs(x='Bloques',y='Respuesta',color='Factor')+
      ylim(input$yRange[1],input$yRange[2])+
      labs(x='Blocks',y='Response')
    
  })
  
  ## Tabla resumen por bloques
  output$TableMeans <- renderTable({
    stadis_desc(dataF1F2())
  })
  
  ## Tabla resumen por tratamiento del bloque
  output$TableMeans1 <- renderTable({
    stadis_desc1(dataF1F2())
  })
  
  
  ## Tabla de ANOVA factor para bloque
  output$ANOVA <- renderPrint({
    d <- dataF1F2() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
     
    res <- lm(response~F1+F2,d)
    anova(res)
  })
  
  # Anova sin el factor del bloque
  output$ANOVA1 <- renderPrint({
    d <- dataF1F2() %>% as.data.frame()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    
    res <- lm(response~F1,d)
    anova(res)
  })
  
  
  
  ## Prueba Barlett para bloques
  output$Barlett <- renderPrint({
    d <- dataF1F2()
    d$F1 <- factor(d$F1)
    d$F2 <- factor(d$F2)
    bartlett.test(response~F1,data=d)
  })
  

  # Plot emmeans con bloques
  output$plot_emmeans2 <- renderPlot({
    
    datos <- dataF1F2() %>% as.data.frame()
    
    datos$F1 <- factor(datos$F1)
    datos$F2 <- factor(datos$F2)
    
    res <- lm(response~F1+F2,datos)
    tky1 = as.data.frame(TukeyHSD(aov(res))$F1)
    tky1$pair = rownames(tky1) 
    
    # Plot pairwise TukeyHSD comparisons and color by significance level
    p10 <-  ggplot(tky1, aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), 
                                        label=c("p<0.01","p<0.05","Non-Sig")))) +
      geom_hline(yintercept=0, linetype ='dashed') +
      geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2,size=0.8) +
      geom_point(aes(pair, diff),size=2) +
      labs(colour="") +
      ggtitle(('Model = respuesta ~ treat')) +
      theme(plot.title = element_text(hjust = 0.5, size = rel(1.5))) +
      labs(y="Diferencia", x="Pares")
    
    p10
    
    })
  
  # Plot emmeans sin bloques
  output$plot_emmeans31 <- renderPlot({
    
    datos <- dataF1F2() %>% as.data.frame()
    
    datos$F1 <- factor(datos$F1)
    datos$F2 <- factor(datos$F2)
    
    res <- lm(response~F1,datos)
    tky1 = as.data.frame(TukeyHSD(aov(res))$F1)
    tky1$pair = rownames(tky1) 
    
    # Plot pairwise TukeyHSD comparisons and color by significance level
    p10 <-  ggplot(tky1, aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), 
                                        label=c("p<0.01","p<0.05","Non-Sig")))) +
      geom_hline(yintercept=0, linetype ='dashed') +
      geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2,size=0.8) +
      geom_point(aes(pair, diff),size=2) +
      labs(colour="") +
      ggtitle(('Model = respuesta ~ treat')) +
      theme(plot.title = element_text(hjust = 0.5, size = rel(1.5))) +
      labs(y="Diferencia", x="Pares")
    
    p10
    
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
  

  #################### Data generation for simulation
  
  dataF1F2_sim_sim <- function(rows,n,cols,sigma){
    
    eF1 <- t(dF1())
    eF2 <- t(dF2())
    
    data <- data.frame(
      F1=rep(rep(1:rows,n),cols),
      F2=rep(rep(1:cols),rows) %>% sort()
    )
    ra1 <- rows
    ca1 <- cols
    n1 <- n
    k1 <- ra1*ca1*n1
    
    ss1 <- sigma
    #res5 <- rnorm(k1,0,ss1)
    data <- data %>% mutate(response=mu+eF1[F1]+eF2[F2]+rnorm(k1,0,ss1))
    
    data$F1 <- as.factor(data$F1)
    data$F2 <- as.factor(data$F2)
    
    data
    
    modd <- anova(lm(response ~ F1+F2,data))
    r3 <- modd$`F value`[2]
    r4 <- modd$`F value`[1]

    list(f_int=r2,f_b=r3,f_a=r4,data=data)
  }
  
  # Para activar con botón
  dataF1F2_sim <- eventReactive(input$go,{
    
  dataF1F2_sim_sim(rows=input$rows,n=input$n,
                 cols=input$cols,sigma=input$sigma)
    
  })

  
  # Simulación de una distribución F factor A
  
  simulation_f_a <- function(num_sim,mu_sim, sigma_sim,n_sim,rows_sim,cols_sim){

    fila <- rep(t(dF1()),length(t(dF1())))
    eff_i <- matrix(fila,nrow = input$rows, ncol=input$cols,byrow = F)
    colum <- rep(t(dF2()),length(t(dF2())))
    eff_j <- matrix(colum,nrow = input$rows, ncol = input$cols,byrow = T)
    cell_mean <- input$mu + eff_i + eff_j
    
    #mm <- mean(colMeans(cell_mean))
    #mi <- rowMeans(cell_mean)
    mj <- rowMeans(cell_mean)
    
    #css <- sum((cell_mean-mm)^2)
    #cssi <- sum((mi-mean(mi))^2)
    cssj <- sum((mj-mean(mj))^2)
    
    pnc <- cols_sim*cssj/sigma_sim^2
    
    
    f <- replicate(num_sim, dataF1F2_sim_sim(rows_sim,n_sim,cols_sim,sigma_sim)$f_a) # cocientes F simulados
    data <- data.frame(f) # cocientes F en forma de tabla
    
    lambda <- pnc # parametro de no centralidad
    
    # Distribución F de simulación y la F teorica
    ua <- qf(p = 0.95, df1 = rows_sim-1, 
             df2 = (cols_sim - 1)*(rows_sim - 1))
    
    power <- sum(f>ua)/num_sim
    
    powert <- 1-pf(ua,rows_sim-1,
                   (cols_sim - 1)*(rows_sim - 1),lambda)%>%round(2)

    
    p5 <- ggplot(data, aes(f))+
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
          df2 = (cols_sim - 1)*(rows_sim - 1),
          ncp = lambda)) +
      
      stat_function(
        fun = "df",
        geom = "area",
        linetype = 2,
        fill = "red",
        alpha = .6,
        args = list(
          df1 = rows_sim-1,
          df2 = (cols_sim - 1)*(rows_sim - 1),
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
          df2 = (cols_sim - 1)*(rows_sim - 1)
        )
      ) + 
      xlim(0,30)+
      #   geom_vline(xintercept = ua,color="red",linetype="dashed")+
      annotate(geom = "text",x=10, y=0.2, label = paste("Potencia por simulación: ",power))+
      annotate(geom = "text",x=10, y=0.25, label = paste("Potencia teórica: ",powert))
    
    
    #     power <- sum(f>qf(1-alfas,ks-1,(n_tot)-ks))/nsim
    list(p5=p5, cssi=cssj)
    
  }
  
  
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
    
    withProgress(message = 'Computing Simulations', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
        Sys.sleep(0.15)
      }
      
    
   plot2()
    
    #    plot_final1 <- dibujito1 + xlim(input$xRange[1],input$xRange[2])
    
    #    dibujito1
   
    })
  })
  
  
  # Gráfico de homocedasticidad para simulación
  output$homocedas <- renderPlot({
    d = dataF1F2()
    homocedast(d)
  })
  
  # qq-plot
  output$normali <- renderPlot({
    d = dataF1F2()
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
    
    d = dataF1F2() 
    shapi <- lm(response~F1+F2,d)
    shapiro.test(shapi$residuals)
    
  })
  
  ## Curva de potencia para factor A
  output$curve_powera <- renderPlotly({
    
    withProgress(message = 'Computing Simulations', value = 0, {
      for (i in 1:15) {
        incProgress(1/15)
        Sys.sleep(0.15)
      }
    
    x1 <- 2
    x2 <- 30
    nrep <- c(x1:x2)
    
    a <- input$cols
    b <- input$rows
    css <- simulation_f_a(num_sim=input$num_sim,mu_sim=input$mu, 
                          sigma_sim=input$sigma,n_sim=input$n,
                          rows_sim=input$rows,cols_sim=input$cols)$cssi
    
    lambda_a <- (nrep * css)/(input$sigma^2)
    lambda_a1 <- (input$cols * css)/(input$sigma^2)
    
    dfa <- input$rows - 1
    df2ab <- (nrep - 1) * (input$rows - 1)
    df2ab1 <- (input$cols - 1) * (input$rows - 1)
    
    critical_fa <- qf(1-input$alfa, df1 = dfa, 
                      df2 = df2ab)
    critical_fa1 <- qf(1-input$alfa, df1 = dfa, 
                      df2 = df2ab1)
    
    powera <- 1 - pf(critical_fa, dfa, df2ab, lambda_a)
    powera1 <- 1 - pf(critical_fa1, dfa, df2ab1, lambda_a1) %>% round(3)
    
    dataa <- data.frame(nrep,powera)
    
    p17 <- ggplot(dataa, aes(x = nrep, y = powera))+
      geom_line(size=0.5) + 
      theme(axis.text=element_text(size=10), 
            axis.title=element_text(size=10), 
            legend.text=element_text(size=14))+
      #ggtitle("Curva de potencia para factor B") + 
      theme(plot.title = element_text(hjust = 0.5))+
      geom_segment(aes(x = input$cols, y = 0, xend = input$cols, yend = powera1),
                   linetype = "dashed", colour='orange')+
      geom_segment(aes(x = 0, y = powera1, xend = input$cols, yend = powera1),
                   linetype = "dashed", colour='orange')+
      geom_point(aes(x=input$cols,y=powera1), colour="blue", size=2)+
      annotate(geom = "text",x=10, y=0.3, label = paste("The power is: ",powera1))+
      labs(x='Blocks', y='Power')
    
    ggplotly(p17)
    
    })
    
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
  
  ## Gráfico de medias sin desviación estándar
  output$PlotCleanMeans <- renderPlot({
    x <- LETTERS[1:input$rows]
    y <- letters[1:input$cols]
    data <- expand.grid(X=x, Y=y)
    data$Magnitud <- dataF1F2()[,-(1:2)]
    
    # Give extreme colors:
    ggplot(data, aes(X, Y, fill= Magnitud)) + 
      geom_tile() +
      scale_fill_viridis(discrete=FALSE) +
      theme_ipsum()+
      labs(x='Niveles del factor', y='Bloques')
  })
  
  

  
  output$algo <- renderPrint({
    
    a <- simulation_f_a(num_sim=input$num_sim,
                   mu_sim=input$mu, 
                   sigma_sim=input$sigma,
                   n_sim=input$n,
                   rows_sim=input$rows, 
                   cols_sim=input$cols)$cssi
    a
    
  })
  
  
  
  
  
  
  

  
  
  
})









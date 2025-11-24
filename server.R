

library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(agricolae)
library(dplyr)
library(tidyr)

server <- function(input, output, session) {
  
  # ANOVA Unifactorial 
  calcular_anova_unifactorial <- function(datos_texto, alpha) {
    lineas <- strsplit(datos_texto, "\n")[[1]]
    datos_lista <- lapply(lineas, function(x) as.numeric(unlist(strsplit(x, ","))))
    
    matriz_datos <- do.call(rbind, datos_lista)
    n_grupos <- ncol(matriz_datos)
    n_replicas <- nrow(matriz_datos)

    y <- as.vector(matriz_datos)
    grupos <- factor(rep(1:n_grupos, each = n_replicas))

    modelo_aov <- aov(y ~ grupos)
    resumen <- summary(modelo_aov)

    medias <- colMeans(matriz_datos)
    desviaciones <- apply(matriz_datos, 2, sd)
    
    tabla_resumen <- data.frame(
      Grupo = paste("Grupo", 1:n_grupos),
      Media = round(medias, 3),
      Desviacion = round(desviaciones, 3),
      N = rep(n_replicas, n_grupos)
    )
    
    p_valor <- resumen[[1]][["Pr(>F)"]][1]
    significancia <- ifelse(p_valor < alpha, "Significativo", "No significativo")
    
    resultado <- list(
      modelo = modelo_aov,
      resumen = resumen,
      tabla = tabla_resumen,
      p_valor = p_valor,
      significancia = significancia,
      datos = data.frame(y = y, grupos = grupos),
      medias = medias
    )
    
    return(resultado)
  }
  
  # ANOVA Bifactorial
  calcular_anova_bifactorial <- function(datos_texto, alpha) {
    lineas <- strsplit(datos_texto, "\n")[[1]]
    matriz_datos <- matrix(nrow = length(lineas), ncol = 0)
    
    for (i in 1:length(lineas)) {
      fila <- as.numeric(unlist(strsplit(lineas[i], ",")))
      if (i == 1) {
        matriz_datos <- matrix(nrow = length(lineas), ncol = length(fila))
      }
      matriz_datos[i, ] <- fila
    }
    
    n_filas <- nrow(matriz_datos)
    n_cols <- ncol(matriz_datos)
    y <- as.vector(t(matriz_datos))
    factor_fila <- factor(rep(1:n_filas, each = n_cols))
    factor_columna <- factor(rep(1:n_cols, times = n_filas))
    
    modelo_bi <- aov(y ~ factor_fila + factor_columna)
    resumen_bi <- summary(modelo_bi)
    
    medias_filas <- rowMeans(matriz_datos)
    medias_columnas <- colMeans(matriz_datos)
    media_general <- mean(matriz_datos)
    
    tabla_interacciones <- expand.grid(
      Fila = 1:n_filas,
      Columna = 1:n_cols
    )
    tabla_interacciones$Media <- as.vector(t(matriz_datos))
    
    SS_filas <- resumen_bi[[1]][["Sum Sq"]][1]
    SS_columnas <- resumen_bi[[1]][["Sum Sq"]][2]
    SS_error <- resumen_bi[[1]][["Sum Sq"]][3]
    
    resultado <- list(
      modelo = modelo_bi,
      resumen = resumen_bi,
      medias_filas = medias_filas,
      medias_columnas = medias_columnas,
      tabla_interacciones = tabla_interacciones,
      matriz_original = matriz_datos
    )
    
    return(resultado)
  }
  
  #  Cuadrado Latino
  analizar_cuadrado_latino <- function(datos_texto, tratamientos_texto) {
    lineas_datos <- strsplit(datos_texto, "\n")[[1]]
    matriz_respuesta <- matrix(nrow = length(lineas_datos), ncol = 0)
    
    for (i in 1:length(lineas_datos)) {
      fila <- as.numeric(unlist(strsplit(lineas_datos[i], ",")))
      if (i == 1) {
        matriz_respuesta <- matrix(nrow = length(lineas_datos), ncol = length(fila))
      }
      matriz_respuesta[i, ] <- fila
    }
    
    lineas_trat <- strsplit(tratamientos_texto, "\n")[[1]]
    matriz_tratamientos <- matrix(nrow = length(lineas_trat), ncol = ncol(matriz_respuesta))
    
    for (i in 1:length(lineas_trat)) {
      matriz_tratamientos[i, ] <- unlist(strsplit(lineas_trat[i], ","))
    }

    n <- nrow(matriz_respuesta)
    y <- as.vector(t(matriz_respuesta)) 
    filas <- factor(rep(1:n, each = n))
    columnas <- factor(rep(1:n, times = n))
    tratamientos <- factor(as.vector(t(matriz_tratamientos)))
    
    modelo_latino <- aov(y ~ filas + columnas + tratamientos)
    resumen_latino <- summary(modelo_latino)
    
    medias_tratamientos <- tapply(y, tratamientos, mean)
    
    efectos_filas <- tapply(y, filas, mean) - mean(y)
    efectos_columnas <- tapply(y, columnas, mean) - mean(y)
    efectos_tratamientos <- medias_tratamientos - mean(y)
    
    tabla_resumen <- data.frame(
      Tratamiento = names(medias_tratamientos),
      Media = round(medias_tratamientos, 2),
      Efecto = round(efectos_tratamientos, 2),
      N = as.numeric(table(tratamientos))
    )
    
    resultado <- list(
      modelo = modelo_latino,
      resumen = resumen_latino,
      tabla_efectos = tabla_resumen,
      matriz_respuesta = matriz_respuesta,
      matriz_tratamientos = matriz_tratamientos
    )
    
    return(resultado)
  }
  
  #  Diseño Factorial 2^k
  analizar_factorial_2k <- function(k, respuestas_texto, nombres_factores) {
    respuestas <- as.numeric(unlist(strsplit(respuestas_texto, ",")))
    nombres <- unlist(strsplit(nombres_factores, ","))
    nombres <- trimws(nombres)
    
    n_runs <- 2^k
    matriz_diseno <- matrix(nrow = n_runs, ncol = k)
    
    for (i in 1:k) {
      nivel_rep <- 2^(k-i)
      patron <- rep(c(rep(-1, nivel_rep), rep(1, nivel_rep)), 2^(i-1))
      matriz_diseno[, i] <- patron
    }
    
    colnames(matriz_diseno) <- nombres[1:k]
    
    efectos_principales <- numeric(k)
    for (i in 1:k) {
      positivos <- respuestas[matriz_diseno[, i] == 1]
      negativos <- respuestas[matriz_diseno[, i] == -1]
      efectos_principales[i] <- mean(positivos) - mean(negativos)
    }
    
    names(efectos_principales) <- nombres[1:k]
    
    if (k >= 2) {
      n_interacciones <- choose(k, 2)
      interacciones_2 <- numeric(n_interacciones)
      nombres_int <- character(n_interacciones)
      idx <- 1
      
      for (i in 1:(k-1)) {
        for (j in (i+1):k) {
          interaccion <- matriz_diseno[, i] * matriz_diseno[, j]
          positivos <- respuestas[interaccion == 1]
          negativos <- respuestas[interaccion == -1]
          interacciones_2[idx] <- mean(positivos) - mean(negativos)
          nombres_int[idx] <- paste(nombres[i], nombres[j], sep = ":")
          idx <- idx + 1
        }
      }
      names(interacciones_2) <- nombres_int
    }
    
    df_completo <- data.frame(matriz_diseno, Respuesta = respuestas)
    
    formula_texto <- paste("Respuesta ~", paste(nombres[1:k], collapse = " * "))
    modelo_2k <- lm(as.formula(formula_texto), data = df_completo)
    
    resultado <- list(
      matriz = df_completo,
      efectos_principales = efectos_principales,
      interacciones = if (exists("interacciones_2")) interacciones_2 else NULL,
      modelo = modelo_2k
    )
    
    return(resultado)
  }
  
  # Efectos Principales con Réplicas
  calcular_efectos_con_replicas <- function(tipo_diseno, datos, con_replicas, datos_replicas = NULL) {
    k <- as.numeric(tipo_diseno)
    n_runs <- 2^k
    
    if (con_replicas && !is.null(datos_replicas)) {
      lineas <- strsplit(datos_replicas, "\n")[[1]]
      matriz_replicas <- matrix(nrow = length(lineas), ncol = 0)
      
      for (i in 1:length(lineas)) {
        fila <- as.numeric(unlist(strsplit(lineas[i], ",")))
        if (i == 1) {
          matriz_replicas <- matrix(nrow = length(lineas), ncol = length(fila))
        }
        matriz_replicas[i, ] <- fila
      }
      
      respuestas <- rowMeans(matriz_replicas)
      varianzas <- apply(matriz_replicas, 1, var)
    } else {
      respuestas <- as.numeric(unlist(strsplit(datos, ",")))
      varianzas <- rep(0, length(respuestas))
    }
    
    matriz_diseno <- matrix(nrow = n_runs, ncol = k)
    for (i in 1:k) {
      nivel_rep <- 2^(k-i)
      patron <- rep(c(rep(-1, nivel_rep), rep(1, nivel_rep)), 2^(i-1))
      matriz_diseno[, i] <- patron
    }
    
    efectos <- numeric(k)
    errores_estandar <- numeric(k)
    
    for (i in 1:k) {
      positivos <- respuestas[matriz_diseno[, i] == 1]
      negativos <- respuestas[matriz_diseno[, i] == -1]
      efectos[i] <- mean(positivos) - mean(negativos)
      
      if (con_replicas) {
        var_pos <- mean(varianzas[matriz_diseno[, i] == 1])
        var_neg <- mean(varianzas[matriz_diseno[, i] == -1])
        errores_estandar[i] <- sqrt((var_pos + var_neg) / (n_runs/2))
      }
    }
    
    resultado <- list(
      efectos = efectos,
      errores_estandar = errores_estandar,
      significancia = abs(efectos) > (2 * errores_estandar)
    )
    
    return(resultado)
  }
  
# Interacciones Múltiples
  analizar_interacciones_multiples <- function(num_factores, datos, nombres, tipo) {
    respuestas <- as.numeric(unlist(strsplit(datos, ",")))
    nombres_factores <- trimws(unlist(strsplit(nombres, ",")))
    n_runs <- 2^num_factores

    matriz <- matrix(nrow = n_runs, ncol = num_factores)
    for (i in 1:num_factores) {
      each_rep <- 2^(i-1) 
      patron <- rep(c(-1, 1), each = each_rep, length.out = n_runs)
      matriz[, i] <- patron
    }
    
    colnames(matriz) <- nombres_factores[1:num_factores]
    
    resultado_interacciones <- list()
    
    if (tipo == 2 || tipo == 0) {
      inter_2 <- list()
      if (num_factores >= 2) {
        combinaciones <- combn(1:num_factores, 2)
        for(c in 1:ncol(combinaciones)) {
          idx1 <- combinaciones[1, c]
          idx2 <- combinaciones[2, c]
          producto <- matriz[, idx1] * matriz[, idx2]
          efecto <- mean(respuestas[producto == 1]) - mean(respuestas[producto == -1])
          nombre <- paste(nombres_factores[idx1], nombres_factores[idx2], sep = ":")
          inter_2[[nombre]] <- efecto
        }
      }
      resultado_interacciones$dobles <- inter_2
    }
    
    if ((tipo == 3 || tipo == 0) && num_factores >= 3) {
      inter_3 <- list()
      combinaciones <- combn(1:num_factores, 3)
      for(c in 1:ncol(combinaciones)) {
        idx1 <- combinaciones[1, c]
        idx2 <- combinaciones[2, c]
        idx3 <- combinaciones[3, c]
        producto <- matriz[, idx1] * matriz[, idx2] * matriz[, idx3]
        efecto <- mean(respuestas[producto == 1]) - mean(respuestas[producto == -1])
        nombre <- paste(nombres_factores[idx1], nombres_factores[idx2], nombres_factores[idx3], sep = ":")
        inter_3[[nombre]] <- efecto
      }
      resultado_interacciones$triples <- inter_3
    }
    
    todos_efectos <- unlist(resultado_interacciones)
    if (length(todos_efectos) > 0) {
        tabla_resumen <- data.frame(
          Interaccion = names(todos_efectos),
          Efecto = round(todos_efectos, 3),
          Abs_Efecto = abs(round(todos_efectos, 3))
        )
        tabla_resumen <- tabla_resumen[order(tabla_resumen$Abs_Efecto, decreasing = TRUE), ]
    } else {
        tabla_resumen <- data.frame(Interaccion=character(), Efecto=numeric(), Abs_Efecto=numeric())
    }
    
    return(list(
      tabla = tabla_resumen,
      interacciones = resultado_interacciones
    ))
  }
  
# Prueba de Tukey
  realizar_prueba_tukey <- function(datos_texto, nombres_grupos, alpha, ordenar) {
    lineas <- strsplit(datos_texto, "\n")[[1]]
    datos_lista <- lapply(lineas, function(x) as.numeric(unlist(strsplit(x, ","))))
    
    y <- unlist(datos_lista)
    n_grupos <- length(datos_lista)
    longitudes <- sapply(datos_lista, length)
    
    grupos <- factor(rep(1:n_grupos, longitudes))
    if (nombres_grupos != "") {
      nombres <- trimws(unlist(strsplit(nombres_grupos, ",")))
      if(length(nombres) >= n_grupos) {
          levels(grupos) <- nombres[1:n_grupos]
      }
    }
    
    es_balanceado <- all(longitudes == longitudes[1])
    
    if (es_balanceado && longitudes[1] > 1) {
        n_bloques <- longitudes[1]
        bloques <- factor(rep(1:n_bloques, times = n_grupos)) 
        
        modelo <- aov(y ~ grupos + bloques)
    } else {
        modelo <- aov(y ~ grupos)
    }

    resumen_anova <- summary(modelo)

    idx_error <- length(resumen_anova[[1]][["Mean Sq"]])
    MSE <- resumen_anova[[1]][["Mean Sq"]][idx_error]
    df_error <- resumen_anova[[1]][["Df"]][idx_error]

    medias <- tapply(y, grupos, mean)

    tukey_resultado <- TukeyHSD(modelo, "grupos", ordered = ordenar, conf.level = 1 - alpha)

    q_critico <- qtukey(1 - alpha, n_grupos, df_error)
    n_promedio <- mean(longitudes) 
    diferencia_critica <- q_critico * sqrt(MSE / n_promedio)

    matriz_comp <- matrix(nrow = n_grupos, ncol = n_grupos)
    rownames(matriz_comp) <- names(medias)
    colnames(matriz_comp) <- names(medias)
    
    for (i in 1:n_grupos) {
      for (j in 1:n_grupos) {
        if (i != j) {
          diff_val <- abs(medias[i] - medias[j])
          matriz_comp[i, j] <- ifelse(diff_val > diferencia_critica, "*", "ns")
        } else {
          matriz_comp[i, j] <- "-"
        }
      }
    }
    
    resultado <- list(
      tukey = tukey_resultado,
      medias = medias,
      diferencia_critica = diferencia_critica,
      matriz_comparaciones = matriz_comp,
      resumen_anova = resumen_anova,
      MSE = MSE,
      modelo_usado = ifelse(es_balanceado, "ANOVA con Bloques (RCBD)", "ANOVA Unifactorial")
    )
    
    return(resultado)
  }
  
  
  # ANOVA Unifactorial
  observeEvent(input$calcular_uni, {
    resultado <- calcular_anova_unifactorial(input$datos_uni, input$alpha_uni)
    
    output$resultado_uni <- renderPrint({
      cat("=== Análisis de Varianza (ANOVA) ===\n")
      print(resultado$resumen)
      cat("\n=== Conclusión ===\n")
      cat("p-valor:", round(resultado$p_valor, 5), "\n")
      cat("Resultado:", resultado$significancia, "alfa de ", input$alpha_uni, "\n")
    })
    
    output$tabla_anova_uni <- renderDT({
      datatable(resultado$tabla, options = list(
        pageLength = 10,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ))
    })
    
    output$grafico_uni <- renderPlotly({
      plot_ly(data = resultado$datos, x = ~grupos, y = ~y, type = "box") %>%
        layout(title = "Análisis ANOVA Unifactorial",
               xaxis = list(title = "Grupos"),
               yaxis = list(title = "Valores"))
    })
  })
  
  # ANOVA Bifactorial
  observeEvent(input$calcular_bi, {
    resultado <- calcular_anova_bifactorial(input$datos_bi, input$alpha_bi)
    
    output$resultado_bi <- renderPrint({
      cat("=== Análisis de Varianza Bifactorial ===\n")
      print(resultado$resumen)
    })
    
    output$tabla_anova_bi <- renderDT({
      datatable(resultado$tabla_interacciones, options = list(
        pageLength = 10,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ))
    })
    
    output$grafico_bi <- renderPlotly({
      plot_ly(data = resultado$tabla_interacciones, 
              x = ~Fila, y = ~Media, color = ~factor(Columna),
              type = "scatter", mode = "lines+markers") %>%
        layout(title = "Gráfico de Interacciones",
               xaxis = list(title = "Factor Fila"),
               yaxis = list(title = "Media de Respuesta"))
    })
  })
  
  # Cuadrado Latino
  observeEvent(input$calcular_latino, {
    resultado <- analizar_cuadrado_latino(input$datos_latino, input$tratamientos_latino)
    
    output$resultado_latino <- renderPrint({
      cat("=== Análisis del Cuadrado Latino ===\n")
      print(resultado$resumen)
    })
    
    output$tabla_latino <- renderDT({
      datatable(resultado$tabla_efectos, options = list(
        pageLength = 10,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ))
    })
    
    output$grafico_latino <- renderPlotly({
      df_plot <- data.frame(
        Tratamiento = resultado$tabla_efectos$Tratamiento,
        Media = resultado$tabla_efectos$Media
      )
      
      plot_ly(data = df_plot, x = ~Tratamiento, y = ~Media, 
              type = "bar", marker = list(color = "steelblue")) %>%
        layout(title = "Medias por Tratamiento - Cuadrado Latino",
               xaxis = list(title = "Tratamiento"),
               yaxis = list(title = "Media"))
    })
  })
  
  # Diseño Factorial 2^k
  observeEvent(input$calcular_2k, {
    resultado <- analizar_factorial_2k(input$k_factorial, input$respuestas_2k, 
                                       input$nombres_factores)
    
    output$matriz_2k <- renderDT({
      datatable(resultado$matriz, options = list(
        pageLength = 10,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ))
    })
    
    output$efectos_2k <- renderPrint({
      cat("=== Efectos Principales ===\n")
      print(round(resultado$efectos_principales, 3))
      if (!is.null(resultado$interacciones)) {
        cat("\n=== Interacciones de Dos Factores ===\n")
        print(round(resultado$interacciones, 3))
      }
    })
    
    output$grafico_2k <- renderPlotly({
      efectos_df <- data.frame(
        Factor = names(resultado$efectos_principales),
        Efecto = resultado$efectos_principales
      )
      
      plot_ly(data = efectos_df, x = ~Factor, y = ~Efecto, 
              type = "bar", marker = list(color = ~Efecto,
                                         colorscale = 'Viridis')) %>%
        layout(title = "Efectos Principales - Diseño 2^k",
               xaxis = list(title = "Factor"),
               yaxis = list(title = "Efecto"))
    })
  })
  
  # Efectos Principales
  observeEvent(input$calcular_efectos, {
    datos_replicas <- if(input$con_replicas) input$replicas_datos else NULL
    resultado <- calcular_efectos_con_replicas(input$tipo_diseno, input$datos_efectos,
                                               input$con_replicas, datos_replicas)
    
    output$resultado_efectos <- renderPrint({
      cat("=== Efectos Principales Calculados ===\n\n")
      for (i in 1:length(resultado$efectos)) {
        cat(sprintf("Factor %d: %.3f", i, resultado$efectos[i]))
        if (input$con_replicas && length(resultado$errores_estandar) > 0) {
          cat(sprintf(" ± %.3f", resultado$errores_estandar[i]))
          if (resultado$significancia[i]) cat(" *")
        }
        cat("\n")
      }
      if (input$con_replicas) {
        cat("\n* = Efecto significativo (|efecto| > 2*SE)\n")
      }
    })
    
    output$tabla_efectos <- renderDT({
      df <- data.frame(
        Factor = paste("Factor", 1:length(resultado$efectos)),
        Efecto = round(resultado$efectos, 3)
      )
      if (input$con_replicas && length(resultado$errores_estandar) > 0) {
        df$Error_Estandar <- round(resultado$errores_estandar, 3)
        df$Significativo <- ifelse(resultado$significancia, "Sí", "No")
      }
      datatable(df, options = list(
        pageLength = 10,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ))
    })
    
    output$grafico_efectos <- renderPlotly({
      df <- data.frame(
        Factor = paste("Factor", 1:length(resultado$efectos)),
        Efecto = abs(resultado$efectos)
      )
      df <- df[order(df$Efecto, decreasing = TRUE), ]
      
      plot_ly(data = df, x = ~reorder(Factor, Efecto), y = ~Efecto,
              type = "bar", marker = list(color = "orange")) %>%
        layout(title = "Diagrama de Pareto de Efectos",
               xaxis = list(title = "Factor"),
               yaxis = list(title = "|Efecto|"))
    })
  })
  
  # Interacciones CORREGIDAS
  observeEvent(input$calcular_int, {
    resultado <- analizar_interacciones_multiples(input$num_factores_int, 
                                                  input$datos_int,
                                                  input$factores_int, 
                                                  as.numeric(input$tipo_interaccion))
    
    output$tabla_interacciones <- renderDT({
      datatable(resultado$tabla, options = list(
        pageLength = 15,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ))
    })
    
    output$resultado_interacciones <- renderPrint({
      cat("=== Resumen de Interacciones ===\n\n")
      cat("Número total de interacciones calculadas:", nrow(resultado$tabla), "\n")
      cat("Interacción más fuerte:", resultado$tabla$Interaccion[1], 
          "con efecto =", resultado$tabla$Efecto[1], "\n\n")
      cat("=== Valores Específicos ===\n")
      if ("T:C" %in% resultado$tabla$Interaccion) {
        idx_TC <- which(resultado$tabla$Interaccion == "T:C")
        cat("T:C =", resultado$tabla$Efecto[idx_TC], "\n")
      }
      if ("T:K" %in% resultado$tabla$Interaccion) {
        idx_TK <- which(resultado$tabla$Interaccion == "T:K")
        cat("T:K =", resultado$tabla$Efecto[idx_TK], "\n")
      }
      if ("C:K" %in% resultado$tabla$Interaccion) {
        idx_CK <- which(resultado$tabla$Interaccion == "C:K")
        cat("C:K =", resultado$tabla$Efecto[idx_CK], "\n")
      }
    })
    
    output$grafico_interacciones <- renderPlotly({
      top_10 <- head(resultado$tabla, 10)
      
      plot_ly(data = top_10, x = ~reorder(Interaccion, Abs_Efecto), 
              y = ~Efecto, type = "bar",
              marker = list(color = ~Efecto,
                           colorscale = list(c(0, "red"), c(0.5, "yellow"), c(1, "green")))) %>%
        layout(title = "Top 10 Interacciones por Magnitud",
               xaxis = list(title = "Interacción"),
               yaxis = list(title = "Efecto"))
    })
  })
  
  # Prueba de Tukey CORREGIDA
  observeEvent(input$calcular_tukey, {
    resultado <- realizar_prueba_tukey(input$datos_tukey, input$nombres_grupos,
                                       input$alpha_tukey, input$ordenar_medias)
    
    output$resultado_tukey <- renderPrint({
      cat("=== Prueba de Comparaciones Múltiples de Tukey ===\n\n")
      cat("MSE =", round(resultado$MSE, 3), "\n")
      cat("Diferencia Crítica de Tukey =", round(resultado$diferencia_critica, 3), "\n\n")
      cat("=== Resultados de TukeyHSD ===\n")
      print(resultado$tukey)
      cat("\n=== Interpretación ===\n")
      cat("Las comparaciones con p adj < ", input$alpha_tukey, " son significativas\n")
    })
    
    output$tabla_tukey <- renderDT({
      tukey_df <- as.data.frame(resultado$tukey[[1]])
      tukey_df$Comparacion <- rownames(tukey_df)
      tukey_df <- tukey_df[, c("Comparacion", "diff", "lwr", "upr", "p adj")]
      colnames(tukey_df) <- c("Comparación", "Diferencia", "LI", "LS", "p-ajustado")
      tukey_df$Significativo <- ifelse(tukey_df$`p-ajustado` < input$alpha_tukey, "Sí", "No")
      
      datatable(tukey_df, options = list(
        pageLength = 15,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      )) %>%
        formatRound(columns = 2:5, digits = 3)
    })
    
    output$grafico_tukey <- renderPlotly({
      medias_df <- data.frame(
        Grupo = names(resultado$medias),
        Media = as.numeric(resultado$medias)
      )
      
      plot_ly(data = medias_df, x = ~Grupo, y = ~Media,
              type = "scatter", mode = "markers+lines",
              marker = list(size = 10, color = "blue"),
              name = "Medias") %>%
        add_trace(y = ~Media + resultado$diferencia_critica/2,
                 mode = "lines", line = list(dash = "dash", color = "red"),
                 name = "Límite Superior") %>%
        add_trace(y = ~Media - resultado$diferencia_critica/2,
                 mode = "lines", line = list(dash = "dash", color = "red"),
                 name = "Límite Inferior") %>%
        layout(title = "Comparación de Medias - Prueba de Tukey",
               xaxis = list(title = "Grupos"),
               yaxis = list(title = "Media"),
               showlegend = TRUE)
    })
  })
  
}
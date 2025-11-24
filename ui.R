
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Diseño de Experimentos - Análisis Estadístico"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("ANOVA Unifactorial", tabName = "anova_uni", icon = icon("chart-bar")),
      menuItem("ANOVA Bifactorial", tabName = "anova_bi", icon = icon("table")),
      menuItem("Cuadrado Latino", tabName = "cuadrado_latino", icon = icon("square")),
      menuItem("Diseño Factorial 2^k", tabName = "factorial_2k", icon = icon("cubes")),
      menuItem("Efectos Principales", tabName = "efectos", icon = icon("arrows-alt")),
      menuItem("Interacciones", tabName = "interacciones", icon = icon("exchange-alt")),
      menuItem("Prueba de Tukey", tabName = "tukey", icon = icon("balance-scale"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      ")),
      tags$script(HTML("
        $(document).ready(function() {
          $.extend($.fn.dataTable.defaults, {
            language: {
              'sProcessing':     'Procesando...',
              'sLengthMenu':     'Mostrar _MENU_ registros',
              'sZeroRecords':    'No se encontraron resultados',
              'sEmptyTable':     'Ningún dato disponible en esta tabla',
              'sInfo':           'Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros',
              'sInfoEmpty':      'Mostrando registros del 0 al 0 de un total de 0 registros',
              'sInfoFiltered':   '(filtrado de un total de _MAX_ registros)',
              'sInfoPostFix':    '',
              'sSearch':         'Buscar:',
              'sUrl':            '',
              'sInfoThousands':  ',',
              'sLoadingRecords': 'Cargando...',
              'oPaginate': {
                'sFirst':    'Primero',
                'sLast':     'Último',
                'sNext':     'Siguiente',
                'sPrevious': 'Anterior'
              },
              'oAria': {
                'sSortAscending':  ': Activar para ordenar la columna de manera ascendente',
                'sSortDescending': ': Activar para ordenar la columna de manera descendente'
              }
            }
          });
        });
      "))
    ),
    
    tabItems(
      # ANOVA Unifactorial
      tabItem(tabName = "anova_uni",
        fluidRow(
          box(
            title = "Entrada de Datos - ANOVA Unifactorial",
            status = "primary", solidHeader = TRUE, width = 6,
            helpText("Ejemplo: Tasas de mortalidad con 4 tratamientos"),
            textAreaInput("datos_uni", "Ingrese los datos (separados por comas, cada grupo en una línea):",
                         value = "33.6,31.4,29.8,32.1\n32.5,30.1,28.5,29.9\n35.3,33.2,29.5,28.7\n34.4,28.6,33.9,30.1\n37.3,34.1,28.5,29.4",
                         rows = 6),
            numericInput("alpha_uni", "alfa (α):", value = 0.05, min = 0.01, max = 0.10, step = 0.01),
            actionButton("calcular_uni", "Calcular ANOVA", class = "btn-primary", icon = icon("calculator"))
          ),
          box(
            title = "Resultados ANOVA",
            status = "success", solidHeader = TRUE, width = 6,
            verbatimTextOutput("resultado_uni"),
            DTOutput("tabla_anova_uni")
          )
        ),
        fluidRow(
          box(
            title = "Gráfico de Medias por Grupo",
            status = "info", width = 12,
            plotlyOutput("grafico_uni", height = "400px")
          )
        )
      ),
      
      # ANOVA Bifactorial
      tabItem(tabName = "anova_bi",
        fluidRow(
          box(
            title = "Entrada de Datos - ANOVA Bifactorial",
            status = "primary", solidHeader = TRUE, width = 6,
            helpText("Ejemplo: Efectividad de detergentes y lavadoras"),
            textAreaInput("datos_bi", "Ingrese los datos (matriz de datos):",
                         value = "53,50,59\n54,54,60\n56,58,63\n50,45,58",
                         rows = 5),
            numericInput("alpha_bi", "alfa (α):", value = 0.05, min = 0.01, max = 0.10, step = 0.01),
            actionButton("calcular_bi", "Calcular ANOVA Bifactorial", class = "btn-success", icon = icon("th"))
          ),
          box(
            title = "Resultados ANOVA Bifactorial",
            status = "warning", solidHeader = TRUE, width = 6,
            verbatimTextOutput("resultado_bi"),
            DTOutput("tabla_anova_bi")
          )
        ),
        fluidRow(
          box(
            title = "Gráfico de Interacciones",
            status = "info", width = 12,
            plotlyOutput("grafico_bi", height = "400px")
          )
        )
      ),
      
      # Cuadrado Latino
      tabItem(tabName = "cuadrado_latino",
        fluidRow(
          box(
            title = "Diseño Cuadrado Latino",
            status = "primary", solidHeader = TRUE, width = 6,
            helpText("Ejemplo: Emisiones con 4 conductores, 4 coches, 4 aditivos"),
            textAreaInput("datos_latino", "Ingrese los datos del cuadrado latino:",
                         value = "19,24,23,26\n23,24,19,30\n15,14,15,16\n19,18,19,16",
                         rows = 5),
            textAreaInput("tratamientos_latino", "Ingrese los tratamientos (letras latinas):",
                         value = "A,B,D,C\nD,C,A,B\nB,D,C,A\nC,A,B,D",
                         rows = 4),
            actionButton("calcular_latino", "Analizar Cuadrado Latino", class = "btn-warning", icon = icon("latin"))
          ),
          box(
            title = "Resultados del Análisis",
            status = "success", solidHeader = TRUE, width = 6,
            verbatimTextOutput("resultado_latino"),
            DTOutput("tabla_latino")
          )
        ),
        fluidRow(
          box(
            title = "Visualización del Diseño",
            width = 12,
            plotlyOutput("grafico_latino", height = "400px")
          )
        )
      ),
      
      #Diseño Factorial 2^k
      tabItem(tabName = "factorial_2k",
        fluidRow(
          box(
            title = "Configuración del Diseño 2^k",
            status = "primary", solidHeader = TRUE, width = 6,
            helpText("Ejemplo: Proceso químico con 3 factores binarios"),
            numericInput("k_factorial", "Número de factores (k):", value = 3, min = 2, max = 5),
            textAreaInput("respuestas_2k", "Respuestas del experimento (orden estándar):",
                         value = "60,72,54,68,52,83,45,80",
                         rows = 2),
            textInput("nombres_factores", "Nombres de factores (separados por comas):",
                     value = "Temperatura,Concentración,Catalizador"),
            actionButton("calcular_2k", "Analizar Diseño 2^k", class = "btn-info", icon = icon("cube"))
          ),
          box(
            title = "Matriz de Diseño y Efectos",
            status = "success", solidHeader = TRUE, width = 6,
            DTOutput("matriz_2k"),
            verbatimTextOutput("efectos_2k")
          )
        ),
        fluidRow(
          box(
            title = "Gráfico de Efectos Principales",
            width = 12,
            plotlyOutput("grafico_2k", height = "400px")
          )
        )
      ),
      
      # Efectos Principales
      tabItem(tabName = "efectos",
        fluidRow(
          box(
            title = "Cálculo de Efectos Principales",
            status = "primary", solidHeader = TRUE, width = 6,
            helpText("Calcula el efecto promedio de cada factor"),
            radioButtons("tipo_diseno", "Tipo de diseño:",
                        choices = list("2^2" = 2, "2^3" = 3, "2^4" = 4),
                        selected = 3),
            textAreaInput("datos_efectos", "Ingrese las respuestas:",
                         value = "60,72,54,68,52,83,45,80",
                         rows = 2),
            checkboxInput("con_replicas", "¿Con réplicas?", value = FALSE),
            conditionalPanel(
              condition = "input.con_replicas == true",
              textAreaInput("replicas_datos", "Datos de réplicas (cada réplica en una línea):",
                           value = "28,25,27\n36,32,32\n18,19,23\n31,30,29",
                           rows = 4)
            ),
            actionButton("calcular_efectos", "Calcular Efectos", class = "btn-primary", icon = icon("chart-line"))
          ),
          box(
            title = "Resultados de Efectos Principales",
            status = "warning", solidHeader = TRUE, width = 6,
            verbatimTextOutput("resultado_efectos"),
            DTOutput("tabla_efectos")
          )
        ),
        fluidRow(
          box(
            title = "Gráfico de Pareto de Efectos",
            width = 12,
            plotlyOutput("grafico_efectos", height = "400px")
          )
        )
      ),
      
      #Interacciones
      tabItem(tabName = "interacciones",
        fluidRow(
          box(
            title = "Análisis de Interacciones",
            status = "primary", solidHeader = TRUE, width = 6,
            helpText("Analiza efectos combinados entre factores"),
            numericInput("num_factores_int", "Número de factores:", value = 3, min = 2, max = 4),
            textAreaInput("datos_int", "Respuestas del experimento:",
                         value = "60,72,54,68,52,83,45,80",
                         rows = 2),
            textInput("factores_int", "Nombres de factores:",
                     value = "T,C,K"),
            radioButtons("tipo_interaccion", "Tipo de interacción:",
                        choices = list("Dobles" = 2, "Triples" = 3, "Todas" = 0)),
            actionButton("calcular_int", "Calcular Interacciones", class = "btn-success", icon = icon("exchange-alt"))
          ),
          box(
            title = "Tabla de Interacciones",
            status = "info", solidHeader = TRUE, width = 6,
            DTOutput("tabla_interacciones"),
            verbatimTextOutput("resultado_interacciones")
          )
        ),
        fluidRow(
          box(
            title = "Gráfico de Interacciones",
            width = 12,
            plotlyOutput("grafico_interacciones", height = "400px")
          )
        )
      ),
      
      #Prueba de Tukey
      tabItem(tabName = "tukey",
        fluidRow(
          box(
            title = "Configuración de la Prueba de Tukey",
            status = "primary", solidHeader = TRUE, width = 6,
            helpText("Comparaciones múltiples entre grupos"),
            textAreaInput("datos_tukey", "Ingrese los datos por grupos:",
                         value = "53,50,59\n54,54,60\n56,58,63\n50,45,58",
                         rows = 4),
            textInput("nombres_grupos", "Nombres de los grupos:",
                     value = "Detergente1,Detergente2,Detergente3,Detergente4"),
            numericInput("alpha_tukey", "alfa (α):", 
                        value = 0.05, min = 0.01, max = 0.10, step = 0.01),
            checkboxInput("ordenar_medias", "Ordenar por medias", value = TRUE),
            actionButton("calcular_tukey", "Realizar Prueba de Tukey", class = "btn-warning", icon = icon("balance-scale"))
          ),
          box(
            title = "Resultados de Comparaciones Múltiples",
            status = "success", solidHeader = TRUE, width = 6,
            verbatimTextOutput("resultado_tukey"),
            DTOutput("tabla_tukey")
          )
        ),
        fluidRow(
          box(
            title = "Diagrama de Comparaciones",
            width = 12,
            plotlyOutput("grafico_tukey", height = "400px")
          )
        )
      )
    )
  )
)
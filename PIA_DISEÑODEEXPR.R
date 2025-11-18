# install.packages("shiny")

library(shiny)

# ---- Interfaz de usuario ----
ui <- fluidPage(

  # ---- Estilos visuales personalizados ----
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #a8edea, #fed6e3);
        font-family: 'Segoe UI', sans-serif;
        color: #333;
      }
      .main-box {
        background-color: rgba(255,255,255,0.9);
        border-radius: 25px;
        padding: 30px;
        margin-top: 40px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
      }
      h2 {
        text-align: center;
        color: #444;
        font-weight: bold;
        margin-bottom: 25px;
      }
      .btn {
        border-radius: 15px;
        font-size: 16px;
        padding: 10px 20px;
        background-color: #66a6ff;
        color: white;
        border: none;
        box-shadow: 0 3px 6px rgba(0,0,0,0.2);
      }
      .btn:hover {
        background-color: #5a8dee;
      }
      table {
        width: 100%;
        margin-top: 15px;
        background-color: white;
        border-radius: 10px;
      }
      th {
        background-color: #89f7fe;
        color: #333;
        text-align: center;
      }
      td {
        text-align: center;
        padding: 8px;
      }
      .delete-btn {
        background-color: #ff6b6b;
        color: white;
        border: none;
        border-radius: 8px;
        padding: 5px 10px;
      }
      .delete-btn:hover {
        background-color: #ee5253;
      }
    "))
  ),

  # ---- Contenedor principal ----
  div(class = "main-box",
      h2("🌸 Gestor de Temas y Problemas 🌸"),

      fluidRow(
        column(6,
               textInput("tema", "🧩 Nombre del tema:", placeholder = "Ejemplo: Álgebra, Programación, Física...")),
        column(6,
               textInput("problema", "📘 Descripción del problema:", placeholder = "Ejemplo: Resolver ecuación cuadrática"))
      ),

      actionButton("agregar", "Agregar", class = "btn"),
      br(), br(),

      h4("📋 Lista de temas y problemas agregados:"),
      tableOutput("tabla")
  )
)

# ---- Lógica del servidor ----
server <- function(input, output, session) {

  # Almacenamiento reactivo (tabla en memoria)
  datos <- reactiveVal(data.frame(Tema = character(),
                                  Problema = character(),
                                  stringsAsFactors = FALSE))

  # Agregar nuevos temas/problemas
  observeEvent(input$agregar, {
    if (input$tema != "" && input$problema != "") {
      nuevo <- data.frame(Tema = input$tema,
                          Problema = input$problema)
      datos(rbind(datos(), nuevo))
      updateTextInput(session, "tema", value = "")
      updateTextInput(session, "problema", value = "")
    }
  })

  # Mostrar la tabla
  output$tabla <- renderTable({
    datos()
  })

}

# ---- Ejecutar la app ----
shinyApp(ui, server)
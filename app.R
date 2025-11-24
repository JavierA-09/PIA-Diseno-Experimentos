
library(shiny)

# Cargar la interfaz de usuario y el servidor
source("ui.R")
source("server.R")

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
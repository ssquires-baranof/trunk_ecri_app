library(shiny)

options(shiny.host = "0.0.0.0")
options(shiny.port = 8777)

server <- source("server.R", local=T)$value

ui <- source(file= "ui.R", local = T)

shinyApp(ui = ui, server = server)
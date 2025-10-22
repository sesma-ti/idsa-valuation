###
# Aplicación para valuar una compañía
# Diplomatura en Ciencias de Datos con Python y R
# Tutor: Julio Paredes
# Alumno: Sebastian Ledesma
###
library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(ggplot2)
library(DT)
library(shinyjs)

source("ValuacionNegocio.R")

nombres_amigables <- list(
  "PeriodoMayorVentas" = "Período con más ventas",
  "PeriodoMenorVentas" = "Período con menos ventas",
  "HoraMasVentas" = "Hora con más ventas",
  "HoraMenosVentas" = "Hora con menos ventas",
  "ProductoMayorVentas" = "Producto con más ventas",
  "ProductoMenorVentas" = "Producto con menos ventas",
  "MetodoPagoMasUsado" = "Método de pago más usado",
  "MetodoPagoMenosUsado" = "Método de pago menos usado",
  "FacturacionPromedio" = "Facturación promedio",
  "RangoFacturacion" = "Rango de facturación",
  "CiudadMayorFacturacion" = "Ciudad con mayor facturación",
  "CiudadMenorFacturacion" = "Ciudad con menor facturación",
  "CalificacionPromedio" = "Calificación promedio",
  "FacturacionPromedioPorLineaProducto" = "Facturación promedio por línea de producto"
)

html_escenario <- '
<div style="font-family: Arial, sans-serif; line-height: 1.6; padding: 20px; border: 1px solid #ddd; border-radius: 8px; background-color: #fdfdfd;">

  <h3 style="color: #34495e;">Objetivo del Proyecto</h3>
  <p>
    Desarrollar una solución basada en ciencia de datos que permita valorar una empresa dedicada a la comercialización de productos, y generar estrategias personalizadas para maximizar su valor económico.
  </p>

  <h3 style="color: #34495e;">Usuario Final</h3>
  <p>
    Propietarios o directivos de empresas que deseen comprender el valor actual de su negocio y explorar acciones estratégicas para optimizar su crecimiento futuro.
  </p>

  <h3 style="color: #34495e;">Tecnología Utilizada</h3>
  <p>
    El proyecto se implementa utilizando el ecosistema de <strong>R</strong>, específicamente mediante una aplicación interactiva desarrollada en <strong>Shiny</strong>, complementada con un informe ejecutivo en <strong>R Markdown</strong>.
  </p>

  <h3 style="color: #34495e;">Enfoque Analítico</h3>
  <ol style="margin-left: 20px;">
    <li><strong>Análisis Descriptivo:</strong> Exploración inicial de los datos históricos de facturación, utilizando métricas estadísticas estándar y visualizaciones para identificar patrones clave.</li>
    <li><strong>Análisis Diagnóstico:</strong> División de los datos en conjuntos de entrenamiento y prueba. Evaluación de varios modelos predictivos y selección del modelo con menor RMSE.</li>
    <li><strong>Análisis Predictivo:</strong> Proyección de ventas futuras a partir del modelo más preciso, permitiendo estimar ingresos esperados.</li>
    <li><strong>Análisis Prescriptivo:</strong> Transformación de las proyecciones en flujos de caja libre, utilizados para valorar la compañía mediante flujos de caja descontados (DCF).</li>
  </ol>

  <h3 style="color: #34495e;">Recomendaciones Estratégicas</h3>
  <p>
    Se diseñan tres portafolios de productos adaptados a diferentes perfiles de riesgo:
    <ul style="margin-left: 40px;">
      <li><strong>Agresivo:</strong> Mayor riesgo, mayor retorno esperado.</li>
      <li><strong>Moderado:</strong> Balance entre riesgo y rendimiento.</li>
      <li><strong>Conservador:</strong> Mínimo riesgo, máxima estabilidad.</li>
    </ul>
  </p>

  <h3 style="color: #34495e;">Impacto en el Negocio</h3>
  <p>
    La solución proporciona una <strong>visión cuantitativa y orientada a decisiones</strong> sobre el valor de la empresa, facilitando estrategias para incrementar su rentabilidad de forma sostenible.
  </p>

  <h3 style="color: #34495e;">Modo de Uso</h3>
  <ol style="margin-left: 20px;">
    <li><strong>Carga de Datos:</strong> En la barra lateral, el usuario debe cargar un archivo <code>.csv</code> que contenga el historial de ventas por producto de la empresa.</li>
    <li><strong>Ejecución del Análisis:</strong> Al presionar el botón <strong>"Ejecutar análisis"</strong>, la aplicación iniciará automáticamente el proceso de modelado.</li>
    <li><strong>Procesamiento y Selección de Modelo:</strong> La aplicación evaluará múltiples modelos de series temporales (ETS, ARIMA, NNETAR, Prophet, entre otros) y seleccionará el de mejor desempeño (menor RMSE). Un cuadro de progreso en la esquina inferior derecha indicará el estado del proceso.</li>
    <li><strong>Visualización de Resultados:</strong> Al finalizar el análisis, se habilitarán las siguientes secciones:
      <ul style="margin-left: 20px;">
        <li><strong>Reporte General:</strong> Análisis descriptivo con estadísticas clave del conjunto de datos.</li>
        <li><strong>Proyección de Ventas:</strong> Visualización del modelo seleccionado, su error y la curva de predicción.</li>
        <li><strong>Valuación de la Compañía:</strong> Panel interactivo para ajustar parámetros financieros y calcular la valuación por DCF.</li>
        <li><strong>Portafolios Sugeridos:</strong> Recomendaciones de estrategias comerciales según perfil de riesgo.</li>
      </ul>
    </li>
  </ol>

</div>'

ratios_default <- list(
  MargenOperativo = 0.15,
  DepreciacionPorVenta = 0.03,
  CapexPorVenta = 0.05,
  CapTrabajoPorVenta = 0.02,
  TasaImpositiva = 0.30,
  TasaDescuento = 0.04,
  TasaCrecimiento = 0.03
)

# UI ----
ui <- fluidPage(
  tags$style(
    HTML("
      body { 
        padding-bottom: 60px;
      }
    ")
  ),
  titlePanel("Sesma - Value Optimization"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Subir archivo CSV de ventas", accept = ".csv"),
      actionButton("run_analysis", "Ejecutar Análisis")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Escenario",
          HTML(html_escenario)
        ),
        tabPanel("Reporte general", uiOutput("descripcion_ui")),
        tabPanel("Proyeccion de ventas",
                 h4("Análisis exploratorio de la serie de tiempo"),
                 tableOutput("ts_summary"),
                 br(),
                 h4("Serie de tiempo y forecast"),
                 h5(textOutput("forecast_legend")),
                 plotlyOutput("ts_forecast_plot")
        ),
        tabPanel("Valuacion de la compañia", uiOutput("valor_empresa")),
        tabPanel("Portafolios sugeridos", uiOutput("portfolio_ui"))
      )
    )
  ),
  
  tags$footer(
    tags$div(
      "Aplicación para valuar una compañía | Diplomatura en Ciencias de Datos con Python y R | Tutor: Julio Paredes | Alumno: Sebastian Ledesma",
      style = "
        position:fixed;
        bottom:0;
        left:0;
        width:100%;
        background-color:#f8f9fa;
        text-align:center;
        padding:10px;
        color:gray;
        border-top:1px solid #e7e7e7;
        z-index: 1000;"
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  ratios <- list(
    "MargenOperativo" = 0.15,
    "DepreciacionPorVenta" = 0.03,
    "CapexPorVenta" = 0.05,
    "CapTrabajoPorVenta" = 0.02,
    "TasaImpositiva" = 0.30,
    "TasaDescuento" = 0.04,
    "TasaCrecimiento" = 0.03
  )
  
  # Log de AutoValuation
  log_file <- tempfile(fileext = ".log")
  log_text <- reactiveVal("")
  
  append_log <- function(msg) {
    isolate({
      nuevo <- paste(log_text(), msg, sep = "\n")
      log_text(nuevo)
    })
  }
  
  output$log_output <- renderText({
    log_text()
  })
  
  # Cargar y transformar el archivo CSV
  datos_sales <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath) |>
      mutate(
        Time = lubridate::hm(Time),
        Hour = hour(Time),
        Date = lubridate::mdy(Date),
        Period = case_when(
          Hour >= 3  & Hour < 6  ~ "Madrugada",
          Hour >= 6  & Hour < 10 ~ "Mañana",
          Hour >= 10 & Hour < 12 ~ "Media mañana",
          Hour >= 12 & Hour < 14 ~ "Media tarde",
          Hour >= 14 & Hour < 18 ~ "Tarde",
          Hour >= 18 & Hour < 22 ~ "Noche",
          Hour >= 22 | Hour < 3  ~ "Trasnoche", 
          TRUE ~ NA_character_
        )
      )
    
    return(df)
  })
  
  ratios_usuario <- reactive({
    list(
      MargenOperativo = input$margen_operativo %||% ratios_default$MargenOperativo,
      DepreciacionPorVenta = input$depreciacion %||% ratios_default$DepreciacionPorVenta,
      CapexPorVenta = input$capex %||% ratios_default$CapexPorVenta,
      CapTrabajoPorVenta = input$capital_trabajo %||% ratios_default$CapTrabajoPorVenta,
      TasaImpositiva = input$impuestos %||% ratios_default$TasaImpositiva,
      TasaDescuento = input$descuento %||% ratios_default$TasaDescuento,
      TasaCrecimiento = input$crecimiento %||% ratios_default$TasaCrecimiento
    )
  })
  
  resultado <- eventReactive(input$run_analysis, {
    log_text("")
    writeLines(character(), log_file)
    
    withProgress(message = "Ejecutando análisis...", value = 0, {
      sales <- datos_sales()
      incProgress(0.1, detail = "Generando serie temporal")
      ts <- makeTimeSeries(sales)
      cat("✅ Serie temporal generada\n", file = log_file, append = TRUE)
      
      incProgress(0.2, detail = "Generando forecast")
      fc <- makeForecast(ts, log_fn = function(msg) {
        cat(msg, "\n", file = log_file, append = TRUE)
      })
      cat("✅ Forecast generado\n", file = log_file, append = TRUE)
      
      incProgress(0.2, detail = "Calculando métricas")
      metrics <- buildForecastMetrics(
        fc$forecast,
        ts |> filter(ds > max(ts$ds) - 20)
      )
      cat("✅ Métricas generadas\n", file = log_file, append = TRUE)
      
      incProgress(0.2, detail = "Construyendo portafolios")
      portfolio_tangent <- makeProductPortfolio(sales, "tangent")
      portfolio_max <- makeProductPortfolio(sales, "max-return")
      portfolio_min <- makeProductPortfolio(sales, "min-risk")
      cat("✅ Portafolios construidos\n", file = log_file, append = TRUE)
      
      incProgress(0.3, detail = "Construyendo outputs finales")
      list(
        ts = ts,
        ts_name = paste0("Modelo utilizado: ", fc$name," | Error: ", fc$error),
        desc = makeDescription(sales),
        forecast_metrics = metrics,
        forecast_raw = fc$forecast,
        forecast_plot = buildForecastPlot(metrics),
        portfolios = list(
          "tangent" = plotProductPortfolio(portfolio_tangent),
          "max-return" = plotProductPortfolio(portfolio_max),
          "min-risk" = plotProductPortfolio(portfolio_min)
        )
      )
    })
  })
  
  
  output$descripcion_ui <- renderUI({
    req(resultado())
    
    desc <- resultado()$desc
    
    ui_list <- lapply(names(desc), function(nombre) {
      output_id <- paste0("desc_", nombre)
      valor <- desc[[nombre]]
      
      print(paste("Procesando:", nombre))
      print(valor)
      
      if (is.null(valor)) {
        print("Valor es NULL, retorno NULL")
        return(NULL)
      }
      
      titulo <- nombres_amigables[[nombre]]
      if (is.null(titulo)) titulo <- nombre
      
      if (is.data.frame(valor)) {
        ui_element <- tableOutput(output_id)
      } else if (length(valor) == 1) {
        ui_element <- verbatimTextOutput(output_id)
      } else if (is.atomic(valor)) {
        ui_element <- tableOutput(output_id)
      } else {
        print("Tipo no manejado, retorno NULL")
        return(NULL)
      }
      
      ui_items <- list(
        tags$h4(titulo),
        ui_element,
        tags$hr()
      )
      
      ui_items <- ui_items[!sapply(ui_items, function(x) is.null(x) || length(x) == 0)]
      tagList(ui_items)
    })
    
    print("Lista UI antes de tagList final:")
    print(ui_list)
    
    ui_list <- ui_list[!sapply(ui_list, is.null)]
    
    do.call(tagList, ui_list)
  })
  
  observe({
    req(resultado())
    desc <- resultado()$desc
    
    for (nombre in names(desc)) {
      local({
        nm <- nombre
        output_id <- paste0("desc_", nm)
        valor <- desc[[nm]]
        
        if (is.data.frame(valor)) {
          output[[output_id]] <- renderTable({
            valor
          })
        } else {
          output[[output_id]] <- renderText({
            as.character(valor)
          })
        }
      })
    }
  })
  
  
  output$forecast_plotly <- renderPlotly({
    req(resultado())
    forecast_raw <- resultado()$forecast_raw
    
    plot_ly(forecast_raw, x = ~ds, y = ~y, type = 'scatter', mode = 'lines') %>%
      layout(title = "Forecast de ventas",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Monto"))
  })
  
  output$ts_summary <- renderTable({
    req(resultado())
    ts <- resultado()$ts
    data.frame(
      Min = min(ts$y, na.rm=TRUE),
      Max = max(ts$y, na.rm=TRUE),
      Media = mean(ts$y, na.rm=TRUE),
      SD = sd(ts$y, na.rm=TRUE)
    )
  })
  
  output$forecast_legend <- renderText({
    resultado()$ts_name
  })
  
  
  output$ts_forecast_plot <- renderPlotly({
    req(resultado())
    ts <- resultado()$ts
    forecast_raw <- resultado()$forecast_raw
    
    plot_ly() %>%
      add_lines(data = ts, x = ~ds, y = ~y, name = "Serie histórica", line = list(color = 'black')) %>%
      add_lines(data = forecast_raw, x = ~ds, y = ~y, name = "Forecast", line = list(color = 'red')) %>%
      layout(title = "Serie de tiempo y Forecast",
             xaxis = list(title = "Fecha"),
             yaxis = list(title = "Valor"))
  })
  
  
  
  output$valor_empresa <- renderText({
    req(resultado())
    paste("Valor estimado de la empresa: $", round(resultado()$valuation, 2))
  })
  
  output$portfolio_ui <- renderUI({
    req(resultado())
    
    tagList(
      h4("Portafolio que maximiza el rendimiento - Agresivo:"),
      plotlyOutput("plot_max_return"),
      
      h4("Portafolio con mejor relación riesgo/retorno - Moderado:"),
      plotlyOutput("plot_tangent"),
      
      h4("Portafolio que minimiza el riesgo - Conservador:"),
      plotlyOutput("plot_min_risk")
    )
  })
  
  output$plot_max_return <- renderPlotly({
    req(resultado())
    resultado()$portfolios[["max-return"]]
  })
  
  output$plot_tangent <- renderPlotly({
    req(resultado())
    resultado()$portfolios[["tangent"]]
  })
  
  output$plot_min_risk <- renderPlotly({
    req(resultado())
    resultado()$portfolios[["min-risk"]]
  })
  
  output$valor_empresa <- renderUI({
    req(resultado())
    tagList(
      h4("Ratios del modelo"),
      fluidRow(
        column(6, numericInput("margen_operativo", "Margen Operativo", value = 0.15, step = 0.01)),
        column(6, numericInput("depreciacion", "Depreciación por Venta", value = 0.03, step = 0.01)),
        column(6, numericInput("capex", "Capex por Venta", value = 0.05, step = 0.01)),
        column(6, numericInput("capital_trabajo", "Capital de Trabajo por Venta", value = 0.02, step = 0.01)),
        column(6, numericInput("impuestos", "Tasa Impositiva", value = 0.30, step = 0.01)),
        column(6, numericInput("descuento", "Tasa de Descuento", value = 0.04, step = 0.01)),
        column(6, numericInput("crecimiento", "Tasa de Crecimiento", value = 0.03, step = 0.01))
      ),
      tags$hr(),
      h4("Flujos de caja libre (FCF)"),
      DT::dataTableOutput("tabla_fcf"),
      tags$hr(),
      h4("Gráfico de FCF"),
      plotlyOutput("grafico_fcf"),
      tags$hr(),
      h4("Valor actual neto (NPV)"),
      textOutput("valor_npv"),
      h4("Valor terminal"),
      textOutput("valor_terminal"),
      h4("Valor final de la compañía"),
      textOutput("valor_total")
    )
  })

  output$tabla_fcf <- DT::renderDataTable({
    req(resultado())
    tryCatch({
      fcf <- makeValuation.fcf(resultado()$forecast_raw, ratios_usuario())
      fcf <- fcf |> dplyr::arrange(desc(ds))
      DT::datatable(fcf, options = list(pageLength = 10))
    }, error = function(e) {
      data.frame(Error = e$message)
    })
  })
  
  output$grafico_fcf <- renderPlotly({
    req(resultado())
    tryCatch({
      fcf <- makeValuation.fcf(resultado()$forecast_raw, ratios_usuario())
      plot_ly(fcf, x = ~ds, y = ~fcf, type = 'scatter', mode = 'lines+markers') %>%
        layout(title = "Flujos de Caja Libre (FCF) en el Tiempo",
               xaxis = list(title = "Fecha"),
               yaxis = list(title = "FCF"))
    }, error = function(e) {
      plot_ly() %>% layout(title = paste("Error:", e$message))
    })
  })
  
  
  output$valor_npv <- renderText({
    req(resultado())
    fcf <- makeValuation.fcf(resultado()$forecast_raw, ratios_usuario())
    val <- makeValuation.npv(fcf, ratios_usuario())
    paste0("$ ", format(round(val, 2), big.mark = ","))
  })
  
  
  output$valor_terminal <- renderText({
    req(resultado())
    fcf <- makeValuation.fcf(resultado()$forecast_raw, ratios_usuario())
    val <- makeValuation.terminal_value(fcf, ratios_usuario())
    paste0("$ ", format(round(val, 2), big.mark = ","))
  })
  
  
  output$valor_total <- renderText({
    req(resultado())
    fcf <- makeValuation.fcf(resultado()$forecast_raw, ratios_usuario())
    val_npv <- makeValuation.npv(fcf, ratios_usuario())
    val_tv <- makeValuation.terminal_value(fcf, ratios_usuario())
    total <- val_npv + val_tv
    paste0("$ ", format(round(total, 2), big.mark = ","))
  })
  
}

# Lanzar la app
shinyApp(ui = ui, server = server)

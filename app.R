library(shiny)
library(dplyr)
library(arrow)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(gt)
library(gtExtras)

proyecciones <- arrow::read_parquet("censo_proyecciones_año.parquet")

proyecciones_edad <- arrow::read_parquet("censo_proyecciones_año_edad_genero.parquet")

ui <- fluidPage(
    
    fluidRow(
        column(12,
               hr(),
               p("Elija una o más regiones para luego elegir una o varias comunas que serán incluidas en el gráfico."),
               pickerInput("regiones",
                           label = h4("Regiones"),
                           width = "100%",
                           multiple = TRUE,
                           choices = NULL,
                           options = list(width = FALSE,
                                          noneSelectedText = "Sin selección")
               )
        ),
        column(12,  
               div(   
                   pickerInput("comunas",
                               label = h4("Comunas"),
                               width = "100%",
                               multiple = TRUE,
                               choices = NULL,
                               options = list(maxOptions = 8, 
                                              maxOptionsText = "Máximo 8",
                                              noneSelectedText = "Sin selección",
                                              width = FALSE)
                   )
               ),
               # )
               hr()
               
        )
    ),
    
    # evolución ----
    fluidRow(
        column(12,
               plotOutput("grafico_proyeccion") |> withSpinner()
        )
    ),
    
    # comparar años ----
    fluidRow(
        column(12,
               p("Seleccione dos años"),
               
               sliderInput("comparar_años", label = h4("Años"), 
                           min = min(proyecciones_edad$año), max = max(proyecciones_edad$año),
                           value = c(2002, 2023), ticks = T, sep = "", width = "100%"),
               
               gt_output("comparacion_años")
        )
    ),
        
               
    
    
    # pirámides ----
    fluidRow(
        column(12,
               h2("Pirámides"),
               
               pickerInput("regiones_piramide",
                           label = h4("Regiones"),
                           width = "100%",
                           multiple = F,
                           choices = NULL,
                           options = list(width = FALSE,
                                          noneSelectedText = "Sin selección")
               ),
               
               pickerInput("comuna_piramide",
                           label = h4("Comunas"),
                           width = "100%",
                           multiple = F,
                           choices = NULL,
                           options = list(noneSelectedText = "Sin selección",
                                          width = FALSE)
               ),
               
               sliderInput("año_piramide", label = h4("Año"), 
                           min = min(proyecciones_edad$año), max = max(proyecciones_edad$año),
                           value = 2023, ticks = T, sep = "", width = "100%")
        )
    ),
    
    fluidRow(
        column(12,
               plotOutput("grafico_piramide") |> withSpinner()
        )
    )
    
    
)


server <- function(input, output, session) {
    
    #función que filtra las comunas de las regiones seleccionadas
    filtrar_comunas <- function(datos, seleccion) {
        if ("Todas las regiones" %in% seleccion) {
            lista_comunas <- split(unique(datos$comuna) |> sort(), 
                                   unique(datos$region))
        } else {
            # browser()
            proyecciones_region <- datos |> 
                filter(region %in% seleccion) |> 
                select(region, comuna) |> 
                arrange(region, comuna) |> 
                distinct()
            
            lista_comunas <- split(proyecciones_region, ~region) |> lapply(pull, comuna)
        }
    }
    
    
    #selectores ----
    ## regiones ----
    updatePickerInput(session, "regiones",
                      choices = c("Todas las regiones", as.character(unique(proyecciones$region))),
                      selected = "Metropolitana de Santiago",
                      options = list(width = FALSE, noneSelectedText = "Sin selección")
    )
    updatePickerInput(session, "regiones_piramide",
                      choices = c("Todas las regiones", as.character(unique(proyecciones$region))),
                      selected = "Metropolitana de Santiago",
                      options = list(width = FALSE, noneSelectedText = "Sin selección")
    )
    
    ## filtrar comunas ----
    #filtrar comunas según regiones elegidas
    lista_comunas <- reactive(filtrar_comunas(proyecciones, input$regiones))
    lista_comunas_piramide <- reactive(filtrar_comunas(proyecciones, input$regiones_piramide))
    
    
    ## comunas ----
    #pone las comunas en el selector de comunas según la región elegida
    
    ### evolución ----
    observeEvent(input$regiones, {
        req(length(input$regiones) > 0)
        
        updatePickerInput(session,
                          inputId = "comunas",
                          choices = lista_comunas(),
                          selected = c("La Pintana", "Puente Alto", "Ñuñoa", "Vitacura", "Independencia", "La Florida"),
                          options = list(maxOptions = 8, 
                                         maxOptionsText = "Máximo 8",
                                         width = FALSE,
                                         noneSelectedText = "Sin selección")
        )
    })
    
    ### pirámide ----
    observeEvent(input$regiones_piramide, {
        req(length(input$regiones_piramide) > 0)
        
        updatePickerInput(session,
                          inputId = "comuna_piramide",
                          choices = lista_comunas_piramide(),
                          selected = "Puente Alto",
                          options = list(width = FALSE,
                                         noneSelectedText = "Sin selección")
        )
    })
    
    
    # datos ----
    ## datos evolución ----
    proyecciones_comunas <- reactive({
        req(length(input$comunas) > 0)
        
        proyecciones |> 
            filter(comuna %in% input$comunas) |> 
            mutate(proyeccion = ifelse(año > 2017, "futuro", "pasado"),
                   proyeccion_b = ifelse(año > 2016, "futuro", "pasado")) |> 
            mutate(año_censal = ifelse(año %in% c(2002, 2017), "censo", "proyección"))
    })
    
    ## datos pirámide ----
    proyecciones_piramide <- reactive({
        proyecciones_edad |> 
            filter(comuna == input$comuna_piramide) |> 
            filter(año == input$año_piramide) |> 
            # crear grupos de edad
            mutate(edad_categoría = cut(edad, breaks = c(-Inf, 5, 14, 19, 24, 29, 39, 49, 59, 69, Inf))) |> 
            # sumar población por grupos de edad
            group_by(género, edad_categoría) |> 
            summarize(población = sum(población),
                      min = min(edad),
                      max = max(edad)) |> 
            # generar etiquetas
            mutate(etiqueta = ifelse(min == 70, "70 o más años", paste(min, "a", max, "años")),
                   etiqueta = reorder(etiqueta, edad_categoría))
    })
    
    # gráficos ----
    
    ## grafico evolución ----
    output$grafico_proyeccion <- renderPlot({
        req(length(input$comunas) > 0)
        
        # proyecciones_comunas()
        # browser()
        
        # dev.new()
        proyecciones_comunas() |> 
            ggplot(aes(x = año, y = población,
                       color = comuna, fill = comuna,
                       alpha = proyeccion, size = año_censal,
                       group = comuna)) +
            # figuras
            geom_vline(xintercept = 2024) +
            geom_line(aes(alpha = proyeccion_b), linewidth = 1.2, show.legend = F) +
            geom_point() +
            # escalas
            scale_alpha_manual(values = c("pasado" = 1, "futuro" = 0.3)) +
            scale_size_manual(values = c("proyección" = 0, "censo" = 6), guide = "none") +
            scale_x_continuous(breaks = c(2002, seq(2005, 2035, by = 5), 2017)) +
            scale_y_continuous(labels = function(x) paste(x/1000, "mil")) +
            # temas
            theme_minimal() +
            theme(legend.position = "right",
                  axis.title.x = element_blank(),
                  panel.grid.minor.x = element_blank()) +
            # etiquetas
            labs(y = "Población residente proyectada",
                 caption = "fuente: INE", alpha = "Temporalidad", 
                 color = "Comunas", fill = "Comunas")
        
    })
    
    ## grafico piramide ----
    output$grafico_piramide <- renderPlot({
        req(length(input$comuna_piramide) > 0)
        
        # proyecciones_piramide()
        # browser()
        # dev.new()
        options(scipen = 9999)
        
        max <- signif(412069, digits = 0)
        seq(100, max, by = ifelse(max > 1000, 1000, 100))
        
        proyecciones_piramide() |> 
            mutate(población = ifelse(género == "masculino", población, -población)) |> 
            ggplot(aes(x = población, y = etiqueta, 
                       fill = género, color = género)) +
            scale_x_continuous(labels = ~case_when(.x == 0 ~ "",
                                                   abs(.x) < 1000 ~ as.character(abs(.x)),
                                                   abs(.x) >= 1000 ~ paste(abs(.x)/1000, "mil")),
                               limits = ~c(-max(abs(.x)), max(abs(.x)))
            ) +
            geom_col(width = 0.5) +
            theme_minimal() +
            theme(legend.position = "top") +
            labs(y = "Categorías de edad", x = "Población por grupo de edad y género")
    })
    
    # tablas ----
    
    ## tabla comparar años ----
    
    output$comparacion_años <- render_gt({
        req(length(input$comunas) > 0)
        
        # dev.new()
        # proyecciones_comunas()
        # browser()
        
        año_2 = sym(as.character(input$comparar_años[2]))
        año_1 = sym(as.character(input$comparar_años[1]))
        
        proyecciones_comunas() |> 
            filter(año %in% input$comparar_años) |> 
            tidyr::pivot_wider(id_cols = comuna, names_from = año, values_from = población) |> 
            # mutate(cambio := !!sym(as.character(input$comparar_años[2]))/!!sym(as.character(input$comparar_años[1]))) |>
            mutate(cambio := (!!año_2 - !!año_1) / !!año_2) |> 
            # mutate(flecha = ifelse(cambio >= 1, "arrow-up", "arrow-down")) |> 
            mutate(flecha = case_when(cambio >= 0.5 ~ "angles-up",
                                      cambio >= 0 ~ "arrow-up",
                                      cambio < 0 ~ "arrow-down")) |> 
            relocate(flecha, .before = cambio) |> 
            #tabla
            gt() |> 
            gt_fa_column(flecha, palette = c("angles-up" = "red", "arrow-up" = "black", "arrow-down" = "red")) |> 
            fmt_percent(
                columns = cambio,
                decimals = 1,
            ) |> 
            cols_align(columns = where(is.numeric), align = "left") |> 
            fmt_number(columns = 2:3, sep_mark = ".", decimals = 0) |> 
            cols_label(
                cambio = "% de cambio",
                comuna = "Comuna",
                flecha = ""
            ) |> 
            tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

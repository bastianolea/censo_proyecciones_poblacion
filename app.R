library(shiny)
library(dplyr)
library(arrow)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(gt)
library(gtExtras)
library(thematic)
library(bslib)
library(glue)
library(colorspace)

color_principal = "#365c8d"
color_destacado = "#3a87c8"
color_fondo = "#FFFFFF"
color_negativo = "#e5084b"
color_masculino = "#770072" |> lighten(0.2)
color_femenino = "#3a1582" |> lighten(0.2)


proyecciones <- arrow::read_parquet("censo_proyecciones_año.parquet")

proyecciones_edad <- arrow::read_parquet("censo_proyecciones_año_edad_genero.parquet")

options(spinner.type = 4, spinner.color = color_destacado)

#tema automático
thematic_shiny(font = "auto", bg = color_fondo, fg = color_principal, accent = color_destacado)

css <- function(text) {
    tags$style(glue(text, .open = "{{", .close = "}}"))
}

ui <- fluidPage(
    theme = bs_theme(
        bg = color_fondo, fg = color_principal, primary = color_destacado
    ),
    
    # css ----
    css(".explicacion {
      font-size: 80%; opacity: 1; line-height: 1.3;
      }"),
    
    fluidRow(
        #título ----
        div(style = "padding-top: 12px; padding-bottom: 20px;",
            
            titlePanel(
                h1("Censo de población: proyecciones", 
                   style = glue("font-weight: bold; color: {color_destacado}")),
                windowTitle = "Proyecciones Censo"),
            
            div(style = "margin-bottom: 8px; font-size: 80%;",
                
                markdown("Visualizador de datos oficiales de proyecciones poblacionales realizadas por el [Instituto Nacional de Estadísticas](https://www.ine.gob.cl) de Chile"),
                
                em(tags$a("Bastián Olea Herrera", 
                          href = "http://bastian.olea.biz",
                          target = "_blank"))
            ),
            
            div(style = "padding-top: 18px;",
                markdown("Aplicación web que visualiza los datos oficiales del [Instituto Nacional de Estadísticas](https://www.ine.gob.cl) de Chile sobre [proyecciones de población](https://www.ine.gob.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion); es decir, estimaciones del crecimiento poblacional hacia el futuro, a partir de los datos obtenidos en los censos oficiales.")
            ),
            hr()
        )
    ),
    
    fluidRow(
        column(12, style = "margin-top: -20px;",
               
               # evolución ----
               fluidRow(
                   column(7, 
                          h2("Evolución de la población comunal"),
                          p("En este gráfico puedes analizar los cambios poblacionales proyectados para las comunas que elijas. Los puntos indican mediciones de población realizadas por censos nacionales de población y vivienda, mientras que el resto de las líneas representan proyecciones de dichas mediciones."),
                          
                          p(class = "explicacion",
                            "Selecciona una o más regiones para luego elegir una o varias comunas que serán incluidas en el gráfico."),
                          
                          pickerInput("regiones",
                                      label = h4("Regiones"),
                                      width = "100%",
                                      multiple = TRUE,
                                      choices = NULL,
                                      options = list(width = FALSE,
                                                     noneSelectedText = "Sin selección")
                          ),
                          
                          pickerInput("comunas",
                                      label = h4("Comunas"),
                                      width = "100%",
                                      multiple = TRUE,
                                      choices = NULL,
                                      options = list(maxOptions = 8, 
                                                     maxOptionsText = "Máximo 8",
                                                     noneSelectedText = "Sin selección",
                                                     width = FALSE)
                          ),
                          
                          div(style = "overflow-x: scroll;",
                              plotOutput("grafico_proyeccion", width = "700px") |> withSpinner()
                          ),
                          br()
                   ),
                   
                   # comparar años ----
                   column(5,
                          
                          h2("Cambios en la población comunal"),
                          p("Esta tabla detalla una lista de las comunas seleccionadas, con su población correspondiente para los años que elijas, junto a el cambio porcentual entre ambas fechas."),
                          
                          p(class = "explicacion",
                            "Selecciona dos años para ajustar la comparación entre poblaciones."),
                          
                          sliderInput("comparar_años", 
                                      label = h4("Seleccione dos años a comparar"), 
                                      min = min(proyecciones_edad$año), max = max(proyecciones_edad$año),
                                      value = c(2002, 2023), ticks = T, sep = "", width = "100%"),
                          
                          div(style = "margin-top: 20px;",
                              gt_output("comparacion_años") |> withSpinner()
                          )
                   )
               ),
               
               hr()
        )
    ),
    
    
    
    
    # pirámides ----
    fluidRow(
        column(12,
               h2("Pirámide poblacional"),
               p("Vuelve a elegir una comuna para poder visualizar su pirámide poblacional, que corresponde a la distribución de la población de dicha comuna, pero separada en grupos de edad y género."),
               
               p(class = "explicacion",
                 "Elige una región, luego una comuna, para posteriormente cambiar los años para ver las diferencias en la pirámide poblacional de la comuna elegida."),
               
               pickerInput("regiones_piramide",
                           label = h4("Regiones"),
                           width = "100%",
                           multiple = F,
                           choices = NULL,
                           options = list(width = FALSE,
                                          noneSelectedText = "Sin selección")
               ),
               
               pickerInput("comuna_piramide",
                           label = h4("Comuna"),
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
               div(style = "overflow-x: scroll;",
                   plotOutput("grafico_piramide", width = "700px") |> withSpinner()
               )
        )
    ),
    
    
    ## firma ----
    fluidRow(
        column(12, style = "opacity: 0.5; font-size: 80%;",
               hr(),
               
               p("Diseñado y programado por",
                 tags$a("Bastián Olea Herrera.", target = "_blank", href = "https://bastian.olea.biz")),
               p("Puedes explorar mis otras",
                 tags$a("aplicaciones interactivas sobre datos sociales aquí.",
                        href = "https://bastianolea.github.io/shiny_apps/", target = "_blank")
               ),
               p("Código de fuente de esta app y del procesamiento de los datos",
                 tags$a("disponible en GitHub.", target = "_blank", href = "https://github.com/bastianolea/censo_proyecciones_poblacion")
               ),
               
               div(style = "height: 40px")
               
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
            geom_vline(xintercept = 2024, linewidth = 1, color = color_destacado, alpha = 0.4) +
            geom_line(aes(alpha = proyeccion_b), linewidth = 1.2, show.legend = F) +
            geom_point() +
            # escalas
            scale_alpha_manual(values = c("pasado" = 1, "futuro" = 0.3)) +
            scale_size_manual(values = c("proyección" = NULL, "censo" = 4), guide = "none") +
            scale_x_continuous(breaks = c(2002, seq(2005, 2035, by = 5), 2017)) +
            scale_y_continuous(labels = function(x) paste(x/1000, "mil")) +
            # temas
            theme_minimal() +
            theme(legend.position = "right",
                  axis.title.x = element_blank(),
                  axis.title.y = element_text(color = color_destacado),
                  axis.text = element_text(size = 8),
                  axis.text.x = element_text(size = 8, angle = -90, vjust = 0.5),
                  panel.grid.minor.x = element_blank(),
                  legend.text.align = 0,
                  legend.title = element_text(color = color_destacado)
            ) +
            # etiquetas
            labs(y = "Población residente proyectada",
                 alpha = "Temporalidad", 
                 color = "Comunas", fill = "Comunas") +
            guides(colour = guide_legend(override.aes = list(size = 4)),
                   alpha = guide_legend(override.aes = list(size = 4)))
        
    }, res = 100)
    
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
                               limits = ~c(-max(abs(.x)), max(abs(.x))),
                               expand = expansion(c(0.1, 0.1))
            ) +
            geom_text(aes(
                label = ifelse(género == "masculino",
                               paste(" ", format(abs(población), big.mark = ".", trim = T)),
                               paste(format(abs(población), big.mark = ".", trim = T), " ")
                ),
                hjust = ifelse(género == "masculino", 0, 1)
            ),
            size = 2.4
            ) +
            geom_col(width = 0.5) +
            geom_vline(xintercept = 0, linewidth = 1, color = "white") +
            theme_minimal() +
            scale_fill_manual(values = c("masculino" = color_masculino, "femenino" = color_femenino), 
                              aesthetics = c("fill", "color")) +
            theme(legend.position = "bottom",
                  axis.title = element_text(color = color_destacado),
                  axis.text = element_text(size = 8),
                  panel.grid.minor.x = element_blank(),
                  legend.text.align = 0,
                  legend.title = element_text(color = color_destacado),
                  legend.text = element_text(margin = margin(l = -3, r = 5))
            ) +
            labs(y = "Categorías de edad", x = "Población por grupo de edad y género",
                 fill = "Género", color = "Género")
    }, res = 100)
    
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
            gt_fa_column(flecha, palette = c("angles-up" = color_negativo, "arrow-up" = color_destacado, "arrow-down" = color_negativo)) |> 
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
            tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) |> 
            tab_options(table.font.color = color_principal)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

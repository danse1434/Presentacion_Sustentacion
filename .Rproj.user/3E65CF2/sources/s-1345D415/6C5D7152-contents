#' --- 
#' title: 'Preparación de gráficos interactivos' 
#' subtitle: 'Sustentación de tesis de maestría' 
#' date: '22-08-2021' 
#' author: 
#'        - name: Daniel S. Parra G. 
#'          email: dsparrag@minsalud.gov.co 
#'          institute: FNE 
#' institute: 
#'        - FNE: Misión PRI 1901 - Fondo Nacional de Estupefacientes 
#' abstract: abstract 
#' output:  
#'      - html_document: default 
#'      - pdf_document: default 
#' always_allow_html: true 
#' --- 
#' 

require(tidyverse)
require(plotly)

# Lectura de datos de residuales de modelo final de cefepime
dfPrediccion <- read_csv(file.path('data', 'raw', 'predictions.txt'))


dfPrediccion %>% 
  ggplot(aes(x = y_1, y = indivPred_mean)) +
  geom_point()



plPredicciones <- dfPrediccion %>% 
  rename('Poblacional'=popPred) %>% 
  rename('Individual'=indivPred_mean) %>% 
  rename('ID'=id) %>% 
  pivot_longer(cols = c(`Poblacional`,`Individual`)) %>% 
  # mutate(name = factor(name, levels = c('Poblacional', 'Individual'))) %>%
  plot_ly(x = ~y_1, y = ~value, frame = ~name, type = 'scatter',
          color = ~ID, 
          mode = 'markers', showlegend = F
  ) %>%
  add_trace(x = ~rep(seq(1, 100, length.out=90), 2), 
            y = ~rep(seq(1, 100, length.out=90), 2), 
            type = "scatter", mode='lines', 
            color = I('black'), linetype = I('dot'), 
            hoverinfo = 'skip') %>% 
  animation_slider(
    currentvalue = list(prefix = "Predicción: ", font = list(color="blue"), 
                        xanchor='left', offset=2),
    pad = list(t=60, r=100, l=100)
  ) %>% 
  animation_opts(frame=2e4, transition = 2e4) %>% 
  layout(xaxis = list(title = '<b>Observación</b>'), 
         yaxis = list(title = '<b>Predicción</b>')) %>% 
  config(displayModeBar = FALSE)
  
plPredicciones

htmlwidgets::saveWidget(plPredicciones,
                        file.path('figures', '001_prediccicionesComparacion.html'), 
                        libdir = 'plotly')
saveRDS(plPredicciones, 
        file.path('figures', '001_prediccicionesComparacion.rds'))

#'-------------------------------------------------------------------------------
dfResiduales1 <- read_csv(file.path('data', 'raw', 'VAN_FINAL_y1_residuals.txt'))
dfResiduales2 <- read_csv(file.path('data', 'raw', 'VAN_FINAL_y2_residuals.txt'))

tiposResiduales <- c('pwRes', 'iwRes_mean', 'npde')
steps <- vector(mode = 'list')

for (i in seq_along(tiposResiduales)) {
  step <- list(
    label = str_to_upper(tiposResiduales[i]),
    method = 'restyle',
    value = as.character(i),
    args = list("marker.color", "red")
  )
  
  # step['args'][1][i] = TRUE  
  steps <- append(steps, list(step))
}

# Conformación de df
dfResiduales2 <- dfResiduales1 %>% 
  bind_rows(dfResiduales2, .id = 'DF') %>% 
  select(DF, ID, matches('^pwRes|^npde|^iwRes'), time, prediction_pwRes) %>% 
  pivot_longer(cols= c(pwRes, iwRes_mean, npde))

# LOESS Residuales vs tiempo
loessResiduales1 <- dfResiduales2 %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(res1 = map(data, ~ loess(value ~ time, data = .x)),
         res2 = map(data, ~ loess(value ~ prediction_pwRes, data = .x)),
         pred = pmap(
           list(res1, res2, data),
           function(x, y, z) tibble(time = z$time, 
                                    pwPrediccion = z$prediction_pwRes,
                                    pred1 = predict(x),
                                    pred2 = predict(y)))) %>% 
  unnest(pred) %>% 
  select(-data, -res1) %>% 
  arrange(name, time)


plResiduales1 <- dfResiduales2 %>% 
  plot_ly(x = ~time, y = ~value, frame = ~name, type = 'scatter',
          color = ~ID, symbol = ~DF, symbols = c('circle', 'circle-open'), 
          mode = 'markers', showlegend = F
  ) %>% 
  add_trace(data = loessResiduales1, x=~time, y=~pred1, 
            frame=~name, 
            type = "scatter", mode='lines', linetype = I('dot'),
            color = I('blue'), symbol = NULL) %>% 
  layout(xaxis = list(title = '<b>Tiempo (h)</b>'), 
         yaxis = list(title = '<b>Residual</b>')) %>% 
  config(displayModeBar = FALSE)


plResiduales2 <- dfResiduales2 %>% 
  plot_ly(x = ~prediction_pwRes, y = ~value, frame = ~name, type = 'scatter',
          color = ~ID, symbol = ~DF, symbols = c('circle', 'circle-open'), 
          mode = 'markers', showlegend = F
  ) %>% 
  add_trace(data = arrange(loessResiduales1, pwPrediccion), 
            x=~pwPrediccion, y=~pred2, 
            frame=~name, 
            type = "scatter", mode='lines', linetype = I('dot'),
            color = I('blue'), symbol = NULL) %>% 
  layout(xaxis = list(title = '<b>PPRED (mg/L)</b>'), 
         yaxis = list(title = '<b>Residual</b>')) %>% 
  config(displayModeBar = FALSE)


plResiduales <- subplot(plResiduales1, plResiduales2, 
        shareX = FALSE, shareY = FALSE) %>% 
  animation_opts(frame=2e3, transition = 2e3) %>% 
  animation_slider(
    currentvalue = list(prefix = "Residual: ", font = list(color="blue"), 
                        xanchor='left', offset=2),
    steps = steps,
    pad = list(t=60, r=100, l=100)
  )


plResiduales

htmlwidgets::saveWidget(plResiduales,
                        file.path('figures', '010_residualesComparacion.html'), 
                        libdir = 'plotly')
saveRDS(plResiduales, 
        file.path('figures', '010_residualesComparacion.rds'))

# dfResiduales1 %>% 
#   bind_rows(dfResiduales2, .id = 'DF') %>% 
#   pivot_longer(cols= c(pwRes, iwRes_mean, npde)) %>% ggplot(aes(time, value, color=DF)) + 
#   geom_point() + 
#   facet_wrap(vars(name))


dfPrediccion %>% 
  ggplot(aes(x = y_1, y = indivPred_mean)) +
  geom_point()



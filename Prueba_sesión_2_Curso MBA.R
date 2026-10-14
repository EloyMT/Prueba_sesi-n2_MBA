
# =====================================================================
# cct_per_child
# Elegibilidad por pobreza + transferencia por hijo
# Modelo 2 del curso de simulación basada en agentes
# =====================================================================

# 0. Paquetes y semilla
library(ABM)
library(ggplot2)

set.seed(1)

# 1. Parámetros del modelo
N               <- 5000
Tmax            <- 36
delay_prob_base <- 0.30
lambda_cred     <- 0.30

# Reglas del programa
base_transfer_per_child <- 450
max_children_paid       <- 3
poverty_line            <- 3000

# Características de los hogares
ingreso_mediano  <- 4000
ingreso_desigual <- 0.5
prob_hijos       <- c(0.25, 0.40, 0.25, 0.10)

# Regla heredada del modelo simple
senal_publica <- TRUE

# Validaciones básicas
stopifnot(
  N > 0,
  Tmax > 0,
  sum(prob_hijos) > 0,
  all(prob_hijos >= 0),
  abs(sum(prob_hijos) - 1) < 1e-8,
  delay_prob_base >= 0,
  delay_prob_base <= 1,
  lambda_cred >= 0,
  lambda_cred <= 1,
  max_children_paid >= 0
)

# 2. Crear la población y sus estados iniciales

# Este objeto guardará los atributos de los hogares.
hogares <- data.frame(
  id = seq_len(N),
  n_hijos = integer(N),
  Y_hogar = numeric(N),
  elegible = logical(N),
  estado_final = rep("N", N)
)

# El vector registra la asistencia entre hogares con hijos
# después de cada decisión mensual.
asistencia_hijos_mes <- rep(NA_real_, Tmax + 1)

# Inicializador: se ejecuta una vez para cada agente.
inicializar_hogar <- function(i) {
  
  theta_i <- max(0, rnorm(1, mean = 300, sd = 80))
  cred_i  <- 1 - delay_prob_base
  
  hijos_i <- sample(
    0:3, size = 1, prob = prob_hijos
  )
  
  ingreso_i <- rlnorm(
    1,
    meanlog = log(ingreso_mediano),
    sdlog = ingreso_desigual
  )
  
  elegible_i <- ingreso_i < poverty_line
  
  hogares$n_hijos[i] <<- hijos_i
  hogares$Y_hogar[i] <<- ingreso_i
  hogares$elegible[i] <<- elegible_i
  
  # El primer elemento representa el estado del agente:
  # "A" = asiste; "N" = no asiste.
  list(
    "N",
    theta = theta_i,
    cred = cred_i,
    n_hijos = hijos_i,
    Y_hogar = ingreso_i,
    elegible = elegible_i
  )
}

sim <- Simulation$new(N, inicializar_hogar)

# 3. Contadores de asistencia
sim$addLogger(newCounter("A", "A"))
sim$addLogger(newCounter("N", "N"))

# 4. Regla mensual: leer -> decidir -> actualizar
tick_handler <- function(time, sim, agent) {
  
  # Variable auxiliar para registrar la decisión de cada hogar
  asistencia_actual <- logical(N)
  
  for (i in seq_len(N)) {
    
    # Leer el estado
    ai <- getAgent(sim, i)
    st <- getState(ai)
    
    # Calcular la transferencia según elegibilidad e hijos
    hijos_cubiertos <- min(st$n_hijos,max_children_paid)
    
    transfer_eff <- if (st$elegible) {base_transfer_per_child * hijos_cubiertos
    } else {      0    }
    
    # Beneficio percibido según la credibilidad
    benefit <- transfer_eff * st$cred
    
    # Decisión de asistir
    new_state <- if (benefit >= st$theta) "A" else "N"
    
    # Señal recibida y actualización de la credibilidad
    if (new_state == "A") {
      
      senal <- as.numeric(runif(1) > delay_prob_base)
      
    } else if (senal_publica) {
      
      senal <- 1 - delay_prob_base
      
    } else {
      
      senal <- 0
      
    }
    
    cred_new <- (
      (1 - lambda_cred) * st$cred +
        lambda_cred * senal
    )
    
    # Escribir el nuevo estado, preservando los atributos
    setState(
      ai,
      list(
        new_state,
        theta = st$theta,
        cred = cred_new,
        n_hijos = st$n_hijos,
        Y_hogar = st$Y_hogar,
        elegible = st$elegible
      )
    )
    
    # Registrar la decisión del hogar
    asistencia_actual[i] <- new_state == "A"
    hogares$estado_final[i] <<- new_state
  }
  
  # Guardar la proporción que asiste entre hogares con hijos
  con_hijos <- hogares$n_hijos > 0
  
  asistencia_hijos_mes[time + 1] <<- if (any(con_hijos)) {
    mean(asistencia_actual[con_hijos])
  } else {
    NA_real_
  }
  
  # Programar el siguiente mes, hasta completar Tmax
  if (time < Tmax) {
    schedule(
      agent,
      newEvent(time + 1, tick_handler)
    )
  }
}

# 5. Programar y ejecutar la simulación
schedule(
  sim$get,
  newEvent(0, tick_handler)
)

res <- as.data.frame(sim$run(0:Tmax))

# Proporción de hogares que asiste en cada tiempo observado
res$attend <- res$A / N

# Proporción que asiste entre hogares con hijos.
# La decisión tomada en el evento del tiempo t se observa
# en la fila del tiempo t + 1 del logger.
res$attend_con_hijos <- NA_real_

indices <- match(res$times, 1:Tmax)

validos <- !is.na(indices)

res$attend_con_hijos[validos] <-
  asistencia_hijos_mes[indices[validos]]

# 6. Resultados
escenario <- paste0(
  "monto", base_transfer_per_child,
  "_tope", max_children_paid,
  "_linea", poverty_line
)

cat(
  "\nHogares elegibles:",
  round(mean(hogares$elegible), 3),
  "\nHogares con al menos un hijo:",
  round(mean(hogares$n_hijos > 0), 3),
  "\n"
)

# 6a. Asistencia en los meses seleccionados
meses <- c(1, 12, 36)
filas <- match(meses, res$times)

resumen <- data.frame(
  escenario = escenario,
  mes = meses,
  asistencia_total = round(res$attend[filas], 3),
  asistencia_con_hijos = round(
    res$attend_con_hijos[filas], 3
  )
)

print(resumen)

write.csv(
  resumen,
  paste0("resumen_", escenario, ".csv"),
  row.names = FALSE
)

# 6b. Asistencia final por elegibilidad y número de hijos
por_grupo <- aggregate(
  asiste ~ elegible + n_hijos,
  data = transform(
    hogares,
    asiste = estado_final == "A"
  ),
  FUN = mean
)

por_grupo$asiste <- round(por_grupo$asiste, 3)

print(por_grupo)

write.csv(
  por_grupo,
  paste0("por_grupo_", escenario, ".csv"),
  row.names = FALSE
)

# 7. Gráfico de asistencia total
p <- ggplot(
  res,
  aes(x = times, y = attend)
) +
  geom_area(fill = "yellow2", alpha = 0.12) +
  geom_line(
    linewidth = 1.4,
    color = "orange",
    lineend = "round"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "CCT por hijo con elegibilidad por pobreza",
    subtitle = escenario,
    x = "Mes",
    y = "Proporción que asiste (todos los hogares)"
  ) +
  theme_minimal(base_size = 14)

print(p)

ggsave(
  paste0("asistencia_", escenario, ".png"),
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)

# 8. Gráfico de asistencia por grupo
p2 <- ggplot(
  por_grupo,
  aes(
    x = factor(n_hijos),
    y = asiste,
    fill = ifelse(elegible, "Elegible", "No elegible")
  )
) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      "Elegible" = "orange",
      "No elegible" = "grey70"
    )
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Asistencia en el mes 36 por grupo",
    subtitle = escenario,
    x = "Número de hijos",
    y = "Proporción que asiste",
    fill = NULL
  ) +
  theme_minimal(base_size = 14)

print(p2)

ggsave(
  paste0("por_grupo_", escenario, ".png"),
  plot = p2,
  width = 8,
  height = 5,
  dpi = 300
)

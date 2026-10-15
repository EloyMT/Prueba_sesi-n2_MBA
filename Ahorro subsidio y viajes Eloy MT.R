# =====================================================================
# saving_per_trip. Viaje por distancia + ahorro por viaje
# Modelo 3 del curso de simulación basada en agentes
# Punto de partida: el modelo compuesto, ya corregido con señal pública
# =====================================================================

# =========================
# 0) Paquetes y semilla
# =========================
library(ABM)
library(ggplot2)
set.seed(1)

# =========================
# 1) Parámetros del modelo
# =========================
N               <- 5000   # Número de usuarios
Tmax            <- 36     # Meses a simular
delay_prob_bus <- 0.30   # Probabilidad de que el autobús no pase, o no se detenga o pase lleno. 
lambda_cred     <- 0.30   # Velocidad de aprendizaje de la credibilidad

# --- Reglas del programa (lo que decide el diseño de la política) ---
ahorro_tarifa_pref_por_trip <- 300    # Monto mensual por viaje ahorrado
max_ahorr_trips       <- 2      # Tope de viajes con subsidio por viajero
dist_limit            <- 600   # Distancia en metros límite de la parada, para usar o no el autobús

# --- Población (cómo son los hogares) ---
dist_tipica  <- 500   # Distancia mediana en metros de la parada del autobús
dist_desigual <- 0.5    # Variabilidad de la distancia de la parada del autobús (más alto = más desigualdad)
prob_trips       <- c(0.25, 0.40, 0.25, 0.10)   # Probabilidad de tener 0, 1, 2 o 3 viajes en autobús

# --- Interruptor heredado del modelo simple ---
senal_publica <- TRUE     # TRUE = corrección del modelo simple (quien no viaja recibe la señal pública)
# FALSE = regla original (no asistir cuenta como retraso)

# =====================================
# 2) Crear simulación y estado inicial
# =====================================
# Atributos nuevos en esta sesión:
#   - n_trips:  número de hijxs en edad escolar (0 a 3)
#   - S_traveler:  Gasto mensual del viajero sin el subsidio
#   - elegible: TRUE si la distancia de la parada está por debajo del limite
sim <- Simulation$new(N)

for (i in 1:N) {
  theta    <- rnorm(1, mean = 300, sd = 80)                       # Umbral, igual que en la sesión 1
  cred0    <- 1 - delay_prob_bus                                 # Credibilidad inicial = 0.70
  n_trips  <- sample(0:3, size = 1, prob = prob_hijos)            # Número de viajes
  S_traveler  <- rlnorm(1, meanlog = log(ingreso_mediano), sdlog = ingreso_desigual)  # Ingreso
  elegible <- S_traveler < dist_limit                              # Regla de focalización
  sim$setState(i, list("N", theta = theta, cred = cred0,
                       n_trips = n_trips, S_traveler = S_traveler, elegible = elegible))
}

# Guardamos una copia de los atributos para analizar resultados por grupo al final
viajes <- data.frame(id = 1:N, n_trips = NA, elegible = NA, estado_final = NA)

# ======================================
# 3) Loggers (contadores automáticos)
# ======================================
sim$addLogger(newCounter("A", "A"))
sim$addLogger(newCounter("N", "N"))

# =========================================================
# 4) Regla mensual: leer -> decidir -> escribir
# =========================================================
tick_handler <- function(time, sim, agent) {
  for (i in 1:N) {
    # --- Leer ---
    ai <- getAgent(sim, i)
    st <- getState(ai)
    
    # --- Decidir ---
    # NUEVO: el subsidio depende de la elegibilidad y del número de viajes (con tope de 2)
    trips_cubiertos <- min(st$n_trips, max_ahorr_trips)
    saving_eff    <- if (st$elegible) ahorro_tarifa_pref_por_trip * trips_cubiertos else 0
    saving         <- saving_eff * st$cred                      # Igual que en la sesión 1
    new_state       <- if (saving >= st$theta) "A" else "N"
    
    # --- Señal (igual que en la sesión 1) ---
    if (new_state == "A") {
      senal <- as.numeric(runif(1) > delay_prob_bus)
    } else if (senal_publica) {
      senal <- 1 - delay_prob_bus
    } else {
      senal <- 0
    }
    cred_new <- (1 - lambda_cred) * st$cred + lambda_cred * senal
    
    # --- Escribir --- (todos los atributos, aunque no cambien)
    setState(ai, list(new_state, theta = st$theta, cred = cred_new,
                      n_hijos = st$n_hijos, Y_hogar = st$Y_hogar, elegible = st$elegible))
    
    # Registro para el análisis por grupo
    viajes$n_trips[i]      <<- st$n_trips
    viajes$elegible[i]     <<- st$elegible
    viajes$estado_final[i] <<- new_state
  }
  if (time < Tmax) schedule(agent, newEvent(time + 1, tick_handler))
}

# ==============================
# 5) Ejecutar la simulación
# ==============================
schedule(sim$get, newEvent(0, tick_handler))
res <- sim$run(0:Tmax)
res$attend <- res$A / N
# Nota: el mes 0 es el estado inicial, antes de la primera decisión.

# ==============================
# 6) Resultados
# ==============================
escenario <- paste0("monto", ahorro_tarifa_pref_por_trip, "_tope", max_ahorr_trips, "_linea", dist_limit)

# 6a) ¿Quién es elegible y quién hace viajes?
cat("\nViajes elegibles:", round(mean(viajes$elegible), 3),
    "\nViajes al menos uno:", round(mean(viajes$n_trips > 0), 3), "\n")

# 6b) Viajes totales (todos los usuarios) y entre usuarios con viajes
con_trips <- mean(viajes$n_trips > 0)
resumen <- data.frame(escenario = escenario, mes = c(1, 12, 36),
                      trips_totales     = round(res$attend[match(c(1, 12, 36), res$times)], 3),
                      usuario_con_trips = round(res$attend[match(c(1, 12, 36), res$times)] / con_trips, 3))
print(resumen)
write.csv(resumen, paste0("resumen_", escenario, ".csv"), row.names = FALSE)

# 6c) Viajes por grupo en el último mes (¿para quién funciona el programa?)
por_grupo <- aggregate(viaja ~ elegible + n_trips,
                       data = transform(viajes, viaja = estado_final == "A"),
                       FUN = mean)
por_grupo$viaja <- round(por_grupo$viaja, 2)
print(por_grupo)
write.csv(por_grupo, paste0("por_grupo_", escenario, ".csv"), row.names = FALSE)

# ==============================
# 7) Gráficos
# ==============================
p <- ggplot(res, aes(x = times, y = attend)) +
  geom_area(fill = "yellow2", alpha = 0.12) +
  geom_line(linewidth = 1.4, color = "orange", lineend = "round") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Ahorro por subsidios en viajes",
       subtitle = escenario, x = "Mes", y = "Proporción que viaja (todos los usuarios)") +
  theme_minimal(base_size = 14)
p
ggsave(paste0("viaja_", escenario, ".png"), plot = p, width = 8, height = 5, dpi = 300)

p2 <- ggplot(por_grupo, aes(x = factor(n_trips), y = viaja,
                            fill = ifelse(elegible, "Elegible", "No elegible"))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Elegible" = "orange", "No elegible" = "grey70")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Viajes en el mes 36 por grupo", subtitle = escenario,
       x = "Número de viajes", y = "Proporción que viaja", fill = NULL) +
  theme_minimal(base_size = 14)
p2
ggsave(paste0("por_grupo_", escenario, ".png"), plot = p2, width = 8, height = 5, dpi = 300)

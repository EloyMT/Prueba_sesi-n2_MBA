# =====================================================================
# Trans por enfermos. Elegibilidad por pobreza + transferencia por enfermo de diabetes
# Modelo 2 del curso de simulación basada en agentes
# Punto de partida: el modelo simple, ya corregido con señal pública
#Transferencias por enfermo de diabetes en hogares por debajo de la línea de pobreza
#condicionada a su asistencia al centro de salud para su control
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
N               <- 5000   # Número de hogares
Tmax            <- 36     # Meses a simular
delay_prob_base <- 0.30   # Probabilidad de que el pago llegue tarde
lambda_cred     <- 0.30   # Velocidad de aprendizaje de la credibilidad

# --- Reglas del programa (lo que decide el diseño de la política) ---
base_transfer_per_enfermos <- 300    # Monto mensual por enfermo cubierto
max_enfermos_paid       <- 2      # Tope de enfermos cubiertos por hogar
poverty_line            <- 4500   # Línea de pobreza: elegible si el ingreso del hogar es menor

# --- Población (cómo son los hogares) ---
ingreso_mediano  <- 4000   # Ingreso mensual mediano del hogar
ingreso_desigual <- 0.5    # Dispersión del ingreso (más alto = más desigualdad)
prob_enfermos       <- c(0.25, 0.40, 0.25, 0.10)   # Probabilidad de tener 0, 1, 2 o 3 enfermos en enfermos en casa

# --- Interruptor heredado del modelo simple ---
senal_publica <- TRUE     # TRUE = corrección del modelo simple (quien no asiste recibe la señal pública)
# FALSE = regla original (no asistir cuenta como retraso)

# =====================================
# 2) Crear simulación y estado inicial
# =====================================
# Atributos nuevos en esta sesión:
#   - n_enfermos:  Número de enfermos en enfermos en casa (0 a 3)
#   - Y_hogar:  ingreso mensual del hogar sin la transferencia
#   - elegible: TRUE si el ingreso está por debajo de la línea de pobreza
sim <- Simulation$new(N)

for (i in 1:N) {
  theta    <- rnorm(1, mean = 300, sd = 80)                       # Umbral, igual que en la sesión 1
  cred0    <- 1 - delay_prob_base                                 # Credibilidad inicial = 0.70
  n_enfermos  <- sample(0:3, size = 1, prob = prob_enfermos)            # Número de enfermos
  Y_hogar  <- rlnorm(1, meanlog = log(ingreso_mediano), sdlog = ingreso_desigual)  # Ingreso
  elegible <- Y_hogar < poverty_line                              # Regla de focalización
  sim$setState(i, list("N", theta = theta, cred = cred0,
                       n_enfermos = n_enfermos, Y_hogar = Y_hogar, elegible = elegible))
}

# Guardamos una copia de los atributos para analizar resultados por grupo al final
hogares <- data.frame(id = 1:N, n_enfermos = NA, elegible = NA, estado_final = NA)

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
    # NUEVO: la transferencia depende de la elegibilidad y del Número de enfermos (con tope)
    enfermos_cubiertos <- min(st$n_enfermos, max_enfermos_paid)
    transfer_eff    <- if (st$elegible) base_transfer_per_enfermos * enfermos_cubiertos else 0
    benefit         <- transfer_eff * st$cred                      # Igual que en la sesión 1
    new_state       <- if (benefit >= st$theta) "A" else "N"
    
    # --- Señal (igual que en la sesión 1) ---
    if (new_state == "A") {
      senal <- as.numeric(runif(1) > delay_prob_base)
    } else if (senal_publica) {
      senal <- 1 - delay_prob_base
    } else {
      senal <- 0
    }
    cred_new <- (1 - lambda_cred) * st$cred + lambda_cred * senal
    
    # --- Escribir --- (todos los atributos, aunque no cambien)
    setState(ai, list(new_state, theta = st$theta, cred = cred_new,
                      n_enfermos = st$n_enfermos, Y_hogar = st$Y_hogar, elegible = st$elegible))
    
    # Registro para el análisis por grupo
    hogares$n_enfermos[i]      <<- st$n_enfermos
    hogares$elegible[i]     <<- st$elegible
    hogares$estado_final[i] <<- new_state
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
escenario <- paste0("monto", base_transfer_per_enfermos, "_tope", max_enfermos_paid, "_linea", poverty_line)

# 6a) ¿Quién es elegible y quién tiene enfermos?
cat("\nHogares elegibles:", round(mean(hogares$elegible), 3),
    "\nHogares con al menos un enfermo:", round(mean(hogares$n_enfermos > 0), 3), "\n")

# 6b) Asistencia total (todos los hogares) y entre hogares con enfermos
con_enfermos <- mean(hogares$n_enfermos > 0)
resumen <- data.frame(escenario = escenario, mes = c(1, 12, 36),
                      asistencia_total     = round(res$attend[match(c(1, 12, 36), res$times)], 3),
                      asistencia_con_enfermos = round(res$attend[match(c(1, 12, 36), res$times)] / con_enfermos, 3))
print(resumen)
write.csv(resumen, paste0("resumen_", escenario, ".csv"), row.names = FALSE)

# 6c) Asistencia por grupo en el último mes (¿para quién funciona el programa?)
por_grupo <- aggregate(asiste ~ elegible + n_enfermos,
                       data = transform(hogares, asiste = estado_final == "A"),
                       FUN = mean)
por_grupo$asiste <- round(por_grupo$asiste, 2)
print(por_grupo)
write.csv(por_grupo, paste0("por_grupo_", escenario, ".csv"), row.names = FALSE)

# ==============================
# 7) Gráficos
# ==============================
p <- ggplot(res, aes(x = times, y = attend)) +
  geom_area(fill = "yellow2", alpha = 0.12) +
  geom_line(linewidth = 1.4, color = "orange", lineend = "round") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Transferencias por enfermos con elegibilidad por pobreza",
       subtitle = escenario, x = "Mes", y = "Proporción que asiste (todos los hogares)") +
  theme_minimal(base_size = 14)
p
ggsave(paste0("asistencia_", escenario, ".png"), plot = p, width = 8, height = 5, dpi = 300)

p2 <- ggplot(por_grupo, aes(x = factor(n_enfermos), y = asiste,
                            fill = ifelse(elegible, "Elegible", "No elegible"))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Elegible" = "orange", "No elegible" = "grey70")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(title = "Asistencia en el mes 36 por grupo", subtitle = escenario,
       x = "Número de enfermos", y = "Proporción que asiste", fill = NULL) +
  theme_minimal(base_size = 14)
p2
ggsave(paste0("por_grupo_", escenario, ".png"), plot = p2, width = 8, height = 5, dpi = 300)

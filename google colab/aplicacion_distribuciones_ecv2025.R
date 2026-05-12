# =============================================================================
# APLICACION ANALITICA: DISTRIBUCIONES CONTINUAS
# Encuesta de Calidad de Vida 2025 — Modulo Fuerza de Trabajo (DANE)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Paquetes y datos ---------------------------------------------------------
# -----------------------------------------------------------------------------

# fitdistrplus carga MASS, que enmascara dplyr::select; se carga primero
library(fitdistrplus)
library(tidyverse)
library(patchwork)   # combinar graficas
library(scales)      # formato de ejes

# Garantizar que se usa dplyr::select en todo el script
select <- dplyr::select

ecv_raw <- read.csv("/Users/sadan/Library/CloudStorage/OneDrive-Personal/econometra_lab/book/datos/fuerza_trabajo_ecv_2025.csv", sep = ";", header = TRUE)

# Funcion auxiliar para formatear pesos COP sin conflictos de locale
cop <- function(x) formatC(round(x), format = "f", digits = 0, big.mark = ".", drop0trailing = TRUE)

# Selección de variables -------------------------------------------------------

# Vectores de trabajo: una observacion valida = no-NA y mayor que cero

ingreso_lab <- ecv_raw$P8624[!is.na(ecv_raw$P8624) & ecv_raw$P8624 > 0]
honorarios  <- ecv_raw$P6750[!is.na(ecv_raw$P6750) & ecv_raw$P6750 > 0]
meses_trab  <- ecv_raw$P6426[!is.na(ecv_raw$P6426) & ecv_raw$P6426 > 0]
desplaz_min <- ecv_raw$P6886[!is.na(ecv_raw$P6886) & ecv_raw$P6886 > 0]


# -----------------------------------------------------------------------------
# 1. Estadística descriptivas -------------------------------------------------
# -----------------------------------------------------------------------------

descriptivos <- function(x, nombre) {
  data.frame(
    Variable  = nombre,
    N         = length(x),
    Media     = round(mean(x), 2),
    Mediana   = round(median(x), 2),
    SD        = round(sd(x), 2),
    Min       = min(x),
    Max       = max(x),
    Asimetria = round(mean(((x - mean(x)) / sd(x))^3), 3),
    Curtosis  = round(mean(((x - mean(x)) / sd(x))^4) - 3, 3)
  )
}

desc <- do.call(rbind, list(
  descriptivos(ingreso_lab, "Ingreso laboral (P8624)"),
  descriptivos(honorarios,  "Honorarios/negocio (P6750)"),
  descriptivos(meses_trab,  "Meses trabajando (P6426)"),
  descriptivos(desplaz_min, "Desplazamiento (P6886)")
))

print(desc, row.names = FALSE)



# =============================================================================
# EJERCICIO 1: DISTRIBUCION NORMAL
# Variable: log(P8624) — logaritmo del ingreso laboral
# =============================================================================
# El ingreso crudo tiene asimetria muy alta (~6.5). Su logaritmo natural
# es aproximadamente Normal: este es el modelo log-normal, ampliamente
# usado en economia del trabajo. En escala log, los datos se centran
# simetricamente y la Normal describe bien la dispersion.

# --- Paso 1: Identificacion ---
# Variable continua, positiva → transformar a log para simetria
log_ingreso <- log(ingreso_lab)

# --- Paso 2: Exploracion grafica ---
mu_hat    <- mean(log_ingreso)
sigma_hat <- sd(log_ingreso)

p1_hist <- ggplot(data.frame(x = log_ingreso), aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 50, fill = "#2c7bb6", alpha = 0.6, color = "white") +
  stat_function(
    fun  = dnorm,
    args = list(mean = mu_hat, sd = sigma_hat),
    color = "#d7191c", linewidth = 1.2
  ) +
  geom_density(color = "#1a9641", linewidth = 0.5, linetype = "dashed") +
  labs(
    title    = "Histograma",
    subtitle = "Rojo: Normal teorica | Verde: densidad empirica",
    x        = "ln(Ingreso laboral)",
    y        = "Densidad"
  ) +
  theme_minimal(base_size = 13)

print(p1_hist)

# --- Paso 3: Parametros ---

cat(sprintf("Parametros estimados (log-ingreso):\n"))
cat(sprintf("  mu (media log)    = %.4f\n", mu_hat))
cat(sprintf("  sigma (desv. log) = %.4f\n", sigma_hat))
cat(sprintf("  Mediana estimada  = $%s COP\n",
            cop(exp(mu_hat))))
cat(sprintf("  Media estimada    = $%s COP\n",
            cop(exp(mu_hat + sigma_hat^2 / 2))))
cat(sprintf("  Asimetria log-ingreso = %.4f\n",
            mean(((log_ingreso - mu_hat) / sigma_hat)^3)))

# --- Paso 4: Calculo de probabilidades ---

# Pregunta: ¿Que proporcion de trabajadores tiene un ingreso < $1.500.000?

umbral_cop <- 1500000
umbral_log <- log(umbral_cop)
prob_bajo  <- pnorm(umbral_log, mean = mu_hat, sd = sigma_hat) # distribución normal

prob_bajo
print(prob_bajo * 100) 


# Grafica de probabilidad acumulada
x_seq     <- seq(mu_hat - 4 * sigma_hat, mu_hat + 4 * sigma_hat, length.out = 500)
df_normal <- data.frame(x = x_seq, dens = dnorm(x_seq, mu_hat, sigma_hat))

p1_prob <- ggplot(df_normal, aes(x, dens)) +
  geom_area(data = subset(df_normal, x <= umbral_log),
            fill = "#2c7bb6", alpha = 0.5) +
  geom_line(linewidth = 1.2, color = "#d7191c") +
  geom_vline(xintercept = umbral_log, linetype = "dashed",
             color = "#333333", linewidth = 0.9) +
  annotate("text", x = umbral_log - 0.3, y = max(df_normal$dens) * 0.6,
           label = sprintf("P(X < ln(1.5M))\n= %.1f%%", prob_bajo * 100),
           hjust = 1, size = 4.5, color = "#2c7bb6") +
  scale_x_continuous(
    labels = function(x) paste0("ln(", format(round(exp(x) / 1e6, 1), nsmall = 1), "M)")
  ) +
  labs(
    title    = "Distribucion Normal — Probabilidad acumulada",
    subtitle = "Log-ingreso laboral mensual · ECV 2025",
    x        = "Ingreso (escala logaritmica)",
    y        = "Densidad"
  ) +
  theme_minimal(base_size = 13)

print(p1_prob)


# =============================================================================
# EJERCICIO 2: DISTRIBUCION UNIFORME
# Variable: simulacion de banda salarial $2M–$4M (no hay variable ECV uniforme)
# =============================================================================
# La Uniforme rara vez aparece en datos reales, pero es fundamental como
# base de simulacion y para modelar rangos con restriccion regulatoria.
# Ejemplo: el gobierno fija salarios entre $2M y $4M para un cargo publico;
# dentro del rango, todos los valores son igualmente probables.


# --- Paso 1: Identificacion ---
a_sal <- 2e6  # limite inferior de la banda
b_sal <- 4e6  # limite superior de la banda

# --- Paso 2: Exploracion grafica ---
set.seed(42)
sim_uniforme <- runif(50000, min = a_sal, max = b_sal)

p2_dens <- ggplot(data.frame(x = sim_uniforme), aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40, fill = "#fdae61", alpha = 0.7, color = "white") +
  stat_function(fun  = dunif,
                args = list(min = a_sal, max = b_sal),
                color = "#d7191c", linewidth = 1.3) +
  scale_x_continuous(
    labels = label_dollar(prefix = "$", suffix = "", big.mark = ".")
  ) +
  labs(
    title    = "Densidad Uniforme U(2M, 4M)",
    subtitle = "Simulacion: 50.000 salarios dentro de banda regulatoria",
    x        = "Salario (COP)",
    y        = "Densidad"
  ) +
  theme_minimal(base_size = 13)

print(p2_dens)

# --- Paso 3: Parametros ---
# Para X ~ U(a,b): mu = (a+b)/2; Var = (b-a)^2/12
mu_u  <- (a_sal + b_sal) / 2
var_u <- (b_sal - a_sal)^2 / 12

cat(sprintf("Parametros U(%.0f, %.0f):\n", a_sal, b_sal))
cat(sprintf("  Media       = $%s COP\n", cop(mu_u)))
cat(sprintf("  Varianza    = %.2e\n", var_u))
cat(sprintf("  Desv. Est.  = $%s COP\n", cop(sqrt(var_u))))

# --- Paso 4: Calculo de probabilidades ---
# Pregunta: que proporcion de empleados tiene salario > $3.200.000?

umbral_u <- 3.2e6
prob_sup  <- 1 - punif(umbral_u, min = a_sal, max = b_sal)

cat(sprintf("\nP(salario > $3.200.000) = %.1f%%\n", prob_sup * 100))
cat(sprintf("Formula: (b - x) / (b - a) = (%.0f - %.0f) / (%.0f - %.0f) = %.2f\n",
            b_sal, umbral_u, b_sal, a_sal,
            (b_sal - umbral_u) / (b_sal - a_sal)))

# =============================================================================
# EJERCICIO 3: DISTRIBUCION EXPONENCIAL
# Variable: P6886 — tiempo de desplazamiento al trabajo (minutos)
# =============================================================================
# Muchos trabajadores tienen desplazamientos cortos y pocos tienen commutes
# muy largos: esta asimetria de cola derecha con decaimiento rapido es la
# firma de la Exponencial. El parametro lambda = 1/media captura la tasa
# a la que "ocurre" cada minuto adicional de desplazamiento.


# --- Paso 2: Exploracion grafica ---
desp_plot  <- desplaz_min[desplaz_min <= quantile(desplaz_min, 0.99)]
lambda_hat <- 1 / mean(desplaz_min)   # estimador de metodo de momentos

p3_hist <- ggplot(data.frame(x = desp_plot), aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 50, fill = "#74add1", alpha = 0.65, color = "white") +
  stat_function(fun  = dexp,
                args = list(rate = lambda_hat),
                color = "#d7191c", linewidth = 1.2,
                xlim = c(0, quantile(desplaz_min, 0.99))) +
  geom_density(color = "#1a9641", linewidth = 0.5, linetype = "dashed") +
  labs(
    title    = "Tiempo de desplazamiento",
    subtitle = "Rojo: Exp(lambda) teorica | Verde: densidad empirica",
    x        = "Minutos de desplazamiento",
    y        = "Densidad"
  ) +
  theme_minimal(base_size = 13)

print(p3_hist)

# --- Paso 3: Parametros por MLE ---
fit_exp    <- fitdist(desplaz_min, distr = "exp")
lambda_mle <- fit_exp$estimate["rate"]

cat("Estimacion MLE — Exponencial:\n")
cat(sprintf("  lambda (tasa)      = %.5f min-1\n", lambda_mle))
cat(sprintf("  1/lambda (media)   = %.2f minutos\n", 1 / lambda_mle))
cat(sprintf("  Media empirica     = %.2f minutos\n", mean(desplaz_min)))
cat(sprintf("  Mediana teorica    = %.2f minutos\n", log(2) / lambda_mle))
cat(sprintf("  Mediana empirica   = %.2f minutos\n", median(desplaz_min)))

# --- Paso 4: Calculo de probabilidades ---
# Pregunta: que porcentaje de trabajadores tarda MAS de 45 minutos?
prob_45 <- 1 - pexp(45, rate = lambda_mle)
prob_30 <- 1 - pexp(30, rate = lambda_mle)
t_90    <- qexp(0.90,   rate = lambda_mle)

cat(sprintf("\nP(desplazamiento > 45 min) = %.1f%%\n", prob_45 * 100))
cat(sprintf("P(desplazamiento > 30 min) = %.1f%%\n", prob_30 * 100))
cat(sprintf("El 90%% de los trabajadores tarda <= %.1f minutos\n", t_90))

# Grafica del area de probabilidad
x_max_e <- qexp(0.995, rate = lambda_mle)
df_exp  <- data.frame(
  x    = seq(0, x_max_e, length.out = 500),
  dens = dexp(seq(0, x_max_e, length.out = 500), rate = lambda_mle)
)

p3_prob <- ggplot(df_exp, aes(x, dens)) +
  geom_area(data = subset(df_exp, x >= 45),
            fill = "#74add1", alpha = 0.6) +
  geom_line(linewidth = 1.2, color = "#d7191c") +
  geom_vline(xintercept = 45, linetype = "dashed",
             color = "#333333", linewidth = 0.9) +
  annotate("text", x = 60, y = dexp(0, rate = lambda_mle) * 0.4,
           label = sprintf("P(X > 45) = %.1f%%", prob_45 * 100),
           size = 4.5, color = "#2c7bb6", hjust = 0) +
  labs(
    title    = "Distribucion Exponencial — Probabilidad acumulada",
    subtitle = "Tiempo de desplazamiento · P6886 · ECV 2025",
    x        = "Minutos de desplazamiento",
    y        = "Densidad"
  ) +
  theme_minimal(base_size = 13)

print(p3_prob)


# =============================================================================
# EJERCICIO 4: DISTRIBUCION GAMMA
# Variable: P8624 — ingreso laboral mensual (en millones de COP)
# =============================================================================
# El ingreso crudo tiene asimetria ~6.5 y cola derecha muy larga: los modelos
# simetricos no aplican. La Gamma captura exactamente esta forma mediante
# dos parametros: alpha (forma) controla la asimetria y lambda (tasa) la
# escala. A diferencia de la Exponencial, la Gamma puede tomar formas muy
# variadas segun el valor de alpha.
# Trabajamos en millones de COP para estabilidad numerica del MLE.

# --- Paso 2: Exploracion grafica ---
ingreso_m   <- ingreso_lab / 1e6        # convertir a millones
p99_ingreso <- quantile(ingreso_m, 0.99)
ingreso_plt <- ingreso_m[ingreso_m <= p99_ingreso]

# Estimacion rapida por metodo de momentos para el Q-Q previo al MLE
alpha_mom <- mean(ingreso_m)^2 / var(ingreso_m)
lambda_mom <- mean(ingreso_m)   / var(ingreso_m)

p4_hist <- ggplot(data.frame(x = ingreso_plt), aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 60, fill = "#f46d43", alpha = 0.65, color = "white") +
  geom_density(color = "#1a9641", linewidth = 0.5, linetype = "dashed") +
  labs(
    title    = "Ingreso laboral mensual",
    subtitle = "Histograma + densidad empirica (verde)",
    x        = "Ingreso (millones de COP)",
    y        = "Densidad"
  ) +
  theme_minimal(base_size = 13)

print(p4_hist)

# --- Paso 3: Parametros por MLE ---
fit_gamma    <- fitdist(ingreso_m, distr = "gamma", method = "mle")
alpha_hat    <- fit_gamma$estimate["shape"]
lambda_hat_g <- fit_gamma$estimate["rate"]

cat("Parametros Gamma estimados (MLE) — ingreso en M COP:\n")
cat(sprintf("  alpha (forma)  = %.4f\n", alpha_hat))
cat(sprintf("  lambda (tasa)  = %.4f\n", lambda_hat_g))
cat(sprintf("  Media teorica  = $%s COP\n",
            cop(alpha_hat / lambda_hat_g * 1e6)))
cat(sprintf("  Desv. Est.     = %.4f M COP\n", sqrt(alpha_hat) / lambda_hat_g))
cat(sprintf("  Media empirica = %.4f M COP\n", mean(ingreso_m)))

# Grafica de ajuste Gamma vs datos
x_g  <- seq(0, p99_ingreso, length.out = 500)
df_g <- data.frame(
  x    = x_g,
  dens = dgamma(x_g, shape = alpha_hat, rate = lambda_hat_g)
)

p4_fit <- ggplot(data.frame(x = ingreso_plt), aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 60, fill = "#f46d43", alpha = 0.5, color = "white") +
  geom_line(data = df_g, aes(x, dens),
            color = "#d7191c", linewidth = 1.3) +
  geom_density(color = "#1a9641", linewidth = 0.9, linetype = "dashed") +
  scale_x_continuous(
    labels = function(x) paste0("$", format(x, nsmall = 1), "M")
  ) +
  labs(
    title    = sprintf("Ajuste Gamma — Ingreso laboral mensual"),
    subtitle = sprintf("Gamma(alpha=%.2f, lambda=%.2f) · Rojo: teorica | Verde: empirica",
                       alpha_hat, lambda_hat_g),
    x        = "Ingreso mensual (M COP)",
    y        = "Densidad"
  ) +
  theme_minimal(base_size = 13)

print(p4_fit)

# --- Paso 4: Calculo de probabilidades ---
# Pregunta: que proporcion de trabajadores gana entre $1M y $3M?
p_entre <- pgamma(3, shape = alpha_hat, rate = lambda_hat_g) -
           pgamma(1, shape = alpha_hat, rate = lambda_hat_g)
p_emp   <- mean(ingreso_m > 1 & ingreso_m < 3)

cat(sprintf("\nP($1M < ingreso < $3M) — Gamma teorica = %.1f%%\n", p_entre * 100))
cat(sprintf("Proporcion empirica                    = %.1f%%\n", p_emp * 100))


# =============================================================================
# EJERCICIO 5: DISTRIBUCION WEIBULL
# Variable: P6426 — meses trabajando en el empleo actual
# =============================================================================
# Los meses de permanencia en un empleo son una variable de duracion. La
# Weibull generaliza la Exponencial con un parametro de forma k:
#   k < 1: la tasa de salida DECRECE con el tiempo (quien lleva mas tiempo,
#          tiene menor probabilidad de irse pronto — "antigüedad protege")
#   k = 1: tasa constante (equivalente a Exponencial)
#   k > 1: la tasa CRECE con el tiempo
# El diagnostico clave: si log(-log(S(t))) es lineal en log(t), la Weibull
# ajusta bien.


# --- Paso 2: Exploracion grafica ---
p99_m     <- quantile(meses_trab, 0.99)
meses_plt <- meses_trab[meses_trab <= p99_m]

p5_hist <- ggplot(data.frame(x = meses_plt), aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 50, fill = "#9ecae1", alpha = 0.65, color = "white") +
  geom_density(color = "#1a9641", linewidth = 0.9, linetype = "dashed") +
  labs(
    title    = "Meses trabajando (empleo actual)",
    subtitle = "Histograma + densidad empirica",
    x        = "Meses",
    y        = "Densidad"
  ) +
  theme_minimal(base_size = 13)

# Diagnostico Weibull: log(-log(S(t))) vs log(t) debe ser lineal
ecdf_m  <- ecdf(meses_plt)
t_vals  <- sort(unique(meses_plt))
s_vals  <- pmax(1 - ecdf_m(t_vals), 1e-6)

df_wb_check <- data.frame(
  log_t         = log(t_vals),
  log_neg_log_s = log(-log(s_vals))
)

print(p5_hist)

# --- Paso 3: Parametros por MLE ---
fit_weibull  <- fitdist(meses_trab, distr = "weibull", method = "mle")
k_hat        <- fit_weibull$estimate["shape"]
lambda_hat_w <- fit_weibull$estimate["scale"]

cat("Parametros Weibull estimados (MLE):\n")
cat(sprintf("  k (forma)      = %.4f\n", k_hat))
cat(sprintf("  lambda (escala)= %.4f meses\n", lambda_hat_w))

if (k_hat < 1) {
  cat("\n  k < 1: tasa de salida DECRECE con el tiempo.\n")
  cat("  Quien lleva mas tiempo tiene menor riesgo de irse.\n")
} else if (k_hat > 1) {
  cat("\n  k > 1: tasa de salida CRECE con el tiempo.\n")
} else {
  cat("\n  k ~ 1: tasa constante (similar a Exponencial).\n")
}

media_w <- lambda_hat_w * gamma(1 + 1 / k_hat)
cat(sprintf("\n  Media teorica  = %.2f meses\n", media_w))
cat(sprintf("  Media empirica = %.2f meses\n", mean(meses_trab)))

# --- Paso 4: Funcion de supervivencia ---
# S(t) = P(X > t): que fraccion de empleos dura MAS de t meses
t_seq   <- seq(0, p99_m, length.out = 300)
df_surv <- data.frame(
  t    = t_seq,
  surv = 1 - pweibull(t_seq, shape = k_hat, scale = lambda_hat_w)
)

surv_emp <- data.frame(
  t    = sort(unique(meses_trab)),
  surv = 1 - ecdf(meses_trab)(sort(unique(meses_trab)))
)

p5_surv <- ggplot() +
  geom_step(data = surv_emp[surv_emp$t <= p99_m, ],
            aes(t, surv), color = "#9ecae1", linewidth = 0.7, alpha = 0.8) +
  geom_line(data = df_surv, aes(t, surv),
            color = "#d7191c", linewidth = 1.2) +
  scale_y_continuous(labels = label_percent()) +
  annotate("segment", x = 12, xend = 12, y = 0,
           yend = 1 - pweibull(12, k_hat, lambda_hat_w),
           linetype = "dashed", color = "#555555") +
  annotate("text", x = 15, y = 0.05,
           label = sprintf("S(12) = %.1f%%",
                           (1 - pweibull(12, k_hat, lambda_hat_w)) * 100),
           hjust = 0, size = 4, color = "#d7191c") +
  labs(
    title    = "Funcion de supervivencia — Permanencia en el empleo",
    subtitle = "Rojo: Weibull(k,lambda) | Azul: Kaplan-Meier empirico · P6426, ECV 2025",
    x        = "Meses trabajando",
    y        = "P(duracion > t)"
  ) +
  theme_minimal(base_size = 13)

print(p5_surv)

# Probabilidades de supervivencia en umbrales clave
cat("\nProbabilidad de permanencia segun umbral:\n")
for (u in c(6, 12, 24, 60)) {
  s <- 1 - pweibull(u, shape = k_hat, scale = lambda_hat_w)
  cat(sprintf("  P(permanencia > %2d meses) = %.1f%%\n", u, s * 100))
}


# =============================================================================
# EJERCICIO 6: DISTRIBUCION BETA
# Variable: P6886 / 480 — proporcion de la jornada dedicada al desplazamiento
# =============================================================================
# La Beta modela proporciones acotadas en (0, 1). Construimos:
#   pi = minutos de desplazamiento / 480 minutos (jornada 8h)
# Esta variable representa la "carga de commute": que fraccion de la jornada
# consume el desplazamiento. Valores cercanos a 0 = commutes cortos;
# valores cercanos a 0.5 = la mitad de la jornada en transporte.
# Los parametros alpha y beta controlan la forma: con alpha < beta la
# distribucion se sesga hacia valores bajos (la mayoria tiene poco commute).


# --- Paso 1: Construccion de la variable proporcion ---
df_beta  <- ecv_raw[!is.na(ecv_raw$P6886) & ecv_raw$P6886 > 0 & ecv_raw$P6886 < 480, ]
prop_vec <- df_beta$P6886 / 480   # proporcion en (0, 1)

cat(sprintf("Trabajadores con desplazamiento registrado: %d\n", length(prop_vec)))

# --- Paso 2: Exploracion grafica ---
fit_beta_pre <- fitdist(prop_vec, distr = "beta", method = "mle")
a_pre <- fit_beta_pre$estimate["shape1"]
b_pre <- fit_beta_pre$estimate["shape2"]

p6_hist <- ggplot(data.frame(x = prop_vec), aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 50, fill = "#abdda4", alpha = 0.65, color = "white") +
  geom_density(color = "#1a9641", linewidth = 0.9, linetype = "dashed") +
  scale_x_continuous(labels = label_percent()) +
  labs(
    title    = "Proporcion de jornada en desplazamiento",
    subtitle = "pi = P6886 / 480 minutos",
    x        = "Proporcion de la jornada laboral",
    y        = "Densidad"
  ) +
  theme_minimal(base_size = 13)

print(p6_hist)

# --- Paso 3: Parametros por MLE ---
fit_beta <- fitdist(prop_vec, distr = "beta", method = "mle")
alpha_b  <- fit_beta$estimate["shape1"]
beta_b   <- fit_beta$estimate["shape2"]

media_b <- alpha_b / (alpha_b + beta_b)
moda_b  <- if (alpha_b > 1 & beta_b > 1) (alpha_b - 1) / (alpha_b + beta_b - 2) else NA

cat("Parametros Beta estimados (MLE):\n")
cat(sprintf("  alpha (shape1) = %.4f\n", alpha_b))
cat(sprintf("  beta  (shape2) = %.4f\n", beta_b))
cat(sprintf("  Media teorica  = %.4f (%.1f%% de la jornada = %.0f min)\n",
            media_b, media_b * 100, media_b * 480))
if (!is.na(moda_b))
  cat(sprintf("  Moda           = %.4f (%.1f%% = %.0f min)\n",
              moda_b, moda_b * 100, moda_b * 480))
cat(sprintf("  Media empirica = %.4f (%.1f%% = %.0f min)\n",
            mean(prop_vec), mean(prop_vec) * 100, mean(prop_vec) * 480))

# Grafica de ajuste Beta
x_b  <- seq(0.001, 0.999, length.out = 500)
df_b <- data.frame(
  x    = x_b,
  dens = dbeta(x_b, shape1 = alpha_b, shape2 = beta_b)
)

p6_fit <- ggplot(data.frame(x = prop_vec), aes(x)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 50, fill = "#abdda4", alpha = 0.5, color = "white") +
  geom_line(data = df_b, aes(x, dens),
            color = "#d7191c", linewidth = 1.3) +
  geom_density(color = "#1a9641", linewidth = 0.9, linetype = "dashed") +
  scale_x_continuous(labels = label_percent()) +
  labs(
    title    = sprintf("Ajuste Beta — Proporcion de jornada en desplazamiento"),
    subtitle = sprintf("Beta(alpha=%.2f, beta=%.2f) · Rojo: teorica | Verde: empirica",
                       alpha_b, beta_b),
    x        = "Proporcion de la jornada laboral (P6886 / 480)",
    y        = "Densidad"
  ) +
  theme_minimal(base_size = 13)

print(p6_fit)

# --- Paso 4: Calculo de probabilidades ---
# Pregunta: que porcentaje dedica MAS del 10% de su jornada al desplazamiento?
# 10% de 480 min = 48 minutos
umbral_10pct <- 0.10
prob_sup_10  <- 1 - pbeta(umbral_10pct, shape1 = alpha_b, shape2 = beta_b)
prob_emp_10  <- mean(prop_vec > umbral_10pct)

cat(sprintf("\nP(pi > 10%% jornada) — Beta teorica  = %.1f%%\n", prob_sup_10 * 100))
cat(sprintf("Proporcion empirica                  = %.1f%%\n", prob_emp_10 * 100))
cat(sprintf("(equivale a > %.0f minutos de desplazamiento)\n", umbral_10pct * 480))

mediana_prop <- qbeta(0.5, shape1 = alpha_b, shape2 = beta_b)
q10_b <- qbeta(0.10, shape1 = alpha_b, shape2 = beta_b)
q90_b <- qbeta(0.90, shape1 = alpha_b, shape2 = beta_b)

cat(sprintf("Mediana teorica: %.1f%% de la jornada = %.0f minutos\n",
            mediana_prop * 100, mediana_prop * 480))
cat(sprintf("Rango del 80%% central: %.1f%% – %.1f%% (%.0f – %.0f minutos)\n",
            q10_b * 100, q90_b * 100, q10_b * 480, q90_b * 480))


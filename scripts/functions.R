# R Script: Custom Functions for Autism Communication Analysis

# ==========================
# 📦 PLOTTING FUNCTIONS
# ==========================

# Boxplot with jittered points
plot_boxplots <- function(data, group_var, response_var) {
  ggplot(data, aes(x = {{ group_var }}, y = {{ response_var }})) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.1) +
    theme_minimal() +
    labs(
      title = "Boxplots of Response Variable by Group",
      x = as.character(substitute(group_var)),
      y = as.character(substitute(response_var))
    )
}

# Scatterplot with fitted lines (original, log, sqrt)
plot_scatter_with_fitted_line <- function(data, predict_var, response_var,
                                          log_transformation = FALSE,
                                          sqrt_transformation = FALSE) {
  p_original <- ggplot(data, aes(x = {{ predict_var }}, y = {{ response_var }})) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
    theme_minimal() +
    ylim(0, NA) +
    labs(
      title = "Scatterplot with Smoothing",
      x = as_label(enquo(predict_var)),
      y = as_label(enquo(response_var))
    )
  print(p_original)
  
  if (log_transformation) {
    p_log <- ggplot(data, aes(x = {{ predict_var }}, y = log({{ response_var }}))) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
      theme_minimal() +
      labs(
        title = "Log-transformed Scatterplot with Smoothing",
        x = as_label(enquo(predict_var)),
        y = paste0("log(", as_label(enquo(response_var)), ")")
      )
    print(p_log)
  }
  
  if (sqrt_transformation) {
    p_sqrt <- ggplot(data, aes(x = {{ predict_var }}, y = sqrt({{ response_var }}))) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
      theme_minimal() +
      ylim(0, NA) +
      labs(
        title = "Square Root-transformed Scatterplot with Smoothing",
        x = as_label(enquo(predict_var)),
        y = paste0("sqrt(", as_label(enquo(response_var)), ")")
      )
    print(p_sqrt)
  }
}


# ==========================
# 🧪 DIAGNOSTICS & STATISTICS
# ==========================

# Diagnostic plots for models
run_diagnostic_plots <- function(data, group_var, response_var,
                                 log_transformation = FALSE,
                                 sqrt_transformation = FALSE) {
  par(mfrow = c(1, 2))
  plot(lm(data[[response_var]] ~ data[[group_var]], data = data), which = c(1, 2))
  
  if (log_transformation) {
    par(mfrow = c(1, 2))
    plot(lm(log(data[[response_var]]) ~ data[[group_var]], data = data), which = c(1, 2))
  }
  
  if (sqrt_transformation) {
    par(mfrow = c(1, 2))
    plot(lm(sqrt(data[[response_var]]) ~ data[[group_var]], data = data), which = c(1, 2))
  }
}

# Run one-way ANOVA with optional transformation
run_one_way_anova <- function(data, group_var, response_var, method = "original") {
  if (!method %in% c("original", "log", "sqrt")) {
    stop("Invalid method. Choose one of: 'original', 'log', or 'sqrt'.")
  }
  
  if (method == "original") {
    model <- lm(data[[response_var]] ~ data[[group_var]], data = data)
  } else if (method == "log") {
    model <- lm(log(data[[response_var]]) ~ data[[group_var]], data = data)
  } else {
    model <- lm(sqrt(data[[response_var]]) ~ data[[group_var]], data = data)
  }
  return(anova(model))
}

# Run Tukey HSD with emmeans
run_tukey_hsd <- function(data, group_var, response_var, method = "original") {
  if (!method %in% c("original", "log", "sqrt")) {
    stop("Invalid method. Choose from: 'original', 'log', or 'sqrt'.")
  }
  
  formula <- as.formula(paste(response_var, "~", group_var))
  if (method == "log") {
    formula <- as.formula(paste("log(", response_var, ") ~", group_var))
  } else if (method == "sqrt") {
    formula <- as.formula(paste("sqrt(", response_var, ") ~", group_var))
  }
  
  model <- lm(formula, data = data)
  model_emm <- emmeans(model, specs = group_var, type = "response")
  print(model_emm)
  print(pairs(model_emm))
  print(confint(pairs(model_emm)))
}

# Simple linear regression with optional transformation
run_simple_linear_regression <- function(data, predict_var, response_var, method = "original") {
  if (!method %in% c("original", "log", "sqrt")) {
    stop("Invalid method. Choose one of: 'original', 'log', or 'sqrt'.")
  }
  
  if (method == "original") {
    model <- lm(data[[response_var]] ~ data[[predict_var]], data = data)
  } else if (method == "log") {
    model <- lm(log(data[[response_var]]) ~ data[[predict_var]], data = data)
  } else {
    model <- lm(sqrt(data[[response_var]]) ~ data[[predict_var]], data = data)
  }
  
  return(summary(model))
}

# ==========================
# 🔎 CHECKING DISTRIBUTIONS
# ==========================

# Compare actual vs simulated distributions in Mixed group
check_distribution_in_mixed_pairs <- function(data, response_var) {
  data_filtered <- data |> filter(research_group == "Mixed")
  
  actual_plot <- ggplot(data_filtered, aes(x = {{ response_var }})) +
    geom_histogram(bins = 10, fill = "steelblue", colour = "black", alpha = 0.7) +
    facet_wrap(~asc_status, nrow = 2) +
    theme_light() +
    labs(
      title = "Frequency Distribution by ASC status in Mixed Pairs",
      x = as.character(substitute(response_var)),
      y = "Count"
    )
  
  data_summary <- data_filtered |>
    group_by(asc_status) |>
    summarise(
      mean = mean({{ response_var }}, na.rm = TRUE),
      sd = sd({{ response_var }}, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  set.seed(123)
  normal_sim <- data_summary |>
    rowwise() |>
    mutate(simulated = list(rnorm(n, mean, sd))) |>
    unnest_longer(simulated)
  
  sim_plot <- ggplot(normal_sim, aes(x = simulated)) +
    geom_histogram(bins = 10, fill = "darkgreen", colour = "black", alpha = 0.7) +
    facet_wrap(~asc_status, nrow = 2) +
    theme_light() +
    labs(
      title = "Simulated Normal Distribution by ASC status",
      x = as.character(substitute(response_var)),
      y = "Count"
    )
  
  plot(actual_plot)
  plot(sim_plot)
}
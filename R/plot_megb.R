

#' Plot Random Effects, Residuals, Train vs. Validation Loss, and Feature Importance
#'
#' @param megb_object object of class MEGB which contains the boosting model
#' @param ... additional arguments
#' @import ggplot2
#' @import stats
#' @importFrom gridExtra grid.arrange
#' @importFrom xgboost xgb.plot.importance
#' @return series of four plots, i.e. a QQ-Plot of the Random Effects, a QQPlot of the Residuals, plot containing the loss for train and validation data, a feature importance plot
#' @export
#' @method plot MEGB
#' @name plot.MEGB

groesse <- 18
plot_theme <-       theme_bw(base_size = 20) +
  theme(
    text = element_text(size = groesse),
    plot.title = element_text(size = groesse),
    axis.title = element_text(size = groesse),
    legend.text = element_text(size = groesse),
    legend.position = "none", # Legende in Einzelplots aus
    axis.text.x = element_text(hjust = 1, size = 12),
    panel.grid.minor = element_blank() # Optional: Cleaner Look
  )


plot.MEGB <- function(megb_object) {
  groesse <- 18
  plot_theme <-       theme_bw(base_size = 20) +
    theme(
      text = element_text(size = groesse),
      plot.title = element_text(size = groesse),
      axis.title = element_text(size = groesse),
      legend.text = element_text(size = groesse),
      legend.position = "none", # Legende in Einzelplots aus
      axis.text.x = element_text(hjust = 1, size = 12),
      panel.grid.minor = element_blank() # Optional: Cleaner Look
    )
  
  # QQ Plot for Random Effects
  qqplot_random_effects <- function(megb_object, plot_theme) {
    # Daten extrahieren und in Dataframe umwandeln
    data <- data.frame(
      values = megb_object$megb_model$dom_name_effects[[1]][, 1]
    )
    
    # Plot erstellen
    p <-ggplot(data, aes(sample = values)) +
      geom_qq() +
      geom_qq_line(color = "red") + # Rote Linie für bessere Sichtbarkeit
      labs(
        title = "QQPlot for Random Effects",
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      plot_theme
    print(p)
  }
  
  # QQ Plot for Residuals
  qqplot_residuals <- function(megb_object, plot_theme) {
    # Daten extrahieren und in Dataframe umwandeln
    data <- data.frame(
      residuals = megb_object$megb_model$residuals
    )
    
    # Plot erstellen
    p <-ggplot(data, aes(sample = residuals)) +
      geom_qq() +
      geom_qq_line(color = "red") +
      labs(
        title = "QQPlot for Residuals",
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      plot_theme 
    print(p)
  }
  
  # Train vs. Validation Loss Plot
  train_validation_loss_plot <- function(megb_object, plot_theme) {
    plot <- megb_object$megb_model$eval_log %>%
      ggplot(aes(x = iter)) +
      geom_line(aes(y = train_rmse_mean, color = "Train"), lwd = 1) +
      geom_line(aes(y = test_rmse_mean, color = "Validation"), lwd = 1) +
      labs(
        title = "Train vs. Validation Loss
 XGBoost (inside EM-Algorithm)",
        x = "Iteration",
        y = "RMSE",
        color = "Dataset"
      ) +
      plot_theme + # Einheitliches Styling anwenden
      geom_vline(xintercept = megb_object$megb_model$best_iter,
                 col = "black") +
      annotate(
        "text",
        x = megb_object$megb_model$best_iter,
        y = max(megb_object$megb_model$eval_log$train_rmse_mean),
        label = "Used nrounds",
        vjust = 1.5,
        hjust = 1,
        color = "black",
        size = 5
      )
    
    
    # print plot
    print(plot)
  }
  
  # Feature Importance Plot
  importance_plot <- function(megb_object) {
    # Plot feature importance
    importance <- megb_object$megb_model$importance_matrix
    p <- xgb.ggplot.importance(importance,
                               main = "Feature Importance",
                               xlab = "Importance",
                               ylab = "Features") + plot_theme +
      scale_fill_viridis_d(option = "D", end = 0.8)
    print(p)
  }
  
  # List of plot functions
  plot_functions <- list(
    function(x) qqplot_random_effects(x, plot_theme),
    function(x) qqplot_residuals(x, plot_theme),
    function(x) train_validation_loss_plot(x, plot_theme),
    function(x) importance_plot(x)
  )
  if (megb_object$gbm_engine != "xgboost") {
    warning(
      "Boosting-related plots are only available for xgboost. ",
      "For catboost or lightgbm, please access boosting information via ",
      "`megb_object$megb_model$boosting`."
    )
    plot_functions <- plot_functions[1:2]
  }
  # plot each function:
  for (plot_fun in plot_functions) {
    # Plot aufrufen
    plot_fun(megb_object)
    
    # wait for Enter
    readline(prompt = "Press ENTER to continue...")
  }
}


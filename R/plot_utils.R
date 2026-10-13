#' Plot MTD Selection Probabilities 
#'
#' @param sim_results Output list from run_unified_simulations.
#' @return A ggplot2 object displaying the probabilities.
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual geom_vline labs theme_minimal theme element_text
#' @importFrom reshape2 melt
#' @export
plot_mtd_selection <- function(sim_results) {
  df <- reshape2::melt(sim_results$selections, id.vars="Dose", variable.name="Method", value.name="Percentage")
  ggplot2::ggplot(df, ggplot2::aes(x = Dose, y = Percentage, fill = Method)) +
    ggplot2::geom_bar(stat="identity", position="dodge", alpha=0.9) +
    ggplot2::scale_fill_manual(values = c("3+3" = "#6c757d", "CRM" = "#0d6efd", "BOIN" = "#198754")) +
    ggplot2::geom_vline(xintercept = as.character(sim_results$true_mtd), linetype="dashed", color="red", linewidth=1.2) +
    ggplot2::labs(title = "MTD Selection Probabilities", subtitle = "Dashed red line indicates the True MTD", y = "Selection Probability (%)", x = "Dose Level") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face="bold", size=14))
}

#' Plot Safety Profile
#'
#' @param sim_results Output list from run_unified_simulations.
#' @return A ggplot2 object displaying overdose vs underdose constraints.
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual labs theme_minimal theme element_text
#' @importFrom reshape2 melt
#' @export
plot_safety_profile <- function(sim_results) {
  df_sub <- sim_results$metrics[, c("Method", "Overdose_Rate_Pct", "Underdose_Rate_Pct")]
  df <- reshape2::melt(df_sub, id.vars="Method", variable.name="Metric", value.name="Rate")
  df$Metric <- gsub("_Rate_Pct", "", df$Metric)
  ggplot2::ggplot(df, ggplot2::aes(x = Method, y = Rate, fill = Metric)) +
    ggplot2::geom_bar(stat="identity", position="dodge", alpha=0.9) +
    ggplot2::scale_fill_manual(values = c("Overdose" = "#dc3545", "Underdose" = "#ffc107")) +
    ggplot2::labs(title = "Safety Profile: Underdose vs Overdose Rates", y = "Proportion of Patients (%)", x = "Method") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face="bold", size=14))
}

#' Plot Patient Allocation
#'
#' @param sim_results Output list from run_unified_simulations.
#' @return A ggplot2 object distribution plot of patient allocations.
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual labs theme_minimal theme element_text
#' @importFrom reshape2 melt
#' @export
plot_patient_allocation <- function(sim_results) {
  df <- reshape2::melt(sim_results$allocations, id.vars="Dose", variable.name="Method", value.name="Patients")
  ggplot2::ggplot(df, ggplot2::aes(x = Dose, y = Patients, fill = Method)) +
    ggplot2::geom_bar(stat="identity", position="dodge", alpha=0.9) +
    ggplot2::scale_fill_manual(values = c("3+3" = "#6c757d", "CRM" = "#0d6efd", "BOIN" = "#198754")) +
    ggplot2::labs(title = "Patient Allocation Distribution", subtitle = "Average number of patients treated at each dose level", y = "Average Number of Patients", x = "Dose Level") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face="bold", size=14))
}

#' Plot Dose-Toxicity Curve
#'
#' @param true_prob Numeric vector of true toxicity probabilities.
#' @param target_tox Target toxicity threshold percentage mapping.
#' @return A ggplot2 object representing the toxicity distribution.
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline scale_y_continuous labs theme_minimal theme element_text
#' @export
plot_dose_tox_curve <- function(true_prob, target_tox) {
  df <- data.frame(Dose = 1:length(true_prob), Toxicity = true_prob)
  ggplot2::ggplot(df, ggplot2::aes(x = as.factor(Dose), y = Toxicity, group=1)) +
    ggplot2::geom_line(color="#0dcaf0", linewidth=1.5) +
    ggplot2::geom_point(color="#000000", size=3) +
    ggplot2::geom_hline(yintercept = target_tox, linetype="dashed", color="red", linewidth=1.2) +
    ggplot2::labs(title = "True Dose-Toxicity Curve", subtitle = "Dashed red line represents target toxicity rate", x = "Dose Level", y = "True Probability of DLT") +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face="bold", size=14))
}

#' Simulate a single 3+3 Phase I Trial
#'
#' @param true_prob Numeric vector of true toxicity probabilities.
#' @param max_n Maximum sample size permitted.
#' @return A list containing the MTD, treated patient counts, and toxicity counts.
#' @keywords internal
simulate_3plus3_single <- function(true_prob, max_n) {
  ndose <- length(true_prob)
  n_treated <- rep(0, ndose)
  n_tox <- rep(0, ndose)
  dose <- 1
  mtd <- 0
  
  while (sum(n_treated) < max_n) {
    n_treated[dose] <- n_treated[dose] + 3
    tox <- sum(stats::rbinom(3, 1, true_prob[dose]))
    n_tox[dose] <- n_tox[dose] + tox
    
    if (n_treated[dose] == 3) {
      if (tox == 0) {
        if (dose == ndose) { mtd <- dose; break } else { dose <- dose + 1 }
      } else if (tox >= 2) {
        mtd <- ifelse(dose == 1, 0, dose - 1); break
      }
    } else if (n_treated[dose] == 6) {
      if (n_tox[dose] <= 1) {
        if (dose == ndose) { mtd <- dose; break } else { dose <- dose + 1 }
      } else {
        mtd <- ifelse(dose == 1, 0, dose - 1); break
      }
    } else { break }
  }
  return(list(mtd = mtd, n_treated = n_treated, n_tox = n_tox))
}

#' Run 3+3 Monte Carlo Simulations
#'
#' @param true_prob Numeric vector of true toxicity probabilities.
#' @param max_n Maximum sample size permitted.
#' @param n_sim Number of simulated trials.
#' @return A summary matrix of selection and patient allocation.
#' @keywords internal
run_3plus3_mc <- function(true_prob, max_n, n_sim) {
  ndose <- length(true_prob)
  res <- replicate(n_sim, simulate_3plus3_single(true_prob, max_n), simplify = FALSE)
  
  mtds <- sapply(res, function(x) x$mtd)
  treated_mat <- do.call(rbind, lapply(res, function(x) x$n_treated))
  
  tab <- table(factor(mtds, levels = 0:ndose))
  selpercent <- as.numeric(tab) / n_sim * 100
  
  list(
    selpercent = selpercent,
    npatients = colMeans(treated_mat),
    totaln = mean(rowSums(treated_mat))
  )
}

#' Run Unified Multi-Model Simulations
#'
#' Executes robust Monte Carlo simulations for 3+3, CRM, and BOIN designs under identical parameters.
#'
#' @param true_prob A numeric vector of true toxicity probabilities per dose level.
#' @param target_tox Target toxicity rate (numeric between 0 and 1).
#' @param num_sims Number of simulation loops to run (e.g. 1000).
#' @param max_n Maximum total trial sample size.
#' @param cohort_size Number of patients per cohort (usually 3).
#'
#' @importFrom BOIN get.oc
#' @importFrom dfcrm crmsim
#' @importFrom utils capture.output
#' @export
#'
#' @return A formatted list returning comparative metric matrices and plot-ready dataframes.
#'
#' @examples
#' \dontrun{
#' res <- run_unified_simulations(c(0.05, 0.1, 0.25, 0.4), 0.25, 100, 36, 3)
#' }
run_unified_simulations <- function(true_prob, target_tox, num_sims, max_n, cohort_size) {
  ndose <- length(true_prob)
  
  utils::capture.output(
    boin_res <- BOIN::get.oc(target = target_tox, p.true = true_prob, 
                             ncohort = floor(max_n / cohort_size), cohortsize = cohort_size, 
                             ntrial = num_sims, startdose = 1)
  ) -> invisible_out
  
  skeleton <- sort(seq(0.05, 0.6, length.out = ndose))
  utils::capture.output(
    crm_res <- dfcrm::crmsim(PI = true_prob, prior = skeleton, target = target_tox,
                             n = max_n, x0 = 1, nsim = num_sims, mcohort = cohort_size)
  ) -> invisible_out
  
  res_3plus3 <- run_3plus3_mc(true_prob, max_n, num_sims)
  
  true_mtd <- which.min(abs(true_prob - target_tox))
  
  crm_none <- (1 - sum(crm_res$MTD)) * 100
  crm_sel <- c(crm_none, crm_res$MTD * 100)
  crm_npats <- crm_res$level
  crm_totn <- sum(crm_res$level)
  
  boin_sel_re <- c(boin_res$selpercent[ndose+1], boin_res$selpercent[1:ndose])
  
  calculate_stats <- function(sel_vec, npats_vec, totn, method_name) {
    overdose_pats <- ifelse(true_mtd < ndose, sum(npats_vec[(true_mtd + 1):ndose], na.rm=TRUE), 0)
    overdose_rate <- (overdose_pats / totn) * 100
    underdose_pats <- ifelse(true_mtd > 1, sum(npats_vec[1:(true_mtd - 1)], na.rm=TRUE), 0)
    underdose_rate <- (underdose_pats / totn) * 100
    data.frame(Method = method_name, Correct_MTD_Sel_Pct = sel_vec[true_mtd + 1], Overdose_Rate_Pct = overdose_rate, Underdose_Rate_Pct = underdose_rate, Avg_Sample_Size = totn, stringsAsFactors = FALSE)
  }

  metrics <- rbind(
    calculate_stats(res_3plus3$selpercent, res_3plus3$npatients, res_3plus3$totaln, "3+3"),
    calculate_stats(crm_sel, crm_npats, crm_totn, "CRM"),
    calculate_stats(boin_sel_re, boin_res$npatients, boin_res$totaln, "BOIN")
  )
  
  allocations <- data.frame(Dose = as.factor(1:ndose), `3+3` = res_3plus3$npatients, CRM = crm_npats, BOIN = boin_res$npatients, check.names = FALSE)
  selections <- data.frame(Dose = factor(c("None", 1:ndose), levels=c("None", 1:ndose)), `3+3` = res_3plus3$selpercent, CRM = crm_sel, BOIN = boin_sel_re, check.names = FALSE)
  
  list(metrics = metrics, allocations = allocations, selections = selections, true_mtd = true_mtd)
}

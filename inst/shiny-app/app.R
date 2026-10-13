library(shiny)
library(bslib)
library(ggplot2)
library(gt)
library(Phase1DesignDecision)

ui <- page_sidebar(
  title = "Phase I Trial Dose-Finding Simulation",
  theme = bs_theme(version = 5, bootswatch = "pulse", primary = "#4B0082"),
  
  sidebar = sidebar(
    width = 350,
    h4("Trial Parameters"),
    sliderInput("target_tox", "Target Toxicity Rate:", min=0.1, max=0.4, value=0.25, step=0.01),
    numericInput("num_doses", "Number of Dose Levels:", value=6, min=3, max=10),
    
    # Pre-defined Scenarios block
    hr(),
    h5("Predefined Scenarios"),
    actionButton("btn_scen_a", "Scenario A: Increasing", class="btn-info my-1 w-100"),
    actionButton("btn_scen_b", "Scenario B: Steep", class="btn-warning my-1 w-100"),
    actionButton("btn_scen_c", "Scenario C: Flat", class="btn-secondary my-1 w-100"),
    
    hr(),
    h5("True Toxicity Probabilities:"),
    uiOutput("tox_inputs_ui"),
    
    hr(),
    numericInput("cohort_size", "Cohort Size:", value=3, min=1, max=10),
    numericInput("max_n", "Maximum Sample Size:", value=36, min=10, max=200),
    numericInput("n_sim", "Number of Simulations:", value=500, min=10, max=5000),
    
    hr(),
    actionButton("run_sim", "Run Simulations", class="btn-primary btn-lg w-100", icon=icon("play"))
  ),
  
  # Main Body
  navset_card_pill(
    nav_panel("Summary Statistics", 
              h3("Simulation Performance Metrics", class="mt-3"),
              p("Comparison of the 3+3, CRM, and BOIN designs based on the selected trial parameters."),
              gt_output("metrics_table")),
              
    nav_panel("Visualizations",
              layout_columns(
                col_widths = c(6, 6, 6, 6),
                card(plotOutput("plot_mtd")),
                card(plotOutput("plot_safety")),
                card(plotOutput("plot_alloc")),
                card(plotOutput("plot_curve"))
              )
    ),
    
    nav_panel("Simulation Study Plan",
              h3("About the Scenarios"),
              p("The sidebar buttons allow you to quickly load the 3 predefined scenarios for this simulation study:"),
              tags$ul(
                tags$li(strong("Scenario A (Increasing):"), " A steady linear increase. MTD is centrally located."),
                tags$li(strong("Scenario B (Steep):"), " Toxicity sharply spikes right after the MTD. Tests the models' safety reactions."),
                tags$li(strong("Scenario C (Flat):"), " Toxicity slowly creeps up over many doses. Tests ability to escalate efficiently.")
              )
    )
  )
)

server <- function(input, output, session) {
  
  output$tox_inputs_ui <- renderUI({
    req(input$num_doses)
    lapply(1:input$num_doses, function(i) {
      numericInput(paste0("prob_", i), paste("Dose", i, "Prob:"), 
                   value = min(0.05 * i, 0.99), min = 0, max = 1, step = 0.01)
    })
  })
  
  observeEvent(input$btn_scen_a, {
    updateNumericInput(session, "num_doses", value = 6)
    updateNumericInput(session, "target_tox", value = 0.25)
    probs <- c(0.05, 0.10, 0.15, 0.25, 0.40, 0.55)
    for(i in 1:6) updateNumericInput(session, paste0("prob_", i), value = probs[i])
  })
  
  observeEvent(input$btn_scen_b, {
    updateNumericInput(session, "num_doses", value = 6)
    updateNumericInput(session, "target_tox", value = 0.25)
    probs <- c(0.02, 0.05, 0.10, 0.25, 0.50, 0.80)
    for(i in 1:6) updateNumericInput(session, paste0("prob_", i), value = probs[i])
  })
  
  observeEvent(input$btn_scen_c, {
    updateNumericInput(session, "num_doses", value = 6)
    updateNumericInput(session, "target_tox", value = 0.25)
    probs <- c(0.05, 0.10, 0.15, 0.18, 0.25, 0.30)
    for(i in 1:6) updateNumericInput(session, paste0("prob_", i), value = probs[i])
  })
  
  sim_results <- eventReactive(input$run_sim, {
    probs <- sapply(1:input$num_doses, function(i) input[[paste0("prob_", i)]])
    
    withProgress(message = 'Running Monte Carlo Simulations...', value = 0, {
      incProgress(0.5, detail = "Crunching data for 3 models...")
      res <- Phase1DesignDecision::run_unified_simulations(
        true_prob = probs,
        target_tox = input$target_tox,
        num_sims = input$n_sim,
        max_n = input$max_n,
        cohort_size = input$cohort_size
      )
      incProgress(0.5, detail = "Done!")
      return(res)
    })
  })
  
  output$metrics_table <- render_gt({
    req(sim_results())
    sim_results()$metrics %>%
      gt() %>%
      tab_header(
        title = "Operating Characteristics Comparison"
      ) %>%
      fmt_number(columns = c(2,3,4,5), decimals = 1) %>%
      cols_label(
        Correct_MTD_Sel_Pct = "MTD Selection (%)",
        Overdose_Rate_Pct = "Overdose Rate (%)",
        Underdose_Rate_Pct = "Underdose Rate (%)",
        Avg_Sample_Size = "Avg. Total Sample"
      ) %>%
      tab_options(heading.background.color = "#4B0082")
  })
  
  output$plot_mtd <- renderPlot({ req(sim_results()); Phase1DesignDecision::plot_mtd_selection(sim_results()) })
  output$plot_safety <- renderPlot({ req(sim_results()); Phase1DesignDecision::plot_safety_profile(sim_results()) })
  output$plot_alloc <- renderPlot({ req(sim_results()); Phase1DesignDecision::plot_patient_allocation(sim_results()) })
  
  output$plot_curve <- renderPlot({
    req(input$num_doses)
    probs <- sapply(1:input$num_doses, function(i) {
        val <- input[[paste0("prob_", i)]]
        if(is.null(val)) return(NA)
        return(val)
    })
    if(any(is.na(probs))) return(NULL)
    Phase1DesignDecision::plot_dose_tox_curve(probs, input$target_tox)
  })
}

shinyApp(ui, server)

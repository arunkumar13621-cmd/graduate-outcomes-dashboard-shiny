library(shiny); library(ggplot2); library(dplyr)
source("R/data_functions.R")
data_all <- load_outcomes()
fmt_pct <- function(x) ifelse(is.na(x), "suppressed", sprintf("%.1f%%", 100 * x))
fmt_wage <- function(x) ifelse(is.na(x), "suppressed", sprintf("$%.2f", x))

ui <- fluidPage(
  tags$head(tags$style(HTML("body{font-family:Arial,sans-serif} .kpi{background:#f3f6fa;border-radius:6px;padding:10px;text-align:center;margin-bottom:10px} .kpi h3{margin:0;font-size:26px} .kpi p{margin:0;color:#555}"))),
  titlePanel("Graduate Outcomes Explorer (synthetic data)"),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectizeInput("inst", "Institution", sort(unique(data_all$institution_id)), multiple = TRUE, options = list(placeholder = "All institutions")),
      selectizeInput("prog", "Program area", sort(unique(data_all$program_area)), multiple = TRUE, options = list(placeholder = "All programs")),
      checkboxGroupInput("yr", "Graduation year", sort(unique(data_all$grad_year)), selected = sort(unique(data_all$grad_year)), inline = TRUE),
      downloadButton("dl", "Download table (CSV)"),
      hr(), helpText("Groups with fewer than 10 respondents are suppressed. Employment rates use institution weights.")),
    mainPanel(width = 9,
      fluidRow(column(3, div(class = "kpi", h3(textOutput("k_n")), p("Respondents"))),
               column(3, div(class = "kpi", h3(textOutput("k_emp")), p("Employed within 6 months"))),
               column(3, div(class = "kpi", h3(textOutput("k_wage")), p("Median hourly wage"))),
               column(3, div(class = "kpi", h3(textOutput("k_q")), p("Mean quality rating (1-5)")))),
      tabsetPanel(
        tabPanel("Employment by program", plotOutput("p_emp", height = 330)),
        tabPanel("Wages", plotOutput("p_wage", height = 330)),
        tabPanel("Table", tableOutput("tab"))))))

server <- function(input, output, session) {
  filtered <- reactive(filter_outcomes(data_all, input$inst, input$prog, as.integer(input$yr)))
  tab <- reactive(summarise_by(filtered(), "program_area"))
  k <- reactive(kpis(filtered()))
  output$k_n    <- renderText(format(k()$respondents, big.mark = ","))
  output$k_emp  <- renderText(fmt_pct(k()$employment_rate))
  output$k_wage <- renderText(fmt_wage(k()$median_wage))
  output$k_q    <- renderText(ifelse(is.na(k()$mean_quality), "suppressed", sprintf("%.2f", k()$mean_quality)))
  output$p_emp <- renderPlot({
    t <- tab(); req(nrow(t) > 0)
    ggplot(t, aes(reorder(program_area, employment_rate), employment_rate, fill = suppressed)) + geom_col() +
      geom_text(aes(label = paste0("n=", n)), hjust = -0.1, size = 3.5) + coord_flip() +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + scale_fill_manual(values = c(`FALSE` = "#2b6cb0", `TRUE` = "grey70"), guide = "none") +
      labs(x = NULL, y = "Employed within 6 months (weighted)") + theme_minimal(base_size = 13)
  })
  output$p_wage <- renderPlot({
    d <- filtered(); d <- d[!is.na(d$hourly_wage), ]; req(nrow(d) >= MIN_CELL)
    ggplot(d, aes(program_area, hourly_wage)) + geom_boxplot(fill = "#90cdf4") + coord_flip() +
      labs(x = NULL, y = "Hourly wage ($)") + theme_minimal(base_size = 13)
  })
  output$tab <- renderTable({ t <- tab(); t$employment_rate <- fmt_pct(t$employment_rate); t$median_wage <- fmt_wage(t$median_wage)
    t$mean_quality <- ifelse(is.na(t$mean_quality), "suppressed", sprintf("%.2f", t$mean_quality)); t$suppressed <- NULL; t })
  output$dl <- downloadHandler(filename = function() "outcomes_summary.csv", content = function(file) write.csv(tab(), file, row.names = FALSE, na = "suppressed"))
}
shinyApp(ui, server)

library(shiny)

options(shiny.autoreload = FALSE)

palette_groups <- c("#007C91", "#E07A5F", "#6A4C93", "#D4A72C", "#3D9970", "#C44569")

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(class = "hero",
      div(class = "eyebrow", "INTERACTIVE STATISTICS LAB"),
      h1("ANOVA & Kruskal–Wallis Explorer"),
      p("Generate grouped data, inspect the partition of variability, and compare a parametric test with its rank-based alternative.")),
  navlistPanel(
    id = "main_tabs", widths = c(3, 9), well = FALSE,
    tabPanel(
      "1 · Generate data",
      div(class = "section-heading", h2("Build a dataset"), p("Choose a scenario, then regenerate to see sampling variation.")),
      fluidRow(
        column(4, div(class = "control-card",
          sliderInput("k", "Number of groups", min = 2, max = 6, value = 3, step = 1),
          sliderInput("n", "Observations per group", min = 5, max = 80, value = 20, step = 1),
          selectInput("shape", "Distribution", choices = c("Normal" = "normal", "Right-skewed" = "skew", "Heavy-tailed" = "heavy")),
          sliderInput("spread", "Within-group SD", min = 0.2, max = 4, value = 1.2, step = 0.1),
          sliderInput("effect", "Separation between group means", min = 0, max = 5, value = 1.2, step = 0.1),
          numericInput("seed", "Random seed", value = 2026, min = 1, step = 1),
          actionButton("generate", "Generate new sample", class = "btn-primary")
        )),
        column(8,
          div(class = "plot-card", plotOutput("data_plot", height = "420px")),
          div(class = "insight-card", uiOutput("data_story"))
        )
      ),
      h3("Generated observations"),
      div(class = "table-card", tableOutput("data_preview"))
    ),
    tabPanel(
      "2 · One-way ANOVA",
      div(class = "section-heading", h2("Decompose variability"), p("ANOVA compares variation among group means with variation inside the groups.")),
      fluidRow(
        column(4,
          div(class = "metric-card", span("F statistic"), strong(textOutput("f_value", inline = TRUE))),
          div(class = "metric-card", span("p-value"), strong(textOutput("anova_p", inline = TRUE))),
          div(class = "metric-card", span("η² = SSbetween / SStotal"), strong(textOutput("eta_value", inline = TRUE))),
          div(class = "explain-card",
            h3("The identity"),
            tags$div(class = "formula", "yᵢⱼ − ȳ = (ȳⱼ − ȳ) + (yᵢⱼ − ȳⱼ)"),
            p("Each observation’s deviation from the grand mean is split into a between-group component and a within-group residual."))
        ),
        column(8, div(class = "plot-card", plotOutput("ss_segments", height = "430px")))
      ),
      fluidRow(
        column(7, div(class = "plot-card", plotOutput("ss_bars", height = "340px"))),
        column(5, div(class = "table-card", h3("ANOVA table"), tableOutput("anova_table")))
      ),
      div(class = "insight-card", uiOutput("anova_story"))
    ),
    tabPanel(
      "3 · Kruskal–Wallis",
      div(class = "section-heading", h2("Replace values with ranks"), p("Kruskal–Wallis tests whether groups tend to occupy different parts of the pooled rank distribution.")),
      fluidRow(
        column(4,
          div(class = "metric-card", span("Kruskal–Wallis H"), strong(textOutput("kw_value", inline = TRUE))),
          div(class = "metric-card", span("p-value"), strong(textOutput("kw_p", inline = TRUE))),
          div(class = "explain-card", h3("What changes?"), p("The original measurements are pooled and ranked. This reduces sensitivity to extreme values, but the test is about rank distributions—not automatically about means."))
        ),
        column(8, div(class = "plot-card", plotOutput("rank_plot", height = "430px")))
      ),
      fluidRow(
        column(6, div(class = "table-card", h3("Mean ranks by group"), tableOutput("rank_table"))),
        column(6, div(class = "compare-card", h3("ANOVA and Kruskal–Wallis"), uiOutput("comparison")))
      ),
      div(class = "insight-card", uiOutput("kw_story"))
    )
  ),
  tags$footer("Runs entirely in your browser with R via WebAssembly · No data are uploaded")
)

server <- function(input, output, session) {
  sample_id <- reactiveVal(0)
  observeEvent(input$generate, sample_id(sample_id() + 1), ignoreInit = TRUE)

  dataset <- reactive({
    sample_id()
    set.seed(as.integer(input$seed) + sample_id())
    k <- input$k; n <- input$n
    centres <- seq(-(k - 1) / 2, (k - 1) / 2, length.out = k) * input$effect
    z <- switch(input$shape,
      normal = rnorm(k * n),
      skew = (rexp(k * n) - 1),
      heavy = rt(k * n, df = 3) / sqrt(3)
    )
    data.frame(
      group = factor(rep(paste("Group", LETTERS[seq_len(k)]), each = n), levels = paste("Group", LETTERS[seq_len(k)])),
      value = rep(centres, each = n) + input$spread * z
    )
  })

  fit <- reactive(aov(value ~ group, data = dataset()))
  kw <- reactive(kruskal.test(value ~ group, data = dataset()))

  output$data_plot <- renderPlot({
    d <- dataset(); cols <- palette_groups[seq_len(nlevels(d$group))]
    jitter_x <- jitter(as.numeric(d$group), amount = 0.16)
    par(mar = c(4.3, 4.4, 1.2, 0.6), bty = "l")
    plot(jitter_x, d$value, xaxt = "n", xlab = "", ylab = "Observed value", pch = 21,
         bg = adjustcolor(cols[as.numeric(d$group)], 0.55), col = "white", cex = 1.25)
    axis(1, seq_len(nlevels(d$group)), levels(d$group))
    means <- tapply(d$value, d$group, mean)
    segments(seq_along(means) - .25, means, seq_along(means) + .25, means, lwd = 4, col = cols)
    abline(h = mean(d$value), lty = 2, col = "#273043")
    legend("topleft", "Grand mean", lty = 2, col = "#273043", bty = "n")
  }, res = 110)

  output$data_story <- renderUI({
    d <- dataset(); gmeans <- tapply(d$value, d$group, mean)
    tags$p(tags$b("Reading the sample: "), sprintf("%d observations across %d groups. Sample means range from %.2f to %.2f.", nrow(d), nlevels(d$group), min(gmeans), max(gmeans)))
  })
  output$data_preview <- renderTable({ do.call(rbind, lapply(split(dataset(), dataset()$group), head, 5)) }, striped = TRUE, digits = 3)

  ss_values <- reactive({
    d <- dataset(); gm <- mean(d$value); group_means <- ave(d$value, d$group, FUN = mean)
    c(total = sum((d$value - gm)^2), between = sum((group_means - gm)^2), within = sum((d$value - group_means)^2))
  })

  output$ss_segments <- renderPlot({
    d <- dataset(); gm <- mean(d$value); group_means <- ave(d$value, d$group, FUN = mean)
    ord <- order(d$group, d$value); d <- d[ord, ]; group_means <- group_means[ord]
    x <- seq_len(nrow(d)); cols <- palette_groups[as.numeric(d$group)]
    par(mar = c(4.3, 4.4, 1.2, 0.8), bty = "l")
    plot(x, d$value, pch = 16, col = adjustcolor(cols, .8), xlab = "Observations, grouped", ylab = "Value", xaxt = "n")
    segments(x, gm, x, group_means, col = adjustcolor("#007C91", .55), lwd = 2)
    segments(x, group_means, x, d$value, col = adjustcolor("#E07A5F", .58), lwd = 2)
    abline(h = gm, lty = 2, lwd = 2, col = "#273043")
    legend("topleft", c("Between: grand mean → group mean", "Within: group mean → observation"),
           col = c("#007C91", "#E07A5F"), lwd = 3, bty = "n", cex = .9)
  }, res = 110)

  output$ss_bars <- renderPlot({
    ss <- ss_values()
    par(mar = c(5, 4.4, 1.2, .6), bty = "l")
    bp <- barplot(c(ss["total"], ss["between"], ss["within"]),
      names.arg = c("Total SS", "Between SS", "Within SS"),
      col = c("#273043", "#007C91", "#E07A5F"), border = NA, ylab = "Sum of squares")
    text(bp, c(ss["total"], ss["between"], ss["within"]), labels = format(round(c(ss["total"], ss["between"], ss["within"]), 1), nsmall = 1), pos = 3)
    mtext("Total SS = Between SS + Within SS", side = 1, line = 3.5, col = "#596275")
  }, res = 110)

  output$anova_table <- renderTable({
    tab <- summary(fit())[[1]]
    data.frame(Source = c("Between groups", "Within groups"), Df = tab$Df,
      `Sum of squares` = tab$`Sum Sq`, `Mean square` = tab$`Mean Sq`,
      F = c(tab$`F value`[1], NA), `p-value` = c(tab$`Pr(>F)`[1], NA), check.names = FALSE)
  }, digits = 4, striped = TRUE)
  output$f_value <- renderText(sprintf("%.3f", summary(fit())[[1]]$`F value`[1]))
  output$anova_p <- renderText(format.pval(summary(fit())[[1]]$`Pr(>F)`[1], digits = 3, eps = .001))
  output$eta_value <- renderText(sprintf("%.3f", ss_values()["between"] / ss_values()["total"]))
  output$anova_story <- renderUI({
    p <- summary(fit())[[1]]$`Pr(>F)`[1]
    tags$p(tags$b("Interpretation: "), if (p < .05) "The sample provides evidence that not all population means are equal. ANOVA does not identify which groups differ; that requires planned contrasts or post-hoc comparisons." else "At the 5% level, the sample does not provide enough evidence to reject equality of all population means. This is not proof that the means are identical.")
  })

  output$rank_plot <- renderPlot({
    d <- dataset(); d$rank <- rank(d$value, ties.method = "average")
    cols <- palette_groups[seq_len(nlevels(d$group))]
    par(mar = c(4.3, 4.4, 1.2, .6), bty = "l")
    boxplot(rank ~ group, data = d, col = adjustcolor(cols, .62), border = cols,
            ylab = "Pooled rank", xlab = "", outline = FALSE)
    points(jitter(as.numeric(d$group), .15), d$rank, pch = 16, col = adjustcolor(cols[as.numeric(d$group)], .48))
    overall <- (nrow(d) + 1) / 2
    abline(h = overall, lty = 2, col = "#273043")
  }, res = 110)
  output$rank_table <- renderTable({
    d <- dataset(); d$rank <- rank(d$value, ties.method = "average")
    out <- aggregate(cbind(value, rank) ~ group, d, function(x) c(n = length(x), mean = mean(x), median = median(x)))
    data.frame(Group = out$group, N = out$value[, "n"], Mean = out$value[, "mean"], Median = out$value[, "median"], `Mean rank` = out$rank[, "mean"], check.names = FALSE)
  }, digits = 2, striped = TRUE)
  output$kw_value <- renderText(sprintf("%.3f", unname(kw()$statistic)))
  output$kw_p <- renderText(format.pval(kw()$p.value, digits = 3, eps = .001))
  output$comparison <- renderUI({
    pa <- summary(fit())[[1]]$`Pr(>F)`[1]; pk <- kw()$p.value
    tags$div(
      tags$p(tags$b("ANOVA p: "), format.pval(pa, digits = 3, eps = .001)),
      tags$p(tags$b("Kruskal–Wallis p: "), format.pval(pk, digits = 3, eps = .001)),
      tags$p("Try heavy tails or skewness, change the seed, and watch when the conclusions agree or diverge."),
      actionLink("go_data", "← Change the data-generating scenario")
    )
  })
  observeEvent(input$go_data, updateNavlistPanel(session, "main_tabs", selected = "1 · Generate data"))
  output$kw_story <- renderUI({
    p <- kw()$p.value
    tags$p(tags$b("Interpretation: "), if (p < .05) "The rank distributions are not all alike. If group distributions have comparable shapes, this is often interpreted as evidence of a location shift." else "At the 5% level, the ranked sample does not provide enough evidence of group differences.")
  })
}

shinyApp(ui, server)

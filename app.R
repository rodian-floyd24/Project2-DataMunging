library(shiny)      # core Shiny
library(bslib)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(tidyr)
library(forcats)
library(broom)
library(stringr)

# Load data once (robust to being sourced or run via runApp)
app_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(...) getwd())
path <- file.path(app_dir, "online_retail_clean.csv")
if (!file.exists(path)) {
  path <- "online_retail_clean.csv"
}
dt <- fread(path) %>%  # single load on startup
  mutate(
    InvoiceDateTime = as.POSIXct(InvoiceDateTime, tz = "UTC"), # fix timezone for reproducibility
    Quantity = as.numeric(Quantity),
    UnitPrice = as.numeric(UnitPrice)
  )

sales <- dt %>%
  mutate(
    InvoiceDate = as.Date(InvoiceDateTime),
    InvoiceMonth = floor_date(InvoiceDate, unit = "month"),
    TotalPrice = Quantity * UnitPrice,
    hour = hour(InvoiceDateTime),
    wday = wday(InvoiceDateTime, label = TRUE, abbr = TRUE)
  ) %>%
  filter(!is.na(TotalPrice) & TotalPrice >= 0) # drop returns/credits for revenue views

min_date <- min(sales$InvoiceDate, na.rm = TRUE)
max_date <- max(sales$InvoiceDate, na.rm = TRUE)

country_levels <- sales %>%
  count(Country, wt = TotalPrice, name = "revenue") %>%
  arrange(desc(revenue)) %>%
  pull(Country)

missing_summary <- data.frame(
  column = names(dt),
  missing_pct = sapply(dt, function(x) mean(is.na(x)) * 100)
) %>% arrange(desc(missing_pct))

# Global seasonal and RFM stats used for captions/narrative
quarter_rev <- sales %>%
  mutate(quarter = quarter(InvoiceDate, with_year = FALSE)) %>%
  group_by(quarter) %>%
  summarise(revenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop")

q3_rev <- quarter_rev %>% filter(quarter == 3) %>% pull(revenue)
q4_rev <- quarter_rev %>% filter(quarter == 4) %>% pull(revenue)
q4_vs_q3_pct <- if (length(q3_rev) && length(q4_rev) && q3_rev > 0) {
  round((q4_rev / q3_rev - 1) * 100, 1)
} else {
  NA_real_
}

global_max_date <- max(sales$InvoiceDate, na.rm = TRUE)

rfm_global <- sales %>%
  filter(!is.na(CustomerID)) %>%
  group_by(CustomerID) %>%
  summarise(
    recency = as.numeric(global_max_date + days(1) - max(InvoiceDate, na.rm = TRUE)),
    frequency = n_distinct(InvoiceNo),
    monetary = sum(TotalPrice, na.rm = TRUE),
    .groups = "drop"
  )

rfm_fit <- if (nrow(rfm_global) > 1) lm(monetary ~ recency + frequency, data = rfm_global) else NULL
rfm_betas <- if (!is.null(rfm_fit)) tidy(rfm_fit) else NULL
rfm_r2 <- if (!is.null(rfm_fit)) summary(rfm_fit)$r.squared else NA_real_

ui <- page_fluid(
  theme = bs_theme(bootswatch = "cosmo"),
  titlePanel("Online Retail: Interactive EDA"),
  navset_tab(
    nav_panel(
      "Overview",
      layout_sidebar(
        sidebar = sidebar(
          sliderInput("daterange", "Invoice date range", min = min_date, max = max_date,
                      value = c(min_date, max_date), timeFormat = "%Y-%m-%d"),
          selectInput("countries", "Countries (top 10 prefilled)",
                      choices = country_levels, selected = head(country_levels, 10), multiple = TRUE),
          sliderInput("topN", "Top N products", min = 5, max = 25, value = 10, step = 1)
        ),
        layout_columns(
          col_widths = c(3,3,3,3),
          card(class = "p-3",
               h5("Transactions"),
               textOutput("tx_box")),
          card(class = "p-3",
               h5("Customers"),
               textOutput("cust_box")),
          card(class = "p-3",
               h5("Revenue"),
               textOutput("rev_box")),
          card(class = "p-3",
               h5("Avg ticket"),
               textOutput("avg_ticket_box"))
        ),
        card(class = "mt-3",
             h4("Path"),
             p("Seasonality → timing → products/customers → data quality + modeling."),
             p("Use filters on the left to subset date range and countries; top N controls product depth."))
      )
    ),
    nav_panel(
      "Trends",
      layout_columns(
        card(plotlyOutput("daily_plot", height = 350), class = "p-3",
             textOutput("daily_caption")),
        card(plotlyOutput("monthly_plot", height = 350), class = "p-3",
             p("Orders and revenue rise together: growth from more orders, not just ticket."))
      )
    ),
    nav_panel(
      "Products & Customers",
      layout_columns(
        card(plotlyOutput("top_products_plot", height = 400), class = "p-3",
             p("Bar: revenue is concentrated in a few SKUs (long tail exists).")),
        card(plotlyOutput("rfm_scatter", height = 400), class = "p-3",
             p("Scatter: recent + frequent buyers drive spend; targets for retention."))
      ),
      layout_columns(
        card(class = "p-3",
             h5("Country summary (filtered)"),
             p("Revenue, orders, and distinct customers by country after applying filters."),
             DTOutput("country_table")),
        card(class = "p-3",
             h5("Recent transactions (filtered sample)"),
             p("Last 50 line items in the current filter for quick spot checks."),
             DTOutput("recent_table"))
      ),
      layout_columns(
        card(plotlyOutput("product_country_heat", height = 400), class = "p-3",
             p("Heatmap: which top products drive revenue in the top countries."))
      )
    ),
    nav_panel(
      "Operations",
      layout_columns(
        card(plotlyOutput("heat_plot", height = 400), class = "p-3",
             p("Heatmap: demand peaks midweek 9–15h; useful for staffing/promos.")),
        card(plotlyOutput("price_box", height = 400), class = "p-3",
             p("Boxplot: unit price dispersion tight across top countries; pricing consistent."))
      )
    ),
    nav_panel(
      "Data Quality & Methods",
      layout_columns(
        card(DTOutput("missing_table"), class = "p-3",
             h5("Missingness & recoding"),
             tags$ul(
               tags$li("Invoice fields and numeric measures: 0% missing."),
              tags$li("CustomerID gaps kept (one-off buyers) to preserve revenue totals."),
              tags$li("Description blanks removed from product rankings to avoid noise."),
              tags$li("Negative/NA TotalPrice rows (returns/credits) excluded from revenue views."),
              tags$li("Derived: TotalPrice, InvoiceMonth, date/time splits via lubridate."),
              tags$li("Missingness shown on cleaned dataset; raw-to-clean steps are in the munging Rmd.")
            )
        ),
        card(class = "p-3",
             h5("Multi-angle: Recency/Frequency → Monetary"),
             p("Regression estimated on filtered data; caption summarizes global model; plot shows predicted vs actual."),
             plotlyOutput("lm_plot", height = 300),
             DTOutput("lm_table"),
             textOutput("lm_caption"))
      ),
      card(class = "p-3 mt-3",
           h5("Narrative highlights"),
           tags$ul(
             tags$li("Seasonality: Q4 lift with weekly pattern."),
             tags$li("Products: few SKUs dominate; long tail present."),
             tags$li("Customers: recent + frequent drive spend; churn risk at high recency."),
             tags$li("Geography: UK-heavy; pricing consistent across top markets."),
             tags$li("Timing: midweek daytime demand concentration."),
             tags$li("Path: seasonality → timing → products/countries → customers + model.")),
           h5("Group contributions"),
           tags$ul(
             tags$li("Rich-Ann: cleaning, missingness audit, derived fields, docs."),
             tags$li("Ben and Morgan: EDA visuals, interactivity, timing heatmap."),
             tags$li("Ray: RFM + regression, narrative, app assembly."))
      )
    )
  )
)

server <- function(input, output, session) {
  # Central slice of sales reused everywhere; all filters flow from here
  filtered <- reactive({
    dat <- sales %>%
      filter(between(InvoiceDate, input$daterange[1], input$daterange[2]))
    if (length(input$countries) > 0) {
      dat <- dat %>% filter(Country %in% input$countries)
    }
    dat
  }) %>% bindCache(input$daterange, input$countries)

  daily_df <- reactive({
    filtered() %>%
      group_by(InvoiceDate) %>%
      summarise(revenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop")
  }) %>% bindCache(filtered())

  monthly_df <- reactive({
    filtered() %>%
      group_by(InvoiceMonth) %>%
      summarise(revenue = sum(TotalPrice, na.rm = TRUE), orders = n_distinct(InvoiceNo), .groups = "drop") %>%
      pivot_longer(c(revenue, orders), names_to = "metric", values_to = "value")
  }) %>% bindCache(filtered())

  country_summary <- reactive({
    filtered() %>%
      group_by(Country) %>%
      summarise(
        revenue = sum(TotalPrice, na.rm = TRUE),
        orders = n_distinct(InvoiceNo),
        customers = n_distinct(CustomerID),
        .groups = "drop"
      ) %>%
      arrange(desc(revenue))
  }) %>% bindCache(filtered())

  top_country_names <- reactive({
    country_summary() %>%
      slice_max(order_by = revenue, n = 10) %>%
      pull(Country)
  })

  rfm_df <- reactive({
    dat <- filtered()
    if (nrow(dat) == 0) return(NULL)
    local_max_date <- max(dat$InvoiceDate, na.rm = TRUE) # recompute recency per filtered slice
    dat %>%
      filter(!is.na(CustomerID)) %>% # drop anonymous customers to avoid lumping them
      group_by(CustomerID) %>%
      summarise(
        recency = as.numeric(local_max_date + days(1) - max(InvoiceDate, na.rm = TRUE)),
        frequency = n_distinct(InvoiceNo),
        monetary = sum(TotalPrice, na.rm = TRUE),
        .groups = "drop"
      )
  }) %>% bindCache(filtered())

  product_country_df <- reactive({
    dat <- filtered() %>%
      filter(!is.na(Description) & Description != "")
    if (nrow(dat) == 0) return(NULL)
    # Top slice is driven by filtered data and the UI topN slider
    top_countries <- dat %>%
      group_by(Country) %>%
      summarise(revenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop") %>%
      slice_max(order_by = revenue, n = 6) %>%
      pull(Country)
    top_products <- dat %>%
      group_by(Description) %>%
      summarise(revenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop") %>%
      slice_max(order_by = revenue, n = input$topN) %>%
      pull(Description)
    country_totals <- dat %>%
      group_by(Country) %>%
      summarise(country_rev = sum(TotalPrice, na.rm = TRUE), .groups = "drop")
    dat %>%
      filter(Country %in% top_countries, Description %in% top_products) %>%
      group_by(Country, Description) %>%
      summarise(revenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop") %>%
      left_join(country_totals, by = "Country") %>%
      mutate(share = revenue / country_rev)
  }) %>% bindCache(filtered(), input$topN)

  output$tx_box <- renderText({ comma(n_distinct(filtered()$InvoiceNo)) })
  output$cust_box <- renderText({ comma(n_distinct(filtered()$CustomerID)) })
  output$rev_box <- renderText({ dollar(sum(filtered()$TotalPrice, na.rm = TRUE)) })
  output$avg_ticket_box <- renderText({
    tx <- n_distinct(filtered()$InvoiceNo)
    rev <- sum(filtered()$TotalPrice, na.rm = TRUE)
    if (tx == 0) "-" else dollar(rev / tx)
  })

  output$daily_plot <- renderPlotly({
    df <- daily_df()
    validate(need(nrow(df) > 0, "No data for this selection."))
    p <- ggplot(df, aes(InvoiceDate, revenue)) +
      geom_line(color = "#2c7fb8") +
      geom_smooth(method = "loess", se = FALSE, color = "#f28e2b") +
      scale_y_continuous(labels = dollar) +
      labs(x = "Date", y = "Revenue", title = "Daily revenue") +
      theme_minimal()
    ggplotly(p)
  })

  output$monthly_plot <- renderPlotly({
    df <- monthly_df()
    validate(need(nrow(df) > 0, "No data for this selection."))
    p <- ggplot(df, aes(InvoiceMonth, value, color = metric, group = metric)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("revenue" = "#1f77b4", "orders" = "#d62728"), labels = c("Orders", "Revenue")) +
      scale_y_continuous(labels = comma) +
      labs(x = "Month", y = "Value", color = "Metric", title = "Monthly trend (separate scales)") +
      facet_wrap(~metric, scales = "free_y", ncol = 1, labeller = as_labeller(c(revenue = "Revenue", orders = "Orders"))) +
      theme_minimal()
    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.2))
  })

  output$top_products_plot <- renderPlotly({
    df <- filtered() %>%
      group_by(Description) %>%
      summarise(revenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(Description) & Description != "") %>%
      slice_max(order_by = revenue, n = input$topN) %>%
      arrange(revenue) %>%
      mutate(desc_wrapped = str_wrap(Description, width = 28))
    validate(need(nrow(df) > 0, "No data for this selection."))
    p <- ggplot(df, aes(x = revenue, y = reorder(desc_wrapped, revenue))) +
      geom_col(fill = "#4e79a7") +
      scale_x_continuous(labels = dollar) +
      labs(x = "Revenue", y = "Product", title = paste("Top", input$topN, "products")) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 9))
    ggplotly(p, tooltip = c("y", "x"))
  })

  output$rfm_scatter <- renderPlotly({
    df <- rfm_df()
    validate(need(!is.null(df) && nrow(df) > 0, "No data for this selection."))
    p <- ggplot(df, aes(x = recency, y = monetary, color = frequency)) +
      geom_point(alpha = 0.6) +
      scale_color_viridis_c(option = "plasma") +
      scale_y_continuous(labels = dollar) +
      labs(x = "Recency (days since last purchase)", y = "Monetary", color = "Frequency", title = "Customer RFM") +
      theme_minimal()
    ggplotly(p)
  })

  output$country_table <- renderDT({
    df <- country_summary()
    datatable(df, options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatCurrency("revenue", currency = "$")
  })

  output$recent_table <- renderDT({
    df <- filtered() %>%
      arrange(desc(InvoiceDateTime)) %>%
      select(InvoiceNo, InvoiceDateTime, Description, Quantity, UnitPrice, TotalPrice, CustomerID, Country) %>%
      slice_head(n = 50)
    datatable(df, options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatCurrency(c("UnitPrice", "TotalPrice"), currency = "$")
  })

  output$heat_plot <- renderPlotly({
    df <- filtered() %>%
      group_by(wday, hour) %>%
      summarise(revenue = sum(TotalPrice, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(df) > 0, "No data for this selection."))
    p <- ggplot(df, aes(x = hour, y = wday, fill = revenue)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = "magma", labels = dollar) +
      labs(x = "Hour of day", y = "Weekday", fill = "Revenue", title = "When do customers buy?") +
      theme_minimal()
    ggplotly(p)
  })

  output$price_box <- renderPlotly({
    df <- top_country_names()
    px <- filtered() %>% filter(Country %in% df, UnitPrice > 0)
    validate(need(nrow(px) > 0, "No data for this selection."))
    px <- px %>% mutate(Country = fct_reorder(Country, UnitPrice, median))
    p <- ggplot(px, aes(x = Country, y = UnitPrice, fill = Country)) +
      geom_violin(scale = "width", alpha = 0.45, color = NA, show.legend = FALSE) +
      geom_boxplot(width = 0.18, outlier.alpha = 0.15, show.legend = FALSE) +
      scale_y_continuous(labels = dollar) +
      coord_flip() +
      labs(x = "Country", y = "Unit price", title = "Unit price spread (top countries)") +
      theme_minimal()
    ggplotly(p)
  })

  output$missing_table <- renderDT({
    datatable(missing_summary, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE) %>%
      formatRound("missing_pct", digits = 2)
  })

  output$product_country_heat <- renderPlotly({
    df <- product_country_df()
    validate(need(!is.null(df) && nrow(df) > 0, "No data for this selection."))
    df <- df %>%
      mutate(
        Country = fct_reorder(Country, revenue, .fun = sum),
        Description = fct_reorder(Description, revenue, .fun = sum)
      )
    p <- ggplot(df, aes(x = Description, y = Country, fill = share)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = "inferno", labels = percent) +
      labs(x = "Product", y = "Country", fill = "Share of country revenue", title = "Top products by top countries (mix)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })

  output$lm_table <- renderDT({
    df <- rfm_df()
    if (is.null(df) || nrow(df) < 2) return(datatable(data.frame(message = "Not enough data after filters.")))
    fit <- lm(monetary ~ recency + frequency, data = df)
    tidy(fit) %>%
      mutate(across(where(is.numeric), ~round(., 3))) %>%
      datatable(options = list(dom = "t", pageLength = 5))
  })

  output$lm_plot <- renderPlotly({
    df <- rfm_df()
    validate(need(!is.null(df) && nrow(df) > 1, "Not enough data after filters."))
    fit <- lm(monetary ~ recency + frequency, data = df)
    df <- df %>%
      mutate(pred = predict(fit))
    p <- ggplot(df, aes(x = pred, y = monetary, color = frequency)) +
      geom_point(alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
      scale_color_viridis_c(option = "plasma") +
      scale_x_continuous(labels = dollar) +
      scale_y_continuous(labels = dollar) +
      labs(x = "Predicted monetary", y = "Actual monetary", color = "Frequency",
           title = "Regression fit: predicted vs actual (filtered)") +
      theme_minimal()
    ggplotly(p)
  })

  output$daily_caption <- renderText({
    if (!is.na(q4_vs_q3_pct)) {
      paste0("Line + LOESS: Q4 revenue runs about ", q4_vs_q3_pct, "% higher than Q3; weekly seasonality visible.")
    } else {
      "Line + LOESS: Q4 lift and weekly seasonality visible."
    }
  })

  output$lm_caption <- renderText({
    if (is.null(rfm_betas)) return("Scatter shows pattern; regression runs when enough data is available (not causal).")
    rec_beta <- rfm_betas %>% filter(term == "recency") %>% pull(estimate)
    freq_beta <- rfm_betas %>% filter(term == "frequency") %>% pull(estimate)
    freq_txt <- if (length(freq_beta)) paste0("each extra distinct invoice is associated with ", dollar(round(freq_beta, 0)), " more spend") else "frequency effect unavailable"
    rec_txt <- if (length(rec_beta)) paste0("recency coefficient is ", dollar(round(rec_beta, 0)), " per day") else "recency effect unavailable"
    r2_txt <- if (!is.na(rfm_r2)) paste0("global model R^2 ≈ ", round(rfm_r2, 2), "; ") else ""
    paste0(r2_txt, "caption uses global model; table updates with current filters: ", freq_txt, " while ", rec_txt, " (directional, not causal).")
  })
}

shinyApp(ui, server)

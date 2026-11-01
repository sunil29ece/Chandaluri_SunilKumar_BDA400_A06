# BDA400 Assignment 6 - Technical Analysis using R: Visualization Phase
# Student: SunilKumar Chandaluri
# Run: shiny::runApp(".") or click Run App in RStudio.
# Sections follow the four assignment grading criteria.

# SECTION 1 - DATA COLLECTION AND SETUP (20 marks)
required_packages <- c("shiny", "ggplot2", "quantmod", "TTR")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install required packages first: install.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
}
library(shiny)
library(ggplot2)
library(quantmod)

# Fetch data on demand, with validation, explicit errors, and a backup provider.
fetch_prices <- function(symbol, start_date, end_date) {
  symbol <- toupper(trimws(symbol))
  if (!grepl("^[A-Z0-9.^-]{1,20}$", symbol)) stop("Enter a valid stock symbol.")
  if (is.na(start_date) || is.na(end_date) || start_date >= end_date) {
    stop("Start date must be earlier than end date.")
  }
  # Yahoo Finance is the required primary source; Stooq is an optional fallback.
  yahoo <- tryCatch(
    suppressWarnings(quantmod::getSymbols(symbol, src = "yahoo", from = start_date,
                                           to = end_date + 1, auto.assign = FALSE)),
    error = function(e) NULL
  )
  provider <- "Yahoo Finance"
  raw <- yahoo
  if (is.null(raw) || NROW(raw) == 0L) {
    raw <- tryCatch(
      suppressWarnings(quantmod::getSymbols(symbol, src = "stooq", from = start_date,
                                             to = end_date, auto.assign = FALSE)),
      error = function(e) NULL
    )
    provider <- "Stooq (backup)"
  }
  if (is.null(raw) || NROW(raw) == 0L) stop("No stock prices returned. Check the symbol, dates, and internet connection.")
  raw <- raw[order(zoo::index(raw))]
  raw <- raw[paste0(start_date, "/", end_date)]
  if (NROW(raw) == 0L) stop("No observations in the requested date range.")
  # Standardize provider-specific column names via quantmod OHLC accessors.
  df <- data.frame(
    Date = as.Date(zoo::index(raw)),
    Open = as.numeric(quantmod::Op(raw)),
    High = as.numeric(quantmod::Hi(raw)),
    Low = as.numeric(quantmod::Lo(raw)),
    Close = as.numeric(quantmod::Cl(raw))
  )
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (NROW(df) == 0L) stop("The provider returned no complete OHLC records.")
  list(data = df, provider = provider)
}

# Aggregate OHLC bars into weekly/monthly candles. The last available close is used.
aggregate_bars <- function(df, time_frame) {
  if (time_frame == "Daily") return(df)
  grouping <- if (time_frame == "Weekly") {
    # Monday-start weeks, stable across calendar years.
    as.Date(cut(df$Date, breaks = "week", start.on.monday = TRUE))
  } else {
    as.Date(format(df$Date, "%Y-%m-01"))
  }
  pieces <- split(seq_len(nrow(df)), grouping)
  out <- do.call(rbind, lapply(pieces, function(idx) {
    x <- df[idx, , drop = FALSE]
    data.frame(Date = tail(x$Date, 1), Open = x$Open[1],
               High = max(x$High), Low = min(x$Low), Close = tail(x$Close, 1))
  }))
  rownames(out) <- NULL
  out[order(out$Date), , drop = FALSE]
}

# SECTION 2 - SHINY SKELETON AND INTERACTIVE WIDGETS (30 marks)
ui <- fluidPage(
  tags$head(tags$style(HTML("\n    body {background:#f5f7fb; color:#17233b;}\n    .well {background:white; border:1px solid #dce4ee; border-radius:12px;}\n    .panel-box {background:white; border:1px solid #dce4ee; border-radius:12px; padding:16px; margin-bottom:14px;}\n    h2 {font-weight:700;} .help-block {color:#56657c;}\n  "))),
  titlePanel("Portfolio Technical Analysis Dashboard | BDA400 A06"),
  sidebarLayout(
    sidebarPanel(width = 3,
      textInput("symbol", "Stock ticker", value = "AAPL"),
      dateRangeInput("date_range", "Historical date range",
                     start = Sys.Date() - 540, end = Sys.Date(),
                     max = Sys.Date()),
      selectInput("time_frame", "Time frame", c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Price chart", c("Line", "Area", "Candlestick")),
      checkboxGroupInput("technical_indicators", "Indicator overlays / panels",
                         choices = c("SMA", "EMA", "RSI", "MACD"),
                         selected = c("SMA", "EMA", "RSI", "MACD")),
      numericInput("short_period", "Short moving-average period", 20, min = 2, max = 250),
      numericInput("long_period", "Long moving-average period", 50, min = 3, max = 300),
      numericInput("rsi_period", "RSI period", 14, min = 2, max = 100),
      checkboxInput("show_signals", "Show Buy / Sell chart annotations", TRUE),
      actionButton("refresh", "Fetch / refresh prices", class = "btn-primary"),
      helpText("Yahoo Finance is primary; Stooq is attempted if Yahoo is unavailable."),
      helpText("Educational analysis only; not financial advice.")
    ),
    mainPanel(width = 9,
      div(class = "panel-box", strong(textOutput("summary")),
          textOutput("data_source")),
      div(class = "panel-box", h4("Stock price and moving averages"),
          plotOutput("stock_chart", height = "420px")),
      conditionalPanel("input.technical_indicators && input.technical_indicators.includes('RSI')",
        div(class = "panel-box", h4("Relative Strength Index (RSI)"),
            plotOutput("rsi_chart", height = "200px"))),
      conditionalPanel("input.technical_indicators && input.technical_indicators.includes('MACD')",
        div(class = "panel-box", h4("MACD and signal line"),
            plotOutput("macd_chart", height = "220px"))),
      div(class = "panel-box", h4("Trading signals and recent observations"),
          tableOutput("signal_table"),
          helpText("Buy: short SMA crosses above long SMA. Sell: short SMA crosses below long SMA. Otherwise Hold."))
    )
  )
)

# SECTION 3 - TECHNICAL INDICATORS AND DYNAMIC OVERLAYS (25 marks)
server <- function(input, output, session) {
  # Event reactive prevents re-fetching the same historical prices on every UI change.
  downloaded <- eventReactive(input$refresh, {
    validate(need(input$date_range[1] < input$date_range[2],
                  "Choose a valid date range."))
    tryCatch(fetch_prices(input$symbol, as.Date(input$date_range[1]),
                          as.Date(input$date_range[2])),
             error = function(e) list(error = conditionMessage(e)))
  }, ignoreNULL = FALSE)

  stock_data <- reactive({
    result <- downloaded()
    validate(need(is.null(result$error), result$error))
    aggregate_bars(result$data, input$time_frame)
  })

  # SECTION 4 - TRADING RULES AND ANNOTATIONS (25 marks)
  calculated <- reactive({
    df <- stock_data()
    short_n <- as.integer(input$short_period)
    long_n <- as.integer(input$long_period)
    rsi_n <- as.integer(input$rsi_period)
    validate(
      need(is.finite(short_n) && is.finite(long_n) && short_n >= 2 && long_n > short_n,
           "Long MA period must be greater than short MA period (minimum short period: 2)."),
      need(is.finite(rsi_n) && rsi_n >= 2, "RSI period must be at least 2."),
      need(nrow(df) >= long_n + 2,
           paste("Not enough", input$time_frame, "bars for the selected long MA period. Increase the date range or reduce periods."))
    )
    # TTR indicators return NA during their warm-up periods.
    df$ShortSMA <- as.numeric(TTR::SMA(df$Close, n = short_n))
    df$LongSMA <- as.numeric(TTR::SMA(df$Close, n = long_n))
    df$EMA <- as.numeric(TTR::EMA(df$Close, n = short_n))
    df$RSI <- if (nrow(df) > rsi_n) as.numeric(TTR::RSI(df$Close, n = rsi_n)) else rep(NA_real_, nrow(df))
    macd <- if (nrow(df) >= 27) {
      TTR::MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = FALSE)
    } else NULL
    df$MACD <- if (is.null(macd)) rep(NA_real_, nrow(df)) else as.numeric(macd[, 1])
    df$MACDSignal <- if (is.null(macd)) rep(NA_real_, nrow(df)) else as.numeric(macd[, 2])
    df$Histogram <- df$MACD - df$MACDSignal

    # A signal occurs ONLY on a crossing bar, not on every bar above/below.
    previous_short <- c(NA_real_, head(df$ShortSMA, -1))
    previous_long <- c(NA_real_, head(df$LongSMA, -1))
    valid <- is.finite(df$ShortSMA) & is.finite(df$LongSMA) &
             is.finite(previous_short) & is.finite(previous_long)
    buy <- valid & df$ShortSMA > df$LongSMA & previous_short <= previous_long
    sell <- valid & df$ShortSMA < df$LongSMA & previous_short >= previous_long
    df$Signal <- ifelse(buy, "Buy", ifelse(sell, "Sell", "Hold"))
    df
  })

  output$summary <- renderText({
    df <- calculated()
    paste(toupper(input$symbol), "|", input$time_frame, "bars:", nrow(df),
          "| Latest close:", sprintf("%.2f", tail(df$Close, 1)),
          "| Buy signals:", sum(df$Signal == "Buy"),
          "| Sell signals:", sum(df$Signal == "Sell"))
  })
  output$data_source <- renderText({
    result <- downloaded()
    validate(need(is.null(result$error), result$error))
    paste("Data source:", result$provider, "| Retrieved:", nrow(result$data), "daily bars")
  })

  output$stock_chart <- renderPlot({
    df <- calculated()
    p <- ggplot(df, aes(x = Date)) +
      labs(x = NULL, y = "Price", title = paste(toupper(input$symbol), "stock price")) +
      theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold"))
    if (input$chart_type == "Line") {
      p <- p + geom_line(aes(y = Close), colour = "#254b83", linewidth = 0.75)
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(aes(y = Close), fill = "#b6d4f4", alpha = 0.65) +
        geom_line(aes(y = Close), colour = "#254b83", linewidth = 0.6)
    } else {
      up <- df$Close >= df$Open
      df$BarColor <- ifelse(up, "Up", "Down")
      width <- switch(input$time_frame, Daily = 0.65, Weekly = 4, Monthly = 16)
      p <- p + geom_linerange(aes(ymin = Low, ymax = High, colour = BarColor), linewidth = 0.55) +
        geom_rect(aes(xmin = Date - width / 2, xmax = Date + width / 2,
                      ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = BarColor),
                  colour = NA, alpha = 0.85) +
        scale_fill_manual(values = c(Up = "#13846b", Down = "#d34e55")) +
        scale_colour_manual(values = c(Up = "#13846b", Down = "#d34e55"))
    }
    if ("SMA" %in% input$technical_indicators) {
      p <- p + geom_line(aes(y = ShortSMA), colour = "#ed8b24", linewidth = 0.8, na.rm = TRUE) +
        geom_line(aes(y = LongSMA), colour = "#8d60b7", linewidth = 0.8, na.rm = TRUE)
    }
    if ("EMA" %in% input$technical_indicators) {
      p <- p + geom_line(aes(y = EMA), colour = "#1b9a9a", linewidth = 0.8, na.rm = TRUE)
    }
    if (isTRUE(input$show_signals)) {
      signals <- df[df$Signal != "Hold", , drop = FALSE]
      if (nrow(signals)) {
        p <- p + geom_point(data = signals, aes(y = Close, shape = Signal, colour = Signal),
                            size = 3, inherit.aes = TRUE) +
          geom_text(data = signals, aes(y = Close, label = Signal, colour = Signal),
                    vjust = -0.9, size = 3.3, check_overlap = TRUE) +
          scale_shape_manual(values = c(Buy = 24, Sell = 25)) +
          scale_colour_manual(values = c(Buy = "#11865e", Sell = "#c93e50"))
      }
    }
    print(p)
  })

  output$rsi_chart <- renderPlot({
    df <- calculated()
    validate(need(any(is.finite(df$RSI)), "Insufficient observations to calculate RSI."))
    print(ggplot(df, aes(Date, RSI)) + geom_line(colour = "#8d60b7", linewidth = 0.75, na.rm = TRUE) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed", colour = "#9ba5b5") +
      coord_cartesian(ylim = c(0, 100)) + labs(x = NULL, y = "RSI (0-100)") + theme_minimal())
  })
  output$macd_chart <- renderPlot({
    df <- calculated()
    validate(need(any(is.finite(df$MACDSignal)), "Insufficient observations to calculate MACD signal line."))
    print(ggplot(df, aes(x = Date)) +
      geom_col(aes(y = Histogram), fill = "#b9c8da", na.rm = TRUE) +
      geom_line(aes(y = MACD), colour = "#254b83", linewidth = 0.8, na.rm = TRUE) +
      geom_line(aes(y = MACDSignal), colour = "#ed8b24", linewidth = 0.8, na.rm = TRUE) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      labs(x = NULL, y = "MACD") + theme_minimal())
  })
  output$signal_table <- renderTable({
    df <- calculated()
    x <- tail(df[, c("Date", "Close", "ShortSMA", "LongSMA", "Signal")], 12)
    x$Close <- round(x$Close, 2)
    x$ShortSMA <- round(x$ShortSMA, 2)
    x$LongSMA <- round(x$LongSMA, 2)
    x
  }, striped = TRUE, bordered = FALSE, spacing = "s")
}

shinyApp(ui = ui, server = server)

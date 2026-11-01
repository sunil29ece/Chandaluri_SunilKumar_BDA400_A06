# BDA400 Assignment 6 — Technical Analysis using R (Visualization Phase)

**Student:** SunilKumar Chandaluri  
**Course:** BDA400 — Data Science Tools and Techniques  
**Assignment:** A06 — Visualization Phase (15%)  
**Repository to create:** https://github.com/sunil29ece/Chandaluri_SunilKumar_BDA400_A06

## Project overview

A Shiny dashboard that downloads stock OHLC history from Yahoo Finance (with Stooq fallback), supports line/area/candlestick charts, aggregates daily/weekly/monthly bars, overlays SMA and EMA, shows RSI and MACD panels, and annotates true moving-average crossover Buy/Sell signals.

## Installation

1. Install R and RStudio.
2. In the RStudio Console run:

```r
install.packages(c("shiny", "ggplot2", "quantmod", "TTR"))
```

3. Put `app.R` in your Assignment 6 folder and run:

```r
setwd("D:/OSAP/BDA400/Assignment6")
shiny::runApp(".")
```

Alternatively, open `app.R` in RStudio and click **Run App**. Internet access is required to fetch live market history.

## How to test each rubric section

1. **Data Collection (20):** Enter AAPL, select a date range of at least one year, click **Fetch / refresh prices**, and verify the source label and number of bars. Try an invalid symbol to see a friendly error.
2. **Visualization (30):** Change Daily/Weekly/Monthly, switch Line/Area/Candlestick, and verify the chart updates. Test MSFT or NVDA.
3. **Indicators (25):** Toggle SMA, EMA, RSI, and MACD independently. SMA/EMA appear on price chart; RSI and MACD appear in their own panels to preserve their distinct scales.
4. **Trading (25):** Change short/long MA periods (short must be less than long). Toggle Buy/Sell annotations; verify annotations only appear when the short SMA crosses the long SMA. Hold means no crossing on that bar.

## Notes and limitations

- Yahoo Finance is the primary data source. Stooq is attempted if Yahoo fails. Data-provider availability, rate limits, and market holidays can affect results.
- OHLC data are aggregated before indicator calculations for the selected time frame. Indicators use TTR (which may seed EMA differently from the custom Assignment 5 implementation).
- MACD uses 12/26/9 EMA periods. RSI defaults to 14. SMA/EMA periods are adjustable.
- Signal calculation uses previous and current short/long SMA comparisons, not merely relative position.
- Warm-up periods produce `NA` until enough historical bars exist. The app displays informative validation messages for insufficient data.
- The project is for academic demonstration, not financial advice.

## Submission checklist

- [ ] Run `app.R` locally and capture screenshots showing price chart, indicator panels, and Buy/Sell annotations.
- [ ] Create a public GitHub repository named `Chandaluri_SunilKumar_BDA400_A06`.
- [ ] Upload `app.R`, `README.md`, and `Chandaluri_SunilKumar_BDA400_A06.pdf`.
- [ ] Verify the repository link opens publicly; replace the proposed link on the cover page if you choose another name.
- [ ] Submit the PDF cover page and GitHub URL through LMS before Session 15.

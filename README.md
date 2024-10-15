# Copula-based Trading Strategy

This project implements a copula-based trading strategy using R and v0.dev, focusing on modeling dependencies between pairs of stocks. The strategy evaluates different copula models (Student-t, Clayton, Gumbel) to identify the optimal model for trading decisions, specifically using Amazon (AMZN) and Google (GOOG) stocks.

## Overview

- **Language**: R
- **Platform**: v0.dev, Shiny
- **Stocks Analyzed**: AMZN, GOOG
- **Final Capital**: $297.59
- **Volatility of Returns**: 0.8618
- **Best Copula Model**: Student-t

## Features

- **Copula Model Evaluation**:
  - Evaluates multiple copula models including Student-t, Clayton, and Gumbel.
  - Selects the best model based on AIC, BIC, and log-likelihood scores.

- **Hedge Ratio Calculation**:
  - Uses the Johansen cointegration test to compute hedge ratios for stock pairs.

- **Shiny App Integration**:
  - Interactive dashboard for displaying copula model comparisons and stock performance.
  - Visualizations include:
    - Comparison of AMZN and GOOG stock prices.
    - Equity curve and returns plot based on the trading strategy.

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/copula-trading-strategy.git
cd copula-trading-strategy
library(shiny)
shiny::runApp("app.R")
## Usage

- Input the stock symbols, training period, testing period, and transaction costs.
- The app will automatically fetch the data and analyze the best copula model for the given stock pair.
- The results will include:
  - **Final Capital**
  - **Volatility of Returns**
  - **Best Copula Model**
  - **Sharpe Ratio**
  - **Hedge Ratios**

## Results Summary

- **Final Capital**: $297.59
- **Volatility of Returns**: 0.8618
- **Best Copula Model**: Student-t

## Requirements

- R (version >= 4.0)
- R packages:
  - `quantmod`
  - `copula`
  - `shiny`
  - `PerformanceAnalytics`
  - `tseries`
  - `urca`

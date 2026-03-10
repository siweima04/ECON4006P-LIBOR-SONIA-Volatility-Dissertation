# ECON4006P Dissertation: GBP Interest Rate Swap Volatility

## Overview
This repository contains all replication materials for the dissertation
"The End of a Benchmark: An Analysis of GBP Interest Rate Swap Volatility
Before and After the LIBOR to SONIA Transition", submitted in partial
fulfilment of ECON4006P: Economics Dissertation 2025/26,
University of Glasgow, Adam Smith Business School.

## Repository Contents
- `swap_rates.xlsx` — Daily GBP par swap rates (2Y, 5Y, 10Y) across
  nine series: LIBOR (Jan 2017–Dec 2021), SONIA pre-2022
  (Jan 2017–Dec 2021), and SONIA post-2022 (Jan 2022–Dec 2025).
  Sourced from Bloomberg Terminal. Contains nine sheets:
  'libor 2y', 'libor 5y', 'libor 10y', 'sonia 2y pre 2022',
  'sonia 5y pre 2022', 'sonia 10y pre 2022', 'sonia 2y',
  'sonia 5y', 'sonia 10y'.

- `dissertation_analysis.R` — Single R script containing all
  empirical analysis, organised into the following sections:
    - Section A: Descriptive statistics (swap rate levels)
    - Section B: Time-series plots of daily changes
    - Section C: ACF and PACF plots (all 9 series)
    - Section D: Mean equation selection via BIC, AIC and Ljung-Box
    - Section E: Histograms of daily swap rate changes
    - Section F: GARCH(1,1) baseline estimation (all 9 series)
    - Section G: Event date definitions (MPC, COVID, LDI windows)
    - Section H: Dummy-augmented GARCH(1,1) — pre-2022
    - Section I: Dummy-augmented GARCH(1,1) — post-2022 SONIA
    - Section J: Conditional volatility plots

## How to Run
1. Download both files and save them in the same folder
2. Open `dissertation_analysis.R` in R or RStudio
3. Update the `file_path` variable at the top of the script
   to point to the location of `swap_rates.xlsx`, e.g.:
   file_path <- "C:/your/folder/swap_rates.xlsx"
4. Install required packages if not already installed:
   install.packages(c("readxl", "rugarch", "PerformanceAnalytics"))
5. Run the full script

## Author
Student ID: 2763329
Supervisor: Professor Christian Ewald
University of Glasgow, Adam Smith Business School

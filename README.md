# Motor Insurance Portfolio Analysis
**STAT1011Y – Statistical Methods | University of Mauritius | 2025/2026**

A group statistical analysis of a 1,200-policy motor insurance portfolio and daily claim count data, completed as part of the BSc Actuarial Studies programme. All analysis was done in R.

---

## Project Overview

| Problem | Topic | Method |
|---|---|---|
| 1 | Risk Profiling & Dimension Reduction | Principal Component Analysis (PCA) |
| 2 | Association Between Categorical Variables | Pearson's Chi-Square Test of Independence |
| 3 | Claim Frequency Analysis | Poisson Goodness-of-Fit Test |

---

## Problem 1 – Risk Profiling (PCA)

Explored the underlying risk structure of the motor insurance portfolio using PCA on 8 variables (age, vehicle age, premium, no-claims discount, past claims count, past total paid, annual mileage, engine power).

**Key findings:**
- 3 principal components retained, explaining ~55% of total variance
- PC1 captures claims history vs. no-claims discount (high-risk vs. low-risk policyholders)
- PC2 captures vehicle power and usage patterns
- PC3 captures driver and vehicle age effects
- Robustness confirmed by repeating PCA without premium — component structure remained stable
- Distinct risk profiles identified: Young Power Drivers, High-Mileage Commercial Users, Aged Risk, Low Discount High Claim

---

## Problem 2 – Chi-Square Test of Independence

Investigated whether claim type (Bodily Injury, Other, Property, Windscreen) and transmission type (Automatic, Manual) are independent.

**Key findings:**
- χ² = 8.0551, df = 3, p-value = 0.0449
- Rejected H₀ at the 5% level — evidence of association between claim type and transmission type
- Standardised residuals showed Windscreen claims on Automatic vehicles (residual = 2.45) as the primary driver of the association

---

## Problem 3 – Poisson Claim Frequency Analysis

Assessed whether daily claim counts from 365 days of portfolio data follow a Poisson distribution.

**Key findings:**
- Mean and variance of daily counts compared to assess Poisson suitability
- Visual diagnostics: histogram overlaid with expected Poisson frequencies, QQ-plot
- Chi-square goodness-of-fit test performed to formally assess model adequacy

---

## Tools & Packages

- **Language:** R
- **Packages:** `tidyverse`, `ggplot2`, `FactoMineR`, `factoextra`, `corrplot`, `psych`, `naniar`, `gridExtra`

---

## Files

```
├── data/
│   ├── umu_motor_portfolio_2025.csv       # 1,200-policy motor portfolio
│   └── umu_daily_claim_counts_2025.csv    # 365 days of daily claim counts
├── code/
│   ├── STAT1011Y_Assignment.R
└── report/
    └── STAT1011Y_Assignment_2025.pdf
```

---

## Authors

Bhaavika Ramlugun — BSc (Hons) Actuarial Studies, University of Mauritius
Kirpal Vaiydehi — BSc (Hons) Actuarial Studies, University of Mauritius
Reaoboka Enoch Thamae — BSc (Hons) Actuarial Studies, University of Mauritius
Reenela Ramasawmy — BSc (Hons) Actuarial Studies, University of Mauritius
Vaishavi — BSc (Hons) Actuarial Studies, University of Mauritius


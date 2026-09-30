# ============================================================
# ECON1066 Basic Econometrics
# Cross-country determinants of methane emissions (2022)
# Author: Gia Huy Dang
# Run from the repository root: Rscript methane_regression.R
# ============================================================

rm(list = ls(all = TRUE))

# -- 1. Load and clean data ----------------------------------
# World Bank WDI extract, 166 countries, 2022 (see data/README.md)
data <- read.csv("data/methane_emissions_2022.csv", check.names = FALSE)

names(data)[names(data) == "FertilzerUse"]          <- "FertilizerUse"
names(data)[names(data) == "TotalMethane Emission"] <- "TotalMethaneEmission"
names(data)[names(data) == "AgriValueAddedin"]      <- "AgriValueAdded"

data$logMethane <- log(data$TotalMethaneEmission)
data$logGDPpc   <- log(data$GDPpc)
data$logPop     <- log(data$TotalPop)
data$logFert    <- log(data$FertilizerUse)

# ============================================================
# HELPER: render character vector as clean plot figure
# ============================================================
draw_text_figure <- function(lines, title = "") {
  par(mar = c(0.5, 0.5, 1.8, 0.5), bg = "white")
  plot.new()
  plot.window(xlim = c(0, 1.15), ylim = c(0, 1))
  
  if (nchar(title) > 0)
    text(0.5, 0.98, title, font = 2, cex = 0.9, adj = c(0.5, 1),
         family = "sans")
  
  n       <- length(lines)
  y_start <- 0.91
  y_end   <- 0.03
  y_pos   <- seq(y_start, y_end, length.out = n)
  
  for (i in seq_along(lines)) {
    ln       <- lines[i]
    is_sep   <- grepl("^[=\\-]{4,}$", trimws(ln))
    is_hdr   <- grepl("^\\s*(Variable|Coef|Param|Test)", ln)
    col_txt  <- ifelse(is_sep, "gray70", "black")
    font_use <- ifelse(is_hdr, 2, 1)
    cex_use  <- ifelse(is_sep, 0.55, 0.68)
    
    text(0.02, y_pos[i], ln,
         adj    = c(0, 0.5),
         cex    = cex_use,
         family = "mono",
         font   = font_use,
         col    = col_txt)
  }
}

# ============================================================
# FIGURE 1: Equation 1 regression table
# ============================================================
# -- Estimate Equation 1 -------------------------------------
eq1 <- lm(logMethane ~ logGDPpc + logPop + LiveStockIndex
          + AgriValueAdded + logFert, data = data)

s      <- summary(eq1)
co     <- s$coefficients
n_obs  <- nobs(eq1)
r2     <- s$r.squared
adj_r2 <- s$adj.r.squared
fstat  <- s$fstatistic
fp     <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)

sig_s <- function(p)
  ifelse(p < 0.001, "***", ifelse(p < 0.01,  "** ",
                                  ifelse(p < 0.05,  "*  ", ifelse(p < 0.10,  ".  ", "   "))))

vlabs <- c("(Intercept)"    = "Intercept",
           "logGDPpc"       = "log(GDP per capita)",
           "logPop"         = "log(Population)",
           "LiveStockIndex" = "Livestock Index",
           "AgriValueAdded" = "Agri. Value Added (% GDP)",
           "logFert"        = "log(Fertiliser Use)")

SEP  <- paste(rep("=", 86), collapse = "")
DIV  <- paste(rep("-", 86), collapse = "")
HDR  <- sprintf(" %-26s %9s %8s %8s %8s  %s",
                "Variable", "Estimate", "Std.Err", "t-val", "p-val", "Sig.")

rows <- c(SEP, HDR, DIV)
for (v in rownames(co)) {
  lb   <- ifelse(v %in% names(vlabs), vlabs[v], v)
  rows <- c(rows, sprintf(" %-26s %9.4f %8.4f %8.3f %8.4f  %s",
                          lb, co[v,1], co[v,2], co[v,3],
                          co[v,4],
                          sig_s(co[v,4])))
}
rows <- c(rows, DIV,
          " Signif: *** p<0.001  ** p<0.01  * p<0.05  . p<0.10",
          DIV,
          sprintf(" n = %d    R2 = %.4f    Adj-R2 = %.4f", n_obs, r2, adj_r2),
          sprintf(" F(%g,%g) = %.2f    p-value: %.2e",
                  fstat[2], fstat[3], fstat[1], fp),
          SEP)

draw_text_figure(rows,
                 "Figure 1: Equation 1 - Cross-Country Methane Emissions (n = 166)")

# ============================================================
# FIGURE 2: Coefficient estimates with 95% CIs
# ============================================================
ci  <- confint(eq1, level = 0.95)
rn  <- c("Intercept", "log(GDPpc)", "log(Pop)",
         "Livestock Idx", "AgriValueAdded", "log(Fert)")

SEP2 <- paste(rep("=", 82), collapse = "")
DIV2 <- paste(rep("-", 82), collapse = "")
HDR2 <- sprintf(" %-16s %9s %8s %8s %9s %9s",
                "Coefficient", "Estimate", "Std.Err",
                "t-value", "CI 2.5%", "CI 97.5%")

rows2 <- c(SEP2, HDR2, DIV2)
for (i in seq_along(rn)) {
  v     <- rownames(co)[i]
  rows2 <- c(rows2,
             sprintf(" %-16s %9.4f %8.4f %8.3f %9.4f %9.4f",
                     rn[i], co[v,1], co[v,2], co[v,3], ci[v,1], ci[v,2]))
}
rows2 <- c(rows2, SEP2)

draw_text_figure(rows2,
                 "Figure 2: Coefficient Estimates with 95% Confidence Intervals")

# ============================================================
# FIGURE 3: Exact % change - AgriValueAdded 
# ============================================================
b4        <- coef(eq1)["AgriValueAdded"]
exact_pct <- 100 * (exp(b4 * 1) - 1)
apprx_pct <- 100 * b4

SEP3 <- paste(rep("=", 72), collapse = "")
DIV3 <- paste(rep("-", 72), collapse = "")

rows3 <- c(
  SEP3,
  " Formula : %Dy = 100 * [exp(b4 * Dx) - 1]",
  " where Dx = 1 percentage-point increase in AgriValueAdded",
  DIV3,
  sprintf(" b4  (AgriValueAdded)         = %10.6f", b4),
  sprintf(" Exact   pct. change          = %10.4f %%", exact_pct),
  sprintf(" Approx  (100 * b4)           = %10.4f %%", apprx_pct),
  sprintf(" Difference                   = %10.6f pp  (negligible)",
          abs(exact_pct - apprx_pct)),
  SEP3)

draw_text_figure(rows3,
                 "Figure 3: Exact Percentage Change - AgriValueAdded")

# ============================================================
# FIGURE 4: Correlation heatmap
# ============================================================
reg_vars  <- c("logGDPpc","logPop","LiveStockIndex",
               "AgriValueAdded","logFert")
plot_labs <- c("log(GDPpc)","log(Pop)","Livestock\nIndex",
               "Agri. Value\nAdded","log(Fert)")
cor_full  <- cor(data[, reg_vars])
n_vars    <- length(reg_vars)
pal       <- colorRampPalette(c("#2166AC","#F7F7F7","#D6604D"))(200)

par(mar = c(5, 7, 4, 3), bg = "white")
image(1:n_vars, 1:n_vars,
      t(cor_full[n_vars:1, ]),
      col  = pal, zlim = c(-1, 1),
      xaxt = "n", yaxt = "n",
      xlab = "", ylab = "",
      main = "Figure 4: Correlation Heatmap - Equation 1 Regressors")

axis(1, at = 1:n_vars, labels = plot_labs, cex.axis = 0.80, tick = FALSE)
axis(2, at = 1:n_vars, labels = rev(plot_labs), cex.axis = 0.80,
     las = 2, tick = FALSE)

for (i in 1:n_vars) {
  for (j in 1:n_vars) {
    rv <- round(cor_full[j, i], 3)
    tc <- ifelse(abs(rv) > 0.4, "white", "black")
    tf <- ifelse(abs(rv) >= 0.8, 2, 1)
    text(i, n_vars + 1 - j, sprintf("%.3f", rv),
         cex = 0.78, col = tc, font = tf)
  }
}

# ============================================================
# FIGURE 5: Manual t-test - log(Population) 
# ============================================================
b2     <- coef(eq1)["logPop"]
se2    <- sqrt(diag(vcov(eq1)))["logPop"]
df_res <- nobs(eq1) - length(coef(eq1))
cv5    <- qt(0.975, df_res)
cv1    <- qt(0.995, df_res)
ci2    <- confint(eq1)["logPop", ]

b2_r  <- round(b2,  4)   
se2_r <- round(se2, 4)   
t2_r  <- round(b2_r / se2_r, 2)   

t_unit <- (b2 - 1) / se2
p_unit <- 2 * pt(-abs(t_unit), df_res)

SEP5 <- paste(rep("=", 72), collapse = "")
DIV5 <- paste(rep("-", 72), collapse = "")

rows5 <- c(
  SEP5,
  " Test 1 - H0: beta2 = 0    vs    H1: beta2 != 0",
  DIV5,
  sprintf(" b2   (logPop)               = %.4f", b2_r),
  sprintf(" se(b2)                      = %.4f", se2_r),
  sprintf(" t    = b2 / se(b2)          = %.4f / %.4f = %.2f",
          b2_r, se2_r, t2_r),
  sprintf(" df   = n - k - 1            = 166 - 5 - 1 = %d", df_res),
  DIV5,
  sprintf(" Critical value alpha = 5%%   = %.4f", cv5),
  sprintf(" Critical value alpha = 1%%   = %.4f", cv1),
  sprintf(" Decision: |t| = %.2f >> %.4f  ->  Reject H0 at alpha = 1%%",
          t2_r, cv1),
  SEP5)
draw_text_figure(rows5,
                 "Figure 5: Manual t-Test - log(Population)")

data$logGDPpc_sq <- data$logGDPpc^2

# ============================================================
# FIGURE 6: Equation 2 regression table
# ============================================================
data$logGDPpc_sq <- data$logGDPpc^2
eq2 <- lm(logMethane ~ logGDPpc + logGDPpc_sq + logPop + LiveStockIndex
          + AgriValueAdded + logFert, data = data)

s2       <- summary(eq2)
co2      <- s2$coefficients
n2       <- nobs(eq2)
r2_2     <- s2$r.squared
adj_r2_2 <- s2$adj.r.squared
fstat2   <- s2$fstatistic
fp2      <- pf(fstat2[1], fstat2[2], fstat2[3], lower.tail = FALSE)

vlabs2 <- c("(Intercept)"    = "Intercept",
            "logGDPpc"       = "log(GDP per capita)",
            "logGDPpc_sq"    = "[log(GDP per capita)]^2",
            "logPop"         = "log(Population)",
            "LiveStockIndex" = "Livestock Index",
            "AgriValueAdded" = "Agri. Value Added (% GDP)",
            "logFert"        = "log(Fertiliser Use)")

SEP6 <- paste(rep("=", 87), collapse = "")
DIV6 <- paste(rep("-", 87), collapse = "")
HDR6 <- sprintf(" %-26s %9s %8s %8s %8s  %s",
                "Variable", "Estimate", "Std.Err", "t-val", "p-val", "Sig.")

rows6 <- c(SEP6, HDR6, DIV6)
for (v in rownames(co2)) {
  lb    <- ifelse(v %in% names(vlabs2), vlabs2[v], v)
  rows6 <- c(rows6, sprintf(" %-26s %9.4f %8.4f %8.3f %8.4f  %s",
                            lb, co2[v,1], co2[v,2], co2[v,3],
                            co2[v,4],
                            sig_s(co2[v,4])))
}
rows6 <- c(rows6, DIV6,
           " Signif: *** p<0.001  ** p<0.01  * p<0.05  . p<0.10",
           DIV6,
           sprintf(" n = %d    R2 = %.4f    Adj-R2 = %.4f", n2, r2_2, adj_r2_2),
           sprintf(" F(%g,%g) = %.2f    p-value: %.2e",
                   fstat2[2], fstat2[3], fstat2[1], fp2),
           SEP6)

draw_text_figure(rows6,
                 "Figure 6: Equation 2 - Quadratic in log(GDP per capita) (n = 166)")

# ============================================================
# FIGURE 7: Turning point calculation 
# ============================================================
b1_eq2       <- coef(eq2)["logGDPpc"]
b2_eq2       <- coef(eq2)["logGDPpc_sq"]
x_star_log   <- -b1_eq2 / (2 * b2_eq2)
x_star_level <- exp(x_star_log)
max_gdppc    <- max(data$GDPpc)
pct_below    <- mean(data$GDPpc <= x_star_level) * 100

SEP7 <- paste(rep("=", 79), collapse = "")
DIV7 <- paste(rep("-", 79), collapse = "")

rows7 <- c(
  SEP7,
  " Turning Point Formula:  x* = -b1 / (2 * b2)",
  DIV7,
  sprintf(" b1  [log(GDPpc)]          = %+.6f", b1_eq2),
  sprintf(" b2  [log(GDPpc)]^2        = %+.6f", b2_eq2),
  DIV7,
  sprintf(" x*  = -(%.6f) / (2 * %.6f)", b1_eq2, b2_eq2),
  sprintf("    = %.6f  [in log(GDPpc) units]", x_star_log),
  sprintf(" exp(x*)                   = $%s  [GDPpc level]",
          formatC(x_star_level, format = "f", digits = 2, big.mark = ",")),
  DIV7,
  sprintf(" Max GDPpc in sample       = $%s",
          formatC(max_gdppc, format = "f", digits = 2, big.mark = ",")),
  sprintf(" Ratio x* / Max GDPpc      = %.0fx", x_star_level / max_gdppc),
  sprintf(" %% of sample below x*     = %.1f%%", pct_below),
  DIV7,
  " -> Turning point is ~13,800x the maximum observed GDPpc.",
  " -> Inverted-U not realised within empirical support.",
  " -> Relationship is monotonically increasing over observed range.",
  SEP7)

draw_text_figure(rows7,
                 "Figure 7: Turning Point Calculation")

# ============================================================
# FIGURE 8: Significance tests - two layers 
# ============================================================
library(car)
ftest  <- linearHypothesis(eq2, c("logGDPpc = 0", "logGDPpc_sq = 0"))
F_stat <- round(ftest[2, "F"], 4)
F_p    <- ftest[2, "Pr(>F)"]
F_df1  <- 2
F_df2  <- ftest[2, "Res.Df"]

t_b2       <- co2["logGDPpc_sq", "t value"]
p_b2       <- co2["logGDPpc_sq", "Pr(>|t|)"]
t_b1_eq1   <- summary(eq1)$coefficients["logGDPpc", "t value"]
adj_r2_eq1 <- summary(eq1)$adj.r.squared

SEP8 <- paste(rep("=", 82), collapse = "")
DIV8 <- paste(rep("-", 82), collapse = "")

rows8 <- c(
  SEP8,
  " Layer 1 - Individual t-test on [log(GDPpc)]^2",
  " H0: b2 = 0  (no significant curvature)",
  DIV8,
  sprintf(" t-statistic               = %+.4f", t_b2),
  sprintf(" p-value                   = %.6f", p_b2),
  sprintf(" Decision: p = %.4f > 0.05  ->  Fail to reject H0", p_b2),
  " Curvature is NOT individually significant.",
  DIV8,
  " Layer 2 - Joint F-test",
  " H0: b_logGDPpc = 0  AND  b_logGDPpc_sq = 0",
  DIV8,
  sprintf(" F(%d, %d)                  = %.4f", F_df1, F_df2, F_stat),
  sprintf(" p-value                   = %.6f", F_p),
  " Decision: p < 0.001  ->  Reject H0 at alpha = 1%",
  DIV8,
  " Interpretation of apparent conflict:",
  sprintf(" log(GDPpc) without squared term: t = %.3f (individually sig.)",
          t_b1_eq1),
  " F-test rejection reflects the underlying linear GDPpc relationship,",
  " not evidence of significant curvature.",
  " The squared term remains individually insignificant (p = 0.696).",
  DIV8,
  sprintf(" Adj-R2: Eq1 = %.4f   Eq2 = %.4f   Change = %+.4f",
          adj_r2_eq1, adj_r2_2, adj_r2_2 - adj_r2_eq1),
  " Adj-R2 decreased -> squared term penalised, adds no value.",
  DIV8,
  " Conclusion: Quadratic adds no significant curvature beyond Eq1.",
  " Log-linear specification (Equation 1) is preferred.",
  SEP8)

draw_text_figure(rows8,
                 "Figure 8: Significance Tests - Quadratic GDPpc Term")

# ============================================================
# FIGURE 9: Scatter plot logMethane vs logGDPpc (Q1g Step 6)
# ============================================================
fit_uni <- lm(logMethane ~ logGDPpc + logGDPpc_sq, data = data)
xs      <- seq(min(data$logGDPpc), max(data$logGDPpc), length.out = 200)
ys      <- predict(fit_uni,
                   newdata = data.frame(logGDPpc    = xs,
                                        logGDPpc_sq = xs^2))

par(mar = c(5, 5, 4, 2), bg = "white")
plot(data$logGDPpc, data$logMethane,
     pch  = 16, cex = 0.7, col = "gray40",
     xlab = "log(GDP per capita)",
     ylab = "log(Total Methane Emissions)",
     main = "Figure 9: log(Methane) vs log(GDPpc) with Fitted Quadratic Curve")

lines(xs, ys, col = "#D6604D", lwd = 2.5)

legend("topleft",
       legend = c("Observed (n = 166)",
                  "Fitted curve: bivariate Eq. 2 [log(GDPpc) + log(GDPpc)^2]",
                  "Note: other regressors excluded for 2D visualisation"),
       pch    = c(16, NA, NA),
       lty    = c(NA, 1, NA),
       lwd    = c(NA, 2.5, NA),
       col    = c("gray40", "#D6604D", NA),
       cex    = 0.72, bty = "n")

# ============================================================
# FIGURE 10: Correlation matrix - numeric 
# ============================================================
reg_vars  <- c("logGDPpc","logPop","LiveStockIndex",
               "AgriValueAdded","logFert")
var_short <- c("logGDPpc","logPop","LvstIdx","AgriVA","logFert")

cor_mat           <- round(cor(data[, reg_vars]), 4)
rownames(cor_mat) <- var_short
colnames(cor_mat) <- var_short

SEP10 <- paste(rep("=", 74), collapse = "")
DIV10 <- paste(rep("-", 74), collapse = "")
hdr10 <- sprintf(" %-9s %9s %9s %9s %9s %9s",
                 "", "logGDPpc", "logPop", "LvstIdx", "AgriVA", "logFert")

rows10 <- c(SEP10, hdr10, DIV10)
for (i in seq_along(var_short)) {
  row_line <- sprintf(" %-10s", var_short[i])
  for (j in seq_along(var_short)) {
    val      <- cor_mat[i, j]
    cell     <- ifelse(abs(val) >= 0.8 & i != j,
                       sprintf("[%6.4f]", val),
                       sprintf(" %6.4f ", val))
    row_line <- paste0(row_line, sprintf(" %9s", cell))
  }
  rows10 <- c(rows10, row_line)
}

rows10 <- c(rows10, DIV10,
            " Note: [ ] denotes |r| >= 0.8",
            sprintf(" Key:  r(logGDPpc, AgriVA) = %.4f",
                    cor_mat["logGDPpc", "AgriVA"]),
            SEP10)

draw_text_figure(rows10,
                 "Figure 10: Correlation Matrix - Equation 1 Regressors")

# Variance inflation factors for Equation 1 (added for the portfolio version)
print(car::vif(eq1))

# ============================================================
# FIGURE 11: Heteroskedasticity tests - MLR.5 diagnostic 
# ============================================================
library(lmtest)

# Breusch-Pagan test
bp <- bptest(eq1)

# White test (auxiliary regression on fitted values and squared fitted values)
wh <- bptest(eq1, ~ fitted(eq1) + I(fitted(eq1)^2))

SEP11 <- paste(rep("=", 85), collapse = "")
DIV11 <- paste(rep("-", 85), collapse = "")

rows11 <- c(
  SEP11,
  " Heteroskedasticity Tests for Equation 1 (MLR.5 Diagnostic)",
  DIV11,
  " Test 1 - Breusch-Pagan Test",
  " H0: Var(u | X) = sigma^2  (homoskedasticity)",
  DIV11,
  sprintf(" BP statistic (LM)         = %.4f", bp$statistic),
  sprintf(" df                        = %d",   bp$parameter),
  sprintf(" p-value                   = %.4f", bp$p.value),
  sprintf(" Decision: p = %.4f > 0.05  ->  Fail to reject H0", bp$p.value),
  DIV11,
  " Test 2 - White Test",
  " H0: Var(u | X) = sigma^2  (homoskedasticity)",
  DIV11,
  sprintf(" White statistic (LM)      = %.4f", wh$statistic),
  sprintf(" df                        = %d",   wh$parameter),
  sprintf(" p-value                   = %.4f", wh$p.value),
  sprintf(" Decision: p = %.4f > 0.05  ->  Fail to reject H0", wh$p.value),
  DIV11,
  " Conclusion: Both tests fail to reject homoskedasticity at alpha = 5%.",
  " Log transformation appears sufficient to compress scale-driven variance.",
  SEP11)

draw_text_figure(rows11,
                 "Figure 11: Heteroskedasticity Tests - Equation 1")

# ============================================================
# FIGURE 12: Equation 3 regression table
# ============================================================
eq3 <- lm(TotalMethaneEmission ~ GDPpc + TotalPop + LiveStockIndex
          + AgriValueAdded + logFert, data = data)

s3       <- summary(eq3)
co3      <- s3$coefficients
n3       <- nobs(eq3)
r2_3     <- s3$r.squared
adj_r2_3 <- s3$adj.r.squared
fstat3   <- s3$fstatistic
fp3      <- pf(fstat3[1], fstat3[2], fstat3[3], lower.tail = FALSE)

vlabs3 <- c("(Intercept)"    = "Intercept",
            "GDPpc"          = "GDP per capita",
            "TotalPop"       = "Population (persons)",
            "LiveStockIndex" = "Livestock Index",
            "AgriValueAdded" = "Agri. Value Added (% GDP)",
            "logFert"        = "log(Fertiliser Use)")

SEP12 <- paste(rep("=", 95), collapse = "")
DIV12 <- paste(rep("-", 95), collapse = "")
HDR12 <- sprintf(" %-26s %11s %10s %9s %9s  %s",
                 "Variable", "Estimate", "Std.Err", "t-val", "p-val", "Sig.")

rows12 <- c(SEP12, HDR12, DIV12)
for (v in rownames(co3)) {
  lb     <- ifelse(v %in% names(vlabs3), vlabs3[v], v)
  rows12 <- c(rows12, sprintf(" %-26s %12.4e %11.4e %8.3f %8.4f  %s",
                              lb, co3[v,1], co3[v,2], co3[v,3],
                              co3[v,4],
                              sig_s(co3[v,4])))
}
rows12 <- c(rows12, DIV12,
            " Signif: *** p<0.001  ** p<0.01  * p<0.05  . p<0.10",
            DIV12,
            sprintf(" n = %d    R2 = %.4f    Adj-R2 = %.4f", n3, r2_3, adj_r2_3),
            sprintf(" F(%g,%g) = %.2f    p-value: %.2e",
                    fstat3[2], fstat3[3], fstat3[1], fp3),
            SEP12)

draw_text_figure(rows12,
                 "Figure 12: Equation 3 - Level Specification (n = 166)")

# ============================================================
# FIGURE 13: Equation 3 coefficients with 95% CIs 
# ============================================================
ci3t <- confint(eq3, level = 0.95)
rn3  <- c("Intercept", "GDPpc", "Population", "LivestockIdx",
          "AgriValueAdded", "log(Fert)")

SEP13 <- paste(rep("=", 96), collapse = "")
DIV13 <- paste(rep("-", 96), collapse = "")
HDR13 <- sprintf(" %-16s %12s %11s %8s %11s %12s",
                 "Coefficient", "Estimate", "Std.Err",
                 "t-value", "CI 2.5%", "CI 97.5%")

rows13 <- c(SEP13, HDR13, DIV13)
for (i in seq_along(rn3)) {
  v      <- rownames(co3)[i]
  rows13 <- c(rows13,
              sprintf(" %-16s %12.4e %11.4e %8.3f %12.4e %12.4e",
                      rn3[i], co3[v,1], co3[v,2], co3[v,3],
                      ci3t[v,1], ci3t[v,2]))
}
rows13 <- c(rows13, SEP13)

draw_text_figure(rows13,
                 "Figure 13: Equation 3 - Coefficients with 95% CIs")
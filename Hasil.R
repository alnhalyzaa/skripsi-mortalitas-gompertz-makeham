library(readxl)
library(ggplot2)

## **Menginput data Laki-laki**
datal <- read_excel("C:/Users/LENOVO/Documents/Semester 7/TA SHALALLAHU ALA MUHAMMAD/Data.xlsx", sheet = "Laki-laki")
str(datal)

xl <- datal$x
qxl <- datal$qx
pxl <- 1 - datal$qx
tail(pxl)

## **Menginput data Perempuan**
datap <- read_excel("C:/Users/LENOVO/Documents/Semester 7/TA SHALALLAHU ALA MUHAMMAD/Data.xlsx", sheet = "Perempuan")
str(datap)

xp <- datap$x
qxp <- datap$qx
pxp <- 1 - datap$qx
head(pxp)
n <- length(xp)

# 1 ############################################################
## **Iterasi Metode Gompertz OLS**
S_function1 <- function(B, C, x) {
  exp(-B * C^x / log(C) * (C - 1))
}

F11 <- function(B, C, Y, x, N) {
  S <- S_function1(B, C, x)
  sum(2 * (Y - S) * (C^x * (C - 1) / log(C)) * S)
}

F12 <- function(B, C, Y, x, N) {
  S <- S_function1(B, C, x)
  sum(2 * (Y - S) * (-B * C^(x-1) * ((C-1) + (x + C) * log(C)) / (log(C)^2)) * S)
}

secant_method1 <- function(Y, x, B0, C0, B1, C1, tol = 1e-5, max_iter = 1000) {
  iter <- 0
  
  while (iter < max_iter) {
    if (C1 <= 1) {
      warning("C mendekati atau kurang dari 1")
      break
    }
    
    F1_1 <- F11(B1, C1, Y, x, length(Y))
    F2_1 <- F12(B1, C1, Y, x, length(Y))
    
    F1_0 <- F11(B0, C0, Y, x, length(Y))
    F2_0 <- F12(B0, C0, Y, x, length(Y))
    
    if (is.nan(F1_1) || is.nan(F2_1) || is.infinite(F1_1) || is.infinite(F2_1)) {
      warning("Fungsi menghasilkan NaN atau Inf. Coba nilai awal lain.")
      return(NULL)
    }
    
    if ((F1_1 - F1_0) == 0 || (F2_1 - F2_0) == 0) {
      warning("Denominator nol, coba nilai awal lain.")
      return(NULL)
    }
    
    B_new <- B1 - (B1 - B0) / (F1_1 - F1_0) * F1_1
    C_new <- C1 - (C1 - C0) / (F2_1 - F2_0) * F2_1
    
    if (B_new <= 0 || C_new <= 1) {
      warning("Nilai B atau C tidak valid.")
      return(NULL)
    }
    
    if (abs(B_new - B1) < tol && abs(C_new - C1) < tol) {
      cat("Konvergen: B =", B_new, ", C =", C_new, "\n")
      return(list(B = B_new, C = C_new, iter = iter))
    }
    
    B0 <- B1
    C0 <- C1
    B1 <- B_new
    C1 <- C_new
    
    iter <- iter + 1
  }
  
  warning("Tidak ditemukan solusi yang valid setelah", max_iter, "iterasi.")
  return(NULL)
}

###  1. Laki-laki
hasil11 <- secant_method1(pxl, xl, B0 = 0.05, C0 = 1.04, B1 = 0.01, C1 = 1.03)
print(hasil11)

B11 <- hasil11$B
c11 <- hasil11$C

yduga11 <- 1 - exp(((-B11*c11^xl)/log(c11))*(c11-1))

#### Nilai MSE 
mse11 <- sum((datal$qx - yduga11)^2) / length(datal$qx)
cat("Mean Squared Error (MSE):", mse11, "\n")

#### Plotting
df1 <- data.frame(Index = 1:length(datal$qx), 
                 qx_Obs = datal$qx, 
                 qx_Estimasi = yduga11)

ggplot(df1, aes(x = Index)) +
  geom_point(aes(y = qx_Obs, color = "qx TMI"), size = 2) +
  geom_line(aes(y = qx_Estimasi, color = "qx Estimasi"), linewidth = 1) +
  labs(title = "Perbandingan TMI dengan Estimasi", subtitle = "Gompertz OLS Laki-laki",
       x = "Index", y = "Nilai qx") +
  scale_color_manual(values = c("qx TMI" = "blue", "qx Estimasi" = "red")) +
  theme_classic()

###  2. Perempuan
hasil12 <- secant_method1(pxp, xp, B0 = 0.01, C0 = 1.04, B1 = 0.02, C1 = 1.02)
print(hasil12)

B12 <- hasil12$B
c12 <- hasil12$C

yduga12 <- 1 - exp(((-B12*c12^xp)/log(c12))*(c12-1))

#### Nilai MSE 
mse12 <- sum((datap$qx - yduga12)^2) / length(datap$qx)
cat("Mean Squared Error (MSE):", mse12, "\n")

#### Plotting
df2 <- data.frame(Index = 1:length(datap$qx), 
                  qx_Obs = datap$qx, 
                  qx_Estimasi = yduga12)

ggplot(df2, aes(x = Index)) +
  geom_point(aes(y = qx_Obs, color = "qx TMI"), size = 2) +
  geom_line(aes(y = qx_Estimasi, color = "qx Estimasi"), linewidth = 1) +
  labs(title = "Perbandingan TMI dengan Estimasi", subtitle = "Gompertz OLS Perempuan",
  x = "Index", y = "Nilai qx") +
  scale_color_manual(values = c("qx TMI" = "blue", "qx Estimasi" = "red")) +
  theme_classic()

# 2 ############################################################
## **Iterasi Metode Gompertz WLS**
wt1 <- seq(1, n) / (n + 1)
wt2 <- (seq(1, n) - 0.3) / (n + 0.4)
wt3 <- (seq(1, n) - 0.5) / n
wt4 <- sapply(1:n, function(i) sum(1 / (n - (1:i) + 1)))

S_function2 <- function(B, C, x) {
  exp(-B * C^x / log(C) * (C - 1))
}

F21 <- function(B, C, Y, x, N, w) {
  S <- S_function2(B, C, x)
  sum(2 * w * (Y - S) * (C^x * (C - 1) / log(C)) * S)
}

F22 <- function(B, C, Y, x, N, w) {
  S <- S_function2(B, C, x)
  sum(2 * w * (Y - S) * (-B * C^(x-1) * ((C-1) + (x + C) * log(C)) / (log(C)^2)) * S)
}

secant_method2 <- function(Y, x, B0, C0, B1, C1, w, tol = 1e-5, max_iter = 1000) {
  iter <- 0
  
  while (iter < max_iter) {
    if (C1 <= 1 || B1 <= 0) {
      warning("Nilai B atau C tidak valid. Coba nilai awal lain.")
      return(NULL)
    }
    
    F1_1 <- F21(B1, C1, Y, x, length(Y), w)
    F2_1 <- F22(B1, C1, Y, x, length(Y), w)
    
    F1_0 <- F21(B0, C0, Y, x, length(Y), w)
    F2_0 <- F22(B0, C0, Y, x, length(Y), w)
    
    if (is.nan(F1_1) || is.nan(F2_1) || is.infinite(F1_1) || is.infinite(F2_1)) {
      warning("Fungsi menghasilkan NaN atau Inf. Coba nilai awal lain.")
      return(NULL)
    }
    
    if ((F1_1 - F1_0) == 0 || (F2_1 - F2_0) == 0) {
      warning("Denominator nol, coba nilai awal lain.")
      return(NULL)
    }
    
    B_new <- B1 - (B1 - B0) / (F1_1 - F1_0) * F1_1
    C_new <- C1 - (C1 - C0) / (F2_1 - F2_0) * F2_1
    
    if (B_new <= 0 || C_new <= 1) {
      warning("Nilai B atau C tidak valid.")
      return(NULL)
    }
    
    if (abs(B_new - B1) < tol && abs(C_new - C1) < tol) {
      cat("Konvergen: B =", B_new, ", C =", C_new, "\n")
      return(list(B = B_new, C = C_new, iter = iter))
    }
    
    B0 <- B1
    C0 <- C1
    B1 <- B_new
    C1 <- C_new
    
    iter <- iter + 1
  }
  
  warning("Tidak ditemukan solusi yang valid setelah", max_iter, "iterasi.")
  return(NULL)
}

###  1. Laki-laki
hasil211 <- secant_method2(pxl, xl, B0 = 0.0004, B1 =0.0002, C0 = 1.09, C1 = 1.08, w = wt1)
hasil212 <- secant_method2(pxl, xl, B0 = 0.0002, B1 =0.0001, C0 = 1.1, C1 = 1.09, w = wt2)
hasil213 <- secant_method2(pxl, xl, B0 = 0.0002, B1 =0.0001, C0 = 1.1, C1 = 1.09, w = wt3)
hasil214 <- secant_method2(pxl, xl, B0 = 0.0004, B1 =0.0002, C0 = 1.05, C1 = 1.09, w = wt4)

print(hasil211)
print(hasil212)
print(hasil213)
print(hasil214)

B211 <- hasil211$B
c211 <- hasil211$C

B212 <- hasil212$B
c212 <- hasil212$C

B213 <- hasil213$B
c213 <- hasil213$C

B214 <- hasil214$B
c214 <- hasil214$C

yduga211 <- 1 - exp(((-B211*c211^xl)/log(c211))*(c211-1))
yduga212 <- 1 - exp(((-B212*c212^xl)/log(c212))*(c212-1))
yduga213 <- 1 - exp(((-B213*c213^xl)/log(c213))*(c213-1))
yduga214 <- 1 - exp(((-B214*c214^xl)/log(c214))*(c214-1))

#### Nilai MSE 
k1 <- 2
mse211 <- sum(wt1*((datal$qx - yduga211)^2)) / (n - k1)
mse212 <- sum(wt2*((datal$qx - yduga212)^2)) / (n - k1)
mse213 <- sum(wt3*((datal$qx - yduga213)^2)) / (n - k1)
mse214 <- sum(wt4*((datal$qx - yduga214)^2)) / (n - k1)

cat("MSE Bobot 1:", mse211, "\n")
cat("MSE Bobot 2:", mse212, "\n")
cat("MSE Bobot 3:", mse213, "\n")
cat("MSE Bobot 4:", mse214, "\n")

###  2. Perempuan
hasil221 <- secant_method2(pxp, xp, B0 = 0.0008, B1 =0.0004, C0 = 1.1, C1 = 1.07, w = wt1)
hasil222 <- secant_method2(pxp, xp, B0 = 0.0001, B1 =0.0004, C0 = 1.07, C1 = 1.1, w = wt2)
hasil223 <- secant_method2(pxp, xp, B0 = 0.0001, B1 =0.0004, C0 = 1.07, C1 = 1.1, w = wt3)
hasil224 <- secant_method2(pxp, xp, B0 = 0.0002, B1 =0.0001, C0 = 1.1, C1 = 1.07, w = wt4)

print(hasil221)
print(hasil222)
print(hasil223)
print(hasil224)

B221 <- hasil221$B
c221 <- hasil221$C

B222 <- hasil222$B
c222 <- hasil222$C

B223 <- hasil223$B
c223 <- hasil223$C

B224 <- hasil224$B
c224 <- hasil224$C

yduga221 <- 1 - exp(((-B221*c221^xl)/log(c221))*(c221-1))
yduga222 <- 1 - exp(((-B222*c222^xl)/log(c222))*(c222-1))
yduga223 <- 1 - exp(((-B223*c223^xl)/log(c223))*(c223-1))
yduga224 <- 1 - exp(((-B224*c224^xl)/log(c224))*(c224-1))

#### Nilai MSE 
k1 <- 2
mse221 <- sum(wt1*((datap$qx - yduga221)^2)) / (n - k1)
mse222 <- sum(wt2*((datap$qx - yduga222)^2)) / (n - k1)
mse223 <- sum(wt3*((datap$qx - yduga223)^2)) / (n - k1)
mse224 <- sum(wt4*((datap$qx - yduga224)^2)) / (n - k1)

cat("MSE Bobot 1:", mse221, "\n")
cat("MSE Bobot 2:", mse222, "\n")
cat("MSE Bobot 3:", mse223, "\n")
cat("MSE Bobot 4:", mse224, "\n")

# 3 ############################################################
## **Iterasi Metode Makeham OLS dengan Metode Secant**
S_function3 <- function(A, B, C, x) {
  exp(-A - (B * C^x / log(C)) * (C - 1))
}

F31 <- function(A, B, C, Y, x) {
  S <- S_function3(A, B, C, x)
  sum(2 * (Y - S) * S)
}

F32 <- function(A, B, C, Y, x) {
  S <- S_function3(A, B, C, x)
  sum(2 * (Y - S) * (C^x * (C - 1) / log(C)) * S)
}

F33 <- function(A, B, C, Y, x) {
  S <- S_function3(A, B, C, x)
  sum(2 * (Y - S) * (B * C^(x-1) * ((x + C) * log(C) - (C - 1)) / (log(C)^2)) * S)
}

secant_method3 <- function(Y, x, A0, B0, C0, A1, B1, C1, tol = 1e-5, max_iter = 10000) {
  iter <- 0
  
  while (iter < max_iter) {
    # Hitung nilai fungsi untuk iterasi sekarang
    F1_1 <- F31(A1, B1, C1, Y, x)  
    F2_1 <- F32(A1, B1, C1, Y, x)  
    F3_1 <- F33(A1, B1, C1, Y, x)  
    
    # Hitung nilai fungsi untuk iterasi sebelumnya
    F1_0 <- F31(A0, B0, C0, Y, x)  
    F2_0 <- F32(A0, B0, C0, Y, x)  
    F3_0 <- F33(A0, B0, C0, Y, x)
    
    # Cek apakah hasil fungsi valid
    if (is.nan(F1_1) || is.nan(F2_1) || is.nan(F3_1) ||
        is.infinite(F1_1) || is.infinite(F2_1) || is.infinite(F3_1)) {
      warning("Fungsi menghasilkan NaN atau Inf. Coba nilai awal lain.")
      return(NULL)
    }
    
    # Cek apakah denominator nol
    if ((F1_1 - F1_0) == 0 || (F2_1 - F2_0) == 0 || (F3_1 - F3_0) == 0) {
      warning("Denominator nol, coba nilai awal lain.")
      return(NULL)
    }
    
    # Perbarui nilai A, B, dan C menggunakan metode secant
    A_new <- A1 - (A1 - A0) / (F1_1 - F1_0) * F1_1
    B_new <- B1 - (B1 - B0) / (F2_1 - F2_0) * F2_1
    C_new <- C1 - (C1 - C0) / (F3_1 - F3_0) * F3_1
    
    # Cek konvergensi
    if (abs(A_new - A1) < tol && abs(B_new - B1) < tol && abs(C_new - C1) < tol) {
      cat("Konvergen: A =", A_new, ", B =", B_new, ", C =", C_new, "\n")
      return(list(A = A_new, B = B_new, C = C_new, iter = iter))
    }
    
    # Perbarui nilai lama dan lanjutkan iterasi
    A0 <- A1
    B0 <- B1
    C0 <- C1
    A1 <- A_new
    B1 <- B_new
    C1 <- C_new
    
    iter <- iter + 1
  }
  
  warning("Tidak ditemukan solusi yang valid setelah", max_iter, "iterasi.")
  return(NULL)
}

###  1. Laki-laki
hasil31 <- secant_method3(pxl, xl, A0 = 0.001, B0 = 0.00001, C0 = 1.095, A1 = 0.002, B1 = 0.00001, C1 = 1.115)

print(hasil31)

A31 <- hasil31$A
B31 <- hasil31$B
c31 <- hasil31$C

yduga31 <- 1 - exp(-A31-((B31*c31^xl)/log(c31))*(c31-1))

#### Nilai MSE 
mse31 <- sum((datal$qx - yduga31)^2) / length(datal$qx)
cat("Mean Squared Error (MSE):", mse31, "\n")

###  2. Perempuan
hasil32 <- secant_method3(pxp, xp, A0 = 0.001, B0 = 0.00001, C0 = 1.085, A1 = 0.002, B1 = 0.00001, C1 = 1.095)
print(hasil32)

A32 <- hasil32$A
B32 <- hasil32$B
c32 <- hasil32$C

yduga32 <- 1 - exp(-A32-((B32*c32^xl)/log(c32))*(c32-1))

#### Nilai MSE 
mse32 <- sum((datap$qx - yduga32)^2) / length(datap$qx)
cat("Mean Squared Error (MSE):", mse32, "\n")

# 4 ############################################################
## **Iterasi Metode Makeham WLS dengan Metode Secant**
n <- 112
wt1 <- seq(1, n) / (n + 1)
wt2 <- (seq(1, n) - 0.3) / (n + 0.4)
wt3 <- (seq(1, n) - 0.5) / n
wt4 <- sapply(1:n, function(i) sum(1 / (n - (1:i) + 1)))

S_function4 <- function(A, B, C, x) {
  exp(-A - (B * C^x / log(C)) * (C - 1))
}

# Persamaan normal dengan bobot
F41 <- function(A, B, C, Y, x, w) {
  S <- S_function4(A, B, C, x)
  sum(2 * w * (Y - S) * S)
}

F42 <- function(A, B, C, Y, x, w) {
  S <- S_function4(A, B, C, x)
  sum(2 * w * (Y - S) * (C^x * (C - 1) / log(C)) * S)
}

F43 <- function(A, B, C, Y, x, w) {
  S <- S_function4(A, B, C, x)
  sum(2 * w * (Y - S) * (-B * C^(x-1) * ((C - 1) + (x + C) * log(C)) / (log(C)^2)) * S)
}

secant_method4 <- function(Y, x, A0, B0, C0, A1, B1, C1, w, tol = 1e-5, max_iter = 1000) {
  iter <- 0
  
  while (iter < max_iter) {
    # Hitung nilai fungsi untuk iterasi sekarang
    F1_1 <- F41(A1, B1, C1, Y, x, w)
    F2_1 <- F42(A1, B1, C1, Y, x, w)
    F3_1 <- F43(A1, B1, C1, Y, x, w)
    
    # Hitung nilai fungsi untuk iterasi sebelumnya
    F1_0 <- F41(A0, B0, C0, Y, x, w)
    F2_0 <- F42(A0, B0, C0, Y, x, w)
    F3_0 <- F43(A0, B0, C0, Y, x, w)
    
    # Cek apakah hasil fungsi valid
    if (is.nan(F1_1) || is.nan(F2_1) || is.nan(F3_1) ||
        is.infinite(F1_1) || is.infinite(F2_1) || is.infinite(F3_1)) {
      warning("Fungsi menghasilkan NaN atau Inf. Coba nilai awal lain.")
      return(NULL)
    }
    
    # Cek apakah denominator nol
    if ((F1_1 - F1_0) == 0 || (F2_1 - F2_0) == 0 || (F3_1 - F3_0) == 0) {
      warning("Denominator nol, coba nilai awal lain.")
      return(NULL)
    }
    
    # Perbarui nilai A, B, dan C menggunakan metode secant
    A_new <- A1 - (A1 - A0) / (F1_1 - F1_0) * F1_1
    B_new <- B1 - (B1 - B0) / (F2_1 - F2_0) * F2_1
    C_new <- C1 - (C1 - C0) / (F3_1 - F3_0) * F3_1
    
    # Cek konvergensi
    if (abs(A_new - A1) < tol && abs(B_new - B1) < tol && abs(C_new - C1) < tol) {
      cat("Konvergen: A =", A_new, ", B =", B_new, ", C =", C_new, "\n")
      return(list(A = A_new, B = B_new, C = C_new, iter = iter))
    }
    
    # Perbarui nilai lama dan lanjutkan iterasi
    A0 <- A1
    B0 <- B1
    C0 <- C1
    A1 <- A_new
    B1 <- B_new
    C1 <- C_new
    
    iter <- iter + 1
  }
  
  warning("Tidak ditemukan solusi yang valid setelah", max_iter, "iterasi.")
  return(NULL)
}

###  1. Laki-laki
hasil411 <- secant_method4(pxl, xl, A0 = 0.003, B0 = 0.00001, C0 = 1.085, A1 = 0.001, B1 = 0.00001, C1 = 1.115, w = wt1)
hasil412 <- secant_method4(pxl, xl, A0 = 0.003, B0 = 0.00001, C0 = 1.085, A1 = 0.001, B1 = 0.00001, C1 = 1.115, w = wt2)
hasil413 <- secant_method4(pxl, xl, A0 = 0.003, B0 = 0.00001, C0 = 1.085, A1 = 0.001, B1 = 0.00001, C1 = 1.115, w = wt3)
hasil414 <- secant_method4(pxl, xl, A0 = 0.002, B0 = 0.00001, C0 = 1.115, A1 = 0.001, B1 = 0.00001, C1 = 1.105, w = wt4)

print(hasil411)
print(hasil412)
print(hasil413)
print(hasil414)

A411 <- hasil411$A
B411 <- hasil411$B
c411 <- hasil411$C

A412 <- hasil412$A
B412 <- hasil412$B
c412 <- hasil412$C

A413 <- hasil413$A
B413 <- hasil413$B
c413 <- hasil413$C

A414 <- hasil414$A
B414 <- hasil414$B
c414 <- hasil414$C

yduga411 <- 1 - exp(-A411-((B411*c411^xl)/log(c411))*(c411-1))
yduga412 <- 1 - exp(-A412-((B412*c412^xl)/log(c412))*(c412-1))
yduga413 <- 1 - exp(-A413-((B413*c413^xl)/log(c413))*(c413-1))
yduga414 <- 1 - exp(-A414-((B414*c414^xl)/log(c414))*(c414-1))

#### Nilai MSE 
k2 <- 3
mse411 <- sum(wt1*((datal$qx - yduga411)^2)) / (n - k2)
mse412 <- sum(wt2*((datal$qx - yduga412)^2)) / (n - k2)
mse413 <- sum(wt3*((datal$qx - yduga413)^2)) / (n - k2)
mse414 <- sum(wt4*((datal$qx - yduga414)^2)) / (n - k2)
cat("MSE Bobot 1:", mse411, "\n")
cat("MSE Bobot 2:", mse412, "\n")
cat("MSE Bobot 3:", mse413, "\n")
cat("MSE Bobot 4:", mse414, "\n")

###  2. Perempuan
hasil421 <- secant_method4(pxp, xp, A0 = 0.001, B0 = 0.00001, C0 = 1.115, A1 = 0.001, B1 = 0.00001, C1 = 1.105, w = wt1)
hasil422 <- secant_method4(pxp, xp, A0 = 0.001, B0 = 0.00001, C0 = 1.115, A1 = 0.001, B1 = 0.00001, C1 = 1.105, w = wt2)
hasil423 <- secant_method4(pxp, xp, A0 = 0.001, B0 = 0.00001, C0 = 1.115, A1 = 0.001, B1 = 0.00001, C1 = 1.105, w = wt3)
hasil424 <- secant_method4(pxp, xp, A0 = 0.001, B0 = 0.00001, C0 = 1.115, A1 = 0.001, B1 = 0.00001, C1 = 1.105, w = wt4)

print(hasil421)
print(hasil422)
print(hasil423)
print(hasil424)

A421 <- hasil421$A
B421 <- hasil421$B
c421 <- hasil421$C

A422 <- hasil422$A
B422 <- hasil422$B
c422 <- hasil422$C

A423 <- hasil423$A
B423 <- hasil423$B
c423 <- hasil423$C

A424 <- hasil424$A
B424 <- hasil424$B
c424 <- hasil424$C

yduga421 <- 1 - exp(-A421-((B421*c421^xp)/log(c421))*(c421-1))
yduga422 <- 1 - exp(-A422-((B422*c422^xp)/log(c422))*(c422-1))
yduga423 <- 1 - exp(-A423-((B423*c423^xp)/log(c423))*(c423-1))
yduga424 <- 1 - exp(-A424-((B424*c424^xp)/log(c424))*(c424-1))

#### Nilai MSE 
k2 <- 3
mse421 <- sum(wt1*((datap$qx - yduga421)^2)) / (n - k2)
mse422 <- sum(wt2*((datap$qx - yduga422)^2)) / (n - k2)
mse423 <- sum(wt3*((datap$qx - yduga423)^2)) / (n - k2)
mse424 <- sum(wt4*((datap$qx - yduga424)^2)) / (n - k2)
cat("MSE Bobot 1:", mse421, "\n")
cat("MSE Bobot 2:", mse422, "\n")
cat("MSE Bobot 3:", mse423, "\n")
cat("MSE Bobot 4:", mse424, "\n")



min(mse11, mse211, mse212, mse213, mse214)



df17 <- data.frame(Index = 1:length(datap$qx), 
                   qx_Obs = datap$qx, 
                   qx_Estimasi = yduga421)

ggplot(df17, aes(x = Index)) +
  geom_point(aes(y = qx_Obs, color = "qx TMI"), size = 2) +
  geom_line(aes(y = qx_Estimasi, color = "qx Estimasi"), linewidth = 1) +
  labs(title = "Perbandingan TMI dengan Estimasi", subtitle = "Perempuan Makeham WLS Bobot 1",
       x = "Index", y = "Nilai qx") +
  scale_color_manual(values = c("qx TMI" = "blue", "qx Estimasi" = "red")) +
  theme_classic()



df20 <- data.frame(Index = 1:length(datap$qx), 
                   qx_Obs = datap$qx, 
                   qx_Estimasi = yduga424)

ggplot(df20, aes(x = Index)) +
  geom_point(aes(y = qx_Obs, color = "qx TMI"), size = 2) +
  geom_line(aes(y = qx_Estimasi, color = "qx Estimasi"), linewidth = 1) +
  labs(title = "Perbandingan TMI dengan Estimasi", subtitle = "Perempuan Makeham WLS Bobot 4",
       x = "Index", y = "Nilai qx") +
  scale_color_manual(values = c("qx TMI" = "blue", "qx Estimasi" = "red")) +
  theme_classic()

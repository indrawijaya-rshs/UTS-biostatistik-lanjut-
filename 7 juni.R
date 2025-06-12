# Langkah 1: Install dan load paket yang diperlukan
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
library(readr)
library(dplyr)

# Langkah 2: Mengakses dan membaca data dari URL
url <- "https://raw.githubusercontent.com/indrawijaya-rshs/UTS-biostatistik-lanjut-/refs/heads/main/merged_data_pef_sex_asthma.csv"
data <- read_csv(url)

# Langkah 3: Memilih kolom yang relevan
data_new <- data[, c("pidlink", "pef", "age", "height", "sex", "Asthma")]

# Langkah 4: Membersihkan missing data (jika ada)
data_clean <- data_new[complete.cases(data_new[, c("pef", "age", "height", "sex", "Asthma")]), ]
cat("Jumlah baris data awal:", nrow(data_new), "\n")
cat("Jumlah baris setelah menghapus missing data:", nrow(data_clean), "\n")

# Langkah 5: Pemeriksaan awal untuk memastikan data cukup untuk analisis
if (nrow(data_clean) < 3) {
  stop("Error: Jumlah observasi terlalu kecil (< 3) untuk analisis.")
}

# Langkah 6: Uji normalitas untuk pef
shapiro_test <- tryCatch({
  shapiro.test(data_clean$pef)
}, error = function(e) {
  cat("Error dalam uji Shapiro-Wilk:", e$message, "\n")
  return(NULL)
})

# Menentukan metode uji berdasarkan normalitas
if (!is.null(shapiro_test)) {
  cat("\nUji Normalitas untuk PEF (Shapiro-Wilk):\n")
  cat("p-value:", shapiro_test$p.value, "\n")
  if (shapiro_test$p.value < 0.05) {
    cat("PEF tidak terdistribusi normal (p < 0.05), gunakan uji non-parametrik.\n")
    normality <- "non-parametric"
  } else {
    cat("PEF terdistribusi normal (p >= 0.05), gunakan uji parametrik.\n")
    normality <- "parametric"
  }
} else {
  cat("Uji normalitas gagal, default ke uji non-parametrik.\n")
  normality <- "non-parametric"
}

# Langkah 7: Analisis Univariat
# 7.1: Korelasi antara pef dan age
cat("\nKorelasi antara PEF dan Age:\n")
if (normality == "non-parametric") {
  corr_age <- cor.test(data_clean$pef, data_clean$age, method = "spearman", exact = FALSE)
  cat("Uji Spearman: rho =", corr_age$estimate, ", p-value =", corr_age$p.value, "\n")
} else {
  corr_age <- cor.test(data_clean$pef, data_clean$age, method = "pearson")
  cat("Uji Pearson: r =", corr_age$estimate, ", p-value =", corr_age$p.value, "\n")
}

# 7.2: Korelasi antara pef dan height
cat("\nKorelasi antara PEF dan Height:\n")
if (normality == "non-parametric") {
  corr_height <- cor.test(data_clean$pef, data_clean$height, method = "spearman", exact = FALSE)
  cat("Uji Spearman: rho =", corr_height$estimate, ", p-value =", corr_height$p.value, "\n")
} else {
  corr_height <- cor.test(data_clean$pef, data_clean$height, method = "pearson")
  cat("Uji Pearson: r =", corr_height$estimate, ", p-value =", corr_height$p.value, "\n")
}

# 7.3: Hubungan antara pef dan sex
cat("\nHubungan antara PEF dan Sex:\n")
if (length(unique(data_clean$sex)) != 2) {
  cat("Error: Kolom 'sex' tidak memiliki tepat dua kategori, uji tidak dilakukan.\n")
} else {
  if (normality == "non-parametric") {
    test_sex <- wilcox.test(pef ~ sex, data = data_clean)
    cat("Uji Mann-Whitney U: p-value =", test_sex$p.value, "\n")
  } else {
    test_sex <- t.test(pef ~ sex, data = data_clean)
    cat("Uji t-test: p-value =", test_sex$p.value, "\n")
  }
  # Ringkasan statistik PEF berdasarkan sex
  cat("Statistik Deskriptif PEF berdasarkan Sex:\n")
  print(tapply(data_clean$pef, data_clean$sex, summary))
}

# 7.4: Hubungan antara pef dan Asthma
cat("\nHubungan antara PEF dan Asthma:\n")
if (length(unique(data_clean$Asthma)) != 2) {
  cat("Error: Kolom 'Asthma' tidak memiliki tepat dua kategori, uji tidak dilakukan.\n")
} else {
  if (normality == "non-parametric") {
    test_asthma <- wilcox.test(pef ~ Asthma, data = data_clean)
    cat("Uji Mann-Whitney U: p-value =", test_asthma$p.value, "\n")
  } else {
    test_asthma <- t.test(pef ~ Asthma, data = data_clean)
    cat("Uji t-test: p-value =", test_asthma$p.value, "\n")
  }
  # Ringkasan statistik PEF berdasarkan Asthma
  cat("Statistik Deskriptif PEF berdasarkan Asthma:\n")
  print(tapply(data_clean$pef, data_clean$Asthma, summary))
}

# Langkah 8: Visualisasi
par(mfrow = c(2, 2))
# Plot untuk pef vs age
plot(data_clean$age, data_clean$pef, main = "PEF vs Age", xlab = "Age (years)", ylab = "PEF (L/min)")
# Plot untuk pef vs height
plot(data_clean$height, data_clean$pef, main = "PEF vs Height", xlab = "Height (cm)", ylab = "PEF (L/min)")
# Boxplot untuk pef vs sex
if (length(unique(data_clean$sex)) == 2) {
  boxplot(pef ~ sex, data = data_clean, main = "PEF by Sex", ylab = "PEF (L/min)")
}
# Boxplot untuk pef vs Asthma
if (length(unique(data_clean$Asthma)) == 2) {
  boxplot(pef ~ Asthma, data = data_clean, main = "PEF by Asthma", ylab = "PEF (L/min)")
}

# Langkah 9: Menyimpan data yang sudah dibersihkan (opsional)
write_csv(data_clean, "data_clean_merged_analysis.csv")

====
  
  9.4: Hubungan antara pef dan Asthma
cat("\nHubungan antara PEF dan Asthma:\n")
if (length(unique(na.omit(data_clean$Asthma))) != 2) {
  cat("Error: Kolom 'Asthma' tidak memiliki tepat dua kategori, uji tidak dilakukan.\n")
} else {
  if (normality == "non-parametric") {
    test_asthma <- wilcox.test(pef ~ Asthma, data = data_clean)
    cat("Uji Mann-Whitney U: p-value =", test_asthma$p.value, "\n")
  } else {
    test_asthma <- t.test(pef ~ Asthma, data = data_clean)
    cat("Uji t-test: p-value =", test_asthma$p.value, "\n")
  }
  # Ringkasan statistik PEF berdasarkan Asthma
  cat("Statistik Deskriptif PEF berdasarkan Asthma:\n")
  print(tapply(data_clean$pef, data_clean$Asthma, summary))
}

# Langkah 10: Visualisasi
par(mfrow = c(2, 2))
# Plot untuk pef vs age
plot(data_clean$age, data_clean$pef, main = "PEF vs Age", xlab = "Age (years)", ylab = "PEF (L/min)")
# Plot untuk pef vs height
plot(data_clean$height, data_clean$pef, main = "PEF vs Height", xlab = "Height (cm)", ylab = "PEF (L/min)")
# Boxplot untuk pef vs sex
if (length(unique(na.omit(data_clean$sex))) == 2) {
  boxplot(pef ~ sex, data = data_clean, main = "PEF by Sex", ylab = "PEF (L/min)")
}
# Boxplot untuk pef vs Asthma
if (length(unique(na.omit(data_clean$Asthma))) == 2) {
  boxplot(pef ~ Asthma, data = data_clean, main = "PEF by Asthma", ylab = "PEF (L/min)")
}

# Langkah 11: Menyimpan data yang sudah dibersihkan (opsional)
write_csv(data_clean, "data_clean_merged_analysis.csv")

data_new$Asthma <- ifelse(data_new$Asthma %in% c("No-Asthma", "Yes-Asthma"), 
                          data_new$Asthma, NA)
cat("\nDistribusi nilai Asthma setelah penyeragaman:\n")
print(table(data_new$Asthma, useNA = "ifany"))

# Langkah 9: Analisis Univariat
# 9.1: Korelasi antara pef dan age
cat("\nKorelasi antara PEF dan Age:\n")
if (normality == "non-parametric") {
  corr_age <- cor.test(data_clean$pef, data_clean$age, method = "spearman", exact = FALSE)
  cat("Uji Spearman: rho =", corr_age$estimate, ", p-value =", corr_age$p.value, "\n")
} else {
  corr_age <- cor.test(data_clean$pef, data_clean$age, method = "pearson")
  cat("Uji Pearson: r =", corr_age$estimate, ", p-value =", corr_age$p.value, "\n")
}

# 9.2: Korelasi antara pef dan height
cat("\nKorelasi antara PEF dan Height:\n")
if (normality == "non-parametric") {
  corr_height <- cor.test(data_clean$pef, data_clean$height, method = "spearman", exact = FALSE)
  cat("Uji Spearman: rho =", corr_height$estimate, ", p-value =", corr_height$p.value, "\n")
} else {
  corr_height <- cor.test(data_clean$pef, data_clean$height, method = "pearson")
  cat("Uji Pearson: r =", corr_height$estimate, ", p-value =", corr_height$p.value, "\n")
}

# 9.3: Hubungan antara pef dan sex
cat("\nHubungan antara PEF dan Sex:\n")
cat("Distribusi nilai Sex:\n")
print(table(data_clean$sex, useNA = "ifany"))
if (length(unique(na.omit(data_clean$sex))) != 2) {
  cat("Error: Kolom 'sex' tidak memiliki tepat dua kategori, uji tidak dilakukan.\n")
} else {
  if (normality == "non-parametric") {
    test_sex <- wilcox.test(pef ~ sex, data = data_clean)
    cat("Uji Mann-Whitney U: p-value =", test_sex$p.value, "\n")
  } else {
    test_sex <- t.test(pef ~ sex, data = data_clean)
    cat("Uji t-test: p-value =", test_sex$p.value, "\n")
  }
  # Ringkasan statistik PEF berdasarkan sex
  cat("Statistik Deskriptif PEF berdasarkan Sex:\n")
  print(tapply(data_clean$pef, data_clean$sex, summary))
}

# 9.4: Hubungan antara pef dan Asthma
cat("\nHubungan antara PEF dan Asthma:\n")
if (length(unique(na.omit(data_clean$Asthma))) != 2) {
  cat("Error: Kolom 'Asthma' tidak memiliki tepat dua kategori, uji tidak dilakukan.\n")
} else {
  if (normality == "non-parametric") {
    test_asthma <- wilcox.test(pef ~ Asthma, data = data_clean)
    cat("Uji Mann-Whitney U: p-value =", test_asthma$p.value, "\n")
  } else {
    test_asthma <- t.test(pef ~ Asthma, data = data_clean)
    cat("Uji t-test: p-value =", test_asthma$p.value, "\n")
  }
  # Ringkasan statistik PEF berdasarkan Asthma
  cat("Statistik Deskriptif PEF berdasarkan Asthma:\n")
  print(tapply(data_clean$pef, data_clean$Asthma, summary))
}

# Langkah 10: Visualisasi
par(mfrow = c(2, 2))
# Plot untuk pef vs age
plot(data_clean$age, data_clean$pef, main = "PEF vs Age", xlab = "Age (years)", ylab = "PEF (L/min)")
# Plot untuk pef vs height
plot(data_clean$height, data_clean$pef, main = "PEF vs Height", xlab = "Height (cm)", ylab = "PEF (L/min)")
# Boxplot untuk pef vs sex
if (length(unique(na.omit(data_clean$sex))) == 2) {
  boxplot(pef ~ sex, data = data_clean, main = "PEF by Sex", ylab = "PEF (L/min)")
}
# Boxplot untuk pef vs Asthma
if (length(unique(na.omit(data_clean$Asthma))) == 2) {
  boxplot(pef ~ Asthma, data = data_clean, main = "PEF by Asthma", ylab = "PEF (L/min)")
}

# Langkah 11: Menyimpan data yang sudah dibersihkan (opsional)
write_csv(data_clean, "data_clean_merged_analysis.csv")
ata_new$Asthma <- ifelse(data_new$Asthma %in% c("No-Asthma", "Yes-Asthma"), 
                         data_new$Asthma, NA)
cat("\nDistribusi nilai Asthma setelah penyeragaman:\n")
print(table(data_new$Asthma, useNA = "ifany"))

===
  # Langkah 1: Install dan load paket yang diperlukan
  if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
library(readr)
library(dplyr)

# Langkah 2: Mengakses dan membaca data dari URL
url <- "https://raw.githubusercontent.com/indrawijaya-rshs/UTS-biostatistik-lanjut-/refs/heads/main/merged_data_pef_sex_asthma.csv"
data <- read_csv(url)

# Langkah 3: Memilih kolom yang relevan
data_new <- data[, c("pidlink", "pef", "age", "height", "sex", "Asthma")]
cat("\nJumlah baris data awal:", nrow(data_new), "\n")

# Langkah 4: Memeriksa distribusi nilai Asthma sebelum filtering
cat("\nDistribusi nilai Asthma sebelum filtering:\n")
print(table(data_new$Asthma, useNA = "ifany"))

# Langkah 5: Memfilter observasi dengan Asthma non-NA
data_filtered <- data_new[!is.na(data_new$Asthma), ]
cat("\nJumlah baris setelah memfilter Asthma non-NA:", nrow(data_filtered), "\n")

# Langkah 6: Memeriksa distribusi nilai Asthma setelah filtering
cat("\nDistribusi nilai Asthma setelah filtering:\n")
print(table(data_filtered$Asthma, useNA = "ifany"))

# Langkah 7: Membersihkan missing data untuk kolom lain
data_clean <- data_filtered[complete.cases(data_filtered[, c("pef", "age", "height", "sex")]), ]
cat("\nJumlah baris setelah menghapus missing data untuk pef, age, height, sex:", nrow(data_clean), "\n")

# Langkah 8: Memeriksa distribusi nilai Asthma setelah pembersihan
cat("\nDistribusi nilai Asthma setelah pembersihan lengkap:\n")
print(table(data_clean$Asthma, useNA = "ifany"))

# Langkah 9: Pemeriksaan awal untuk memastikan data cukup untuk analisis
if (nrow(data_clean) < 3) {
  stop("Error: Jumlah observasi terlalu kecil (< 3) untuk analisis.")
}

# Langkah 10: Memeriksa jumlah kategori Asthma
asthma_categories <- length(unique(na.omit(data_clean$Asthma)))
cat("\nJumlah kategori unik Asthma:", asthma_categories, "\n")
if (asthma_categories != 2) {
  cat("Peringatan: Kolom 'Asthma' tidak memiliki tepat dua kategori. Uji untuk Asthma tidak dilakukan.\n")
  cat("Kategori yang ditemukan:", paste(unique(na.omit(data_clean$Asthma)), collapse = ", "), "\n")
}

# Langkah 11: Uji normalitas untuk pef
shapiro_test <- tryCatch({
  shapiro.test(data_clean$pef)
}, error = function(e) {
  cat("Error dalam uji Shapiro-Wilk:", e$message, "\n")
  return(NULL)
})

# Menentukan metode uji berdasarkan normalitas
if (!is.null(shapiro_test)) {
  cat("\nUji Normalitas untuk PEF (Shapiro-Wilk):\n")
  cat("p-value:", shapiro_test$p.value, "\n")
  if (shapiro_test$p.value < 0.05) {
    cat("PEF tidak terdistribusi normal (p < 0.05), gunakan uji non-parametrik.\n")
    normality <- "non-parametric"
  } else {
    cat("PEF terdistribusi normal (p >= 0.05), gunakan uji parametrik.\n")
    normality <- "parametric"
  }
} else {
  cat("Uji normalitas gagal, default ke uji non-parametrik.\n")
  normality <- "non-parametric"
}

# Langkah 12: Analisis Univariat
# 12.1: Korelasi antara pef dan age
cat("\nKorelasi antara PEF dan Age:\n")
if (normality == "non-parametric") {
  corr_age <- cor.test(data_clean$pef, data_clean$age, method = "spearman", exact = FALSE)
  cat("Uji Spearman: rho =", corr_age$estimate, ", p-value =", corr_age$p.value, "\n")
} else {
  corr_age <- cor.test(data_clean$pef, data_clean$age, method = "pearson")
  cat("Uji Pearson: r =", corr_age$estimate, ", p-value =", corr_age$p.value, "\n")
}

# 12.2: Korelasi antara pef dan height
cat("\nKorelasi antara PEF dan Height:\n")
if (normality == "non-parametric") {
  corr_height <- cor.test(data_clean$pef, data_clean$height, method = "spearman", exact = FALSE)
  cat("Uji Spearman: rho =", corr_height$estimate, ", p-value =", corr_height$p.value, "\n")
} else {
  corr_height <- cor.test(data_clean$pef, data_clean$height, method = "pearson")
  cat("Uji Pearson: r =", corr_height$estimate, ", p-value =", corr_height$p.value, "\n")
}

# 12.3: Hubungan antara pef dan sex
cat("\nHubungan antara PEF dan Sex:\n")
cat("Distribusi nilai Sex:\n")
print(table(data_clean$sex, useNA = "ifany"))
if (length(unique(na.omit(data_clean$sex))) != 2) {
  cat("Error: Kolom 'sex' tidak memiliki tepat dua kategori, uji tidak dilakukan.\n")
} else {
  if (normality == "non-parametric") {
    test_sex <- wilcox.test(pef ~ sex, data = data_clean)
    cat("Uji Mann-Whitney U: p-value =", test_sex$p.value, "\n")
  } else {
    test_sex <- t.test(pef ~ sex, data = data_clean)
    cat("Uji t-test: p-value =", test_sex$p.value, "\n")
  }
  # Ringkasan statistik PEF berdasarkan sex
  cat("Statistik Deskriptif PEF berdasarkan Sex:\n")
  print(tapply(data_clean$pef, data_clean$sex, summary))
}

# 12.4: Hubungan antara pef dan Asthma
cat("\nHubungan antara PEF dan Asthma:\n")
if (asthma_categories != 2) {
  cat("Error: Kolom 'Asthma' tidak memiliki tepat dua kategori, uji tidak dilakukan.\n")
} else {
  if (normality == "non-parametric") {
    test_asthma <- wilcox.test(pef ~ Asthma, data = data_clean)
    cat("Uji Mann-Whitney U: p-value =", test_asthma$p.value, "\n")
  } else {
    test_asthma <- t.test(pef ~ Asthma, data = data_clean)
    cat("Uji t-test: p-value =", test_asthma$p.value, "\n")
  }
  # Ringkasan statistik PEF berdasarkan Asthma
  cat("Statistik Deskriptif PEF berdasarkan Asthma:\n")
  print(tapply(data_clean$pef, data_clean$Asthma, summary))
}

# Langkah 13: Visualisasi
par(mfrow = c(2, 2))
# Plot untuk pef vs age
plot(data_clean$age, data_clean$pef, main = "PEF vs Age", xlab = "Age (years)", ylab = "PEF (L/min)")
# Plot untuk pef vs height
plot(data_clean$height, data_clean$pef, main = "PEF vs Height", xlab = "Height (cm)", ylab = "PEF (L/min)")
# Boxplot untuk pef vs sex
if (length(unique(na.omit(data_clean$sex))) == 2) {
  boxplot(pef ~ sex, data = data_clean, main = "PEF by Sex", ylab = "PEF (L/min)")
}
# Boxplot untuk pef vs Asthma
if (asthma_categories == 2) {
  boxplot(pef ~ Asthma, data = data_clean, main = "PEF by Asthma", ylab = "PEF (L/min)")
}

# Langkah 14: Menyimpan data yang sudah dibersihkan
write_csv(data_clean, "data_clean_filtered_analysis.csv")

table(data_clean$Asthma, useNA = "ifany")
# 12.4: Hubungan antara pef dan Asthma
cat("\nHubungan antara PEF dan Asthma:\n")
if (asthma_categories != 2) {
  cat("Error: Kolom 'Asthma' tidak memiliki tepat dua kategori, uji tidak dilakukan.\n")
} else {
  if (normality == "non-parametric") {
    test_asthma <- wilcox.test(pef ~ Asthma, data = data_clean)
    cat("Uji Mann-Whitney U: p-value =", test_asthma$p.value, "\n")
  } else {
    test_asthma <- t.test(pef ~ Asthma, data = data_clean)
    cat("Uji t-test: p-value =", test_asthma$p.value, "\n")
  }
  # Ringkasan statistik PEF berdasarkan Asthma
  cat("Statistik Deskriptif PEF berdasarkan Asthma:\n")
  print(tapply(data_clean$pef, data_clean$Asthma, summary))
}
library(dplyr)

filtered_data <- read.csv("merged_data_pef_sex_asthma.csv") %>%
  filter(!is.na(Asthma))
# 12.4: Hubungan antara pef dan Asthma
cat("\nHubungan antara PEF dan Asthma:\n")
if (asthma_categories != 2) {
  cat("Error: Kolom 'Asthma' tidak memiliki tepat dua kategori, uji tidak dilakukan.\n")
} else {
  if (normality == "non-parametric") {
    test_asthma <- wilcox.test(pef ~ Asthma, data = data_clean)
    cat("Uji Mann-Whitney U: p-value =", test_asthma$p.value, "\n")
  } else {
    test_asthma <- t.test(pef ~ Asthma, data = data_clean)
    cat("Uji t-test: p-value =", test_asthma$p.value, "\n")
  }
  # Ringkasan statistik PEF berdasarkan Asthma
  cat("Statistik Deskriptif PEF berdasarkan Asthma:\n")
  print(tapply(data_clean$pef, data_clean$Asthma, summary))

  # Langkah 1: Install dan load paket yang diperlukan
  if (!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
  # Langkah 2: Asumsikan data_clean sudah ada (hasil dari kode sebelumnya)
  # Jika belum ada, muat data dari URL dan filter seperti sebelumnya
  if (!exists("data_clean")) {
    if (!require("readr")) install.packages("readr")
    library(readr)
    url <- "https://raw.githubusercontent.com/indrawijaya-rshs/UTS-biostatistik-lanjut-/refs/heads/main/merged_data_pef_sex_asthma.csv"
    data <- read_csv(url)
    data_new <- data[, c("pidlink", "pef", "age", "height", "sex", "Asthma")]
    data_filtered <- data_new[!is.na(data_new$Asthma), ]
    data_clean <- data_filtered[complete.cases(data_filtered[, c("pef", "age", "height", "sex")]), ]
  }
  
  # Langkah 3: Periksa distribusi nilai Asthma
  cat("\nDistribusi nilai Asthma:\n")
  asthma_table <- table(data_clean$Asthma, useNA = "ifany")
  print(asthma_table)
  
  # Langkah 4: Hitung jumlah kategori unik Asthma (non-NA)
  asthma_categories <- length(unique(na.omit(data_clean$Asthma)))
  cat("\nJumlah kategori unik Asthma:", asthma_categories, "\n")
  
  # Langkah 5: Pemeriksaan awal untuk memastikan data cukup
  if (nrow(data_clean) < 3) {
    stop("Error: Jumlah observasi terlalu kecil (< 3) untuk analisis.")
  }
  
  # Langkah 6: Uji normalitas untuk pef
  shapiro_test <- tryCatch({
    shapiro.test(data_clean$pef)
  }, error = function(e) {
    cat("Error dalam uji Shapiro-Wilk:", e$message, "\n")
    return(NULL)
  })
  
  # Menentukan metode uji berdasarkan normalitas
  if (!is.null(shapiro_test)) {
    cat("\nUji Normalitas untuk PEF (Shapiro-Wilk):\n")
    cat("p-value:", shapiro_test$p.value, "\n")
    if (shapiro_test$p.value < 0.05) {
      cat("PEF tidak terdistribusi normal (p < 0.05), gunakan uji non-parametrik.\n")
      normality <- "non-parametric"
    } else {
      cat("PEF terdistribusi normal (p >= 0.05), gunakan uji parametrik.\n")
      normality <- "parametric"
    }
  } else {
    cat("Uji normalitas gagal, default ke uji non-parametrik.\n")
    normality <- "non-parametric"
  }
  
  # Langkah 7: Analisis Univariat PEF dan Asthma
  cat("\nAnalisis Univariat: Hubungan antara PEF dan Asthma:\n")
  
  if (asthma_categories < 2) {
    cat("Error: Kolom 'Asthma' memiliki kurang dari dua kategori, uji tidak dilakukan.\n")
  } else if (asthma_categories == 2) {
    # Uji untuk dua kategori (t-test atau Mann-Whitney U)
    if (normality == "non-parametric") {
      test_asthma <- wilcox.test(pef ~ Asthma, data = data_clean)
      cat("Uji Mann-Whitney U: p-value =", test_asthma$p.value, "\n")
    } else {
      test_asthma <- t.test(pef ~ Asthma, data = data_clean)
      cat("Uji t-test: p-value =", test_asthma$p.value, "\n")
    }
  } else {
    # Uji untuk lebih dari dua kategori (ANOVA atau Kruskal-Wallis)
    if (normality == "non-parametric") {
      test_asthma <- kruskal.test(pef ~ Asthma, data = data_clean)
      cat("Uji Kruskal-Wallis: p-value =", test_asthma$p.value, "\n")
    } else {
      test_asthma <- aov(pef ~ Asthma, data = data_clean)
      cat("Uji ANOVA: p-value =", summary(test_asthma)[[1]][["Pr(>F)"]][1], "\n")
    }
  }
  
  # Langkah 8: Statistik Deskriptif PEF berdasarkan Asthma
  cat("\nStatistik Deskriptif PEF berdasarkan Asthma:\n")
  print(tapply(data_clean$pef, data_clean$Asthma, summary))
  
  # Langkah 9: Visualisasi
  cat("\nMembuat boxplot untuk PEF berdasarkan Asthma\n")
  if (asthma_categories >= 2) {
    boxplot(pef ~ Asthma, data = data_clean, main = "PEF by Asthma", 
            xlab = "Asthma", ylab = "PEF (L/min)")
  } else {
    cat("Boxplot tidak dibuat karena Asthma memiliki kurang dari dua kategori.\n")
  }
  
  library(datasets)
  
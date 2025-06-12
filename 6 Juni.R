rm (data)
rm (data_merged)
rm (data_new)
rm (pef_asma)
rm (pef_data_bersih)
rm (data_filtered)
rm (pef_dataW5)
set.seed(0002)
pef_ds = sample_frac(pef, 0.85, replace = FALSE)

Langkah 3: Memilih kolom yang relevan (pidlink, pef, age, height)
data_new <- pef_ds[, c("pidlink", "pef", "age", "height")]
=====
# Langkah 4: Membersihkan missing data (NA) untuk pef, age, dan height
data_clean <- data_new[complete.cases(data_new[, c("pef", "age", "height")]), ]

Langkah 4: Membersihkan missing data (NA) untuk pef, age, dan height
data_clean <- data_new[complete.cases(data_new[, c("pef", "age", "height")]), ]

# Langkah 5: Menentukan batasan logis untuk pef, age, dan height
pef_min <- 50    # PEF minimum (L/min)
pef_max <- 1000  # PEF maksimum (L/min)
age_min <- 18     # Usia minimum (tahun)
age_max <- 100   # Usia maksimum (tahun)
height_min <- 50 # Tinggi minimum (cm)
height_max <- 250 # Tinggi maksimum (cm)

# Langkah 6: Mengidentifikasi dan membersihkan nilai ekstrem
data_clean <- data_clean[
  data_clean$pef >= pef_min & data_clean$pef <= pef_max &
    data_clean$age >= age_min & data_clean$age <= age_max &
    data_clean$height >= height_min & data_clean$height <= height_max,
]

# Langkah 7: (Opsional) Identifikasi outlier menggunakan metode IQR
pef_iqr <- IQR(data_clean$pef)
pef_q <- quantile(data_clean$pef, c(0.25, 0.75))
pef_lower <- pef_q[1] - 1.5 * pef_iqr
pef_upper <- pef_q[2] + 1.5 * pef_iqr

age_iqr <- IQR(data_clean$age)
age_q <- quantile(data_clean$age, c(0.25, 0.75))
age_lower <- age_q[1] - 1.5 * age_iqr
age_upper <- age_q[2] + 1.5 * age_iqr

height_iqr <- IQR(data_clean$height)
height_q <- quantile(data_clean$height, c(0.25, 0.75))
height_lower <- height_q[1] - 1.5 * height_iqr
height_upper <- height_q[2] + 1.5 * height_iqr

# Filter data untuk menghapus outlier berdasarkan IQR (opsional)
data_clean_iqr <- data_clean[
  data_clean$pef >= pef_lower & data_clean$pef <= pef_upper &
    data_clean$age >= age_lower & data_clean$age <= age_upper &
    data_clean$height >= height_lower & data_clean$height <= height_upper,
]

# Langkah 8: Menampilkan ringkasan data
cat("Jumlah baris data awal:", nrow(data_new), "\n")
cat("Jumlah baris setelah menghapus missing data:", nrow(data_clean), "\n")
cat("Jumlah baris setelah menghapus nilai ekstrem (batasan logis):", nrow(data_clean), "\n")
cat("Jumlah baris setelah menghapus outlier IQR (opsional):", nrow(data_clean_iqr), "\n")

cat("\nStatistik deskriptif sebelum pembersihan ekstrem:\n")
summary(data_new[, c("pef", "age", "height")])

cat("\nStatistik deskriptif setelah pembersihan ekstrem (batasan logis):\n")
summary(data_clean[, c("pef", "age", "height")])

cat("\nStatistik deskriptif setelah pembersihan outlier IQR (opsional):\n")
summary(data_clean_iqr[, c("pef", "age", "height")])

# Langkah 9: Visualisasi distribusi data
par(mfrow = c(1, 3))
boxplot(data_clean$pef, main = "PEF (setelah batasan logis)", ylab = "L/min")
boxplot(data_clean$age, main = "Age (setelah batasan logis)", ylab = "Years")
boxplot(data_clean$height, main = "Height (setelah batasan logis)", ylab = "cm")

# Langkah 10: Menyimpan data yang sudah dibersihkan ke file CSV (opsional)
write_csv(data_clean, "data_clean_pef_age_height_no_extremes.csv")
write_csv(data_clean_iqr, "data_clean_pef_age_height_no_outliers_iqr.csv")

=========
  Langkah 2: Mengakses dan membaca data dari URL
url <- "https://raw.githubusercontent.com/dwi-agustian/biostat/main/w5.csv"
data <- read_csv(url)

# Langkah 3: Memilih hanya kolom pidlink, sex, dan Asthma
data_new1 <- data[, c("pidlink", "sex", "Asthma")]

# Langkah 4: Menampilkan ringkasan data baru
cat("Jumlah baris data baru:", nrow(data_new1), "\n")
cat("Jumlah kolom data baru:", ncol(data_new1), "\n")
print("Pratinjau data baru:")
head(data_new1)

# Langkah 5: Menyimpan data baru ke file CSV (opsional)
write_csv(data_new1, "data_new_pidlink_sex_Asthma.csv")

==
  Langkah 4: Memfilter observasi dengan data non-missing pada kolom sex
data_filtered1 <- data_new1[data_new1$Asthma %in% c("No-Asthma", "Yes-Asthma"), ]
data_filtered2 <- data_new1[!is.na(data_new1$sex), ]

=====
  
# Langkah 1: Install dan load paket yang diperlukan
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
library(readr)
library(dplyr)

# Langkah 2: Mengakses dan membaca kedua dataset
# Dataset w5.csv (berisi pidlink, sex, Asthma)
url_w5 <- "https://raw.githubusercontent.com/dwi-agustian/biostat/main/w5.csv"
data_w5 <- read_csv(url_w5)
data_sex_asthma <- data_w5[, c("pidlink", "sex", "Asthma")]

# Dataset PEF (berisi pidlink, pef, age, height)
url_pef <- "https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef.csv"
data_pef <- read_csv(url_pef)
data_pef <- data_pef[, c("pidlink", "pef", "age", "height")]

# Langkah 3: Memeriksa tipe data pidlink
cat("Tipe data pidlink di dataset w5 (sex_asthma):", class(data_sex_asthma$pidlink), "\n")
cat("Tipe data pidlink di dataset PEF:", class(data_pef$pidlink), "\n")

# Langkah 4: Memeriksa ringkasan pidlink
cat("\nRingkasan pidlink di dataset w5 (sex_asthma):\n")
summary(data_sex_asthma$pidlink)
cat("Jumlah nilai unik:", length(unique(data_sex_asthma$pidlink)), "\n")
cat("Jumlah duplikasi:", sum(duplicated(data_sex_asthma$pidlink)), "\n")

cat("\nRingkasan pidlink di dataset PEF:\n")
summary(data_pef$pidlink)
cat("Jumlah nilai unik:", length(unique(data_pef$pidlink)), "\n")
cat("Jumlah duplikasi:", sum(duplicated(data_pef$pidlink)), "\n")

# Langkah 5: Memeriksa contoh nilai pidlink
cat("\nContoh nilai pidlink di dataset w5 (sex_asthma):\n")
head(data_sex_asthma$pidlink, 5)
cat("\nContoh nilai pidlink di dataset PEF:\n")
head(data_pef$pidlink, 5)

# Langkah 6: Konversi format pidlink jika diperlukan
# Mengonversi ke character untuk konsistensi
if (class(data_sex_asthma$pidlink) != class(data_pef$pidlink) || class(data_sex_asthma$pidlink) != "character") {
  data_sex_asthma$pidlink <- as.character(data_sex_asthma$pidlink)
  data_pef$pidlink <- as.character(data_pef$pidlink)
  cat("\nKonversi dilakukan: pidlink dikonversi ke character di kedua dataset.\n")
} else {
  cat("\nTipe data pidlink sudah kompatibel (character).\n")
}

# Langkah 7: Memeriksa kesamaan nilai pidlink untuk merging
common_pidlink <- intersect(data_sex_asthma$pidlink, data_pef$pidlink)
cat("\nJumlah pidlink yang sama di kedua dataset:", length(common_pidlink), "\n")
cat("Jumlah pidlink unik di w5 (sex_asthma) yang tidak ada di PEF:", 
    length(setdiff(data_sex_asthma$pidlink, data_pef$pidlink)), "\n")
cat("Jumlah pidlink unik di PEF yang tidak ada di w5 (sex_asthma):", 
    length(setdiff(data_pef$pidlink, data_sex_asthma$pidlink)), "\n")

# Langkah 8: Menyimpan dataset yang telah disesuaikan (opsional)
write_csv(data_sex_asthma, "data_sex_asthma_prepared.csv")
write_csv(data_pef, "data_pef_prepared.csv")

# Langkah 9: Contoh merging (opsional, untuk verifikasi)
merged_data <- merge(data_sex_asthma, data_pef, by = "pidlink", all = FALSE)
cat("\nJumlah baris setelah merging (inner join):", nrow(merged_data), "\n")
print("Pratinjau data hasil merging:")
head(merged_data)
=======================
  
  # Langkah 1: Install dan load paket yang diperlukan
  if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
library(readr)
library(dplyr)

# Langkah 2: Mengakses dan membaca kedua dataset
# Dataset PEF (pidlink, pef, age, height)
url_pef <- "https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef.csv"
data_pef <- read_csv(url_pef)
data_pef <- data_pef[, c("pidlink", "pef", "age", "height")]

# Dataset w5 (pidlink, sex, Asthma)
url_w5 <- "https://raw.githubusercontent.com/dwi-agustian/biostat/main/w5.csv"
data_w5 <- read_csv(url_w5)
data_sex_asthma <- data_w5[, c("pidlink", "sex", "Asthma")]

# Langkah 3: Memeriksa tipe data pidlink
cat("Tipe data pidlink di dataset PEF:", class(data_pef$pidlink), "\n")
cat("Tipe data pidlink di dataset w5 (sex_asthma):", class(data_sex_asthma$pidlink), "\n")

# Langkah 4: Konversi format pidlink ke character untuk konsistensi
data_pef$pidlink <- as.character(data_pef$pidlink)
data_sex_asthma$pidlink <- as.character(data_sex_asthma$pidlink)
cat("\nKonversi dilakukan: pidlink dikonversi ke character di kedua dataset.\n")

# Langkah 5: Memeriksa ringkasan pidlink
cat("\nRingkasan pidlink di dataset PEF:\n")
cat("Jumlah baris:", nrow(data_pef), "\n")
cat("Jumlah nilai unik:", length(unique(data_pef$pidlink)), "\n")
cat("Jumlah duplikasi:", sum(duplicated(data_pef$pidlink)), "\n")

cat("\nRingkasan pidlink di dataset w5 (sex_asthma):\n")
cat("Jumlah baris:", nrow(data_sex_asthma), "\n")
cat("Jumlah nilai unik:", length(unique(data_sex_asthma$pidlink)), "\n")
cat("Jumlah duplikasi:", sum(duplicated(data_sex_asthma$pidlink)), "\n")

# Langkah 6: Melakukan left join dengan dataset PEF sebagai patokan
merged_data <- left_join(data_pef, data_sex_asthma, by = "pidlink")

# Langkah 7: Memverifikasi jumlah observasi
cat("\nJumlah baris dataset PEF:", nrow(data_pef), "\n")
cat("Jumlah baris dataset hasil merging:", nrow(merged_data), "\n")
if (nrow(merged_data) == nrow(data_pef)) {
  cat("Jumlah observasi sesuai: Dataset final memiliki jumlah baris sama dengan dataset PEF.\n")
} else {
  cat("Peringatan: Jumlah observasi tidak sesuai!\n")
}

# Langkah 8: Menampilkan pratinjau dan ringkasan dataset hasil merging
print("Pratinjau dataset hasil merging:")
head(merged_data)

cat("\nRingkasan dataset hasil merging:\n")
summary(merged_data)

# Langkah 9: Memeriksa missing data di kolom sex dan Asthma
cat("\nJumlah missing data di kolom sex dan Asthma setelah merging:\n")
colSums(is.na(merged_data[, c("sex", "Asthma")]))

# Langkah 10: Menyimpan dataset hasil merging ke file CSV (opsional)
write_csv(merged_data, "merged_data_pef_sex_asthma.csv")
rm (data_merged)

===

 
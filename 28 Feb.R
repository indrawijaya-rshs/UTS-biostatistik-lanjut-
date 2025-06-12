class(c_data$nv)
class(c_data$region)
#kode untuk mengkonversi variabel text menjadi kategori 
c_data$type = as.factor(c_data$tpe)
class(c_data$type)

#membaca dari URL 
library(readr)
pef_data <- read.csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef.csv")
# Melihat jumlah data yang duplikat berdasarkan pidlink
sum(duplicated(pef_data$pidlink))
# Menampilkan baris-baris yang duplikat
pef_data[duplicated(pef_data$pidlink), ]
# Menghapus duplikat
pef_unique <- pef_data[!duplicated(pef_data$pidlink), ]
pef_data <- pef_data[!duplicated(pef_data$pidlink), ]
#mendownload data yang sudah bebas duplikasi 
write.csv(pef_data, "pef_data_bersih.csv", row.names = FALSE)

#Mempersiapkan data untuk analisis variabel pef, age, sex, height:
# Aktifkan
library(dplyr)
#Set Seed dan Mengambil Sampel
set.seed(0002)  
pef_ds <- sample_frac(pef, 0.85, replace = FALSE)
#Data age
sample_per_age <- pef_ds %>%
  group_by(age) %>%
  sample_frac(0.85) %>%
  ungroup()
#Data pef
sample_per_pef <- pef_ds %>%
  group_by(pef) %>%
  sample_frac(0.85) %>%
  ungroup()
#Data sex
sample_per_sex <- pef_ds %>%
  group_by(sex) %>%
  sample_frac(0.85) %>%
  ungroup()
#Catatan tidak ada data tentang sex/pria/wanita
#Data height
sample_per_height <- pef_ds %>%
  group_by(height) %>%
  sample_frac(0.85) %>%
  ungroup()
#membersihkan (mengexclude) missing data untuk variable pef, age, sex, dan height 
#(tidak ada data sex)
library(dplyr)
pef_data_bersih <- pef_data %>%
  filter(!is.na(pef)
         pef_data_bersih<- pef_data %>%  
           filter(!is.na(age)    
                  pef_data_bersih<- pef_data %>%  
                    filter(!is.na(height)   
                           
#Menyiapkan data yang komplit (memiliki nila pef, age, sex dan height)
# Mengambil hanya baris yang nilai pef, age, sex, dan height-nya tidak NA

library(dplyr)
pef_komplit <- pef_data %>%
  filter(!is.na(pef)
         pef_data_bersih<- pef_data %>%  
           filter(!is.na(age)    
                  pef_data_bersih<- pef_data %>%  
                    filter(!is.na(height)   
library(dplyr)
pef_komplit <- pef[complete.cases(pef[, c("pef", "age", "height")]), ]
write.csv(pef_komplit, "pef_komplit.csv", row.names = FALSE)                           

#Mengidentifikasi nilai-nilai ekstrem dari data pef, age, dan height
#Melakukan pembersihan data dengan mengeluarkan data-data yang tidak masuk akal 
library(dplyr)

pef_databersih2 <- pef %>%
  filter(!is.na(pef) & pef >= 300 & pef <= 700,
    !is.na(age) & age >= 18 & age <= 70,
    !is.na(height) & height >= 140 & height <= 200)
write.csv(pef_databersih2, "pef_databersih2.csv", row.names = FALSE)                         

#Mempersiapkan data morbidity asma untuk merging dengan data pef.
#Meng-akses data W5
library(readr)
pef_dataW5 <- read.csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/w5.csv")
#Menyiapkan data baru yang hanya berisikan variable pidlink dan Asthma
# Mengambil hanya kolom pidlink dan Asthma
data_asma <- pef_dataW5 %>%
  select(pidlink, Asthma)
#MengeCek frekuensi dari kategori Astma
library(dplyr)
pef_dataW5 %>%
  count(Asthma)
#Memilih oberservasi yang berisikan kategori ‘No-Asthma” atau “Yes-Asthma”
library(dplyr)
pef_asma <- pef_dataW5 %>%
  filter(Asthma %in% c("No-Asthma", "Yes-Asthma"))
#Cek variable pidlink, lihat kompatibilitasnya dengan variable pidlink pada data pef.
all(data_asma$pidlink %in% pef$pidlink)
# MeLihat pidlink di data_asma yang tidak ada di pef
setdiff(data_asma$pidlink, pef$pidlink)

#MeLakukan konversi format variable apabila diperlukan untuk persiapan merging.
#Cek Tipe Data pidlink di Kedua Dataset (PEF dan W5)
str(pef$pidlink)
str(data_asma$pidlink)
#MeSamakan Tipe Data (Ubah pidlink jadi karakter di kedua data set)
pef$pidlink <- as.character(pef$pidlink)
data_asma$pidlink <- as.character(data_asma$pidlink)
#Lakukan merging data dengan perintah join.
library(dplyr)
pef_joined <- pef %>%
  left_join(data_asma, by = "pidlink")
#jadikan pef sebagai patokan.(sudah sama 58.297)
#Membuat data profile diagram
summary(pef)
library(dplyr)

# 1. Import data
pef <- read.csv("path/pef.csv")
dataw5 <- read.csv("path/w5.csv")

n1 <- nrow(pef)  # Awal

# 2. Hapus duplikasi pidlink
pef <- pef %>% distinct(pidlink, .keep_all = TRUE)
n2 <- nrow(pef)

# 3. Hapus missing data penting
pef <- pef %>% filter(!is.na(pef), !is.na(age), !is.na(height))
n3 <- nrow(pef)

# 4. Hapus outlier tidak masuk akal
pef <- pef %>%
  filter(
    pef >= 50 & pef <= 800,
    age >= 5 & age <= 100,
    height >= 100 & height <= 220
  )
n4 <- nrow(pef)

# 5. Join dengan dataW5
pef$pidlink <- as.character(pef$pidlink)
pef_dataW5$pidlink <- as.character(w5$pidlink)

data_final <- left_join(pef, w5, by = "pidlink")
n5 <- nrow(pef_joined)

library(skimr)
skim(pef_joined)  
write.csv(pef_dataW5, "pef_dataW5.csv", row.names = FALSE)   
write.csv(pef_asma, "pef_asma.csv", row.names = FALSE)   
write.csv(pef_joined, "pef_join.csv", row.names = FALSE)   

#Mempersiapkan data morbidity asma & sex untuk merging dengan data pef.
#Akses data w5
library(dplyr)
dataW5 <- read.csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/w5.csv")
write.csv(dataW5, "dataW5.csv", row.names = FALSE)   
#Siapkan data baru yang hanya berisikan variable pidlink, sex dan Asthma
data_baru <- dataW5 %>%
  select(pidlink, sex, Asthma)
write.csv(data_baru, "data_baru.csv", row.names = FALSE)
#Cek frekuensi dari kategori Astma dan sex
# Cek frekuensi masing-masing variabel
library(dplyr)

# Frekuensi Asthma
dataW5 %>% count(Asthma)

# Frekuensi sex
dataW5 %>% count(sex)

# Kombinasi sex dan Asthma
dataW5 %>% count(sex, Asthma)

#Memilih observasi yang berisikan kategori ‘No-Asthma” atau “Yes-Asthma”
# Ubah nilai 0/1 (jika ada) menjadi label "No-Asthma" dan "Yes-Asthma"
dataW5$Asthma <- factor(dataW5$Asthma, levels = c(0, 1), labels = c("No-Asthma", "Yes-Asthma"))
# Filter data untuk hanya ambil "No-Asthma" dan "Yes-Asthma"
data_asthma_filtered <- dataW5 %>%
  filter(Asthma %in% c("No-Asthma", "Yes-Asthma"))
#Memilih observasi yang berisikan data sex (non-missing)
library(dplyr)

# Pilih observasi dengan nilai sex yang tidak missing
data_sex_nonmissing <- dataW5 %>%
  filter(!is.na(sex))
#MenCek variable pidlink
all(data_sex_nonmissing$pidlink %in% pef$pidlink)
#Melihat kompaTbilitas nya dengan variable pidlink pada data pef.
length(unique(dataW5$pidlink))  # Jumlah unik di dataW5
length(unique(pef$pidlink))     # Jumlah unik di pef
# Cek pidlink dari dataW5 yang ada juga di pef
sum(dataW5$pidlink %in% pef$pidlink)

# Cek sebaliknya
sum(pef$pidlink %in% dataW5$pidlink)

#MeLakukan konversi format variable apabila diperlukan untuk persiapan merging.
#Cek format variabel pidlink di kedua dataset
str(dataW5$pidlink)
str(pef$pidlink)
# Konversi ke character
dataW5$pidlink <- as.character(dataW5$pidlink)
pef$pidlink <- as.character(pef$pidlink)
#MeLakukan merging data dengan perintah join, menjadikan pef sebagai patokan 
(jumlah observasi data set final harus sama dengan jumlah observasi data pef)
library(dplyr)

# Pastikan pidlink dalam format yang sama
pef$pidlink <- as.character(pef$pidlink)
dataW5$pidlink <- as.character(dataW5$pidlink)

# Lakukan penggabungan: pef sebagai patokan (kiri), dataW5 digabungkan
data_merged <- left_join(pef, dataW5, by = "pidlink")

#Membuat data profile diagram.
library(dplyr)

# Misal jumlah aktualnya dihitung seperti ini:
n_import <- nrow(dataW5)
n_select_var <- n_import
n_non_missing_sex <- dataW5 %>% filter(!is.na(sex)) %>% nrow()
n_non_missing_asthma <- dataW5 %>% filter(!is.na(sex), !is.na(Asthma)) %>% nrow()

dataW5$pidlink <- as.character(dataW5$pidlink)
pef$pidlink <- as.character(pef$pidlink)

n_matched <- dataW5 %>%
  filter(!is.na(sex), !is.na(Asthma), pidlink %in% pef$pidlink) %>% nrow()
data_steps <- data.frame(
  Langkah = c(
    "1. Import dataW5",
    "2. Seleksi variabel: pidlink, sex, Asthma",
    "3. Hapus missing 'sex'",
    "4. Hapus missing 'Asthma'",
    "5. Konversi pidlink ke character",
    "6. Join dengan pef (left join)",
    "7. Data siap dianalisis"
  ),
  Jumlah = c(
    n_import,
    n_select_var,
    n_non_missing_sex,
    n_non_missing_asthma,
    n_non_missing_asthma, # setelah konversi format tidak ubah jumlah
    n_matched,
    n_matched
  )
)
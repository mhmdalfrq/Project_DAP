# Load library yang diperlukan
library(caret)       # Data cleaning
library(dplyr)       # Data manipulation
library(tidyverse)   # Data wrangling
library(ggplot2)     # Data visualization
library(reshape2)    # Reshape data
library(GGally)      # Additional visualization for pair plots
library(arules)      # Membuat model association
library(arulesViz)   # Visualisasi untuk model association

# 1. IMPORT DATA AWAL
# ================================
data <- read.csv("C:/Users/anast/OneDrive/Documents/ITE/SEMESTER 3/DAP/telcoo.csv")

# Melihat struktur data
str(data)
head(data)
summary(data)

# 2. PENGECEKAN DATA DUPLIKAT & MISSING VALUES 
# ================================
# Mengecek data duplikat
data[duplicated(data) | duplicated(data, fromLast = TRUE)]

# Mengecek missing values
cat("Total data yang hilang:", sum(is.na(data)), "\n")
colSums(is.na(data))  # Missing values per kolom

# Menghapus baris dengan NA di kolom penting (misal: `TotalCharges`)
cust_bersih <- data[complete.cases(data), ]
cat("Nilai yang hilang setelah dibersihkan:", sum(is.na(cust_bersih)), "\n")

# 3. MENGUBAH TIPE DATA DAN MENYERAGAMKAN NAMA KOLOM
# ================================
# Menyeragamkan nama kolom
names(cust_bersih)[names(cust_bersih) == "customerID"] <- "CustomerID"
names(cust_bersih)[names(cust_bersih) == "gender"] <- "Gender"
names(cust_bersih)[names(cust_bersih) == "tenure"] <- "Tenure"

# Mengubah `SeniorCitizen` menjadi kategori "No" dan "Yes"
cust_bersih$SeniorCitizen <- ifelse(cust_bersih$SeniorCitizen == 0, "No", "Yes")

# Transformasi tipe data numerik untuk TotalCharges jika diperlukan
cust_bersih$TotalCharges <- as.numeric(as.character(cust_bersih$TotalCharges))

# Daftar kolom yang akan dikonversi
cols_to_factor <- c("SeniorCitizen", "Partner", "Dependents", "PhoneService", 
                    "MultipleLines", "InternetService", "OnlineSecurity", 
                    "OnlineBackup", "DeviceProtection", "TechSupport", 
                    "StreamingTV", "StreamingMovies", "Contract", 
                    "PaperlessBilling", "PaymentMethod")

# Konversi kolom-kolom tersebut menjadi faktor
cust_bersih[cols_to_factor] <- lapply(cust_bersih[cols_to_factor], factor)

# Periksa apakah kolom telah dikonversi
str(cust_bersih)

# 4. VISUALISASI DATA
# ================================
# Membuat grafik batang untuk kolom Churn
ggplot(cust_bersih, aes(x = Churn, fill = Churn)) +
  geom_bar() +
  labs(title = "Keputusan Pemberhentian Langganan", x = "", y = "Jumlah Customer") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("No" = "seagreen3", "Yes" = "coral2")) +
  guides(fill = FALSE)

# Distribusi churn berdasarkan Gender
ggplot(cust_bersih, aes(x = Gender, fill = Churn)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribusi Churn Berdasarkan Gender", x = "Gender", y = "Jumlah Pelanggan") +
  theme_minimal()

# Boxplot MonthlyCharges berdasarkan Churn
ggplot(cust_bersih, aes(x = Churn, y = MonthlyCharges, fill = Churn)) +
  geom_boxplot() +
  labs(title = "Distribusi MonthlyCharges Berdasarkan Churn", x = "Churn", y = "Monthly Charges") +
  theme_minimal()

# Keputusan Pemberhentian Langganan berdasarkan Durasi Berlangganan (dalam Bulan)
ggplot(cust_bersih, aes(x = Tenure, fill = Churn)) +
  geom_histogram(binwidth = 5, color = "black", position = "dodge") +
  facet_wrap(~ Churn) +
  labs(title = "Keputusan Pemberhentian Langganan berdasarkan Durasi Berlangganan (dalam Bulan)",
       x = "Durasi Berlangganan (Bulan)",
       y = "Jumlah Customer") +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "seagreen3", "Yes" = "coral2"))

# Keputusan Pemberhentian Langganan berdasarkan Jumlah Tagihan Per Bulan
ggplot(cust_bersih, aes(x = MonthlyCharges, fill = Churn)) +
  geom_histogram(binwidth = 5, color = "black", position = "dodge") +
  facet_wrap(~ Churn) +
  labs(title = "Keputusan Pemberhentian Langganan berdasarkan Jumlah Tagihan Per Bulan",
       x = "Jumlah Tagihan Per Bulan",
       y = "Jumlah Customer") +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "seagreen3", "Yes" = "coral2"))

# 5. ANALISIS ASSOCIATION RULES
# ================================
# Memilih kolom layanan untuk association rules
cust_bersih_1 <- cust_bersih %>% select(PhoneService, InternetService, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, StreamingTV, StreamingMovies)

# Mengonversi data frame menjadi objek transaksi
cust_bersih_1.tr <- as(cust_bersih_1, "transactions")

# Membuat grafik frekuensi item
itemFrequencyPlot(cust_bersih_1.tr, 
                  topN = 15,                # Menampilkan 15 item teratas
                  type = "absolute",        # Menampilkan frekuensi dalam jumlah absolut
                  ylim = c(0, 7000),        # Mengatur batas y-axis dari 0 hingga 7000
                  main = "Top 15 Layanan Telco",  # Judul grafik
                  col = rainbow(15))        # Warna batang
#
freq.itemset <- apriori(cust_bersih_1, parameter = list(support=0.3, conf= 0.8, minlen=2, target="frequent"))
freq.itemset
inspect(freq.itemset)

#rules spesifik 1
rules1 <- apriori(cust_bersih_1.tr, 
                  parameter = list(supp = 0.2, conf = 0.5),
                  appearance = list(default = "lhs", rhs = c("StreamingMovies=Yes")),
                  control = list(verbose = FALSE))

rules1 <- sort(rules1, decreasing = TRUE, by = "confidence")
inspect(rules1)

# Visualisasi aturan dengan kontrol warna menggunakan parameter `colors`
plot(rules1, method = "graph", control = list(colors = c("red", "blue")))

#rules spesifik 2
rules2<-apriori(cust_bersih_1, parameter=list(supp=0.2,conf = 0.5,minlen=2), appearance = list(default="rhs",lhs=c("StreamingMovies=Yes", "StreamingMovies=No")),control = list(verbose=F))
rules2<-sort(rules2, decreasing=TRUE,by="confidence")
inspect(rules2)

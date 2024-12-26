#Nama: Gusti Bagus Alvian Ditya Parasurama /NIM: 2415091075/Kelas: SI-Dps

library(readxl)
library(car)
library(ggplot2)
library(dplyr)

# 1. Import Data Excel
# Ganti path sesuai lokasi file Anda
data <- read_excel("~/data anova 2.xlsx", sheet = "Data")

# Melihat data untuk memastikan formatnya benar
head(data)

# 2. Analisis Uji Asumsi

# a. Uji Normalitas (Shapiro-Wilk test)
cat("Uji Normalitas (Shapiro-Wilk):\n")
apps <- unique(data$book_type)
for (app in apps) {
  cat("\nNormalitas untuk", book, ":\n")
  print(shapiro.test(data$score[data$book_type == app]))
}

# b. Uji Homogenitas Varians (Levene Test)
cat("\nUji Homogenitas Varians (Levene):\n")
levene_test <- leveneTest(score ~ book_type, data = data)
print(levene_test)

# c. Independensi (Independensi diasumsikan melalui desain penelitian)

# 3. Lakukan Uji Signifikansi
# Hipotesis:
# H0: Tidak ada perbedaan rata-rata skor antar buku.
# H1: Ada perbedaan rata-rata skor antar buku.

anova_result <- aov(score ~ book_type, data = data)
cat("\nHasil ANOVA:\n")
summary(anova_result)

# Jika ANOVA signifikan, lakukan uji post-hoc Tukey HSD
cat("\nUji Post-Hoc Tukey HSD:\n")
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# 4. Visualisasi Data

# a. Boxplot untuk distribusi skor
ggplot(data, aes(x = book_type, y = score, fill = book_type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Distribusi Skor Berdasarkan Jenis Buku",
    x = "Jenis Buku",
    y = "Skor"
  )

# b. Distribusi skor untuk setiap buku
ggplot(data, aes(x = score, fill = book_type)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Distribusi Skor Per Buku",
    x = "Skor",
    y = "Kepadatan"
  ) +
  facet_wrap(~ book_type)


# Estimasi Hukum Mortalitas Gompertz dan Makeham

Repository ini berisi kode dan dokumentasi penelitian skripsi berjudul **“Pendekatan Least Squares untuk Estimasi Parameter pada Hukum Mortalitas Gompertz dan Makeham dalam Konstruksi Tabel Mortalitas Indonesia IV”**.

Penelitian ini bertujuan untuk mengestimasi parameter pada hukum mortalitas **Gompertz** dan **Makeham** menggunakan metode **Least Squares**, baik **Ordinary Least Squares (OLS)** maupun **Weighted Least Squares (WLS)**, untuk membangun tabel peluang kematian.

Data yang digunakan berasal dari **Tabel Mortalitas Indonesia IV tahun 2019**. Estimasi parameter dilakukan dengan pendekatan numerik menggunakan **metode Secant**, kemudian hasil estimasi digunakan untuk membentuk tabel peluang kematian dan dibandingkan dengan data aktual.

## Metode

* Data: Tabel Mortalitas Indonesia IV (2019)
* Model: Gompertz & Makeham
* Estimasi: Least Squares & Weighted Least Squares
* Optimasi numerik: Secant Method
* Evaluasi: Mean Squared Error (MSE)
* Software: R

## 📊 Results

Berdasarkan nilai MSE, model terbaik diperoleh dari **hukum Makeham dengan Weighted Least Squares menggunakan bobot w₁**, dengan:

| Kelompok  |          MSE |
| --------- | -----------: |
| Laki-laki | 0.0001423178 |
| Perempuan | 0.0002606468 |

Hasil penelitian menunjukkan bahwa **model Makeham memberikan performa yang lebih baik dibandingkan model Gompertz** dalam membangun tabel mortalitas menggunakan pendekatan Least Squares.

# Pengantar ----

# Pada basic rvest kali ini kita akan belajar untuk mengambil
# konten berita dari portal berita tirto.id
#
# Elemen - elemen yang akan diambil dalam berita tersebut, a.l:
# 
# 1. Judul
# 2. Tanggal terbit
# 3. Isi/konten
#
# rvest merupakan packages yang dikembangkan oleh Hadley Wickham
# yang digunakan untuk mengambil data pada static website. rvest 
# dibuat dengan mengambil inpirasi dari modul Beautiful Soup pada
# python.
#
# Fungsi-fungsi rvest yang akan sering digunakan pada tutorial kali
# ini, antara lain:
#
# 1. read_html()    : Parsing halaman HTML
# 2. html_nodes()   : Memilih node dari dokumen HTML
# 3. html_text()    : Mengekstrak teks dari dari node terpilih
# 4. html_attr()    : Mengekstrak atribut dari node terpilih


# Import Library -----

# install.packages("tidyverse")
# install.packages("furrr)
library(tidyverse)
library(rvest)
library(furrr)

plan(multiprocess)

# Ekstrak Komponen Berita ----

page <- read_html("https://tirto.id/jurus-travel-haji-umrah-hadapi-corona-phk-hingga-reseller-produk-fE9H")

# 1. Judul
judul <- page %>%
  html_nodes('.my-3') %>%
  html_text() %>%
  .[2]

# 2. Tanggal terbit
tanggal <- page %>%
  html_nodes('.mt-1') %>%
  html_text() %>%
  # Buang karakter sebelum tanda "- " dan tanda "- "
  str_replace(".*- ", "") %>%
  str_trim("both")

# 3. Isi/konten
isi <- page %>%
  html_nodes('.content-text-editor:nth-child(2)') %>%
  html_text() %>%
  str_replace(".*- ", "")

# Membuat Fungsi Untuk Mengekstrak Isi Berita ----

# Untuk mencegah proses pengambilan elemen web secara berulang-
# ulang, proses pengambilan elemen dapat dibentuk ke dalam sebuah
# fungsi.
#
# Anatomi fungsi dalam R dapat dituliskan sebagai berikut:
#
# <nama_fungsi> <- function(<argumen>){
#                 
#                   perintah
#
#}
#
# Fungsi berita() merupakan fungsi untuk mengambil elemen berita.
# Fungsi ini memiliki sebuah argumen, yaitu: alaman website. Fungsi
# berita() memiliki tiga buah perintah, yaitu:
#
# 1. Parsing html dan menyimpannya ke dalam objek page
# 2. Tibble yang berisi kolom-kolom elemen yang diambil dan 
#    disimpan ke dalam objek data
# 3. Menonaktifkan scraper sebelum digunakan kembali. Hal ini berguna
#    apabila scraper digunakan untuk mengambil data dari beberapa
#    url. Penonaktifan akan mencegah server website untuk memutus
#    koneksi yang dibentuk oleh fungsi akibat pengambilan terus-
#    menerus. 

berita <- function(url){
  
  page <- read_html(url)
  
  data <- tibble(
    
    # 1. Judul
    judul = page %>%
      html_nodes('.my-3') %>%
      html_text() %>%
      .[2],
    
    # 2. Tanggal terbit
    tanggal = page %>%
      html_nodes('.mt-1') %>%
      html_text() %>%
      # Buang karakter sebelum tanda "- " dan tanda "- "
      str_replace(".*- ", "") %>%
      str_trim("both"),
    
    # 3. Isi/konten
    isi = page %>%
      html_nodes('.content-text-editor:nth-child(2)') %>%
      html_text() %>%
      str_replace(".*- ", "")%>%
      str_replace_all("[\n\r]", " ")
    
  )%>%
    mutate(tanggal = case_when(str_detect(tanggal, "Januari") ~ str_replace(tanggal, "Januari","1"),
                               str_detect(tanggal, "Februari") ~ str_replace(tanggal, "Februari", "2"),
                               str_detect(tanggal, "Maret") ~ str_replace(tanggal, "Maret", "3"),
                               str_detect(tanggal, "April") ~ str_replace(tanggal, "April", "4"),
                               str_detect(tanggal, "Mei") ~ str_replace(tanggal, "Mei", "5"),
                               str_detect(tanggal, "Juni") ~ str_replace(tanggal, "Juni", "6"),
                               str_detect(tanggal, "Juli") ~ str_replace(tanggal, "Juli", "7"),
                               str_detect(tanggal, "Agustus") ~ str_replace(tanggal, "Agustus", "8"),
                               str_detect(tanggal, "September") ~ str_replace(tanggal, "September", "9"),
                               str_detect(tanggal, "Oktober") ~ str_replace(tanggal, "Oktober", "10"),
                               str_detect(tanggal, "November") ~ str_replace(tanggal, "November", "11"),
                               TRUE ~ str_replace(tanggal, "Desember", "12")
                               )) %>%
    mutate(tanggal = lubridate::dmy(tanggal))
  
  Sys.sleep(runif(1, 5, 8))
  
  return(data)
}

# Test fungsi berita()

url <- "https://tirto.id/alasan-keluarga-tak-bisa-pindahkan-jenazah-pdp-meski-negatif-corona-fF4W"
berita(url)

# Mangambil Link Berita dari Halaman Pencarian Berita ----

# Terkadang kita membutuhkan beberapa berita dengan topik sama
# yang akan dianalisa. Untuk mengambil beberapa berita tersebut,
# terlebih dahulu kita perlu membuat sebuah fungsi untuk mengambil
# link berita dari halaman pencarian sebelum isi link tersebut
# diekstrak.

# Ambil link dari halaman pertama

url <- "https://tirto.id/search?q=haji%202020&p=1"

link <- read_html(url) %>%
  html_nodes('.hidden-over+ a') %>%
  html_attr('href') %>%
  paste0("https://tirto.id/",.)

# Uji link terambil

data <- map_dfr(link, possibly(berita, otherwise = NULL))


# Membuat Fungsi Untuk Mengekstrak Tautan ----

link <- function(url){
  
  data <- tibble(
  tautan = read_html(url) %>%
    html_nodes('.hidden-over+ a') %>%
    html_attr('href') %>%
    paste0("https://tirto.id/",.)
  )
    
  
  return(data)
}


# Mengambil Sejumlah Berita Berdasarkan Kata Kunci ----

# Ambil tautan berita dengan kata kunci Haji 2020

url <- paste0("https://tirto.id/search?q=haji%202020&p=",1:20)

# single thread
tautan <- map_dfr(url, possibly(link, otherwise = NULL))

# multi threads
tautan <- future_map_dfr(url, possibly(link, otherwise = NULL),
                          .progress = TRUE)

# Ekstrak berita

# single thread
haji_2020 <- map_dfr(tautan%>% select(tautan)%>%pull(), 
                     possibly(berita, otherwise = NULL))

# multi threads
haji_2020 <- future_map_dfr(tautan%>% select(tautan)%>%pull(), 
                     possibly(berita, otherwise = NULL),
                     .progress = TRUE)


# Simpan Data ----

write_rds(haji_2020, "data/haji-2020.rds")

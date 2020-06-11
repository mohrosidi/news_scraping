# Pendahuluan -----

# rvest sangat mudah digunakan untuk melakukan scraping pada static
# website. Namun, rvest tidak dapat digunakan untuk melakukan scraping
# pada website yang memuat kontennya secara dinamik.
#
# Pada tutorial kali ini kita akan mengambil konten berita dari
# kumparan.com. Pada halaman pencarian berita pada website tersebut,
# daftar berita dimuat secara dynamic. Daftar berita akan terus dimuat
# saat kita melakukan scroll pada akhir halaman. Untuk mengambil
# link dari daftar berita tersebut, kita memerlukan RSelenium 
# yang dapat menangani javascript pada website tersebut. Daftar 
# tautan berita yang telah diambil dengan bantuan RSelenium
# selanjutnya akan dilakukan scraping menggunakan bantuan rvest.
# 

# Import Packages -----

# install.packages("tidyverse")
# install.packages("furrr")
# install.packages("RSelenium")
library(tidyverse)
library(rvest)
library(furrr)
library(RSelenium)

plan(multiprocess)

# Mengambil Elemen Berita ----

url <- "https://kumparan.com/kumparannews/kemenag-soal-haji-batal-ada-jemaah-yang-tetap-ingin-berangkat-meninggal-syahid-1tXcxcV1V4o/full"
page <- read_html(url)

# 1. Judul

judul <- page %>%
  html_nodes('.URDbM') %>%
  html_text()

# 2. Tangal terbit

tanggal <- page %>%
  html_nodes('.iPcvDt') %>%
  html_text()

# 3. Isi/konten

isi <- page %>%
  html_nodes('.dWDCSS') %>%
  html_text() %>%
  paste(collapse = " ")


# Membuat Fungsi Untuk Mengambil Konten Berita ----

berita <- function(url){
  
  page <- read_html(url)
  
  data <- tibble(
    
    # 1. Judul
    
    judul = page %>%
      html_nodes('.URDbM') %>%
      html_text(),
    
    # 2. Tangal terbit
    
    tanggal = page %>%
      html_nodes('.iPcvDt') %>%
      html_text(),
    
    # 3. Isi/konten
    
    isi = page %>%
      html_nodes('.dWDCSS') %>%
      html_text() %>%
      paste(collapse = " ")
    
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
    mutate(tanggal = lubridate::dmy_hm(tanggal))
  
  Sys.sleep(runif(1,5,8))
  
  return(data)
  
}

# Test fungsi
berita(url)

# Mengambil Tautan Menggunakan RSelenium -----

# start the server and browser(you can use other browsers here)
rD <- rsDriver(browser=c("chrome"), port = 4445L)

driver <- rD$client

# navigate to an URL
driver$navigate("https://kumparan.com/search/haji%202020/")



#close the driver
driver$close()

#close the server
rD$server$stop()

# Mengambil Konten Berita dari Tautan ------

# single thread
haji_2020 <- map_dfr(tautan, possibly(berita, otherwise = NULL))

# multi threads
haji_2020 <- future_map_dfr(tautan, possibly(berita, otherwise = NULL),
                            .progress = TRUE)

# Menyimpan Konten Berita -----

write_rds(haji_2020, "data/haji-2020-kumparan.rds")



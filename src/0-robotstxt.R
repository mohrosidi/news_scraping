# Pendahuluan ----
#
# Sebelum melakukan kegiatan scraping, terlebih dahulu kita perlu
# mengetahui bagian-bagian website mana saja yang dapat diambil.
# Untuk melakukannya kita dapat mengecek file robots.txt yang ada
# pada tiap website. Untuk melakukannya, kita dapat menambahkan
# robots.txt pada akhir nama website atau kita dapat menggunakan
# packages robotstxt.


# Import Packages ----
# install.packages("robotstxt)
library(robotstxt)

# Cek Konten File RObots.txt Suautu Website ----

rtxt <- robotstxt(domain = "https://kumparan.com/")
rtxt$permissions %>% tbl_df()

# Cek Apakah Suatu Dapat Diakses/Diambil Datanya ----
paths_allowed("https://kumparan.com/babelhits/cerita-warga-bangka-selatan-gagal-berhaji-setelah-menunggu-9-tahun-1tapDxXPe1L")


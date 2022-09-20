library(here)
library(x3ptools)
library(tidyverse)
library(bulletxtrctr)
library(reticulate)

x3p_files <- list.files(recursive = TRUE,
                        pattern = "\\.x3p$") 
x3p_files <- x3p_files[x3p_files %>% str_detect("Scan 1")]

bullets <- tibble(file_path = x3p_files)
bullets <- bullets %>% mutate(
  x3p = purrr::map(file_path, read_x3p)
)

bullets <- bullets %>% mutate(
  info = map(file_path, function(p) {
    tibble(
      barrel = p %>% str_extract("Barrel \\d+") %>% str_extract("\\d+"),
      bullet = p %>% str_extract("Bullet \\d+") %>% str_extract("\\d+"),
      land = p %>% str_extract("Land \\d+") %>% str_extract("\\d+")
    )
  })
) %>% unnest(cols = info)

# down_sample by 2
bullets_down_sampled <- bullets %>% mutate(
  x3p = purrr::map(x3p, x3p_sample)
)

surface_matrix_dim <- sapply(bullets_down_sampled$x3p, function(xx) {
  dim(xx$surface.matrix)
}) %>% apply(1, max)

output_arr <- array(0, dim = c(nrow(bullets_down_sampled),
                               surface_matrix_dim[1],
                               surface_matrix_dim[2]))
for (i in 1:nrow(bullets_down_sampled)) {
  tmp_ <- bullets_down_sampled$x3p[[i]]$surface.matrix
  
  output_arr[i, 1:dim(tmp_)[1], 1:dim(tmp_)[2]] <- tmp_
}

np = import("numpy")
np$save("python_data/barrel_1_data.npy", r_to_py(output_arr))








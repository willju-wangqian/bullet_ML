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


x3p_image(bullets_down_sampled$x3p[[7]])
print(bullets_down_sampled$x3p[[1]])


bullets_down_sampled <- bullets_down_sampled %>% mutate(
  x3p = x3p %>% purrr::map(rotate_x3p, angle = 180)
)

bullets_down_sampled <- bullets_down_sampled %>% 
  mutate(crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize))
bullets_down_sampled <- bullets_down_sampled %>% 
  mutate(ccdata = purrr::map2(.x = x3p, .y = crosscut, 
                              .f = x3p_crosscut))
bullets_down_sampled <- bullets_down_sampled %>% 
  mutate(grooves = ccdata %>% purrr::map(.f = cc_locate_grooves, 
                                         method = "middle", adjust = 30, return_plot = TRUE))
bullets_down_sampled <- bullets_down_sampled %>% 
  mutate(sigs = purrr::map2(.x = ccdata, .y = grooves, 
                            .f = function(x, y) {
                              cc_get_signature(ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
                            }))

bullets_down_sampled$bulletland <- paste0(bullets_down_sampled$bullet, "-", bullets_down_sampled$land)
lands <- unique(bullets_down_sampled$bulletland)
comparisons <- data.frame(expand.grid(land1 = lands, land2 = lands), stringsAsFactors = FALSE)

comparisons <- comparisons %>% mutate(aligned = purrr::map2(.x = land1, .y = land2, 
                                                            .f = function(xx, yy) {
                                                              land1 <- bullets_down_sampled$sigs[bullets_down_sampled$bulletland == xx][[1]]
                                                              land2 <- bullets_down_sampled$sigs[bullets_down_sampled$bulletland == yy][[1]]
                                                              land1$bullet <- "first-land"
                                                              land2$bullet <- "second-land"
                                                              
                                                              sig_align(land1$sig, land2$sig)
                                                            }))

library(cmpsR)
comparisons <- comparisons %>% mutate(
  cmps = purrr::map(aligned, function(aa) {
    sig1 <- aa$lands$sig1
    sig2 <- aa$lands$sig2
    extract_feature_cmps(sig1, sig2, include = 'full_result')
  })
)

comparisons_light <- comparisons %>% select(-aligned)

comparisons_light$cmps_score <- sapply(comparisons_light$cmps, function(cc) cc$CMPS_score)
comparisons_light$nseg <- sapply(comparisons_light$cmps, function(cc) cc$nseg)
comparisons_light <- comparisons_light %>% filter(land1 %in% lands[1:6], land2 %in% lands[7:12])

comparisons_light %>% select(-cmps)

result_matrix <- matrix(comparisons_light$cmps_score / comparisons_light$nseg, nrow=6)





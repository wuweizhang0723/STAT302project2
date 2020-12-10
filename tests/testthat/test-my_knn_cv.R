data("my_penguins")
data <- drop_na(my_penguins) %>% dplyr::select(-island, -sex, -year)
train <- data %>% dplyr::select(-species)
species <- data$species
test_that("my_knn_cv returns a list", {
  expect_is(my_knn_cv(train, species, 1, 5), "list")
})

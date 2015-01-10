#Conditional Probabilities through Bootstrapping -> Thresholding based on clustering (1D)
#Graph of interconnected things (like that D3 graph) once we pair stuff up
#Graphical Network Analysis (eigenvalues)
#PCA!!
#Clustering analysis?

#Conditional Heatmap - Analyze probabilities for the proposed groups versus probabilities for rest

#Writing this up in an Rmd file

sales.file.name <- 'sales-data.csv'

get.sales.data <- function() {
    sales.file <- read.csv(sales.file.name)
    return(sales.file)
}

get.sales.items <- function(sales) {
    sales.items <- grep('item', colnames(sales), value = TRUE)
    return(sales.items)
}

get.probabilities <- function(sales) {
    probabilities <- sapply(get.sales.items(sales), function(x) {
        sum(sales[[x]]) / nrow(sales)
    })
}

get.conditional.probabilities <- function(sales, item) {
    sales.subset <- filter.sales(sales, item)
    conditional.probabilities <- get.probabilities(sales.subset)
}

make.coincidence.matrix <- function(sales) {
    sales.items <- get.sales.items(sales)
    coincidence.matrix <- data.frame()
    for(i in 1:length(sales.items)) {
        sale.item <- sales.items[i]
        conditional.probabilities.for.item <- get.conditional.probabilities(sales, sale.item)
        for(j in 1:length(conditional.probabilities.for.item)) {
            coincidence.matrix[i, j] = conditional.probabilities.for.item[j]
        }
    }
    colnames(coincidence.matrix) <- sales.items
    rownames(coincidence.matrix) <- sales.items
    return(coincidence.matrix)
}

filter.sales <- function(sales, item) {
    sales.subset <- sales[sales[[item]] == 1, ]
    return(sales.subset)
}

probability.analysis <- function() {
    total.sales <- get.sales.data()
    probs <- get.probabilities(total.sales)
    plot(1:length(probs), probs)
}

conditional.probability.analysis <- function() {
    total.sales <- get.sales.data()
    matrix <- make.coincidence.matrix(total.sales)
    colors <- colorRampPalette(c("white","cornflowerblue"))(32);
    heatmap(t(data.matrix(matrix)), col = colors, symm = TRUE)
    head(sort(unlist(get.probabilities(total.sales)), decreasing = TRUE), n = 15)
}
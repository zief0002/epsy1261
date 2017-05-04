#' Histogram
#'
#' This function allows you to interactively create a scatterplot and acess the ggplot syntax.
#' @param data dataframe.
#' @keywords histogram
#' @export
#' @examples
#' data(cars)
#' histogram(cars)

histogram = function(data){

  # Keep name of data
  dataname = deparse(substitute(data))

  # Make sure dataframe has numeric variables
  if( sum(sapply(data, class) == "numeric" | sapply(data, class) == "integer") < 1  ){
    cat("The dataset does not include numeric variables.")
  }

  # Store only the numeric variables
  temp = data[sapply(data, class) == "numeric" | sapply(data, class) == "integer"]


  manipulate::manipulate({

    # Create and print ggplot
    p = ggplot2::ggplot(data = temp, ggplot2::aes_string(x = X)) + ggplot2::geom_histogram(color = "black", bins = numBins)
    print(p)

    # Print syntax for button click
    if(show){
      cat("\014")
      cat(paste('ggplot(data = ', dataname, ', aes(x = ', X, ')) + \n geom_histogram(color = "black", bins = ', numBins, ') \n\n', sep = ""))
    }
  },

  # Add interactivity to plot
  X = manipulate::picker(as.list(colnames(temp)), initial = as.list(colnames(temp))[[1]] ),
  numBins = manipulate::slider(min = 1, max = 30, initial = 30, label = "Number of Bins"),
  show = manipulate::button("Show ggplot syntax")
  )
}



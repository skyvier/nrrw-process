library(ggplot2)
library(ggthemes)

process_simulations <- function (folder_path, step_parameter) {
   read_degrees_file <- function (filename) {
      data.list <- read.table(filename)
      data <- unlist(data.list, use.names=F)
      data.tail <- Filter(function (x) { return(x >= 2); }, data)
      return(data.tail);
   }

   approx_cdf <- function (sample, ks) {
      mapped <- Map(function (k) { length(sample[sample >= k + 1]) / length(sample) }, ks)
      return(unlist(mapped))
   }

   # Calculations

   files <- list.files(folder_path, pattern="^degrees_[0-9]+.csv")

   complete_data <- c()
   for (file in files) {
      degrees <- read_degrees_file(file.path(folder_path, file))
      complete_data <- c(complete_data, degrees) 
   }

   ks <- 1:(max(complete_data)-1)
   sample_cdf <- approx_cdf(complete_data, ks)
   lower_bound_cdf <- ks^(-step_parameter/2)

   # Nonlinear regression
   
   regression.data <- data.frame(y = sample_cdf, x = ks)
   regression.model <- nls(y ~ x^(-b), data = regression.data, start = list(b = step_parameter / 2))
   regression.estimate <- predict(regression.model)

   # Drawing graphs

   common.theme <- theme_hc()

   print("Building CDF plot..")

   y <- c(sample_cdf, regression.estimate, lower_bound_cdf)
   x <- rep(ks, 3)
   groups <- factor(c(rep("Empiirinen", length(ks)), rep("Estimoitu", length(ks)), rep("Alaraja", length(ks))))
   sizes <- c(rep(1, length(ks)), rep(0, length(ks)), rep(1, length(ks)))
   cdf.data <- data.frame(x = x, y = y, groups = groups, sizes = sizes)

   cdf.plot <- ggplot(cdf.data, aes(x, y, linetype=groups, colour=groups)) + 
      scale_y_log10(name = "Komplementaarinen kertymäfunktio") +
      scale_x_log10(name = "Asteluku") + geom_line(size = 1) +
      scale_color_manual(values=c("gray", "black", "blue")) +
      ggtitle("Komplementaarinen kertymäfunktio ja sen alaraja") +
      common.theme + theme(plot.title = element_text(hjust=0.5), 
                         legend.position = c(0.9, 0.9), legend.title = element_blank(),
                         legend.text = element_text(size=15))
   ggsave(filename=file.path(folder_path, 'approx_cdf.jpg'), plot=cdf.plot)

   print("Building HIST plot..")

   init_indices <- 1:(0.998*length(complete_data))

   hist.frequencies <- data.frame(init = sort(complete_data)[init_indices])
   hist.plot <- ggplot(hist.frequencies, aes(x = init)) + geom_histogram(col="black", fill="grey", bins=20) + 
      scale_x_log10(name = "Asteluku", breaks=c(2,10,100,1000,5000)) + 
      scale_y_continuous(name = "Määrä") + ggtitle("Astelukujen esiintyvyys") + 
      common.theme + theme(plot.title = element_text(hjust=0.5))
   ggsave(filename=file.path(folder_path, 'approx_hist.jpg'), plot=hist.plot)

   return(list(sample_cdf = sample_cdf,
               lower_bound_cdf = lower_bound_cdf,
               data = complete_data,
               regression = list(model = regression.model, estimate = regression.estimate)))
}

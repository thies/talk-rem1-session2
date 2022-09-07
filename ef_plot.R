library(tidyr)
library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series

library(svglite)


try( setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))



tick <- c('AMZN', 'AAPL', 'NFLX', 'XOM', 'T')

price_data <- tq_get(tick,
                     from = '2014-01-01',
                     to = '2018-05-31',
                     get = 'stock.prices')

log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

head(log_ret_tidy)


log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()


mean_ret <- colMeans(log_ret_xts)
print(round(mean_ret, 5))


cov_mat <- cov(log_ret_xts) * 252

print(round(cov_mat,4))

# Calculate the random weights
wts <- runif(n = length(tick))
wts <- wts/sum(wts)

# Calculate the portfolio returns
port_returns <- (sum(wts * mean_ret) + 1)^252 - 1

# Calculate the portfolio risk
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))

# Calculate the Sharpe Ratio
sharpe_ratio <- port_returns/port_risk

print(wts)


print(port_returns)


num_port <- 50000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = length(tick))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)




for (i in seq_along(port_returns)) {
  
  wts <- runif(length(tick))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}



# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)



colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

head(portfolio_values)

min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]




library(remotes)
remotes::install_version("Rttf2pt1", version = "1.3.8")
library(extrafont)
font_import(paths="~/.fonts/", prompt =FALSE, pattern="Source")
fonts()



# convex hull

p <- subset(portfolio_values, Risk < quantile(portfolio_values$Risk, 0.95))
ps <- portfolio_values[sample (1:nrow(portfolio_values), size=1000, replace =F),]
h <- chull(portfolio_values[,c("Risk","Return")])
hull <- portfolio_values[h,c("Risk","Return")]
ef <- subset(as.data.frame(hull), Return >= min_var$Return)



plot1 <- function(){
  par(cex.axis=2, cex.lab=2, cex.main=2, cex.sub=2)
  par(mar=c(4,5,1,1))
  plot(p[,c("Risk","Return")], type="n", axes=FALSE)
  points(ps[,c("Risk","Return")], col="black", pch=19, cex=0.5)
  box()
}



plot2 <- function(){
  plot1()
  lines(hull, col="red", lwd=3)
}


plot3 <- function(){
  plot2()
  lines(ef, col="darkblue", lwd=4)
  points(min_var[c("Risk","Return")], cex=3, col="orange", lwd=5)
  legend("topleft", c("Minimum Variance Portfolio","Efficient Frontier", "NOT ☠ the Efficient Frontier"), lwd=5, col=c("orange","darkblue","red"), bty="n", cex=2)
}
plot3()


svg("imgs/ef1.svg", width = 12, height = 6, family="Source Sans Pro Semibold")
plot1()
dev.off()


svg("imgs/ef2.svg", width = 12, height = 6, family="Source Sans Pro Semibold")
plot2()
dev.off()

svg("imgs/ef3.svg", width = 12, height = 6, family="Source Sans Pro Semibold")
plot3()
dev.off()




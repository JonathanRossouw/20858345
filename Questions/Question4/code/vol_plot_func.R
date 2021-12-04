# Plot Smoothed Ret^2
vol_plot_func <- function(data = zar_ret, fit = garch_fit){
    # To view the conditional variance plot, use:
    sigma <- sigma(fit) %>% xts_tbl()
    colnames(sigma) <- c("date", "sigma")
    sigma <- sigma %>% mutate(Date = as.Date(date))

    Plotdata <- data %>%
        mutate(Returns = dlogret,
               Returns_Sqd = dlogret^2,
               Returns_Abs = abs(dlogret)) %>%
        pivot_longer(c("Returns", "Returns_Sqd", "Returns_Abs"), names_to = "ReturnType", values_to = "Returns")

    gg <- Plotdata %>%

        ggplot() +
        geom_line(data = Plotdata %>% filter(ReturnType == "Returns_Sqd") %>% select(Date, Returns) %>% unique() %>% mutate(Returns = sqrt(Returns)), aes(x = Date, y = Returns), alpha = 0.8) +

        geom_line(data = sigma, aes(x = Date, y = sigma), color = "red", size = 2, alpha = 0.8) +
        labs(title = "Comparison: Returns Sigma vs Sigma from Garch", x = "", y = "Comparison of estimated volatility") +
        fmxdat::theme_fmx(title = ggpts(25))
    fmxdat::finplot(gg, y.pct = T, y.pct_acc = 1)

}
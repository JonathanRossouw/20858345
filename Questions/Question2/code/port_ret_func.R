# Portfolio Return Function

port_ret_func <- function(data = T40, sector = "", index = ""){
    fund <- data

    if(!sector == ""){
        fund <- data %>% filter(Sector %in% sector) %>%
            group_by(date) %>%
            mutate(J400 = J400/sum(J400, na.rm = TRUE), J200 = J200/sum(J200, na.rm = TRUE)) %>%
            ungroup()
    }
    if(!index == ""){
        fund <- data %>% filter(Index_Name %in% index) %>%
            group_by(date) %>%
            mutate(J400 = J400/sum(J400, na.rm = TRUE), J200 = J200/sum(J200, na.rm = TRUE)) %>%
            ungroup()
    }
    if(!sector == "" & !index == ""){
        fund <- data %>%
            group_by(date) %>%
            mutate(J400 = J400/sum(J400, na.rm = TRUE), J200 = J200/sum(J200, na.rm = TRUE)) %>%
            ungroup()
    }

    fund_J200_weights <- fund %>% select(date, Tickers, J200) %>% mutate(J200 = coalesce(J200, 0)) %>%
        spread(Tickers, J200) %>%
        tbl_xts()
    fund_J200_weights[is.na(fund_J200_weights)] <- 0

    fund_J400_weights <- fund %>% select(date, Tickers, J400) %>% spread(Tickers, J400) %>% tbl_xts()
    fund_J400_weights[is.na(fund_J400_weights)] <- 0

    fund_returns <- fund %>% select(date, Tickers, Return) %>% spread(Tickers, Return)
    fund_returns[is.na(fund_returns)] <- 0
    fund_returns_xts <- fund_returns %>% tbl_xts()

    J200_RetPort <- rmsfuns::Safe_Return.portfolio(fund_returns_xts,
                                                   weights = fund_J200_weights, lag_weights = TRUE,
                                                   contribution = TRUE, verbose = TRUE,
                                                   value = 1, geometric = TRUE)

    J400_RetPort <- rmsfuns::Safe_Return.portfolio(R = fund_returns_xts,
                                                   weights = fund_J400_weights, lag_weights = TRUE,
                                                   contribution = TRUE, verbose = TRUE,
                                                   value = 1000, geometric = TRUE)

    # Clean and save portfolio returns and weights:
    J200_Contribution <- J200_RetPort$"contribution" %>% xts_tbl() %>%
        mutate(date = lag(date), date = coalesce(date, index(fund_J200_weights)[1]) )

    J200_BPWeight <- J200_RetPort$"BOP.Weight" %>% xts_tbl() %>%
        mutate(date = lag(date), date = coalesce(date, index(fund_J200_weights)[1]) )

    J200_BPValue <- J200_RetPort$"BOP.Value" %>% xts_tbl() %>%
        mutate(date = lag(date), date = coalesce(date, index(fund_J200_weights)[1]) )

    # Clean and save portfolio returns and weights:
    J400_Contribution <- J400_RetPort$"contribution" %>% xts_tbl() %>%
        mutate(date = lag(date), date = coalesce(date, index(fund_J400_weights)[1]) )

    J400_BPWeight <- J400_RetPort$"BOP.Weight" %>% xts_tbl() %>%
        mutate(date = lag(date), date = coalesce(date, index(fund_J400_weights)[1]) )

    J400_BPValue <- J400_RetPort$"BOP.Value" %>% xts_tbl() %>%
        mutate(date = lag(date), date = coalesce(date, index(fund_J400_weights)[1]) )


    names(J200_Contribution) <- c("date", names(J200_RetPort$"contribution"))
    names(J200_BPWeight) <- c("date", names(J200_RetPort$"BOP.Weight"))
    names(J200_BPValue) <- c("date", names(J200_RetPort$"BOP.Value"))
    names(J400_Contribution) <- c("date", names(J400_RetPort$"contribution"))
    names(J400_BPWeight) <- c("date", names(J400_RetPort$"BOP.Weight"))
    names(J400_BPValue) <- c("date", names(J400_RetPort$"BOP.Value"))

    # Let's bind all of these together now:
    df_port_return_J200 <-
        left_join(fund %>% select(date, Tickers, Return),
                  J200_BPWeight %>% gather(Tickers, weight, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(.,
                  J200_BPValue %>% gather(Tickers, value_held, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(.,
                  J200_Contribution %>% gather(Tickers, Contribution, -date),
                  by = c("date", "Tickers"))

    df_port_return_J400 <-
        left_join(fund %>% select(date, Tickers, Return),
                  J400_BPWeight %>% gather(Tickers, weight, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(.,
                  J400_BPValue %>% gather(Tickers, value_held, -date),
                  by = c("date", "Tickers") ) %>%
        left_join(.,
                  J400_Contribution %>% gather(Tickers, Contribution, -date),
                  by = c("date", "Tickers"))

    # Calculate Portfolio Returns:
    df_Portf_J200<- df_port_return_J200 %>% group_by(date) %>% summarise(PortfolioReturn = sum(Return*weight, na.rm = TRUE)) %>%
        filter(PortfolioReturn != 0)
    # Calculate Portfolio Returns:
    df_Portf_J400 <-
        df_port_return_J400 %>% group_by(date) %>% summarise(PortfolioReturn = sum(Return*weight, na.rm = TRUE)) %>% filter(PortfolioReturn != 0)

    out <- left_join(df_Portf_J200 %>% rename(J200 = PortfolioReturn),
                     df_Portf_J400 %>%  rename(J400 = PortfolioReturn), by = "date") %>%
        pivot_longer(c("J200", "J400"), names_to = "Meth", values_to = "Returns")
    out
}
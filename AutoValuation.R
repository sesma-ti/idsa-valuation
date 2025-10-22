AutoValuation <- function(ts_diag, log_fn) {

  test_size <- 20
  fecha_corte <- max(ts_diag$ds) - test_size
  
  ts_train <- ts_diag |> filter(ds <= fecha_corte)
  ts_test  <- ts_diag |> filter(ds > fecha_corte)
  ts_h <- length(ts_test)
  
  buildForecastMetrics <- function(fc) {
    ts_test |>
      inner_join(fc, by = join_by(ds)) |>
      rename(test = y.x, forecast = y.y)
  }
  
  buildForecastPlot <- function(mtrcs) {
    mtrcs |>
      ggplot(aes(x = ds, y = test)) +
      geom_line() +
      geom_line(aes(y = forecast, color = "red")) +
      labs(title = "Test vs Forecast")
  }
  
  buildForecastError <- function(mtrcs) {
    err <- mtrcs |>
      mutate(rmsd =  sqrt((test - forecast)^2 / ts_h))
    
    err$rmsd |> mean() |> round(2)
  }
  
  valuation_model <- function(name, builder, src, h, log_fn) {
    fc <- builder(src, h, log_fn)
    metrics <- buildForecastMetrics(fc)
    error <- buildForecastError(metrics)
    
    obj <- list(
      name = name,
      builder = builder,
      forecast = fc,
      metrics = metrics,
      error = error
    )
    class(obj) <- "valuation_model"
    return(obj)
  }
  
  modelos <- list()
  
  ### Modelo Prophet
  vm_prophet <- valuation_model("Prophet", function(src, h, log_fn){
    log_fn("[Prophet] Inicio")
    
    mdl <- prophet(src)
    ft <- make_future_dataframe(mdl, periods = h)
    fc <- predict(mdl, ft)
    
    x <- fc |>
      mutate(ds = lubridate::ymd(ds)) |>
      filter(ds > fecha_corte) |>
      dplyr::select(ds, yhat) |>
      rename(y = yhat)
    
    log_fn("[Prophet] Fin")
    
    x
  }, ts_train, ts_h, log_fn)
  
  ### Modelo Auto ARIMA
  vm_auto_arima <- valuation_model("AutoARIMA", function(src, h, log_fn) {
    log_fn("[AutoARIMA] Inicio")
    
    x <- src |> 
      model(AutoARIMA = ARIMA(y)) |> 
      forecast(h = h) |>
      as_tibble() |>
      mutate(ds = ymd(ds)) |>
      dplyr::select(ds, .mean) |>
      rename(y = .mean)
    
    log_fn("[AutoARIMA] Fin")
    
    x
  }, ts_train, test_size, log_fn)
  
  ### Modelo Auto ETS
  vm_auto_ets <- valuation_model("AutoETS", function(src, h, log_fn) {
    log_fn("[AutoETS] Inicio")
    
    x <- src |> 
      model(AutoETS = ETS(y)) |> 
      forecast(h = h) |>
      as_tibble() |>
      mutate(ds = ymd(ds)) |>
      dplyr::select(ds, .mean) |>
      rename(y = .mean)
    
    log_fn("[AutoETS] Fin")
    
    x
  }, ts_train, test_size, log_fn)
  
  ### Modelo Auto NNETAR
  nnetar_train_ts <- ts(ts_train$y, frequency = 1)
  nnetar_test_ts  <- ts(ts_test$y, frequency = 1, start = length(nnetar_train_ts) + 1)
  vm_nnetar <- valuation_model("NNETAR", function(src, h, log_fn){
    log_fn("[AutoNNETAR] Inicio")
    
    x <- nnetar(src) |>
      forecast(h = h) |>
      as_tibble() |>
      mutate(ds = ts_test$ds, y = `Point Forecast`) |>
      dplyr::select(ds, y)
    
    log_fn("[AutoNNETAR] Fin")
    
    x
  }, nnetar_train_ts, length(nnetar_test_ts), log_fn)
  
  ### Modelo ARIMA com pruebas basadas en graficos ACF/PACF
  vm_arima <- valuation_model("ARIMA", function(src, h, log_fn){
    log_fn("[ARIMA] Inicio")
    
    src |>
      model(
        ARIMA_100 = ARIMA(y ~ pdq(1, 0, 0)),
        ARIMA_300 = ARIMA(y ~ pdq(3, 0, 0)),
        ARIMA_400 = ARIMA(y ~ pdq(4, 0, 0)), 
        ARIMA_110 = ARIMA(y ~ pdq(1, 1, 0)),
        ARIMA_310 = ARIMA(y ~ pdq(3, 1, 0)),
        ARIMA_410 = ARIMA(y ~ pdq(4, 1, 0))
      ) |>
      forecast(h = h) |>
      accuracy(ts_test) |>
      arrange(RMSE) |>
      select(.model) |>
      head(1)
    
    x <- src |> 
      model(ARIMA = ARIMA(y ~ pdq(3, 0, 0))) |> 
      forecast(h = h) |>
      as_tibble() |>
      mutate(ds = ymd(ds)) |>
      dplyr::select(ds, .mean) |>
      rename(y = .mean)
    
    log_fn("[ARIMA] Fin")
    
    x
  }, ts_train, test_size, log_fn)
  
  ### Modelo ARIMA con optimizacion de hiperparametros
  vm_grid_arima <- valuation_model("GridARIMA", function(src, h, log_fn){
    log_fn("[GridARIMA] Inicio")
    
    grid_arima <- expand.grid(
      p = 0:3, 
      d = 1:2, 
      q = 0:3,
      P = 0:1, 
      D = 0:1, 
      Q = 0:1
    )
    
    arima_results <- pmap_dfr(grid_arima, function(p, d, q, P, D, Q) {
      
      fc <- src |> 
        model(ARIMA = ARIMA(y ~ pdq(p, d, q) + PDQ(P, D, Q) + trend())) |> 
        forecast(h = h) |>
        as_tibble() |>
        mutate(ds = ymd(ds)) |>
        dplyr::select(ds, .mean) |>
        rename(y = .mean)
      
      rmse_val <- buildForecastMetrics(fc) |> buildForecastError()
      
      tibble(p, d, q, P, D, Q, RMSE = rmse_val)
    })
    
    coef_results <- arima_results |> arrange(RMSE) |> head(1)
    
    x <- src |>
      model(
        ARIMA_Opt = ARIMA(y ~ pdq(coef_results$p, coef_results$d, coef_results$q)  + PDQ(coef_results$P, coef_results$D, coef_results$Q) )
      ) |>
      forecast(h = h) |>
      as_tibble() |>
      mutate(ds = ymd(ds)) |>
      dplyr::select(ds, .mean) |>
      rename(y = .mean)
    
    log_fn("[GridARIMA] Fin")
    
    x
  }, ts_train, test_size, log_fn)
  
  ### Modelo ETS con optimizacion de hiperparametros
  vm_grid_ets <- valuation_model("GridETS", function(src, h, log_fn) {
    log_fn("[GridETS] Inicio")
    
    model_grid <- expand.grid(
      Tendencia = c("A", "N"),
      Estacionalidad = c("A", "M", "N")
    )
    
    ets_results <- purrr::pmap_dfr(model_grid, function(Tendencia, Estacionalidad) {
      
      fc <- src |> 
        model(ETS = ETS(y ~ trend(Tendencia |> as.character()) + season(Estacionalidad |> as.character()))) |> 
        forecast(h = h) |>
        as_tibble() |>
        mutate(ds = ymd(ds)) |>
        dplyr::select(ds, .mean) |>
        rename(y = .mean)
      
      rmse_val <- buildForecastMetrics(fc) |> buildForecastError()
      
      tibble(Modelo = paste(Tendencia, Estacionalidad, sep = "_"),
             RMSE = rmse_val)
    })
    
    coef_results <- ets_results |> arrange(RMSE) |> head(1)
    coef_results <- (function(){
      m <- coef_results$Modelo |> str_split("_")
      p <- m[[1]]
      
      list(
        "trend" = p[1],
        "season" = p[2]
      )
    })()
    
    x <- src |> 
      model(ETS = ETS(y ~ trend(coef_results$trend) + season(coef_results$season))) |> 
      forecast(h = h) |>
      as_tibble() |>
      mutate(ds = ymd(ds)) |>
      dplyr::select(ds, .mean) |>
      rename(y = .mean)
    
    log_fn("[GridETS] Fin")
    
    x
  }, ts_train, test_size, log_fn)
  
  ### Modelo NNETAR con optimizacion de hiperparametros
  vm_grid_nnetar <- valuation_model("GridNNETAR", function(src, h, log_fn) {
    log_fn("[GridNNETAR] Inicio")
    
    grid_nnetar <- expand.grid(
      # p = 0:3, 
      size = c(5, 10, 15)
      # repeats = 10:15
    )
    
    nnetar_results <- pmap_dfr(grid_nnetar, function(size) {
      
      fc <- src |> 
        model(NNETAR = NNETAR(box_cox(y, size))) |> 
        forecast(h = h) |>
        as_tibble() |>
        mutate(ds = ymd(ds)) |>
        dplyr::select(ds, .mean) |>
        rename(y = .mean)
      
      rmse_val <- buildForecastMetrics(fc) |> buildForecastError()
      
      tibble(size, RMSE = rmse_val)
    })
    
    coef_results <- nnetar_results |> arrange(RMSE) |> head(1)
    
    x <- src |> 
      model(NNETAR = NNETAR(box_cox(y, coef_results$size))) |> 
      forecast(h = h) |>
      as_tibble() |>
      mutate(ds = ymd(ds)) |>
      dplyr::select(ds, .mean) |>
      rename(y = .mean)
    
    log_fn("[GridNNETAR] Fin")
    
    x
  }, ts_train, test_size, log_fn)
  
  ### Modelo Mixto
  vm_mix <- valuation_model("Mix", function(src, h, log_fn) {
    log_fn("[Mix] Inicio")
    decomp <- src |>
      model(STL(y ~ trend(window = h))) |>
      components()
    
    rem <- decomp |> as_tibble() |> select(ds, remainder) |> rename(y = remainder) |> as_tsibble(index = ds)
    
    fc_trend <- (function(train){
      
      model_grid <- expand.grid(
        Tendencia = c("A", "N"),
        Estacionalidad = c("A", "M", "N")
      )
      
      ets_results <- purrr::pmap_dfr(model_grid, function(Tendencia, Estacionalidad) {
        
        fc <- train |> 
          model(ETS = ETS(y ~ trend(Tendencia |> as.character()) + season(Estacionalidad |> as.character()))) |> 
          forecast(h = test_size) |>
          as_tibble() |>
          mutate(ds = ymd(ds)) |>
          dplyr::select(ds, .mean) |>
          rename(y = .mean)
        
        rmse_val <- buildForecastMetrics(fc) |> buildForecastError()
        
        tibble(Modelo = paste(Tendencia, Estacionalidad, sep = "_"),
               RMSE = rmse_val)
      })
      
      coef_results <- ets_results |> arrange(RMSE) |> head(1)
      coef_results <- (function(){
        m <- coef_results$Modelo |> str_split("_")
        p <- m[[1]]
        
        list(
          "trend" = p[1],
          "season" = p[2]
        )
      })()
      
      train |> 
        model(ETS = ETS(y ~ trend(coef_results$trend) + season(coef_results$season))) |> 
        forecast(h = test_size) |>
        as_tibble() |>
        mutate(ds = ymd(ds)) |>
        dplyr::select(ds, .mean) |>
        rename(y = .mean)
    })(decomp |> as_tibble() |> select(ds, trend) |> rename(y = trend) |> as_tsibble())
    
    fc_season <- (function(d){
      
      seasonal_data <- d |>
        dplyr::select(ds, season_week) |>
        rename(seasonality = season_week) %>%
        filter(!is.na(seasonality))
      
      future_dates <- ts_test %>%
        as_tibble() %>%
        dplyr::select(ds)
      
      pattern <- tail(seasonal_data$seasonality, n = nrow(future_dates))
      
      if (length(pattern) < nrow(future_dates)) {
        pattern <- rep(pattern, length.out = nrow(future_dates))
      }
      
      fc_season <- future_dates %>%
        mutate(seasonality = pattern)
      
      fc_season
      
    })(decomp)
    
    fc_remainder <- (function(t){
      mdl <- prophet(t)
      ft <- make_future_dataframe(mdl, periods = nrow(ts_test))
      fc <- predict(mdl, ft)
      
      fc |>
        mutate(ds = lubridate::ymd(ds)) |>
        filter(ds > fecha_corte) |>
        dplyr::select(ds, yhat) |>
        rename(y = yhat)
    })(rem)
    
    x <- fc_trend |>
      inner_join(fc_season, by = join_by(ds)) |>
      mutate(rem = mean(fc_remainder$y)) |>
      rename(trend = y) |>
      mutate(y = trend + seasonality + rem) |>
      select(ds, y)
    
    log_fn("[Mix] Fin")
    
    x
  }, ts_train, ts_h, log_fn)
  
  ### Resumen de Modelos
  modelos[[paste0("Prophet_",vm_prophet$error)]] <- vm_prophet
  modelos[[paste0("AutoARIMA_",vm_auto_arima$error)]] <- vm_auto_arima
  modelos[[paste0("AutoETS_",vm_auto_ets$error)]] <- vm_auto_ets
  modelos[[paste0("NNETAR_",vm_nnetar$error)]] <- vm_nnetar
  modelos[[paste0("ARIMA_",vm_arima$error)]] <- vm_arima
  modelos[[paste0("GridARIMA_",vm_grid_arima$error)]] <- vm_grid_arima
  modelos[[paste0("GridETS_",vm_grid_ets$error)]] <- vm_grid_ets
  modelos[[paste0("GridNNETAR_",vm_grid_nnetar$error)]] <- vm_grid_nnetar
  modelos[[paste0("Mix_",vm_mix$error)]] <- vm_mix
  
  ### Seleccion del mejor modelo
  mejor_modelo <- modelos[order(sapply(modelos, function(x) x$error))][[1]]
  
  View(modelos)
  
  fc <- mejor_modelo$builder(ts_diag, 90, log_fn)
  
  list(
    "forecast" = fc,
    "name" = mejor_modelo$name,
    "error" = mejor_modelo$error
  )
}





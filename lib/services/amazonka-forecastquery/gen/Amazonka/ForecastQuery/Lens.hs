{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ForecastQuery.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ForecastQuery.Lens
  ( -- * Operations

    -- ** QueryForecast
    queryForecast_endDate,
    queryForecast_nextToken,
    queryForecast_startDate,
    queryForecast_forecastArn,
    queryForecast_filters,
    queryForecastResponse_forecast,
    queryForecastResponse_httpStatus,

    -- ** QueryWhatIfForecast
    queryWhatIfForecast_endDate,
    queryWhatIfForecast_nextToken,
    queryWhatIfForecast_startDate,
    queryWhatIfForecast_whatIfForecastArn,
    queryWhatIfForecast_filters,
    queryWhatIfForecastResponse_forecast,
    queryWhatIfForecastResponse_httpStatus,

    -- * Types

    -- ** DataPoint
    dataPoint_timestamp,
    dataPoint_value,

    -- ** Forecast
    forecast_predictions,
  )
where

import Amazonka.ForecastQuery.QueryForecast
import Amazonka.ForecastQuery.QueryWhatIfForecast
import Amazonka.ForecastQuery.Types.DataPoint
import Amazonka.ForecastQuery.Types.Forecast

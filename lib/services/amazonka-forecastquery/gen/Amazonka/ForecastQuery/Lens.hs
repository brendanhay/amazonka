{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ForecastQuery.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ForecastQuery.Lens
  ( -- * Operations

    -- ** QueryForecast
    queryForecast_endDate,
    queryForecast_startDate,
    queryForecast_nextToken,
    queryForecast_forecastArn,
    queryForecast_filters,
    queryForecastResponse_forecast,
    queryForecastResponse_httpStatus,

    -- * Types

    -- ** DataPoint
    dataPoint_value,
    dataPoint_timestamp,

    -- ** Forecast
    forecast_predictions,
  )
where

import Amazonka.ForecastQuery.QueryForecast
import Amazonka.ForecastQuery.Types.DataPoint
import Amazonka.ForecastQuery.Types.Forecast

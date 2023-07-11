{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ForecastQuery.Types.Forecast
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ForecastQuery.Types.Forecast where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ForecastQuery.Types.DataPoint
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a forecast. Returned as part of the
-- QueryForecast response.
--
-- /See:/ 'newForecast' smart constructor.
data Forecast = Forecast'
  { -- | The forecast.
    --
    -- The /string/ of the string-to-array map is one of the following values:
    --
    -- -   p10
    --
    -- -   p50
    --
    -- -   p90
    --
    -- The default setting is @[\"0.1\", \"0.5\", \"0.9\"]@. Use the optional
    -- @ForecastTypes@ parameter of the
    -- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateForecast.html CreateForecast>
    -- operation to change the values. The values will vary depending on how
    -- this is set, with a minimum of @1@ and a maximum of @5.@
    predictions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [DataPoint])
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Forecast' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictions', 'forecast_predictions' - The forecast.
--
-- The /string/ of the string-to-array map is one of the following values:
--
-- -   p10
--
-- -   p50
--
-- -   p90
--
-- The default setting is @[\"0.1\", \"0.5\", \"0.9\"]@. Use the optional
-- @ForecastTypes@ parameter of the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateForecast.html CreateForecast>
-- operation to change the values. The values will vary depending on how
-- this is set, with a minimum of @1@ and a maximum of @5.@
newForecast ::
  Forecast
newForecast =
  Forecast' {predictions = Prelude.Nothing}

-- | The forecast.
--
-- The /string/ of the string-to-array map is one of the following values:
--
-- -   p10
--
-- -   p50
--
-- -   p90
--
-- The default setting is @[\"0.1\", \"0.5\", \"0.9\"]@. Use the optional
-- @ForecastTypes@ parameter of the
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_CreateForecast.html CreateForecast>
-- operation to change the values. The values will vary depending on how
-- this is set, with a minimum of @1@ and a maximum of @5.@
forecast_predictions :: Lens.Lens' Forecast (Prelude.Maybe (Prelude.HashMap Prelude.Text [DataPoint]))
forecast_predictions = Lens.lens (\Forecast' {predictions} -> predictions) (\s@Forecast' {} a -> s {predictions = a} :: Forecast) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Forecast where
  parseJSON =
    Data.withObject
      "Forecast"
      ( \x ->
          Forecast'
            Prelude.<$> (x Data..:? "Predictions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Forecast where
  hashWithSalt _salt Forecast' {..} =
    _salt `Prelude.hashWithSalt` predictions

instance Prelude.NFData Forecast where
  rnf Forecast' {..} = Prelude.rnf predictions

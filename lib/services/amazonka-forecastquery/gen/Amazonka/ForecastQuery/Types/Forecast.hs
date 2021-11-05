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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ForecastQuery.Types.Forecast where

import qualified Amazonka.Core as Core
import Amazonka.ForecastQuery.Types.DataPoint
import qualified Amazonka.Lens as Lens
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
forecast_predictions :: Lens.Lens' Forecast (Prelude.Maybe (Prelude.HashMap Prelude.Text [DataPoint]))
forecast_predictions = Lens.lens (\Forecast' {predictions} -> predictions) (\s@Forecast' {} a -> s {predictions = a} :: Forecast) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Forecast where
  parseJSON =
    Core.withObject
      "Forecast"
      ( \x ->
          Forecast'
            Prelude.<$> (x Core..:? "Predictions" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Forecast

instance Prelude.NFData Forecast

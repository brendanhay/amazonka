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
-- Module      : Amazonka.Forecast.Types.TimeSeriesSelector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.TimeSeriesSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.TimeSeriesIdentifiers
import qualified Amazonka.Prelude as Prelude

-- | Defines the set of time series that are used to create the forecasts in
-- a @TimeSeriesIdentifiers@ object.
--
-- The @TimeSeriesIdentifiers@ object needs the following information:
--
-- -   @DataSource@
--
-- -   @Format@
--
-- -   @Schema@
--
-- /See:/ 'newTimeSeriesSelector' smart constructor.
data TimeSeriesSelector = TimeSeriesSelector'
  { -- | Details about the import file that contains the time series for which
    -- you want to create forecasts.
    timeSeriesIdentifiers :: Prelude.Maybe TimeSeriesIdentifiers
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeSeriesSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeSeriesIdentifiers', 'timeSeriesSelector_timeSeriesIdentifiers' - Details about the import file that contains the time series for which
-- you want to create forecasts.
newTimeSeriesSelector ::
  TimeSeriesSelector
newTimeSeriesSelector =
  TimeSeriesSelector'
    { timeSeriesIdentifiers =
        Prelude.Nothing
    }

-- | Details about the import file that contains the time series for which
-- you want to create forecasts.
timeSeriesSelector_timeSeriesIdentifiers :: Lens.Lens' TimeSeriesSelector (Prelude.Maybe TimeSeriesIdentifiers)
timeSeriesSelector_timeSeriesIdentifiers = Lens.lens (\TimeSeriesSelector' {timeSeriesIdentifiers} -> timeSeriesIdentifiers) (\s@TimeSeriesSelector' {} a -> s {timeSeriesIdentifiers = a} :: TimeSeriesSelector)

instance Data.FromJSON TimeSeriesSelector where
  parseJSON =
    Data.withObject
      "TimeSeriesSelector"
      ( \x ->
          TimeSeriesSelector'
            Prelude.<$> (x Data..:? "TimeSeriesIdentifiers")
      )

instance Prelude.Hashable TimeSeriesSelector where
  hashWithSalt _salt TimeSeriesSelector' {..} =
    _salt `Prelude.hashWithSalt` timeSeriesIdentifiers

instance Prelude.NFData TimeSeriesSelector where
  rnf TimeSeriesSelector' {..} =
    Prelude.rnf timeSeriesIdentifiers

instance Data.ToJSON TimeSeriesSelector where
  toJSON TimeSeriesSelector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TimeSeriesIdentifiers" Data..=)
              Prelude.<$> timeSeriesIdentifiers
          ]
      )

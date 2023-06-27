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
-- Module      : Amazonka.XRay.Types.ForecastStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ForecastStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The predicted high and low fault count. This is used to determine if a
-- service has become anomalous and if an insight should be created.
--
-- /See:/ 'newForecastStatistics' smart constructor.
data ForecastStatistics = ForecastStatistics'
  { -- | The upper limit of fault counts for a service.
    faultCountHigh :: Prelude.Maybe Prelude.Integer,
    -- | The lower limit of fault counts for a service.
    faultCountLow :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForecastStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faultCountHigh', 'forecastStatistics_faultCountHigh' - The upper limit of fault counts for a service.
--
-- 'faultCountLow', 'forecastStatistics_faultCountLow' - The lower limit of fault counts for a service.
newForecastStatistics ::
  ForecastStatistics
newForecastStatistics =
  ForecastStatistics'
    { faultCountHigh =
        Prelude.Nothing,
      faultCountLow = Prelude.Nothing
    }

-- | The upper limit of fault counts for a service.
forecastStatistics_faultCountHigh :: Lens.Lens' ForecastStatistics (Prelude.Maybe Prelude.Integer)
forecastStatistics_faultCountHigh = Lens.lens (\ForecastStatistics' {faultCountHigh} -> faultCountHigh) (\s@ForecastStatistics' {} a -> s {faultCountHigh = a} :: ForecastStatistics)

-- | The lower limit of fault counts for a service.
forecastStatistics_faultCountLow :: Lens.Lens' ForecastStatistics (Prelude.Maybe Prelude.Integer)
forecastStatistics_faultCountLow = Lens.lens (\ForecastStatistics' {faultCountLow} -> faultCountLow) (\s@ForecastStatistics' {} a -> s {faultCountLow = a} :: ForecastStatistics)

instance Data.FromJSON ForecastStatistics where
  parseJSON =
    Data.withObject
      "ForecastStatistics"
      ( \x ->
          ForecastStatistics'
            Prelude.<$> (x Data..:? "FaultCountHigh")
            Prelude.<*> (x Data..:? "FaultCountLow")
      )

instance Prelude.Hashable ForecastStatistics where
  hashWithSalt _salt ForecastStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` faultCountHigh
      `Prelude.hashWithSalt` faultCountLow

instance Prelude.NFData ForecastStatistics where
  rnf ForecastStatistics' {..} =
    Prelude.rnf faultCountHigh
      `Prelude.seq` Prelude.rnf faultCountLow

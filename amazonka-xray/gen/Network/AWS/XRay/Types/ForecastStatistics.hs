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
-- Module      : Network.AWS.XRay.Types.ForecastStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ForecastStatistics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The predicted high and low fault count. This is used to determine if a
-- service has become anomalous and if an insight should be created.
--
-- /See:/ 'newForecastStatistics' smart constructor.
data ForecastStatistics = ForecastStatistics'
  { -- | The lower limit of fault counts for a service.
    faultCountLow :: Core.Maybe Core.Integer,
    -- | The upper limit of fault counts for a service.
    faultCountHigh :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ForecastStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faultCountLow', 'forecastStatistics_faultCountLow' - The lower limit of fault counts for a service.
--
-- 'faultCountHigh', 'forecastStatistics_faultCountHigh' - The upper limit of fault counts for a service.
newForecastStatistics ::
  ForecastStatistics
newForecastStatistics =
  ForecastStatistics'
    { faultCountLow = Core.Nothing,
      faultCountHigh = Core.Nothing
    }

-- | The lower limit of fault counts for a service.
forecastStatistics_faultCountLow :: Lens.Lens' ForecastStatistics (Core.Maybe Core.Integer)
forecastStatistics_faultCountLow = Lens.lens (\ForecastStatistics' {faultCountLow} -> faultCountLow) (\s@ForecastStatistics' {} a -> s {faultCountLow = a} :: ForecastStatistics)

-- | The upper limit of fault counts for a service.
forecastStatistics_faultCountHigh :: Lens.Lens' ForecastStatistics (Core.Maybe Core.Integer)
forecastStatistics_faultCountHigh = Lens.lens (\ForecastStatistics' {faultCountHigh} -> faultCountHigh) (\s@ForecastStatistics' {} a -> s {faultCountHigh = a} :: ForecastStatistics)

instance Core.FromJSON ForecastStatistics where
  parseJSON =
    Core.withObject
      "ForecastStatistics"
      ( \x ->
          ForecastStatistics'
            Core.<$> (x Core..:? "FaultCountLow")
            Core.<*> (x Core..:? "FaultCountHigh")
      )

instance Core.Hashable ForecastStatistics

instance Core.NFData ForecastStatistics

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
-- Module      : Network.AWS.CostExplorer.Types.CoverageHours
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CoverageHours where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | How long a running instance either used a reservation or was On-Demand.
--
-- /See:/ 'newCoverageHours' smart constructor.
data CoverageHours = CoverageHours'
  { -- | The number of instance running hours that reservations covered.
    reservedHours :: Core.Maybe Core.Text,
    -- | The total instance usage, in hours.
    totalRunningHours :: Core.Maybe Core.Text,
    -- | The number of instance running hours that On-Demand Instances covered.
    onDemandHours :: Core.Maybe Core.Text,
    -- | The percentage of instance hours that a reservation covered.
    coverageHoursPercentage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CoverageHours' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedHours', 'coverageHours_reservedHours' - The number of instance running hours that reservations covered.
--
-- 'totalRunningHours', 'coverageHours_totalRunningHours' - The total instance usage, in hours.
--
-- 'onDemandHours', 'coverageHours_onDemandHours' - The number of instance running hours that On-Demand Instances covered.
--
-- 'coverageHoursPercentage', 'coverageHours_coverageHoursPercentage' - The percentage of instance hours that a reservation covered.
newCoverageHours ::
  CoverageHours
newCoverageHours =
  CoverageHours'
    { reservedHours = Core.Nothing,
      totalRunningHours = Core.Nothing,
      onDemandHours = Core.Nothing,
      coverageHoursPercentage = Core.Nothing
    }

-- | The number of instance running hours that reservations covered.
coverageHours_reservedHours :: Lens.Lens' CoverageHours (Core.Maybe Core.Text)
coverageHours_reservedHours = Lens.lens (\CoverageHours' {reservedHours} -> reservedHours) (\s@CoverageHours' {} a -> s {reservedHours = a} :: CoverageHours)

-- | The total instance usage, in hours.
coverageHours_totalRunningHours :: Lens.Lens' CoverageHours (Core.Maybe Core.Text)
coverageHours_totalRunningHours = Lens.lens (\CoverageHours' {totalRunningHours} -> totalRunningHours) (\s@CoverageHours' {} a -> s {totalRunningHours = a} :: CoverageHours)

-- | The number of instance running hours that On-Demand Instances covered.
coverageHours_onDemandHours :: Lens.Lens' CoverageHours (Core.Maybe Core.Text)
coverageHours_onDemandHours = Lens.lens (\CoverageHours' {onDemandHours} -> onDemandHours) (\s@CoverageHours' {} a -> s {onDemandHours = a} :: CoverageHours)

-- | The percentage of instance hours that a reservation covered.
coverageHours_coverageHoursPercentage :: Lens.Lens' CoverageHours (Core.Maybe Core.Text)
coverageHours_coverageHoursPercentage = Lens.lens (\CoverageHours' {coverageHoursPercentage} -> coverageHoursPercentage) (\s@CoverageHours' {} a -> s {coverageHoursPercentage = a} :: CoverageHours)

instance Core.FromJSON CoverageHours where
  parseJSON =
    Core.withObject
      "CoverageHours"
      ( \x ->
          CoverageHours'
            Core.<$> (x Core..:? "ReservedHours")
            Core.<*> (x Core..:? "TotalRunningHours")
            Core.<*> (x Core..:? "OnDemandHours")
            Core.<*> (x Core..:? "CoverageHoursPercentage")
      )

instance Core.Hashable CoverageHours

instance Core.NFData CoverageHours

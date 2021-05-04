{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CostExplorer.Types.CoverageNormalizedUnits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CoverageNormalizedUnits where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The amount of instance usage, in normalized units. Normalized units
-- enable you to see your EC2 usage for multiple sizes of instances in a
-- uniform way. For example, suppose you run an xlarge instance and a
-- 2xlarge instance. If you run both instances for the same amount of time,
-- the 2xlarge instance uses twice as much of your reservation as the
-- xlarge instance, even though both instances show only one instance-hour.
-- Using normalized units instead of instance-hours, the xlarge instance
-- used 8 normalized units, and the 2xlarge instance used 16 normalized
-- units.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances>
-- in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/.
--
-- /See:/ 'newCoverageNormalizedUnits' smart constructor.
data CoverageNormalizedUnits = CoverageNormalizedUnits'
  { -- | The number of normalized units that are covered by On-Demand Instances
    -- instead of a reservation.
    onDemandNormalizedUnits :: Prelude.Maybe Prelude.Text,
    -- | The percentage of your used instance normalized units that a reservation
    -- covers.
    coverageNormalizedUnitsPercentage :: Prelude.Maybe Prelude.Text,
    -- | The total number of normalized units that you used.
    totalRunningNormalizedUnits :: Prelude.Maybe Prelude.Text,
    -- | The number of normalized units that a reservation covers.
    reservedNormalizedUnits :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CoverageNormalizedUnits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onDemandNormalizedUnits', 'coverageNormalizedUnits_onDemandNormalizedUnits' - The number of normalized units that are covered by On-Demand Instances
-- instead of a reservation.
--
-- 'coverageNormalizedUnitsPercentage', 'coverageNormalizedUnits_coverageNormalizedUnitsPercentage' - The percentage of your used instance normalized units that a reservation
-- covers.
--
-- 'totalRunningNormalizedUnits', 'coverageNormalizedUnits_totalRunningNormalizedUnits' - The total number of normalized units that you used.
--
-- 'reservedNormalizedUnits', 'coverageNormalizedUnits_reservedNormalizedUnits' - The number of normalized units that a reservation covers.
newCoverageNormalizedUnits ::
  CoverageNormalizedUnits
newCoverageNormalizedUnits =
  CoverageNormalizedUnits'
    { onDemandNormalizedUnits =
        Prelude.Nothing,
      coverageNormalizedUnitsPercentage =
        Prelude.Nothing,
      totalRunningNormalizedUnits = Prelude.Nothing,
      reservedNormalizedUnits = Prelude.Nothing
    }

-- | The number of normalized units that are covered by On-Demand Instances
-- instead of a reservation.
coverageNormalizedUnits_onDemandNormalizedUnits :: Lens.Lens' CoverageNormalizedUnits (Prelude.Maybe Prelude.Text)
coverageNormalizedUnits_onDemandNormalizedUnits = Lens.lens (\CoverageNormalizedUnits' {onDemandNormalizedUnits} -> onDemandNormalizedUnits) (\s@CoverageNormalizedUnits' {} a -> s {onDemandNormalizedUnits = a} :: CoverageNormalizedUnits)

-- | The percentage of your used instance normalized units that a reservation
-- covers.
coverageNormalizedUnits_coverageNormalizedUnitsPercentage :: Lens.Lens' CoverageNormalizedUnits (Prelude.Maybe Prelude.Text)
coverageNormalizedUnits_coverageNormalizedUnitsPercentage = Lens.lens (\CoverageNormalizedUnits' {coverageNormalizedUnitsPercentage} -> coverageNormalizedUnitsPercentage) (\s@CoverageNormalizedUnits' {} a -> s {coverageNormalizedUnitsPercentage = a} :: CoverageNormalizedUnits)

-- | The total number of normalized units that you used.
coverageNormalizedUnits_totalRunningNormalizedUnits :: Lens.Lens' CoverageNormalizedUnits (Prelude.Maybe Prelude.Text)
coverageNormalizedUnits_totalRunningNormalizedUnits = Lens.lens (\CoverageNormalizedUnits' {totalRunningNormalizedUnits} -> totalRunningNormalizedUnits) (\s@CoverageNormalizedUnits' {} a -> s {totalRunningNormalizedUnits = a} :: CoverageNormalizedUnits)

-- | The number of normalized units that a reservation covers.
coverageNormalizedUnits_reservedNormalizedUnits :: Lens.Lens' CoverageNormalizedUnits (Prelude.Maybe Prelude.Text)
coverageNormalizedUnits_reservedNormalizedUnits = Lens.lens (\CoverageNormalizedUnits' {reservedNormalizedUnits} -> reservedNormalizedUnits) (\s@CoverageNormalizedUnits' {} a -> s {reservedNormalizedUnits = a} :: CoverageNormalizedUnits)

instance Prelude.FromJSON CoverageNormalizedUnits where
  parseJSON =
    Prelude.withObject
      "CoverageNormalizedUnits"
      ( \x ->
          CoverageNormalizedUnits'
            Prelude.<$> (x Prelude..:? "OnDemandNormalizedUnits")
            Prelude.<*> (x Prelude..:? "CoverageNormalizedUnitsPercentage")
            Prelude.<*> (x Prelude..:? "TotalRunningNormalizedUnits")
            Prelude.<*> (x Prelude..:? "ReservedNormalizedUnits")
      )

instance Prelude.Hashable CoverageNormalizedUnits

instance Prelude.NFData CoverageNormalizedUnits

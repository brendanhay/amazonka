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
-- Module      : Amazonka.CostExplorer.Types.CoverageNormalizedUnits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CoverageNormalizedUnits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The amount of instance usage, in normalized units. You can use
-- normalized units to see your EC2 usage for multiple sizes of instances
-- in a uniform way. For example, suppose that you run an xlarge instance
-- and a 2xlarge instance. If you run both instances for the same amount of
-- time, the 2xlarge instance uses twice as much of your reservation as the
-- xlarge instance, even though both instances show only one instance-hour.
-- When you use normalized units instead of instance-hours, the xlarge
-- instance used 8 normalized units, and the 2xlarge instance used 16
-- normalized units.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances>
-- in the /Amazon Elastic Compute Cloud User Guide for Linux Instances/.
--
-- /See:/ 'newCoverageNormalizedUnits' smart constructor.
data CoverageNormalizedUnits = CoverageNormalizedUnits'
  { -- | The total number of normalized units that you used.
    totalRunningNormalizedUnits :: Prelude.Maybe Prelude.Text,
    -- | The number of normalized units that a reservation covers.
    reservedNormalizedUnits :: Prelude.Maybe Prelude.Text,
    -- | The percentage of your used instance normalized units that a reservation
    -- covers.
    coverageNormalizedUnitsPercentage :: Prelude.Maybe Prelude.Text,
    -- | The number of normalized units that are covered by On-Demand Instances
    -- instead of a reservation.
    onDemandNormalizedUnits :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageNormalizedUnits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalRunningNormalizedUnits', 'coverageNormalizedUnits_totalRunningNormalizedUnits' - The total number of normalized units that you used.
--
-- 'reservedNormalizedUnits', 'coverageNormalizedUnits_reservedNormalizedUnits' - The number of normalized units that a reservation covers.
--
-- 'coverageNormalizedUnitsPercentage', 'coverageNormalizedUnits_coverageNormalizedUnitsPercentage' - The percentage of your used instance normalized units that a reservation
-- covers.
--
-- 'onDemandNormalizedUnits', 'coverageNormalizedUnits_onDemandNormalizedUnits' - The number of normalized units that are covered by On-Demand Instances
-- instead of a reservation.
newCoverageNormalizedUnits ::
  CoverageNormalizedUnits
newCoverageNormalizedUnits =
  CoverageNormalizedUnits'
    { totalRunningNormalizedUnits =
        Prelude.Nothing,
      reservedNormalizedUnits = Prelude.Nothing,
      coverageNormalizedUnitsPercentage =
        Prelude.Nothing,
      onDemandNormalizedUnits = Prelude.Nothing
    }

-- | The total number of normalized units that you used.
coverageNormalizedUnits_totalRunningNormalizedUnits :: Lens.Lens' CoverageNormalizedUnits (Prelude.Maybe Prelude.Text)
coverageNormalizedUnits_totalRunningNormalizedUnits = Lens.lens (\CoverageNormalizedUnits' {totalRunningNormalizedUnits} -> totalRunningNormalizedUnits) (\s@CoverageNormalizedUnits' {} a -> s {totalRunningNormalizedUnits = a} :: CoverageNormalizedUnits)

-- | The number of normalized units that a reservation covers.
coverageNormalizedUnits_reservedNormalizedUnits :: Lens.Lens' CoverageNormalizedUnits (Prelude.Maybe Prelude.Text)
coverageNormalizedUnits_reservedNormalizedUnits = Lens.lens (\CoverageNormalizedUnits' {reservedNormalizedUnits} -> reservedNormalizedUnits) (\s@CoverageNormalizedUnits' {} a -> s {reservedNormalizedUnits = a} :: CoverageNormalizedUnits)

-- | The percentage of your used instance normalized units that a reservation
-- covers.
coverageNormalizedUnits_coverageNormalizedUnitsPercentage :: Lens.Lens' CoverageNormalizedUnits (Prelude.Maybe Prelude.Text)
coverageNormalizedUnits_coverageNormalizedUnitsPercentage = Lens.lens (\CoverageNormalizedUnits' {coverageNormalizedUnitsPercentage} -> coverageNormalizedUnitsPercentage) (\s@CoverageNormalizedUnits' {} a -> s {coverageNormalizedUnitsPercentage = a} :: CoverageNormalizedUnits)

-- | The number of normalized units that are covered by On-Demand Instances
-- instead of a reservation.
coverageNormalizedUnits_onDemandNormalizedUnits :: Lens.Lens' CoverageNormalizedUnits (Prelude.Maybe Prelude.Text)
coverageNormalizedUnits_onDemandNormalizedUnits = Lens.lens (\CoverageNormalizedUnits' {onDemandNormalizedUnits} -> onDemandNormalizedUnits) (\s@CoverageNormalizedUnits' {} a -> s {onDemandNormalizedUnits = a} :: CoverageNormalizedUnits)

instance Data.FromJSON CoverageNormalizedUnits where
  parseJSON =
    Data.withObject
      "CoverageNormalizedUnits"
      ( \x ->
          CoverageNormalizedUnits'
            Prelude.<$> (x Data..:? "TotalRunningNormalizedUnits")
            Prelude.<*> (x Data..:? "ReservedNormalizedUnits")
            Prelude.<*> (x Data..:? "CoverageNormalizedUnitsPercentage")
            Prelude.<*> (x Data..:? "OnDemandNormalizedUnits")
      )

instance Prelude.Hashable CoverageNormalizedUnits where
  hashWithSalt _salt CoverageNormalizedUnits' {..} =
    _salt
      `Prelude.hashWithSalt` totalRunningNormalizedUnits
      `Prelude.hashWithSalt` reservedNormalizedUnits
      `Prelude.hashWithSalt` coverageNormalizedUnitsPercentage
      `Prelude.hashWithSalt` onDemandNormalizedUnits

instance Prelude.NFData CoverageNormalizedUnits where
  rnf CoverageNormalizedUnits' {..} =
    Prelude.rnf totalRunningNormalizedUnits
      `Prelude.seq` Prelude.rnf reservedNormalizedUnits
      `Prelude.seq` Prelude.rnf coverageNormalizedUnitsPercentage
      `Prelude.seq` Prelude.rnf onDemandNormalizedUnits

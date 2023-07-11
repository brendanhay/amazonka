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
-- Module      : Amazonka.CostExplorer.Types.CoverageHours
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CoverageHours where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | How long a running instance either used a reservation or was On-Demand.
--
-- /See:/ 'newCoverageHours' smart constructor.
data CoverageHours = CoverageHours'
  { -- | The percentage of instance hours that a reservation covered.
    coverageHoursPercentage :: Prelude.Maybe Prelude.Text,
    -- | The number of instance running hours that On-Demand Instances covered.
    onDemandHours :: Prelude.Maybe Prelude.Text,
    -- | The number of instance running hours that reservations covered.
    reservedHours :: Prelude.Maybe Prelude.Text,
    -- | The total instance usage, in hours.
    totalRunningHours :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageHours' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coverageHoursPercentage', 'coverageHours_coverageHoursPercentage' - The percentage of instance hours that a reservation covered.
--
-- 'onDemandHours', 'coverageHours_onDemandHours' - The number of instance running hours that On-Demand Instances covered.
--
-- 'reservedHours', 'coverageHours_reservedHours' - The number of instance running hours that reservations covered.
--
-- 'totalRunningHours', 'coverageHours_totalRunningHours' - The total instance usage, in hours.
newCoverageHours ::
  CoverageHours
newCoverageHours =
  CoverageHours'
    { coverageHoursPercentage =
        Prelude.Nothing,
      onDemandHours = Prelude.Nothing,
      reservedHours = Prelude.Nothing,
      totalRunningHours = Prelude.Nothing
    }

-- | The percentage of instance hours that a reservation covered.
coverageHours_coverageHoursPercentage :: Lens.Lens' CoverageHours (Prelude.Maybe Prelude.Text)
coverageHours_coverageHoursPercentage = Lens.lens (\CoverageHours' {coverageHoursPercentage} -> coverageHoursPercentage) (\s@CoverageHours' {} a -> s {coverageHoursPercentage = a} :: CoverageHours)

-- | The number of instance running hours that On-Demand Instances covered.
coverageHours_onDemandHours :: Lens.Lens' CoverageHours (Prelude.Maybe Prelude.Text)
coverageHours_onDemandHours = Lens.lens (\CoverageHours' {onDemandHours} -> onDemandHours) (\s@CoverageHours' {} a -> s {onDemandHours = a} :: CoverageHours)

-- | The number of instance running hours that reservations covered.
coverageHours_reservedHours :: Lens.Lens' CoverageHours (Prelude.Maybe Prelude.Text)
coverageHours_reservedHours = Lens.lens (\CoverageHours' {reservedHours} -> reservedHours) (\s@CoverageHours' {} a -> s {reservedHours = a} :: CoverageHours)

-- | The total instance usage, in hours.
coverageHours_totalRunningHours :: Lens.Lens' CoverageHours (Prelude.Maybe Prelude.Text)
coverageHours_totalRunningHours = Lens.lens (\CoverageHours' {totalRunningHours} -> totalRunningHours) (\s@CoverageHours' {} a -> s {totalRunningHours = a} :: CoverageHours)

instance Data.FromJSON CoverageHours where
  parseJSON =
    Data.withObject
      "CoverageHours"
      ( \x ->
          CoverageHours'
            Prelude.<$> (x Data..:? "CoverageHoursPercentage")
            Prelude.<*> (x Data..:? "OnDemandHours")
            Prelude.<*> (x Data..:? "ReservedHours")
            Prelude.<*> (x Data..:? "TotalRunningHours")
      )

instance Prelude.Hashable CoverageHours where
  hashWithSalt _salt CoverageHours' {..} =
    _salt
      `Prelude.hashWithSalt` coverageHoursPercentage
      `Prelude.hashWithSalt` onDemandHours
      `Prelude.hashWithSalt` reservedHours
      `Prelude.hashWithSalt` totalRunningHours

instance Prelude.NFData CoverageHours where
  rnf CoverageHours' {..} =
    Prelude.rnf coverageHoursPercentage
      `Prelude.seq` Prelude.rnf onDemandHours
      `Prelude.seq` Prelude.rnf reservedHours
      `Prelude.seq` Prelude.rnf totalRunningHours

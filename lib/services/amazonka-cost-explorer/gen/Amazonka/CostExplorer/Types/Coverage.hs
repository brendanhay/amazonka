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
-- Module      : Amazonka.CostExplorer.Types.Coverage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.Coverage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.CoverageCost
import Amazonka.CostExplorer.Types.CoverageHours
import Amazonka.CostExplorer.Types.CoverageNormalizedUnits
import qualified Amazonka.Prelude as Prelude

-- | The amount of instance usage that a reservation covered.
--
-- /See:/ 'newCoverage' smart constructor.
data Coverage = Coverage'
  { -- | The amount of instance usage that the reservation covered, in normalized
    -- units.
    coverageNormalizedUnits :: Prelude.Maybe CoverageNormalizedUnits,
    -- | The amount of instance usage that the reservation covered, in hours.
    coverageHours :: Prelude.Maybe CoverageHours,
    -- | The amount of cost that the reservation covered.
    coverageCost :: Prelude.Maybe CoverageCost
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Coverage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coverageNormalizedUnits', 'coverage_coverageNormalizedUnits' - The amount of instance usage that the reservation covered, in normalized
-- units.
--
-- 'coverageHours', 'coverage_coverageHours' - The amount of instance usage that the reservation covered, in hours.
--
-- 'coverageCost', 'coverage_coverageCost' - The amount of cost that the reservation covered.
newCoverage ::
  Coverage
newCoverage =
  Coverage'
    { coverageNormalizedUnits =
        Prelude.Nothing,
      coverageHours = Prelude.Nothing,
      coverageCost = Prelude.Nothing
    }

-- | The amount of instance usage that the reservation covered, in normalized
-- units.
coverage_coverageNormalizedUnits :: Lens.Lens' Coverage (Prelude.Maybe CoverageNormalizedUnits)
coverage_coverageNormalizedUnits = Lens.lens (\Coverage' {coverageNormalizedUnits} -> coverageNormalizedUnits) (\s@Coverage' {} a -> s {coverageNormalizedUnits = a} :: Coverage)

-- | The amount of instance usage that the reservation covered, in hours.
coverage_coverageHours :: Lens.Lens' Coverage (Prelude.Maybe CoverageHours)
coverage_coverageHours = Lens.lens (\Coverage' {coverageHours} -> coverageHours) (\s@Coverage' {} a -> s {coverageHours = a} :: Coverage)

-- | The amount of cost that the reservation covered.
coverage_coverageCost :: Lens.Lens' Coverage (Prelude.Maybe CoverageCost)
coverage_coverageCost = Lens.lens (\Coverage' {coverageCost} -> coverageCost) (\s@Coverage' {} a -> s {coverageCost = a} :: Coverage)

instance Core.FromJSON Coverage where
  parseJSON =
    Core.withObject
      "Coverage"
      ( \x ->
          Coverage'
            Prelude.<$> (x Core..:? "CoverageNormalizedUnits")
            Prelude.<*> (x Core..:? "CoverageHours")
            Prelude.<*> (x Core..:? "CoverageCost")
      )

instance Prelude.Hashable Coverage where
  hashWithSalt _salt Coverage' {..} =
    _salt
      `Prelude.hashWithSalt` coverageNormalizedUnits
      `Prelude.hashWithSalt` coverageHours
      `Prelude.hashWithSalt` coverageCost

instance Prelude.NFData Coverage where
  rnf Coverage' {..} =
    Prelude.rnf coverageNormalizedUnits
      `Prelude.seq` Prelude.rnf coverageHours
      `Prelude.seq` Prelude.rnf coverageCost

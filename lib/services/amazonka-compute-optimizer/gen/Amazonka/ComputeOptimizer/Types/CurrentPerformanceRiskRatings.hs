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
-- Module      : Amazonka.ComputeOptimizer.Types.CurrentPerformanceRiskRatings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.CurrentPerformanceRiskRatings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the performance risk ratings for a given resource type.
--
-- Resources with a @high@ or @medium@ rating are at risk of not meeting
-- the performance needs of their workloads, while resources with a @low@
-- rating are performing well in their workloads.
--
-- /See:/ 'newCurrentPerformanceRiskRatings' smart constructor.
data CurrentPerformanceRiskRatings = CurrentPerformanceRiskRatings'
  { -- | A count of the applicable resource types with a very low performance
    -- risk rating.
    veryLow :: Prelude.Maybe Prelude.Integer,
    -- | A count of the applicable resource types with a low performance risk
    -- rating.
    low :: Prelude.Maybe Prelude.Integer,
    -- | A count of the applicable resource types with a high performance risk
    -- rating.
    high :: Prelude.Maybe Prelude.Integer,
    -- | A count of the applicable resource types with a medium performance risk
    -- rating.
    medium :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CurrentPerformanceRiskRatings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'veryLow', 'currentPerformanceRiskRatings_veryLow' - A count of the applicable resource types with a very low performance
-- risk rating.
--
-- 'low', 'currentPerformanceRiskRatings_low' - A count of the applicable resource types with a low performance risk
-- rating.
--
-- 'high', 'currentPerformanceRiskRatings_high' - A count of the applicable resource types with a high performance risk
-- rating.
--
-- 'medium', 'currentPerformanceRiskRatings_medium' - A count of the applicable resource types with a medium performance risk
-- rating.
newCurrentPerformanceRiskRatings ::
  CurrentPerformanceRiskRatings
newCurrentPerformanceRiskRatings =
  CurrentPerformanceRiskRatings'
    { veryLow =
        Prelude.Nothing,
      low = Prelude.Nothing,
      high = Prelude.Nothing,
      medium = Prelude.Nothing
    }

-- | A count of the applicable resource types with a very low performance
-- risk rating.
currentPerformanceRiskRatings_veryLow :: Lens.Lens' CurrentPerformanceRiskRatings (Prelude.Maybe Prelude.Integer)
currentPerformanceRiskRatings_veryLow = Lens.lens (\CurrentPerformanceRiskRatings' {veryLow} -> veryLow) (\s@CurrentPerformanceRiskRatings' {} a -> s {veryLow = a} :: CurrentPerformanceRiskRatings)

-- | A count of the applicable resource types with a low performance risk
-- rating.
currentPerformanceRiskRatings_low :: Lens.Lens' CurrentPerformanceRiskRatings (Prelude.Maybe Prelude.Integer)
currentPerformanceRiskRatings_low = Lens.lens (\CurrentPerformanceRiskRatings' {low} -> low) (\s@CurrentPerformanceRiskRatings' {} a -> s {low = a} :: CurrentPerformanceRiskRatings)

-- | A count of the applicable resource types with a high performance risk
-- rating.
currentPerformanceRiskRatings_high :: Lens.Lens' CurrentPerformanceRiskRatings (Prelude.Maybe Prelude.Integer)
currentPerformanceRiskRatings_high = Lens.lens (\CurrentPerformanceRiskRatings' {high} -> high) (\s@CurrentPerformanceRiskRatings' {} a -> s {high = a} :: CurrentPerformanceRiskRatings)

-- | A count of the applicable resource types with a medium performance risk
-- rating.
currentPerformanceRiskRatings_medium :: Lens.Lens' CurrentPerformanceRiskRatings (Prelude.Maybe Prelude.Integer)
currentPerformanceRiskRatings_medium = Lens.lens (\CurrentPerformanceRiskRatings' {medium} -> medium) (\s@CurrentPerformanceRiskRatings' {} a -> s {medium = a} :: CurrentPerformanceRiskRatings)

instance Data.FromJSON CurrentPerformanceRiskRatings where
  parseJSON =
    Data.withObject
      "CurrentPerformanceRiskRatings"
      ( \x ->
          CurrentPerformanceRiskRatings'
            Prelude.<$> (x Data..:? "veryLow")
            Prelude.<*> (x Data..:? "low")
            Prelude.<*> (x Data..:? "high")
            Prelude.<*> (x Data..:? "medium")
      )

instance
  Prelude.Hashable
    CurrentPerformanceRiskRatings
  where
  hashWithSalt _salt CurrentPerformanceRiskRatings' {..} =
    _salt `Prelude.hashWithSalt` veryLow
      `Prelude.hashWithSalt` low
      `Prelude.hashWithSalt` high
      `Prelude.hashWithSalt` medium

instance Prelude.NFData CurrentPerformanceRiskRatings where
  rnf CurrentPerformanceRiskRatings' {..} =
    Prelude.rnf veryLow
      `Prelude.seq` Prelude.rnf low
      `Prelude.seq` Prelude.rnf high
      `Prelude.seq` Prelude.rnf medium

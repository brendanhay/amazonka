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
-- Module      : Network.AWS.CostExplorer.Types.Impact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Impact where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The anomaly\'s dollar value.
--
-- /See:/ 'newImpact' smart constructor.
data Impact = Impact'
  { -- | The cumulative dollar value observed for an anomaly.
    totalImpact :: Core.Maybe Core.Double,
    -- | The maximum dollar value observed for an anomaly.
    maxImpact :: Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Impact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalImpact', 'impact_totalImpact' - The cumulative dollar value observed for an anomaly.
--
-- 'maxImpact', 'impact_maxImpact' - The maximum dollar value observed for an anomaly.
newImpact ::
  -- | 'maxImpact'
  Core.Double ->
  Impact
newImpact pMaxImpact_ =
  Impact'
    { totalImpact = Core.Nothing,
      maxImpact = pMaxImpact_
    }

-- | The cumulative dollar value observed for an anomaly.
impact_totalImpact :: Lens.Lens' Impact (Core.Maybe Core.Double)
impact_totalImpact = Lens.lens (\Impact' {totalImpact} -> totalImpact) (\s@Impact' {} a -> s {totalImpact = a} :: Impact)

-- | The maximum dollar value observed for an anomaly.
impact_maxImpact :: Lens.Lens' Impact Core.Double
impact_maxImpact = Lens.lens (\Impact' {maxImpact} -> maxImpact) (\s@Impact' {} a -> s {maxImpact = a} :: Impact)

instance Core.FromJSON Impact where
  parseJSON =
    Core.withObject
      "Impact"
      ( \x ->
          Impact'
            Core.<$> (x Core..:? "TotalImpact")
            Core.<*> (x Core..: "MaxImpact")
      )

instance Core.Hashable Impact

instance Core.NFData Impact

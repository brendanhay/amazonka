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
-- Module      : Network.AWS.CostExplorer.Types.CoverageCost
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CoverageCost where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | How much it costs to run an instance.
--
-- /See:/ 'newCoverageCost' smart constructor.
data CoverageCost = CoverageCost'
  { -- | How much an On-Demand Instance costs.
    onDemandCost :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CoverageCost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onDemandCost', 'coverageCost_onDemandCost' - How much an On-Demand Instance costs.
newCoverageCost ::
  CoverageCost
newCoverageCost =
  CoverageCost' {onDemandCost = Core.Nothing}

-- | How much an On-Demand Instance costs.
coverageCost_onDemandCost :: Lens.Lens' CoverageCost (Core.Maybe Core.Text)
coverageCost_onDemandCost = Lens.lens (\CoverageCost' {onDemandCost} -> onDemandCost) (\s@CoverageCost' {} a -> s {onDemandCost = a} :: CoverageCost)

instance Core.FromJSON CoverageCost where
  parseJSON =
    Core.withObject
      "CoverageCost"
      ( \x ->
          CoverageCost' Core.<$> (x Core..:? "OnDemandCost")
      )

instance Core.Hashable CoverageCost

instance Core.NFData CoverageCost

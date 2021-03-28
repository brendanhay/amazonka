{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CoverageCost
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.CoverageCost
  ( CoverageCost (..)
  -- * Smart constructor
  , mkCoverageCost
  -- * Lenses
  , ccOnDemandCost
  ) where

import qualified Network.AWS.CostExplorer.Types.OnDemandCost as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | How much it costs to run an instance.
--
-- /See:/ 'mkCoverageCost' smart constructor.
newtype CoverageCost = CoverageCost'
  { onDemandCost :: Core.Maybe Types.OnDemandCost
    -- ^ How much an On-Demand Instance costs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CoverageCost' value with any optional fields omitted.
mkCoverageCost
    :: CoverageCost
mkCoverageCost = CoverageCost'{onDemandCost = Core.Nothing}

-- | How much an On-Demand Instance costs.
--
-- /Note:/ Consider using 'onDemandCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccOnDemandCost :: Lens.Lens' CoverageCost (Core.Maybe Types.OnDemandCost)
ccOnDemandCost = Lens.field @"onDemandCost"
{-# INLINEABLE ccOnDemandCost #-}
{-# DEPRECATED onDemandCost "Use generic-lens or generic-optics with 'onDemandCost' instead"  #-}

instance Core.FromJSON CoverageCost where
        parseJSON
          = Core.withObject "CoverageCost" Core.$
              \ x -> CoverageCost' Core.<$> (x Core..:? "OnDemandCost")

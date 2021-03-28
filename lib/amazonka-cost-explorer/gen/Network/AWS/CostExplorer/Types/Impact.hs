{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Impact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Impact
  ( Impact (..)
  -- * Smart constructor
  , mkImpact
  -- * Lenses
  , iMaxImpact
  , iTotalImpact
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The anomaly's dollar value. 
--
-- /See:/ 'mkImpact' smart constructor.
data Impact = Impact'
  { maxImpact :: Core.Double
    -- ^ The maximum dollar value observed for an anomaly. 
  , totalImpact :: Core.Maybe Core.Double
    -- ^ The cumulative dollar value observed for an anomaly. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Impact' value with any optional fields omitted.
mkImpact
    :: Core.Double -- ^ 'maxImpact'
    -> Impact
mkImpact maxImpact = Impact'{maxImpact, totalImpact = Core.Nothing}

-- | The maximum dollar value observed for an anomaly. 
--
-- /Note:/ Consider using 'maxImpact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iMaxImpact :: Lens.Lens' Impact Core.Double
iMaxImpact = Lens.field @"maxImpact"
{-# INLINEABLE iMaxImpact #-}
{-# DEPRECATED maxImpact "Use generic-lens or generic-optics with 'maxImpact' instead"  #-}

-- | The cumulative dollar value observed for an anomaly. 
--
-- /Note:/ Consider using 'totalImpact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTotalImpact :: Lens.Lens' Impact (Core.Maybe Core.Double)
iTotalImpact = Lens.field @"totalImpact"
{-# INLINEABLE iTotalImpact #-}
{-# DEPRECATED totalImpact "Use generic-lens or generic-optics with 'totalImpact' instead"  #-}

instance Core.FromJSON Impact where
        parseJSON
          = Core.withObject "Impact" Core.$
              \ x ->
                Impact' Core.<$>
                  (x Core..: "MaxImpact") Core.<*> x Core..:? "TotalImpact"

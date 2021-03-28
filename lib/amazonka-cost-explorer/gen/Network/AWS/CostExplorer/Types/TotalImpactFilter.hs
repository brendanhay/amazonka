{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.TotalImpactFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.TotalImpactFilter
  ( TotalImpactFilter (..)
  -- * Smart constructor
  , mkTotalImpactFilter
  -- * Lenses
  , tifNumericOperator
  , tifStartValue
  , tifEndValue
  ) where

import qualified Network.AWS.CostExplorer.Types.NumericOperator as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Filters cost anomalies based on the total impact. 
--
-- /See:/ 'mkTotalImpactFilter' smart constructor.
data TotalImpactFilter = TotalImpactFilter'
  { numericOperator :: Types.NumericOperator
    -- ^ The comparing value used in the filter. 
  , startValue :: Core.Double
    -- ^ The lower bound dollar value used in the filter. 
  , endValue :: Core.Maybe Core.Double
    -- ^ The upper bound dollar value used in the filter. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TotalImpactFilter' value with any optional fields omitted.
mkTotalImpactFilter
    :: Types.NumericOperator -- ^ 'numericOperator'
    -> Core.Double -- ^ 'startValue'
    -> TotalImpactFilter
mkTotalImpactFilter numericOperator startValue
  = TotalImpactFilter'{numericOperator, startValue,
                       endValue = Core.Nothing}

-- | The comparing value used in the filter. 
--
-- /Note:/ Consider using 'numericOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifNumericOperator :: Lens.Lens' TotalImpactFilter Types.NumericOperator
tifNumericOperator = Lens.field @"numericOperator"
{-# INLINEABLE tifNumericOperator #-}
{-# DEPRECATED numericOperator "Use generic-lens or generic-optics with 'numericOperator' instead"  #-}

-- | The lower bound dollar value used in the filter. 
--
-- /Note:/ Consider using 'startValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifStartValue :: Lens.Lens' TotalImpactFilter Core.Double
tifStartValue = Lens.field @"startValue"
{-# INLINEABLE tifStartValue #-}
{-# DEPRECATED startValue "Use generic-lens or generic-optics with 'startValue' instead"  #-}

-- | The upper bound dollar value used in the filter. 
--
-- /Note:/ Consider using 'endValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifEndValue :: Lens.Lens' TotalImpactFilter (Core.Maybe Core.Double)
tifEndValue = Lens.field @"endValue"
{-# INLINEABLE tifEndValue #-}
{-# DEPRECATED endValue "Use generic-lens or generic-optics with 'endValue' instead"  #-}

instance Core.FromJSON TotalImpactFilter where
        toJSON TotalImpactFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("NumericOperator" Core..= numericOperator),
                  Core.Just ("StartValue" Core..= startValue),
                  ("EndValue" Core..=) Core.<$> endValue])

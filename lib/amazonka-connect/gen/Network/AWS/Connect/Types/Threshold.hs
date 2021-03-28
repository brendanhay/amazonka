{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Threshold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.Threshold
  ( Threshold (..)
  -- * Smart constructor
  , mkThreshold
  -- * Lenses
  , tComparison
  , tThresholdValue
  ) where

import qualified Network.AWS.Connect.Types.Comparison as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the threshold for service level metrics.
--
-- /See:/ 'mkThreshold' smart constructor.
data Threshold = Threshold'
  { comparison :: Core.Maybe Types.Comparison
    -- ^ The type of comparison. Only "less than" (LT) comparisons are supported.
  , thresholdValue :: Core.Maybe Core.Double
    -- ^ The threshold value to compare.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Threshold' value with any optional fields omitted.
mkThreshold
    :: Threshold
mkThreshold
  = Threshold'{comparison = Core.Nothing,
               thresholdValue = Core.Nothing}

-- | The type of comparison. Only "less than" (LT) comparisons are supported.
--
-- /Note:/ Consider using 'comparison' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tComparison :: Lens.Lens' Threshold (Core.Maybe Types.Comparison)
tComparison = Lens.field @"comparison"
{-# INLINEABLE tComparison #-}
{-# DEPRECATED comparison "Use generic-lens or generic-optics with 'comparison' instead"  #-}

-- | The threshold value to compare.
--
-- /Note:/ Consider using 'thresholdValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tThresholdValue :: Lens.Lens' Threshold (Core.Maybe Core.Double)
tThresholdValue = Lens.field @"thresholdValue"
{-# INLINEABLE tThresholdValue #-}
{-# DEPRECATED thresholdValue "Use generic-lens or generic-optics with 'thresholdValue' instead"  #-}

instance Core.FromJSON Threshold where
        toJSON Threshold{..}
          = Core.object
              (Core.catMaybes
                 [("Comparison" Core..=) Core.<$> comparison,
                  ("ThresholdValue" Core..=) Core.<$> thresholdValue])

instance Core.FromJSON Threshold where
        parseJSON
          = Core.withObject "Threshold" Core.$
              \ x ->
                Threshold' Core.<$>
                  (x Core..:? "Comparison") Core.<*> x Core..:? "ThresholdValue"

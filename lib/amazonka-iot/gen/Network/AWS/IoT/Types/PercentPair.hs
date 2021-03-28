{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PercentPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.PercentPair
  ( PercentPair (..)
  -- * Smart constructor
  , mkPercentPair
  -- * Lenses
  , ppPercent
  , ppValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the percentile and percentile value.
--
-- /See:/ 'mkPercentPair' smart constructor.
data PercentPair = PercentPair'
  { percent :: Core.Maybe Core.Double
    -- ^ The percentile.
  , value :: Core.Maybe Core.Double
    -- ^ The value of the percentile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PercentPair' value with any optional fields omitted.
mkPercentPair
    :: PercentPair
mkPercentPair
  = PercentPair'{percent = Core.Nothing, value = Core.Nothing}

-- | The percentile.
--
-- /Note:/ Consider using 'percent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPercent :: Lens.Lens' PercentPair (Core.Maybe Core.Double)
ppPercent = Lens.field @"percent"
{-# INLINEABLE ppPercent #-}
{-# DEPRECATED percent "Use generic-lens or generic-optics with 'percent' instead"  #-}

-- | The value of the percentile.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppValue :: Lens.Lens' PercentPair (Core.Maybe Core.Double)
ppValue = Lens.field @"value"
{-# INLINEABLE ppValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON PercentPair where
        parseJSON
          = Core.withObject "PercentPair" Core.$
              \ x ->
                PercentPair' Core.<$>
                  (x Core..:? "percent") Core.<*> x Core..:? "value"

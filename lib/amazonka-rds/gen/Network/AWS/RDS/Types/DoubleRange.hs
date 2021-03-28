{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DoubleRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DoubleRange
  ( DoubleRange (..)
  -- * Smart constructor
  , mkDoubleRange
  -- * Lenses
  , drFrom
  , drTo
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A range of double values.
--
-- /See:/ 'mkDoubleRange' smart constructor.
data DoubleRange = DoubleRange'
  { from :: Core.Maybe Core.Double
    -- ^ The minimum value in the range.
  , to :: Core.Maybe Core.Double
    -- ^ The maximum value in the range.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DoubleRange' value with any optional fields omitted.
mkDoubleRange
    :: DoubleRange
mkDoubleRange
  = DoubleRange'{from = Core.Nothing, to = Core.Nothing}

-- | The minimum value in the range.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drFrom :: Lens.Lens' DoubleRange (Core.Maybe Core.Double)
drFrom = Lens.field @"from"
{-# INLINEABLE drFrom #-}
{-# DEPRECATED from "Use generic-lens or generic-optics with 'from' instead"  #-}

-- | The maximum value in the range.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drTo :: Lens.Lens' DoubleRange (Core.Maybe Core.Double)
drTo = Lens.field @"to"
{-# INLINEABLE drTo #-}
{-# DEPRECATED to "Use generic-lens or generic-optics with 'to' instead"  #-}

instance Core.FromXML DoubleRange where
        parseXML x
          = DoubleRange' Core.<$>
              (x Core..@? "From") Core.<*> x Core..@? "To"

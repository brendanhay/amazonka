{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Range
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Range
  ( Range (..),

    -- * Smart constructor
    mkRange,

    -- * Lenses
    rFrom,
    rStep,
    rTo,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A range of integer values.
--
-- /See:/ 'mkRange' smart constructor.
data Range = Range'
  { -- | The minimum value in the range.
    from :: Core.Maybe Core.Int,
    -- | The step value for the range. For example, if you have a range of 5,000 to 10,000, with a step value of 1,000, the valid values start at 5,000 and step up by 1,000. Even though 7,500 is within the range, it isn't a valid value for the range. The valid values are 5,000, 6,000, 7,000, 8,000...
    step :: Core.Maybe Core.Int,
    -- | The maximum value in the range.
    to :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Range' value with any optional fields omitted.
mkRange ::
  Range
mkRange =
  Range'
    { from = Core.Nothing,
      step = Core.Nothing,
      to = Core.Nothing
    }

-- | The minimum value in the range.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rFrom :: Lens.Lens' Range (Core.Maybe Core.Int)
rFrom = Lens.field @"from"
{-# DEPRECATED rFrom "Use generic-lens or generic-optics with 'from' instead." #-}

-- | The step value for the range. For example, if you have a range of 5,000 to 10,000, with a step value of 1,000, the valid values start at 5,000 and step up by 1,000. Even though 7,500 is within the range, it isn't a valid value for the range. The valid values are 5,000, 6,000, 7,000, 8,000...
--
-- /Note:/ Consider using 'step' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rStep :: Lens.Lens' Range (Core.Maybe Core.Int)
rStep = Lens.field @"step"
{-# DEPRECATED rStep "Use generic-lens or generic-optics with 'step' instead." #-}

-- | The maximum value in the range.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTo :: Lens.Lens' Range (Core.Maybe Core.Int)
rTo = Lens.field @"to"
{-# DEPRECATED rTo "Use generic-lens or generic-optics with 'to' instead." #-}

instance Core.FromXML Range where
  parseXML x =
    Range'
      Core.<$> (x Core..@? "From")
      Core.<*> (x Core..@? "Step")
      Core.<*> (x Core..@? "To")

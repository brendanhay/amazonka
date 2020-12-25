{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.TimestampRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.TimestampRange
  ( TimestampRange (..),

    -- * Smart constructor
    mkTimestampRange,

    -- * Lenses
    trBeginDate,
    trEndDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type is used in the 'AssessmentRunFilter' data type.
--
-- /See:/ 'mkTimestampRange' smart constructor.
data TimestampRange = TimestampRange'
  { -- | The minimum value of the timestamp range.
    beginDate :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum value of the timestamp range.
    endDate :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TimestampRange' value with any optional fields omitted.
mkTimestampRange ::
  TimestampRange
mkTimestampRange =
  TimestampRange' {beginDate = Core.Nothing, endDate = Core.Nothing}

-- | The minimum value of the timestamp range.
--
-- /Note:/ Consider using 'beginDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trBeginDate :: Lens.Lens' TimestampRange (Core.Maybe Core.NominalDiffTime)
trBeginDate = Lens.field @"beginDate"
{-# DEPRECATED trBeginDate "Use generic-lens or generic-optics with 'beginDate' instead." #-}

-- | The maximum value of the timestamp range.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trEndDate :: Lens.Lens' TimestampRange (Core.Maybe Core.NominalDiffTime)
trEndDate = Lens.field @"endDate"
{-# DEPRECATED trEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

instance Core.FromJSON TimestampRange where
  toJSON TimestampRange {..} =
    Core.object
      ( Core.catMaybes
          [ ("beginDate" Core..=) Core.<$> beginDate,
            ("endDate" Core..=) Core.<$> endDate
          ]
      )

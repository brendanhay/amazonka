{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SlotStartTimeRangeRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SlotStartTimeRangeRequest
  ( SlotStartTimeRangeRequest (..),

    -- * Smart constructor
    mkSlotStartTimeRangeRequest,

    -- * Lenses
    sstrrEarliestTime,
    sstrrLatestTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the time period for a Scheduled Instance to start its first schedule.
--
-- /See:/ 'mkSlotStartTimeRangeRequest' smart constructor.
data SlotStartTimeRangeRequest = SlotStartTimeRangeRequest'
  { -- | The earliest date and time, in UTC, for the Scheduled Instance to start.
    earliestTime :: Core.Maybe Core.UTCTime,
    -- | The latest date and time, in UTC, for the Scheduled Instance to start.
    latestTime :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SlotStartTimeRangeRequest' value with any optional fields omitted.
mkSlotStartTimeRangeRequest ::
  SlotStartTimeRangeRequest
mkSlotStartTimeRangeRequest =
  SlotStartTimeRangeRequest'
    { earliestTime = Core.Nothing,
      latestTime = Core.Nothing
    }

-- | The earliest date and time, in UTC, for the Scheduled Instance to start.
--
-- /Note:/ Consider using 'earliestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sstrrEarliestTime :: Lens.Lens' SlotStartTimeRangeRequest (Core.Maybe Core.UTCTime)
sstrrEarliestTime = Lens.field @"earliestTime"
{-# DEPRECATED sstrrEarliestTime "Use generic-lens or generic-optics with 'earliestTime' instead." #-}

-- | The latest date and time, in UTC, for the Scheduled Instance to start.
--
-- /Note:/ Consider using 'latestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sstrrLatestTime :: Lens.Lens' SlotStartTimeRangeRequest (Core.Maybe Core.UTCTime)
sstrrLatestTime = Lens.field @"latestTime"
{-# DEPRECATED sstrrLatestTime "Use generic-lens or generic-optics with 'latestTime' instead." #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SlotDateTimeRangeRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SlotDateTimeRangeRequest
  ( SlotDateTimeRangeRequest (..),

    -- * Smart constructor
    mkSlotDateTimeRangeRequest,

    -- * Lenses
    sdtrrEarliestTime,
    sdtrrLatestTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the time period for a Scheduled Instance to start its first schedule. The time period must span less than one day.
--
-- /See:/ 'mkSlotDateTimeRangeRequest' smart constructor.
data SlotDateTimeRangeRequest = SlotDateTimeRangeRequest'
  { -- | The earliest date and time, in UTC, for the Scheduled Instance to start.
    earliestTime :: Core.UTCTime,
    -- | The latest date and time, in UTC, for the Scheduled Instance to start. This value must be later than or equal to the earliest date and at most three months in the future.
    latestTime :: Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SlotDateTimeRangeRequest' value with any optional fields omitted.
mkSlotDateTimeRangeRequest ::
  -- | 'earliestTime'
  Core.UTCTime ->
  -- | 'latestTime'
  Core.UTCTime ->
  SlotDateTimeRangeRequest
mkSlotDateTimeRangeRequest earliestTime latestTime =
  SlotDateTimeRangeRequest' {earliestTime, latestTime}

-- | The earliest date and time, in UTC, for the Scheduled Instance to start.
--
-- /Note:/ Consider using 'earliestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdtrrEarliestTime :: Lens.Lens' SlotDateTimeRangeRequest Core.UTCTime
sdtrrEarliestTime = Lens.field @"earliestTime"
{-# DEPRECATED sdtrrEarliestTime "Use generic-lens or generic-optics with 'earliestTime' instead." #-}

-- | The latest date and time, in UTC, for the Scheduled Instance to start. This value must be later than or equal to the earliest date and at most three months in the future.
--
-- /Note:/ Consider using 'latestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdtrrLatestTime :: Lens.Lens' SlotDateTimeRangeRequest Core.UTCTime
sdtrrLatestTime = Lens.field @"latestTime"
{-# DEPRECATED sdtrrLatestTime "Use generic-lens or generic-optics with 'latestTime' instead." #-}

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
import qualified Network.AWS.Prelude as Lude

-- | Describes the time period for a Scheduled Instance to start its first schedule. The time period must span less than one day.
--
-- /See:/ 'mkSlotDateTimeRangeRequest' smart constructor.
data SlotDateTimeRangeRequest = SlotDateTimeRangeRequest'
  { earliestTime ::
      Lude.ISO8601,
    latestTime :: Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SlotDateTimeRangeRequest' with the minimum fields required to make a request.
--
-- * 'earliestTime' - The earliest date and time, in UTC, for the Scheduled Instance to start.
-- * 'latestTime' - The latest date and time, in UTC, for the Scheduled Instance to start. This value must be later than or equal to the earliest date and at most three months in the future.
mkSlotDateTimeRangeRequest ::
  -- | 'earliestTime'
  Lude.ISO8601 ->
  -- | 'latestTime'
  Lude.ISO8601 ->
  SlotDateTimeRangeRequest
mkSlotDateTimeRangeRequest pEarliestTime_ pLatestTime_ =
  SlotDateTimeRangeRequest'
    { earliestTime = pEarliestTime_,
      latestTime = pLatestTime_
    }

-- | The earliest date and time, in UTC, for the Scheduled Instance to start.
--
-- /Note:/ Consider using 'earliestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdtrrEarliestTime :: Lens.Lens' SlotDateTimeRangeRequest Lude.ISO8601
sdtrrEarliestTime = Lens.lens (earliestTime :: SlotDateTimeRangeRequest -> Lude.ISO8601) (\s a -> s {earliestTime = a} :: SlotDateTimeRangeRequest)
{-# DEPRECATED sdtrrEarliestTime "Use generic-lens or generic-optics with 'earliestTime' instead." #-}

-- | The latest date and time, in UTC, for the Scheduled Instance to start. This value must be later than or equal to the earliest date and at most three months in the future.
--
-- /Note:/ Consider using 'latestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdtrrLatestTime :: Lens.Lens' SlotDateTimeRangeRequest Lude.ISO8601
sdtrrLatestTime = Lens.lens (latestTime :: SlotDateTimeRangeRequest -> Lude.ISO8601) (\s a -> s {latestTime = a} :: SlotDateTimeRangeRequest)
{-# DEPRECATED sdtrrLatestTime "Use generic-lens or generic-optics with 'latestTime' instead." #-}

instance Lude.ToQuery SlotDateTimeRangeRequest where
  toQuery SlotDateTimeRangeRequest' {..} =
    Lude.mconcat
      [ "EarliestTime" Lude.=: earliestTime,
        "LatestTime" Lude.=: latestTime
      ]

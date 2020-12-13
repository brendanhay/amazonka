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
    sstrrLatestTime,
    sstrrEarliestTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the time period for a Scheduled Instance to start its first schedule.
--
-- /See:/ 'mkSlotStartTimeRangeRequest' smart constructor.
data SlotStartTimeRangeRequest = SlotStartTimeRangeRequest'
  { -- | The latest date and time, in UTC, for the Scheduled Instance to start.
    latestTime :: Lude.Maybe Lude.DateTime,
    -- | The earliest date and time, in UTC, for the Scheduled Instance to start.
    earliestTime :: Lude.Maybe Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SlotStartTimeRangeRequest' with the minimum fields required to make a request.
--
-- * 'latestTime' - The latest date and time, in UTC, for the Scheduled Instance to start.
-- * 'earliestTime' - The earliest date and time, in UTC, for the Scheduled Instance to start.
mkSlotStartTimeRangeRequest ::
  SlotStartTimeRangeRequest
mkSlotStartTimeRangeRequest =
  SlotStartTimeRangeRequest'
    { latestTime = Lude.Nothing,
      earliestTime = Lude.Nothing
    }

-- | The latest date and time, in UTC, for the Scheduled Instance to start.
--
-- /Note:/ Consider using 'latestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sstrrLatestTime :: Lens.Lens' SlotStartTimeRangeRequest (Lude.Maybe Lude.DateTime)
sstrrLatestTime = Lens.lens (latestTime :: SlotStartTimeRangeRequest -> Lude.Maybe Lude.DateTime) (\s a -> s {latestTime = a} :: SlotStartTimeRangeRequest)
{-# DEPRECATED sstrrLatestTime "Use generic-lens or generic-optics with 'latestTime' instead." #-}

-- | The earliest date and time, in UTC, for the Scheduled Instance to start.
--
-- /Note:/ Consider using 'earliestTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sstrrEarliestTime :: Lens.Lens' SlotStartTimeRangeRequest (Lude.Maybe Lude.DateTime)
sstrrEarliestTime = Lens.lens (earliestTime :: SlotStartTimeRangeRequest -> Lude.Maybe Lude.DateTime) (\s a -> s {earliestTime = a} :: SlotStartTimeRangeRequest)
{-# DEPRECATED sstrrEarliestTime "Use generic-lens or generic-optics with 'earliestTime' instead." #-}

instance Lude.ToQuery SlotStartTimeRangeRequest where
  toQuery SlotStartTimeRangeRequest' {..} =
    Lude.mconcat
      [ "LatestTime" Lude.=: latestTime,
        "EarliestTime" Lude.=: earliestTime
      ]

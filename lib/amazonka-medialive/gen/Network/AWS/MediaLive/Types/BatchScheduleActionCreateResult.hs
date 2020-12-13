{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchScheduleActionCreateResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchScheduleActionCreateResult
  ( BatchScheduleActionCreateResult (..),

    -- * Smart constructor
    mkBatchScheduleActionCreateResult,

    -- * Lenses
    bScheduleActions,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ScheduleAction
import qualified Network.AWS.Prelude as Lude

-- | List of actions that have been created in the schedule.
--
-- /See:/ 'mkBatchScheduleActionCreateResult' smart constructor.
newtype BatchScheduleActionCreateResult = BatchScheduleActionCreateResult'
  { -- | List of actions that have been created in the schedule.
    scheduleActions :: [ScheduleAction]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchScheduleActionCreateResult' with the minimum fields required to make a request.
--
-- * 'scheduleActions' - List of actions that have been created in the schedule.
mkBatchScheduleActionCreateResult ::
  BatchScheduleActionCreateResult
mkBatchScheduleActionCreateResult =
  BatchScheduleActionCreateResult' {scheduleActions = Lude.mempty}

-- | List of actions that have been created in the schedule.
--
-- /Note:/ Consider using 'scheduleActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bScheduleActions :: Lens.Lens' BatchScheduleActionCreateResult [ScheduleAction]
bScheduleActions = Lens.lens (scheduleActions :: BatchScheduleActionCreateResult -> [ScheduleAction]) (\s a -> s {scheduleActions = a} :: BatchScheduleActionCreateResult)
{-# DEPRECATED bScheduleActions "Use generic-lens or generic-optics with 'scheduleActions' instead." #-}

instance Lude.FromJSON BatchScheduleActionCreateResult where
  parseJSON =
    Lude.withObject
      "BatchScheduleActionCreateResult"
      ( \x ->
          BatchScheduleActionCreateResult'
            Lude.<$> (x Lude..:? "scheduleActions" Lude..!= Lude.mempty)
      )

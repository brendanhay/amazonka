{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchScheduleActionDeleteResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchScheduleActionDeleteResult
  ( BatchScheduleActionDeleteResult (..),

    -- * Smart constructor
    mkBatchScheduleActionDeleteResult,

    -- * Lenses
    bsadrScheduleActions,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ScheduleAction
import qualified Network.AWS.Prelude as Lude

-- | List of actions that have been deleted from the schedule.
--
-- /See:/ 'mkBatchScheduleActionDeleteResult' smart constructor.
newtype BatchScheduleActionDeleteResult = BatchScheduleActionDeleteResult'
  { scheduleActions ::
      [ScheduleAction]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchScheduleActionDeleteResult' with the minimum fields required to make a request.
--
-- * 'scheduleActions' - List of actions that have been deleted from the schedule.
mkBatchScheduleActionDeleteResult ::
  BatchScheduleActionDeleteResult
mkBatchScheduleActionDeleteResult =
  BatchScheduleActionDeleteResult' {scheduleActions = Lude.mempty}

-- | List of actions that have been deleted from the schedule.
--
-- /Note:/ Consider using 'scheduleActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsadrScheduleActions :: Lens.Lens' BatchScheduleActionDeleteResult [ScheduleAction]
bsadrScheduleActions = Lens.lens (scheduleActions :: BatchScheduleActionDeleteResult -> [ScheduleAction]) (\s a -> s {scheduleActions = a} :: BatchScheduleActionDeleteResult)
{-# DEPRECATED bsadrScheduleActions "Use generic-lens or generic-optics with 'scheduleActions' instead." #-}

instance Lude.FromJSON BatchScheduleActionDeleteResult where
  parseJSON =
    Lude.withObject
      "BatchScheduleActionDeleteResult"
      ( \x ->
          BatchScheduleActionDeleteResult'
            Lude.<$> (x Lude..:? "scheduleActions" Lude..!= Lude.mempty)
      )

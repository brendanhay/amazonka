{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchScheduleActionCreateResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.BatchScheduleActionCreateResult
  ( BatchScheduleActionCreateResult (..)
  -- * Smart constructor
  , mkBatchScheduleActionCreateResult
  -- * Lenses
  , bScheduleActions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.ScheduleAction as Types
import qualified Network.AWS.Prelude as Core

-- | List of actions that have been created in the schedule.
--
-- /See:/ 'mkBatchScheduleActionCreateResult' smart constructor.
newtype BatchScheduleActionCreateResult = BatchScheduleActionCreateResult'
  { scheduleActions :: [Types.ScheduleAction]
    -- ^ List of actions that have been created in the schedule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchScheduleActionCreateResult' value with any optional fields omitted.
mkBatchScheduleActionCreateResult
    :: BatchScheduleActionCreateResult
mkBatchScheduleActionCreateResult
  = BatchScheduleActionCreateResult'{scheduleActions = Core.mempty}

-- | List of actions that have been created in the schedule.
--
-- /Note:/ Consider using 'scheduleActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bScheduleActions :: Lens.Lens' BatchScheduleActionCreateResult [Types.ScheduleAction]
bScheduleActions = Lens.field @"scheduleActions"
{-# INLINEABLE bScheduleActions #-}
{-# DEPRECATED scheduleActions "Use generic-lens or generic-optics with 'scheduleActions' instead"  #-}

instance Core.FromJSON BatchScheduleActionCreateResult where
        parseJSON
          = Core.withObject "BatchScheduleActionCreateResult" Core.$
              \ x ->
                BatchScheduleActionCreateResult' Core.<$>
                  (x Core..:? "scheduleActions" Core..!= Core.mempty)

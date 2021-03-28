{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchScheduleActionDeleteResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.BatchScheduleActionDeleteResult
  ( BatchScheduleActionDeleteResult (..)
  -- * Smart constructor
  , mkBatchScheduleActionDeleteResult
  -- * Lenses
  , bsadrScheduleActions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.ScheduleAction as Types
import qualified Network.AWS.Prelude as Core

-- | List of actions that have been deleted from the schedule.
--
-- /See:/ 'mkBatchScheduleActionDeleteResult' smart constructor.
newtype BatchScheduleActionDeleteResult = BatchScheduleActionDeleteResult'
  { scheduleActions :: [Types.ScheduleAction]
    -- ^ List of actions that have been deleted from the schedule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchScheduleActionDeleteResult' value with any optional fields omitted.
mkBatchScheduleActionDeleteResult
    :: BatchScheduleActionDeleteResult
mkBatchScheduleActionDeleteResult
  = BatchScheduleActionDeleteResult'{scheduleActions = Core.mempty}

-- | List of actions that have been deleted from the schedule.
--
-- /Note:/ Consider using 'scheduleActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsadrScheduleActions :: Lens.Lens' BatchScheduleActionDeleteResult [Types.ScheduleAction]
bsadrScheduleActions = Lens.field @"scheduleActions"
{-# INLINEABLE bsadrScheduleActions #-}
{-# DEPRECATED scheduleActions "Use generic-lens or generic-optics with 'scheduleActions' instead"  #-}

instance Core.FromJSON BatchScheduleActionDeleteResult where
        parseJSON
          = Core.withObject "BatchScheduleActionDeleteResult" Core.$
              \ x ->
                BatchScheduleActionDeleteResult' Core.<$>
                  (x Core..:? "scheduleActions" Core..!= Core.mempty)

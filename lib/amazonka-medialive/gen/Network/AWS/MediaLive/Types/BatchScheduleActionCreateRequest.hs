{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchScheduleActionCreateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchScheduleActionCreateRequest
  ( BatchScheduleActionCreateRequest (..),

    -- * Smart constructor
    mkBatchScheduleActionCreateRequest,

    -- * Lenses
    bsacrScheduleActions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.ScheduleAction as Types
import qualified Network.AWS.Prelude as Core

-- | A list of schedule actions to create (in a request) or that have been created (in a response).
--
-- /See:/ 'mkBatchScheduleActionCreateRequest' smart constructor.
newtype BatchScheduleActionCreateRequest = BatchScheduleActionCreateRequest'
  { -- | A list of schedule actions to create.
    scheduleActions :: [Types.ScheduleAction]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchScheduleActionCreateRequest' value with any optional fields omitted.
mkBatchScheduleActionCreateRequest ::
  BatchScheduleActionCreateRequest
mkBatchScheduleActionCreateRequest =
  BatchScheduleActionCreateRequest' {scheduleActions = Core.mempty}

-- | A list of schedule actions to create.
--
-- /Note:/ Consider using 'scheduleActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsacrScheduleActions :: Lens.Lens' BatchScheduleActionCreateRequest [Types.ScheduleAction]
bsacrScheduleActions = Lens.field @"scheduleActions"
{-# DEPRECATED bsacrScheduleActions "Use generic-lens or generic-optics with 'scheduleActions' instead." #-}

instance Core.FromJSON BatchScheduleActionCreateRequest where
  toJSON BatchScheduleActionCreateRequest {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("scheduleActions" Core..= scheduleActions)]
      )

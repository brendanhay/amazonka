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
import Network.AWS.MediaLive.Types.ScheduleAction
import qualified Network.AWS.Prelude as Lude

-- | A list of schedule actions to create (in a request) or that have been created (in a response).
--
-- /See:/ 'mkBatchScheduleActionCreateRequest' smart constructor.
newtype BatchScheduleActionCreateRequest = BatchScheduleActionCreateRequest'
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

-- | Creates a value of 'BatchScheduleActionCreateRequest' with the minimum fields required to make a request.
--
-- * 'scheduleActions' - A list of schedule actions to create.
mkBatchScheduleActionCreateRequest ::
  BatchScheduleActionCreateRequest
mkBatchScheduleActionCreateRequest =
  BatchScheduleActionCreateRequest' {scheduleActions = Lude.mempty}

-- | A list of schedule actions to create.
--
-- /Note:/ Consider using 'scheduleActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsacrScheduleActions :: Lens.Lens' BatchScheduleActionCreateRequest [ScheduleAction]
bsacrScheduleActions = Lens.lens (scheduleActions :: BatchScheduleActionCreateRequest -> [ScheduleAction]) (\s a -> s {scheduleActions = a} :: BatchScheduleActionCreateRequest)
{-# DEPRECATED bsacrScheduleActions "Use generic-lens or generic-optics with 'scheduleActions' instead." #-}

instance Lude.ToJSON BatchScheduleActionCreateRequest where
  toJSON BatchScheduleActionCreateRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("scheduleActions" Lude..= scheduleActions)]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchScheduleActionDeleteRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchScheduleActionDeleteRequest
  ( BatchScheduleActionDeleteRequest (..),

    -- * Smart constructor
    mkBatchScheduleActionDeleteRequest,

    -- * Lenses
    bsadrActionNames,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of schedule actions to delete.
--
-- /See:/ 'mkBatchScheduleActionDeleteRequest' smart constructor.
newtype BatchScheduleActionDeleteRequest = BatchScheduleActionDeleteRequest'
  { actionNames ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchScheduleActionDeleteRequest' with the minimum fields required to make a request.
--
-- * 'actionNames' - A list of schedule actions to delete.
mkBatchScheduleActionDeleteRequest ::
  BatchScheduleActionDeleteRequest
mkBatchScheduleActionDeleteRequest =
  BatchScheduleActionDeleteRequest' {actionNames = Lude.mempty}

-- | A list of schedule actions to delete.
--
-- /Note:/ Consider using 'actionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsadrActionNames :: Lens.Lens' BatchScheduleActionDeleteRequest [Lude.Text]
bsadrActionNames = Lens.lens (actionNames :: BatchScheduleActionDeleteRequest -> [Lude.Text]) (\s a -> s {actionNames = a} :: BatchScheduleActionDeleteRequest)
{-# DEPRECATED bsadrActionNames "Use generic-lens or generic-optics with 'actionNames' instead." #-}

instance Lude.ToJSON BatchScheduleActionDeleteRequest where
  toJSON BatchScheduleActionDeleteRequest' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("actionNames" Lude..= actionNames)])

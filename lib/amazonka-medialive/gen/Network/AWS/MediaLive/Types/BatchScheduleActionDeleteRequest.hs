{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchScheduleActionDeleteRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.BatchScheduleActionDeleteRequest
  ( BatchScheduleActionDeleteRequest (..)
  -- * Smart constructor
  , mkBatchScheduleActionDeleteRequest
  -- * Lenses
  , bsadrActionNames
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of schedule actions to delete.
--
-- /See:/ 'mkBatchScheduleActionDeleteRequest' smart constructor.
newtype BatchScheduleActionDeleteRequest = BatchScheduleActionDeleteRequest'
  { actionNames :: [Core.Text]
    -- ^ A list of schedule actions to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchScheduleActionDeleteRequest' value with any optional fields omitted.
mkBatchScheduleActionDeleteRequest
    :: BatchScheduleActionDeleteRequest
mkBatchScheduleActionDeleteRequest
  = BatchScheduleActionDeleteRequest'{actionNames = Core.mempty}

-- | A list of schedule actions to delete.
--
-- /Note:/ Consider using 'actionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsadrActionNames :: Lens.Lens' BatchScheduleActionDeleteRequest [Core.Text]
bsadrActionNames = Lens.field @"actionNames"
{-# INLINEABLE bsadrActionNames #-}
{-# DEPRECATED actionNames "Use generic-lens or generic-optics with 'actionNames' instead"  #-}

instance Core.FromJSON BatchScheduleActionDeleteRequest where
        toJSON BatchScheduleActionDeleteRequest{..}
          = Core.object
              (Core.catMaybes [Core.Just ("actionNames" Core..= actionNames)])

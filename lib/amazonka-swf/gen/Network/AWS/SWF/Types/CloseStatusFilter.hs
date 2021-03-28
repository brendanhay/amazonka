{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CloseStatusFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.CloseStatusFilter
  ( CloseStatusFilter (..)
  -- * Smart constructor
  , mkCloseStatusFilter
  -- * Lenses
  , csfStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.CloseStatus as Types

-- | Used to filter the closed workflow executions in visibility APIs by their close status.
--
-- /See:/ 'mkCloseStatusFilter' smart constructor.
newtype CloseStatusFilter = CloseStatusFilter'
  { status :: Types.CloseStatus
    -- ^ The close status that must match the close status of an execution for it to meet the criteria of this filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CloseStatusFilter' value with any optional fields omitted.
mkCloseStatusFilter
    :: Types.CloseStatus -- ^ 'status'
    -> CloseStatusFilter
mkCloseStatusFilter status = CloseStatusFilter'{status}

-- | The close status that must match the close status of an execution for it to meet the criteria of this filter.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfStatus :: Lens.Lens' CloseStatusFilter Types.CloseStatus
csfStatus = Lens.field @"status"
{-# INLINEABLE csfStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON CloseStatusFilter where
        toJSON CloseStatusFilter{..}
          = Core.object
              (Core.catMaybes [Core.Just ("status" Core..= status)])

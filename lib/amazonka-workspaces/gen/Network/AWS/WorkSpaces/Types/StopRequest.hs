{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.StopRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.StopRequest
  ( StopRequest (..)
  -- * Smart constructor
  , mkStopRequest
  -- * Lenses
  , srWorkspaceId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.WorkspaceId as Types

-- | Describes the information used to stop a WorkSpace.
--
-- /See:/ 'mkStopRequest' smart constructor.
newtype StopRequest = StopRequest'
  { workspaceId :: Core.Maybe Types.WorkspaceId
    -- ^ The identifier of the WorkSpace.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopRequest' value with any optional fields omitted.
mkStopRequest
    :: StopRequest
mkStopRequest = StopRequest'{workspaceId = Core.Nothing}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srWorkspaceId :: Lens.Lens' StopRequest (Core.Maybe Types.WorkspaceId)
srWorkspaceId = Lens.field @"workspaceId"
{-# INLINEABLE srWorkspaceId #-}
{-# DEPRECATED workspaceId "Use generic-lens or generic-optics with 'workspaceId' instead"  #-}

instance Core.FromJSON StopRequest where
        toJSON StopRequest{..}
          = Core.object
              (Core.catMaybes [("WorkspaceId" Core..=) Core.<$> workspaceId])

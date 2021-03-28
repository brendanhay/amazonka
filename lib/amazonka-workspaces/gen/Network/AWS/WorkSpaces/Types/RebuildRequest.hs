{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.RebuildRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.RebuildRequest
  ( RebuildRequest (..)
  -- * Smart constructor
  , mkRebuildRequest
  -- * Lenses
  , rrWorkspaceId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.WorkspaceId as Types

-- | Describes the information used to rebuild a WorkSpace.
--
-- /See:/ 'mkRebuildRequest' smart constructor.
newtype RebuildRequest = RebuildRequest'
  { workspaceId :: Types.WorkspaceId
    -- ^ The identifier of the WorkSpace.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RebuildRequest' value with any optional fields omitted.
mkRebuildRequest
    :: Types.WorkspaceId -- ^ 'workspaceId'
    -> RebuildRequest
mkRebuildRequest workspaceId = RebuildRequest'{workspaceId}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrWorkspaceId :: Lens.Lens' RebuildRequest Types.WorkspaceId
rrWorkspaceId = Lens.field @"workspaceId"
{-# INLINEABLE rrWorkspaceId #-}
{-# DEPRECATED workspaceId "Use generic-lens or generic-optics with 'workspaceId' instead"  #-}

instance Core.FromJSON RebuildRequest where
        toJSON RebuildRequest{..}
          = Core.object
              (Core.catMaybes [Core.Just ("WorkspaceId" Core..= workspaceId)])

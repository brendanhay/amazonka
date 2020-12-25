{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.StartRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.StartRequest
  ( StartRequest (..),

    -- * Smart constructor
    mkStartRequest,

    -- * Lenses
    sWorkspaceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.WorkspaceId as Types

-- | Information used to start a WorkSpace.
--
-- /See:/ 'mkStartRequest' smart constructor.
newtype StartRequest = StartRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Core.Maybe Types.WorkspaceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartRequest' value with any optional fields omitted.
mkStartRequest ::
  StartRequest
mkStartRequest = StartRequest' {workspaceId = Core.Nothing}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sWorkspaceId :: Lens.Lens' StartRequest (Core.Maybe Types.WorkspaceId)
sWorkspaceId = Lens.field @"workspaceId"
{-# DEPRECATED sWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

instance Core.FromJSON StartRequest where
  toJSON StartRequest {..} =
    Core.object
      (Core.catMaybes [("WorkspaceId" Core..=) Core.<$> workspaceId])

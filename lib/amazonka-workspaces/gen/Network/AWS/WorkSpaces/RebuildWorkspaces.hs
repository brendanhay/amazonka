{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.RebuildWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rebuilds the specified WorkSpace.
--
-- You cannot rebuild a WorkSpace unless its state is @AVAILABLE@ , @ERROR@ , @UNHEALTHY@ , @STOPPED@ , or @REBOOTING@ .
-- Rebuilding a WorkSpace is a potentially destructive action that can result in the loss of data. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/reset-workspace.html Rebuild a WorkSpace> .
-- This operation is asynchronous and returns before the WorkSpaces have been completely rebuilt.
module Network.AWS.WorkSpaces.RebuildWorkspaces
  ( -- * Creating a request
    RebuildWorkspaces (..),
    mkRebuildWorkspaces,

    -- ** Request lenses
    rwRebuildWorkspaceRequests,

    -- * Destructuring the response
    RebuildWorkspacesResponse (..),
    mkRebuildWorkspacesResponse,

    -- ** Response lenses
    rwrrsFailedRequests,
    rwrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkRebuildWorkspaces' smart constructor.
newtype RebuildWorkspaces = RebuildWorkspaces'
  { -- | The WorkSpace to rebuild. You can specify a single WorkSpace.
    rebuildWorkspaceRequests :: Core.NonEmpty Types.RebuildRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RebuildWorkspaces' value with any optional fields omitted.
mkRebuildWorkspaces ::
  -- | 'rebuildWorkspaceRequests'
  Core.NonEmpty Types.RebuildRequest ->
  RebuildWorkspaces
mkRebuildWorkspaces rebuildWorkspaceRequests =
  RebuildWorkspaces' {rebuildWorkspaceRequests}

-- | The WorkSpace to rebuild. You can specify a single WorkSpace.
--
-- /Note:/ Consider using 'rebuildWorkspaceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwRebuildWorkspaceRequests :: Lens.Lens' RebuildWorkspaces (Core.NonEmpty Types.RebuildRequest)
rwRebuildWorkspaceRequests = Lens.field @"rebuildWorkspaceRequests"
{-# DEPRECATED rwRebuildWorkspaceRequests "Use generic-lens or generic-optics with 'rebuildWorkspaceRequests' instead." #-}

instance Core.FromJSON RebuildWorkspaces where
  toJSON RebuildWorkspaces {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("RebuildWorkspaceRequests" Core..= rebuildWorkspaceRequests)
          ]
      )

instance Core.AWSRequest RebuildWorkspaces where
  type Rs RebuildWorkspaces = RebuildWorkspacesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkspacesService.RebuildWorkspaces")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RebuildWorkspacesResponse'
            Core.<$> (x Core..:? "FailedRequests")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRebuildWorkspacesResponse' smart constructor.
data RebuildWorkspacesResponse = RebuildWorkspacesResponse'
  { -- | Information about the WorkSpace that could not be rebuilt.
    failedRequests :: Core.Maybe [Types.FailedWorkspaceChangeRequest],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RebuildWorkspacesResponse' value with any optional fields omitted.
mkRebuildWorkspacesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RebuildWorkspacesResponse
mkRebuildWorkspacesResponse responseStatus =
  RebuildWorkspacesResponse'
    { failedRequests = Core.Nothing,
      responseStatus
    }

-- | Information about the WorkSpace that could not be rebuilt.
--
-- /Note:/ Consider using 'failedRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrrsFailedRequests :: Lens.Lens' RebuildWorkspacesResponse (Core.Maybe [Types.FailedWorkspaceChangeRequest])
rwrrsFailedRequests = Lens.field @"failedRequests"
{-# DEPRECATED rwrrsFailedRequests "Use generic-lens or generic-optics with 'failedRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrrsResponseStatus :: Lens.Lens' RebuildWorkspacesResponse Core.Int
rwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

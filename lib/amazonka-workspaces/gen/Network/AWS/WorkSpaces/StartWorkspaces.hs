{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.StartWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified WorkSpaces.
--
-- You cannot start a WorkSpace unless it has a running mode of @AutoStop@ and a state of @STOPPED@ .
module Network.AWS.WorkSpaces.StartWorkspaces
  ( -- * Creating a request
    StartWorkspaces (..),
    mkStartWorkspaces,

    -- ** Request lenses
    swStartWorkspaceRequests,

    -- * Destructuring the response
    StartWorkspacesResponse (..),
    mkStartWorkspacesResponse,

    -- ** Response lenses
    srsFailedRequests,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkStartWorkspaces' smart constructor.
newtype StartWorkspaces = StartWorkspaces'
  { -- | The WorkSpaces to start. You can specify up to 25 WorkSpaces.
    startWorkspaceRequests :: Core.NonEmpty Types.StartRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartWorkspaces' value with any optional fields omitted.
mkStartWorkspaces ::
  -- | 'startWorkspaceRequests'
  Core.NonEmpty Types.StartRequest ->
  StartWorkspaces
mkStartWorkspaces startWorkspaceRequests =
  StartWorkspaces' {startWorkspaceRequests}

-- | The WorkSpaces to start. You can specify up to 25 WorkSpaces.
--
-- /Note:/ Consider using 'startWorkspaceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swStartWorkspaceRequests :: Lens.Lens' StartWorkspaces (Core.NonEmpty Types.StartRequest)
swStartWorkspaceRequests = Lens.field @"startWorkspaceRequests"
{-# DEPRECATED swStartWorkspaceRequests "Use generic-lens or generic-optics with 'startWorkspaceRequests' instead." #-}

instance Core.FromJSON StartWorkspaces where
  toJSON StartWorkspaces {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("StartWorkspaceRequests" Core..= startWorkspaceRequests)
          ]
      )

instance Core.AWSRequest StartWorkspaces where
  type Rs StartWorkspaces = StartWorkspacesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkspacesService.StartWorkspaces")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartWorkspacesResponse'
            Core.<$> (x Core..:? "FailedRequests")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartWorkspacesResponse' smart constructor.
data StartWorkspacesResponse = StartWorkspacesResponse'
  { -- | Information about the WorkSpaces that could not be started.
    failedRequests :: Core.Maybe [Types.FailedWorkspaceChangeRequest],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartWorkspacesResponse' value with any optional fields omitted.
mkStartWorkspacesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartWorkspacesResponse
mkStartWorkspacesResponse responseStatus =
  StartWorkspacesResponse'
    { failedRequests = Core.Nothing,
      responseStatus
    }

-- | Information about the WorkSpaces that could not be started.
--
-- /Note:/ Consider using 'failedRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsFailedRequests :: Lens.Lens' StartWorkspacesResponse (Core.Maybe [Types.FailedWorkspaceChangeRequest])
srsFailedRequests = Lens.field @"failedRequests"
{-# DEPRECATED srsFailedRequests "Use generic-lens or generic-optics with 'failedRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartWorkspacesResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

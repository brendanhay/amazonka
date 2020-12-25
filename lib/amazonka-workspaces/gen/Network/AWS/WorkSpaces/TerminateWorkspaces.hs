{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.TerminateWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified WorkSpaces.
--
-- /Important:/ Terminating a WorkSpace is a permanent action and cannot be undone. The user's data is destroyed. If you need to archive any user data, contact AWS Support before terminating the WorkSpace.
-- You can terminate a WorkSpace that is in any state except @SUSPENDED@ .
-- This operation is asynchronous and returns before the WorkSpaces have been completely terminated. After a WorkSpace is terminated, the @TERMINATED@ state is returned only briefly before the WorkSpace directory metadata is cleaned up, so this state is rarely returned. To confirm that a WorkSpace is terminated, check for the WorkSpace ID by using <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaces.html DescribeWorkSpaces> . If the WorkSpace ID isn't returned, then the WorkSpace has been successfully terminated.
module Network.AWS.WorkSpaces.TerminateWorkspaces
  ( -- * Creating a request
    TerminateWorkspaces (..),
    mkTerminateWorkspaces,

    -- ** Request lenses
    twTerminateWorkspaceRequests,

    -- * Destructuring the response
    TerminateWorkspacesResponse (..),
    mkTerminateWorkspacesResponse,

    -- ** Response lenses
    twrrsFailedRequests,
    twrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkTerminateWorkspaces' smart constructor.
newtype TerminateWorkspaces = TerminateWorkspaces'
  { -- | The WorkSpaces to terminate. You can specify up to 25 WorkSpaces.
    terminateWorkspaceRequests :: Core.NonEmpty Types.TerminateRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateWorkspaces' value with any optional fields omitted.
mkTerminateWorkspaces ::
  -- | 'terminateWorkspaceRequests'
  Core.NonEmpty Types.TerminateRequest ->
  TerminateWorkspaces
mkTerminateWorkspaces terminateWorkspaceRequests =
  TerminateWorkspaces' {terminateWorkspaceRequests}

-- | The WorkSpaces to terminate. You can specify up to 25 WorkSpaces.
--
-- /Note:/ Consider using 'terminateWorkspaceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twTerminateWorkspaceRequests :: Lens.Lens' TerminateWorkspaces (Core.NonEmpty Types.TerminateRequest)
twTerminateWorkspaceRequests = Lens.field @"terminateWorkspaceRequests"
{-# DEPRECATED twTerminateWorkspaceRequests "Use generic-lens or generic-optics with 'terminateWorkspaceRequests' instead." #-}

instance Core.FromJSON TerminateWorkspaces where
  toJSON TerminateWorkspaces {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("TerminateWorkspaceRequests" Core..= terminateWorkspaceRequests)
          ]
      )

instance Core.AWSRequest TerminateWorkspaces where
  type Rs TerminateWorkspaces = TerminateWorkspacesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkspacesService.TerminateWorkspaces")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          TerminateWorkspacesResponse'
            Core.<$> (x Core..:? "FailedRequests")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkTerminateWorkspacesResponse' smart constructor.
data TerminateWorkspacesResponse = TerminateWorkspacesResponse'
  { -- | Information about the WorkSpaces that could not be terminated.
    failedRequests :: Core.Maybe [Types.FailedWorkspaceChangeRequest],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateWorkspacesResponse' value with any optional fields omitted.
mkTerminateWorkspacesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  TerminateWorkspacesResponse
mkTerminateWorkspacesResponse responseStatus =
  TerminateWorkspacesResponse'
    { failedRequests = Core.Nothing,
      responseStatus
    }

-- | Information about the WorkSpaces that could not be terminated.
--
-- /Note:/ Consider using 'failedRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twrrsFailedRequests :: Lens.Lens' TerminateWorkspacesResponse (Core.Maybe [Types.FailedWorkspaceChangeRequest])
twrrsFailedRequests = Lens.field @"failedRequests"
{-# DEPRECATED twrrsFailedRequests "Use generic-lens or generic-optics with 'failedRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twrrsResponseStatus :: Lens.Lens' TerminateWorkspacesResponse Core.Int
twrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED twrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

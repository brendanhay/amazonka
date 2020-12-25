{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.CreateWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more WorkSpaces.
--
-- This operation is asynchronous and returns before the WorkSpaces are created.
module Network.AWS.WorkSpaces.CreateWorkspaces
  ( -- * Creating a request
    CreateWorkspaces (..),
    mkCreateWorkspaces,

    -- ** Request lenses
    cwWorkspaces,

    -- * Destructuring the response
    CreateWorkspacesResponse (..),
    mkCreateWorkspacesResponse,

    -- ** Response lenses
    cwrrsFailedRequests,
    cwrrsPendingRequests,
    cwrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkCreateWorkspaces' smart constructor.
newtype CreateWorkspaces = CreateWorkspaces'
  { -- | The WorkSpaces to create. You can specify up to 25 WorkSpaces.
    workspaces :: Core.NonEmpty Types.WorkspaceRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWorkspaces' value with any optional fields omitted.
mkCreateWorkspaces ::
  -- | 'workspaces'
  Core.NonEmpty Types.WorkspaceRequest ->
  CreateWorkspaces
mkCreateWorkspaces workspaces = CreateWorkspaces' {workspaces}

-- | The WorkSpaces to create. You can specify up to 25 WorkSpaces.
--
-- /Note:/ Consider using 'workspaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwWorkspaces :: Lens.Lens' CreateWorkspaces (Core.NonEmpty Types.WorkspaceRequest)
cwWorkspaces = Lens.field @"workspaces"
{-# DEPRECATED cwWorkspaces "Use generic-lens or generic-optics with 'workspaces' instead." #-}

instance Core.FromJSON CreateWorkspaces where
  toJSON CreateWorkspaces {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Workspaces" Core..= workspaces)])

instance Core.AWSRequest CreateWorkspaces where
  type Rs CreateWorkspaces = CreateWorkspacesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkspacesService.CreateWorkspaces")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkspacesResponse'
            Core.<$> (x Core..:? "FailedRequests")
            Core.<*> (x Core..:? "PendingRequests")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateWorkspacesResponse' smart constructor.
data CreateWorkspacesResponse = CreateWorkspacesResponse'
  { -- | Information about the WorkSpaces that could not be created.
    failedRequests :: Core.Maybe [Types.FailedCreateWorkspaceRequest],
    -- | Information about the WorkSpaces that were created.
    --
    -- Because this operation is asynchronous, the identifier returned is not immediately available for use with other operations. For example, if you call 'DescribeWorkspaces' before the WorkSpace is created, the information returned can be incomplete.
    pendingRequests :: Core.Maybe [Types.Workspace],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWorkspacesResponse' value with any optional fields omitted.
mkCreateWorkspacesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateWorkspacesResponse
mkCreateWorkspacesResponse responseStatus =
  CreateWorkspacesResponse'
    { failedRequests = Core.Nothing,
      pendingRequests = Core.Nothing,
      responseStatus
    }

-- | Information about the WorkSpaces that could not be created.
--
-- /Note:/ Consider using 'failedRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrrsFailedRequests :: Lens.Lens' CreateWorkspacesResponse (Core.Maybe [Types.FailedCreateWorkspaceRequest])
cwrrsFailedRequests = Lens.field @"failedRequests"
{-# DEPRECATED cwrrsFailedRequests "Use generic-lens or generic-optics with 'failedRequests' instead." #-}

-- | Information about the WorkSpaces that were created.
--
-- Because this operation is asynchronous, the identifier returned is not immediately available for use with other operations. For example, if you call 'DescribeWorkspaces' before the WorkSpace is created, the information returned can be incomplete.
--
-- /Note:/ Consider using 'pendingRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrrsPendingRequests :: Lens.Lens' CreateWorkspacesResponse (Core.Maybe [Types.Workspace])
cwrrsPendingRequests = Lens.field @"pendingRequests"
{-# DEPRECATED cwrrsPendingRequests "Use generic-lens or generic-optics with 'pendingRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwrrsResponseStatus :: Lens.Lens' CreateWorkspacesResponse Core.Int
cwrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cwrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

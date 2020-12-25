{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.RestoreWorkspace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the specified WorkSpace to its last known healthy state.
--
-- You cannot restore a WorkSpace unless its state is @AVAILABLE@ , @ERROR@ , @UNHEALTHY@ , or @STOPPED@ .
-- Restoring a WorkSpace is a potentially destructive action that can result in the loss of data. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/restore-workspace.html Restore a WorkSpace> .
-- This operation is asynchronous and returns before the WorkSpace is completely restored.
module Network.AWS.WorkSpaces.RestoreWorkspace
  ( -- * Creating a request
    RestoreWorkspace (..),
    mkRestoreWorkspace,

    -- ** Request lenses
    rwWorkspaceId,

    -- * Destructuring the response
    RestoreWorkspaceResponse (..),
    mkRestoreWorkspaceResponse,

    -- ** Response lenses
    rrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkRestoreWorkspace' smart constructor.
newtype RestoreWorkspace = RestoreWorkspace'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Types.WorkspaceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreWorkspace' value with any optional fields omitted.
mkRestoreWorkspace ::
  -- | 'workspaceId'
  Types.WorkspaceId ->
  RestoreWorkspace
mkRestoreWorkspace workspaceId = RestoreWorkspace' {workspaceId}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwWorkspaceId :: Lens.Lens' RestoreWorkspace Types.WorkspaceId
rwWorkspaceId = Lens.field @"workspaceId"
{-# DEPRECATED rwWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

instance Core.FromJSON RestoreWorkspace where
  toJSON RestoreWorkspace {..} =
    Core.object
      (Core.catMaybes [Core.Just ("WorkspaceId" Core..= workspaceId)])

instance Core.AWSRequest RestoreWorkspace where
  type Rs RestoreWorkspace = RestoreWorkspaceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkspacesService.RestoreWorkspace")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RestoreWorkspaceResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRestoreWorkspaceResponse' smart constructor.
newtype RestoreWorkspaceResponse = RestoreWorkspaceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreWorkspaceResponse' value with any optional fields omitted.
mkRestoreWorkspaceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreWorkspaceResponse
mkRestoreWorkspaceResponse responseStatus =
  RestoreWorkspaceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RestoreWorkspaceResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

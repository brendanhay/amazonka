{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the state of the specified WorkSpace.
--
-- To maintain a WorkSpace without being interrupted, set the WorkSpace state to @ADMIN_MAINTENANCE@ . WorkSpaces in this state do not respond to requests to reboot, stop, start, rebuild, or restore. An AutoStop WorkSpace in this state is not stopped. Users cannot log into a WorkSpace in the @ADMIN_MAINTENANCE@ state.
module Network.AWS.WorkSpaces.ModifyWorkspaceState
  ( -- * Creating a request
    ModifyWorkspaceState (..),
    mkModifyWorkspaceState,

    -- ** Request lenses
    mwsWorkspaceId,
    mwsWorkspaceState,

    -- * Destructuring the response
    ModifyWorkspaceStateResponse (..),
    mkModifyWorkspaceStateResponse,

    -- ** Response lenses
    mwsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkModifyWorkspaceState' smart constructor.
data ModifyWorkspaceState = ModifyWorkspaceState'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Types.WorkspaceId,
    -- | The WorkSpace state.
    workspaceState :: Types.TargetWorkspaceState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyWorkspaceState' value with any optional fields omitted.
mkModifyWorkspaceState ::
  -- | 'workspaceId'
  Types.WorkspaceId ->
  -- | 'workspaceState'
  Types.TargetWorkspaceState ->
  ModifyWorkspaceState
mkModifyWorkspaceState workspaceId workspaceState =
  ModifyWorkspaceState' {workspaceId, workspaceState}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwsWorkspaceId :: Lens.Lens' ModifyWorkspaceState Types.WorkspaceId
mwsWorkspaceId = Lens.field @"workspaceId"
{-# DEPRECATED mwsWorkspaceId "Use generic-lens or generic-optics with 'workspaceId' instead." #-}

-- | The WorkSpace state.
--
-- /Note:/ Consider using 'workspaceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwsWorkspaceState :: Lens.Lens' ModifyWorkspaceState Types.TargetWorkspaceState
mwsWorkspaceState = Lens.field @"workspaceState"
{-# DEPRECATED mwsWorkspaceState "Use generic-lens or generic-optics with 'workspaceState' instead." #-}

instance Core.FromJSON ModifyWorkspaceState where
  toJSON ModifyWorkspaceState {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WorkspaceId" Core..= workspaceId),
            Core.Just ("WorkspaceState" Core..= workspaceState)
          ]
      )

instance Core.AWSRequest ModifyWorkspaceState where
  type Rs ModifyWorkspaceState = ModifyWorkspaceStateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkspacesService.ModifyWorkspaceState")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyWorkspaceStateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyWorkspaceStateResponse' smart constructor.
newtype ModifyWorkspaceStateResponse = ModifyWorkspaceStateResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyWorkspaceStateResponse' value with any optional fields omitted.
mkModifyWorkspaceStateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyWorkspaceStateResponse
mkModifyWorkspaceStateResponse responseStatus =
  ModifyWorkspaceStateResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwsrrsResponseStatus :: Lens.Lens' ModifyWorkspaceStateResponse Core.Int
mwsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mwsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

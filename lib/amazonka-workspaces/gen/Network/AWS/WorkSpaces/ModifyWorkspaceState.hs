{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyWorkspaceState (..)
    , mkModifyWorkspaceState
    -- ** Request lenses
    , mwsWorkspaceId
    , mwsWorkspaceState

    -- * Destructuring the response
    , ModifyWorkspaceStateResponse (..)
    , mkModifyWorkspaceStateResponse
    -- ** Response lenses
    , mwsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkModifyWorkspaceState' smart constructor.
data ModifyWorkspaceState = ModifyWorkspaceState'
  { workspaceId :: Types.WorkspaceId
    -- ^ The identifier of the WorkSpace.
  , workspaceState :: Types.TargetWorkspaceState
    -- ^ The WorkSpace state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyWorkspaceState' value with any optional fields omitted.
mkModifyWorkspaceState
    :: Types.WorkspaceId -- ^ 'workspaceId'
    -> Types.TargetWorkspaceState -- ^ 'workspaceState'
    -> ModifyWorkspaceState
mkModifyWorkspaceState workspaceId workspaceState
  = ModifyWorkspaceState'{workspaceId, workspaceState}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwsWorkspaceId :: Lens.Lens' ModifyWorkspaceState Types.WorkspaceId
mwsWorkspaceId = Lens.field @"workspaceId"
{-# INLINEABLE mwsWorkspaceId #-}
{-# DEPRECATED workspaceId "Use generic-lens or generic-optics with 'workspaceId' instead"  #-}

-- | The WorkSpace state.
--
-- /Note:/ Consider using 'workspaceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwsWorkspaceState :: Lens.Lens' ModifyWorkspaceState Types.TargetWorkspaceState
mwsWorkspaceState = Lens.field @"workspaceState"
{-# INLINEABLE mwsWorkspaceState #-}
{-# DEPRECATED workspaceState "Use generic-lens or generic-optics with 'workspaceState' instead"  #-}

instance Core.ToQuery ModifyWorkspaceState where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyWorkspaceState where
        toHeaders ModifyWorkspaceState{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.ModifyWorkspaceState")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyWorkspaceState where
        toJSON ModifyWorkspaceState{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WorkspaceId" Core..= workspaceId),
                  Core.Just ("WorkspaceState" Core..= workspaceState)])

instance Core.AWSRequest ModifyWorkspaceState where
        type Rs ModifyWorkspaceState = ModifyWorkspaceStateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ModifyWorkspaceStateResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyWorkspaceStateResponse' smart constructor.
newtype ModifyWorkspaceStateResponse = ModifyWorkspaceStateResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyWorkspaceStateResponse' value with any optional fields omitted.
mkModifyWorkspaceStateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyWorkspaceStateResponse
mkModifyWorkspaceStateResponse responseStatus
  = ModifyWorkspaceStateResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwsrrsResponseStatus :: Lens.Lens' ModifyWorkspaceStateResponse Core.Int
mwsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mwsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

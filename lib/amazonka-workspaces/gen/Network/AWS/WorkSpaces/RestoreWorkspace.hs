{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RestoreWorkspace (..)
    , mkRestoreWorkspace
    -- ** Request lenses
    , rwWorkspaceId

    -- * Destructuring the response
    , RestoreWorkspaceResponse (..)
    , mkRestoreWorkspaceResponse
    -- ** Response lenses
    , rrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkRestoreWorkspace' smart constructor.
newtype RestoreWorkspace = RestoreWorkspace'
  { workspaceId :: Types.WorkspaceId
    -- ^ The identifier of the WorkSpace.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreWorkspace' value with any optional fields omitted.
mkRestoreWorkspace
    :: Types.WorkspaceId -- ^ 'workspaceId'
    -> RestoreWorkspace
mkRestoreWorkspace workspaceId = RestoreWorkspace'{workspaceId}

-- | The identifier of the WorkSpace.
--
-- /Note:/ Consider using 'workspaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwWorkspaceId :: Lens.Lens' RestoreWorkspace Types.WorkspaceId
rwWorkspaceId = Lens.field @"workspaceId"
{-# INLINEABLE rwWorkspaceId #-}
{-# DEPRECATED workspaceId "Use generic-lens or generic-optics with 'workspaceId' instead"  #-}

instance Core.ToQuery RestoreWorkspace where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RestoreWorkspace where
        toHeaders RestoreWorkspace{..}
          = Core.pure ("X-Amz-Target", "WorkspacesService.RestoreWorkspace")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RestoreWorkspace where
        toJSON RestoreWorkspace{..}
          = Core.object
              (Core.catMaybes [Core.Just ("WorkspaceId" Core..= workspaceId)])

instance Core.AWSRequest RestoreWorkspace where
        type Rs RestoreWorkspace = RestoreWorkspaceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 RestoreWorkspaceResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRestoreWorkspaceResponse' smart constructor.
newtype RestoreWorkspaceResponse = RestoreWorkspaceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreWorkspaceResponse' value with any optional fields omitted.
mkRestoreWorkspaceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RestoreWorkspaceResponse
mkRestoreWorkspaceResponse responseStatus
  = RestoreWorkspaceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RestoreWorkspaceResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.StopWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified WorkSpaces.
--
-- You cannot stop a WorkSpace unless it has a running mode of @AutoStop@ and a state of @AVAILABLE@ , @IMPAIRED@ , @UNHEALTHY@ , or @ERROR@ .
module Network.AWS.WorkSpaces.StopWorkspaces
    (
    -- * Creating a request
      StopWorkspaces (..)
    , mkStopWorkspaces
    -- ** Request lenses
    , swStopWorkspaceRequests

    -- * Destructuring the response
    , StopWorkspacesResponse (..)
    , mkStopWorkspacesResponse
    -- ** Response lenses
    , swrrsFailedRequests
    , swrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkStopWorkspaces' smart constructor.
newtype StopWorkspaces = StopWorkspaces'
  { stopWorkspaceRequests :: Core.NonEmpty Types.StopRequest
    -- ^ The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopWorkspaces' value with any optional fields omitted.
mkStopWorkspaces
    :: Core.NonEmpty Types.StopRequest -- ^ 'stopWorkspaceRequests'
    -> StopWorkspaces
mkStopWorkspaces stopWorkspaceRequests
  = StopWorkspaces'{stopWorkspaceRequests}

-- | The WorkSpaces to stop. You can specify up to 25 WorkSpaces.
--
-- /Note:/ Consider using 'stopWorkspaceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swStopWorkspaceRequests :: Lens.Lens' StopWorkspaces (Core.NonEmpty Types.StopRequest)
swStopWorkspaceRequests = Lens.field @"stopWorkspaceRequests"
{-# INLINEABLE swStopWorkspaceRequests #-}
{-# DEPRECATED stopWorkspaceRequests "Use generic-lens or generic-optics with 'stopWorkspaceRequests' instead"  #-}

instance Core.ToQuery StopWorkspaces where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopWorkspaces where
        toHeaders StopWorkspaces{..}
          = Core.pure ("X-Amz-Target", "WorkspacesService.StopWorkspaces")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopWorkspaces where
        toJSON StopWorkspaces{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("StopWorkspaceRequests" Core..= stopWorkspaceRequests)])

instance Core.AWSRequest StopWorkspaces where
        type Rs StopWorkspaces = StopWorkspacesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StopWorkspacesResponse' Core.<$>
                   (x Core..:? "FailedRequests") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopWorkspacesResponse' smart constructor.
data StopWorkspacesResponse = StopWorkspacesResponse'
  { failedRequests :: Core.Maybe [Types.FailedWorkspaceChangeRequest]
    -- ^ Information about the WorkSpaces that could not be stopped.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopWorkspacesResponse' value with any optional fields omitted.
mkStopWorkspacesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopWorkspacesResponse
mkStopWorkspacesResponse responseStatus
  = StopWorkspacesResponse'{failedRequests = Core.Nothing,
                            responseStatus}

-- | Information about the WorkSpaces that could not be stopped.
--
-- /Note:/ Consider using 'failedRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrrsFailedRequests :: Lens.Lens' StopWorkspacesResponse (Core.Maybe [Types.FailedWorkspaceChangeRequest])
swrrsFailedRequests = Lens.field @"failedRequests"
{-# INLINEABLE swrrsFailedRequests #-}
{-# DEPRECATED failedRequests "Use generic-lens or generic-optics with 'failedRequests' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swrrsResponseStatus :: Lens.Lens' StopWorkspacesResponse Core.Int
swrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE swrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

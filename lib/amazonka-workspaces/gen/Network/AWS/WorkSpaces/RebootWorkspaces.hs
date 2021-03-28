{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.RebootWorkspaces
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reboots the specified WorkSpaces.
--
-- You cannot reboot a WorkSpace unless its state is @AVAILABLE@ or @UNHEALTHY@ .
-- This operation is asynchronous and returns before the WorkSpaces have rebooted.
module Network.AWS.WorkSpaces.RebootWorkspaces
    (
    -- * Creating a request
      RebootWorkspaces (..)
    , mkRebootWorkspaces
    -- ** Request lenses
    , rwRebootWorkspaceRequests

    -- * Destructuring the response
    , RebootWorkspacesResponse (..)
    , mkRebootWorkspacesResponse
    -- ** Response lenses
    , rwrfrsFailedRequests
    , rwrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkRebootWorkspaces' smart constructor.
newtype RebootWorkspaces = RebootWorkspaces'
  { rebootWorkspaceRequests :: Core.NonEmpty Types.RebootRequest
    -- ^ The WorkSpaces to reboot. You can specify up to 25 WorkSpaces.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RebootWorkspaces' value with any optional fields omitted.
mkRebootWorkspaces
    :: Core.NonEmpty Types.RebootRequest -- ^ 'rebootWorkspaceRequests'
    -> RebootWorkspaces
mkRebootWorkspaces rebootWorkspaceRequests
  = RebootWorkspaces'{rebootWorkspaceRequests}

-- | The WorkSpaces to reboot. You can specify up to 25 WorkSpaces.
--
-- /Note:/ Consider using 'rebootWorkspaceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwRebootWorkspaceRequests :: Lens.Lens' RebootWorkspaces (Core.NonEmpty Types.RebootRequest)
rwRebootWorkspaceRequests = Lens.field @"rebootWorkspaceRequests"
{-# INLINEABLE rwRebootWorkspaceRequests #-}
{-# DEPRECATED rebootWorkspaceRequests "Use generic-lens or generic-optics with 'rebootWorkspaceRequests' instead"  #-}

instance Core.ToQuery RebootWorkspaces where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RebootWorkspaces where
        toHeaders RebootWorkspaces{..}
          = Core.pure ("X-Amz-Target", "WorkspacesService.RebootWorkspaces")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RebootWorkspaces where
        toJSON RebootWorkspaces{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("RebootWorkspaceRequests" Core..= rebootWorkspaceRequests)])

instance Core.AWSRequest RebootWorkspaces where
        type Rs RebootWorkspaces = RebootWorkspacesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RebootWorkspacesResponse' Core.<$>
                   (x Core..:? "FailedRequests") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRebootWorkspacesResponse' smart constructor.
data RebootWorkspacesResponse = RebootWorkspacesResponse'
  { failedRequests :: Core.Maybe [Types.FailedWorkspaceChangeRequest]
    -- ^ Information about the WorkSpaces that could not be rebooted.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RebootWorkspacesResponse' value with any optional fields omitted.
mkRebootWorkspacesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RebootWorkspacesResponse
mkRebootWorkspacesResponse responseStatus
  = RebootWorkspacesResponse'{failedRequests = Core.Nothing,
                              responseStatus}

-- | Information about the WorkSpaces that could not be rebooted.
--
-- /Note:/ Consider using 'failedRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrfrsFailedRequests :: Lens.Lens' RebootWorkspacesResponse (Core.Maybe [Types.FailedWorkspaceChangeRequest])
rwrfrsFailedRequests = Lens.field @"failedRequests"
{-# INLINEABLE rwrfrsFailedRequests #-}
{-# DEPRECATED failedRequests "Use generic-lens or generic-optics with 'failedRequests' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrfrsResponseStatus :: Lens.Lens' RebootWorkspacesResponse Core.Int
rwrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rwrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

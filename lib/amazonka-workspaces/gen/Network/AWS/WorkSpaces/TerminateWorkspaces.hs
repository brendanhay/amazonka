{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      TerminateWorkspaces (..)
    , mkTerminateWorkspaces
    -- ** Request lenses
    , twTerminateWorkspaceRequests

    -- * Destructuring the response
    , TerminateWorkspacesResponse (..)
    , mkTerminateWorkspacesResponse
    -- ** Response lenses
    , twrrsFailedRequests
    , twrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkTerminateWorkspaces' smart constructor.
newtype TerminateWorkspaces = TerminateWorkspaces'
  { terminateWorkspaceRequests :: Core.NonEmpty Types.TerminateRequest
    -- ^ The WorkSpaces to terminate. You can specify up to 25 WorkSpaces.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateWorkspaces' value with any optional fields omitted.
mkTerminateWorkspaces
    :: Core.NonEmpty Types.TerminateRequest -- ^ 'terminateWorkspaceRequests'
    -> TerminateWorkspaces
mkTerminateWorkspaces terminateWorkspaceRequests
  = TerminateWorkspaces'{terminateWorkspaceRequests}

-- | The WorkSpaces to terminate. You can specify up to 25 WorkSpaces.
--
-- /Note:/ Consider using 'terminateWorkspaceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twTerminateWorkspaceRequests :: Lens.Lens' TerminateWorkspaces (Core.NonEmpty Types.TerminateRequest)
twTerminateWorkspaceRequests = Lens.field @"terminateWorkspaceRequests"
{-# INLINEABLE twTerminateWorkspaceRequests #-}
{-# DEPRECATED terminateWorkspaceRequests "Use generic-lens or generic-optics with 'terminateWorkspaceRequests' instead"  #-}

instance Core.ToQuery TerminateWorkspaces where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders TerminateWorkspaces where
        toHeaders TerminateWorkspaces{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.TerminateWorkspaces")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON TerminateWorkspaces where
        toJSON TerminateWorkspaces{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("TerminateWorkspaceRequests" Core..= terminateWorkspaceRequests)])

instance Core.AWSRequest TerminateWorkspaces where
        type Rs TerminateWorkspaces = TerminateWorkspacesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 TerminateWorkspacesResponse' Core.<$>
                   (x Core..:? "FailedRequests") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkTerminateWorkspacesResponse' smart constructor.
data TerminateWorkspacesResponse = TerminateWorkspacesResponse'
  { failedRequests :: Core.Maybe [Types.FailedWorkspaceChangeRequest]
    -- ^ Information about the WorkSpaces that could not be terminated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TerminateWorkspacesResponse' value with any optional fields omitted.
mkTerminateWorkspacesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> TerminateWorkspacesResponse
mkTerminateWorkspacesResponse responseStatus
  = TerminateWorkspacesResponse'{failedRequests = Core.Nothing,
                                 responseStatus}

-- | Information about the WorkSpaces that could not be terminated.
--
-- /Note:/ Consider using 'failedRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twrrsFailedRequests :: Lens.Lens' TerminateWorkspacesResponse (Core.Maybe [Types.FailedWorkspaceChangeRequest])
twrrsFailedRequests = Lens.field @"failedRequests"
{-# INLINEABLE twrrsFailedRequests #-}
{-# DEPRECATED failedRequests "Use generic-lens or generic-optics with 'failedRequests' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twrrsResponseStatus :: Lens.Lens' TerminateWorkspacesResponse Core.Int
twrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE twrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of a pull request. 
module Network.AWS.CodeCommit.UpdatePullRequestStatus
    (
    -- * Creating a request
      UpdatePullRequestStatus (..)
    , mkUpdatePullRequestStatus
    -- ** Request lenses
    , uprsPullRequestId
    , uprsPullRequestStatus

    -- * Destructuring the response
    , UpdatePullRequestStatusResponse (..)
    , mkUpdatePullRequestStatusResponse
    -- ** Response lenses
    , uprsrrsPullRequest
    , uprsrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdatePullRequestStatus' smart constructor.
data UpdatePullRequestStatus = UpdatePullRequestStatus'
  { pullRequestId :: Types.PullRequestId
    -- ^ The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
  , pullRequestStatus :: Types.PullRequestStatusEnum
    -- ^ The status of the pull request. The only valid operations are to update the status from @OPEN@ to @OPEN@ , @OPEN@ to @CLOSED@ or from @CLOSED@ to @CLOSED@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePullRequestStatus' value with any optional fields omitted.
mkUpdatePullRequestStatus
    :: Types.PullRequestId -- ^ 'pullRequestId'
    -> Types.PullRequestStatusEnum -- ^ 'pullRequestStatus'
    -> UpdatePullRequestStatus
mkUpdatePullRequestStatus pullRequestId pullRequestStatus
  = UpdatePullRequestStatus'{pullRequestId, pullRequestStatus}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsPullRequestId :: Lens.Lens' UpdatePullRequestStatus Types.PullRequestId
uprsPullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE uprsPullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | The status of the pull request. The only valid operations are to update the status from @OPEN@ to @OPEN@ , @OPEN@ to @CLOSED@ or from @CLOSED@ to @CLOSED@ .
--
-- /Note:/ Consider using 'pullRequestStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsPullRequestStatus :: Lens.Lens' UpdatePullRequestStatus Types.PullRequestStatusEnum
uprsPullRequestStatus = Lens.field @"pullRequestStatus"
{-# INLINEABLE uprsPullRequestStatus #-}
{-# DEPRECATED pullRequestStatus "Use generic-lens or generic-optics with 'pullRequestStatus' instead"  #-}

instance Core.ToQuery UpdatePullRequestStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdatePullRequestStatus where
        toHeaders UpdatePullRequestStatus{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.UpdatePullRequestStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdatePullRequestStatus where
        toJSON UpdatePullRequestStatus{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pullRequestId" Core..= pullRequestId),
                  Core.Just ("pullRequestStatus" Core..= pullRequestStatus)])

instance Core.AWSRequest UpdatePullRequestStatus where
        type Rs UpdatePullRequestStatus = UpdatePullRequestStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdatePullRequestStatusResponse' Core.<$>
                   (x Core..: "pullRequest") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdatePullRequestStatusResponse' smart constructor.
data UpdatePullRequestStatusResponse = UpdatePullRequestStatusResponse'
  { pullRequest :: Types.PullRequest
    -- ^ Information about the pull request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdatePullRequestStatusResponse' value with any optional fields omitted.
mkUpdatePullRequestStatusResponse
    :: Types.PullRequest -- ^ 'pullRequest'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdatePullRequestStatusResponse
mkUpdatePullRequestStatusResponse pullRequest responseStatus
  = UpdatePullRequestStatusResponse'{pullRequest, responseStatus}

-- | Information about the pull request.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsrrsPullRequest :: Lens.Lens' UpdatePullRequestStatusResponse Types.PullRequest
uprsrrsPullRequest = Lens.field @"pullRequest"
{-# INLINEABLE uprsrrsPullRequest #-}
{-# DEPRECATED pullRequest "Use generic-lens or generic-optics with 'pullRequest' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsrrsResponseStatus :: Lens.Lens' UpdatePullRequestStatusResponse Core.Int
uprsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uprsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

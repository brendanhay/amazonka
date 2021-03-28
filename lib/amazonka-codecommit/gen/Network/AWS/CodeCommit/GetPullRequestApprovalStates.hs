{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetPullRequestApprovalStates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the approval states for a specified pull request. Approval states only apply to pull requests that have one or more approval rules applied to them.
module Network.AWS.CodeCommit.GetPullRequestApprovalStates
    (
    -- * Creating a request
      GetPullRequestApprovalStates (..)
    , mkGetPullRequestApprovalStates
    -- ** Request lenses
    , gprasPullRequestId
    , gprasRevisionId

    -- * Destructuring the response
    , GetPullRequestApprovalStatesResponse (..)
    , mkGetPullRequestApprovalStatesResponse
    -- ** Response lenses
    , gprasrrsApprovals
    , gprasrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetPullRequestApprovalStates' smart constructor.
data GetPullRequestApprovalStates = GetPullRequestApprovalStates'
  { pullRequestId :: Types.PullRequestId
    -- ^ The system-generated ID for the pull request.
  , revisionId :: Types.RevisionId
    -- ^ The system-generated ID for the pull request revision.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPullRequestApprovalStates' value with any optional fields omitted.
mkGetPullRequestApprovalStates
    :: Types.PullRequestId -- ^ 'pullRequestId'
    -> Types.RevisionId -- ^ 'revisionId'
    -> GetPullRequestApprovalStates
mkGetPullRequestApprovalStates pullRequestId revisionId
  = GetPullRequestApprovalStates'{pullRequestId, revisionId}

-- | The system-generated ID for the pull request.
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprasPullRequestId :: Lens.Lens' GetPullRequestApprovalStates Types.PullRequestId
gprasPullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE gprasPullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | The system-generated ID for the pull request revision.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprasRevisionId :: Lens.Lens' GetPullRequestApprovalStates Types.RevisionId
gprasRevisionId = Lens.field @"revisionId"
{-# INLINEABLE gprasRevisionId #-}
{-# DEPRECATED revisionId "Use generic-lens or generic-optics with 'revisionId' instead"  #-}

instance Core.ToQuery GetPullRequestApprovalStates where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPullRequestApprovalStates where
        toHeaders GetPullRequestApprovalStates{..}
          = Core.pure
              ("X-Amz-Target",
               "CodeCommit_20150413.GetPullRequestApprovalStates")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetPullRequestApprovalStates where
        toJSON GetPullRequestApprovalStates{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pullRequestId" Core..= pullRequestId),
                  Core.Just ("revisionId" Core..= revisionId)])

instance Core.AWSRequest GetPullRequestApprovalStates where
        type Rs GetPullRequestApprovalStates =
             GetPullRequestApprovalStatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPullRequestApprovalStatesResponse' Core.<$>
                   (x Core..:? "approvals") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetPullRequestApprovalStatesResponse' smart constructor.
data GetPullRequestApprovalStatesResponse = GetPullRequestApprovalStatesResponse'
  { approvals :: Core.Maybe [Types.Approval]
    -- ^ Information about users who have approved the pull request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPullRequestApprovalStatesResponse' value with any optional fields omitted.
mkGetPullRequestApprovalStatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPullRequestApprovalStatesResponse
mkGetPullRequestApprovalStatesResponse responseStatus
  = GetPullRequestApprovalStatesResponse'{approvals = Core.Nothing,
                                          responseStatus}

-- | Information about users who have approved the pull request.
--
-- /Note:/ Consider using 'approvals' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprasrrsApprovals :: Lens.Lens' GetPullRequestApprovalStatesResponse (Core.Maybe [Types.Approval])
gprasrrsApprovals = Lens.field @"approvals"
{-# INLINEABLE gprasrrsApprovals #-}
{-# DEPRECATED approvals "Use generic-lens or generic-optics with 'approvals' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gprasrrsResponseStatus :: Lens.Lens' GetPullRequestApprovalStatesResponse Core.Int
gprasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gprasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

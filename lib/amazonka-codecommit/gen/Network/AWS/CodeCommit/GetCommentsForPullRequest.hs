{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetCommentsForPullRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns comments made on a pull request.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.GetCommentsForPullRequest
    (
    -- * Creating a request
      GetCommentsForPullRequest (..)
    , mkGetCommentsForPullRequest
    -- ** Request lenses
    , gcfprPullRequestId
    , gcfprAfterCommitId
    , gcfprBeforeCommitId
    , gcfprMaxResults
    , gcfprNextToken
    , gcfprRepositoryName

    -- * Destructuring the response
    , GetCommentsForPullRequestResponse (..)
    , mkGetCommentsForPullRequestResponse
    -- ** Response lenses
    , gcfprrrsCommentsForPullRequestData
    , gcfprrrsNextToken
    , gcfprrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCommentsForPullRequest' smart constructor.
data GetCommentsForPullRequest = GetCommentsForPullRequest'
  { pullRequestId :: Types.PullRequestId
    -- ^ The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
  , afterCommitId :: Core.Maybe Types.CommitId
    -- ^ The full commit ID of the commit in the source branch that was the tip of the branch at the time the comment was made.
  , beforeCommitId :: Core.Maybe Types.CommitId
    -- ^ The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was created.
  , maxResults :: Core.Maybe Core.Int
    -- ^ A non-zero, non-negative integer used to limit the number of returned results. The default is 100 comments. You can return up to 500 comments with a single request.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that, when provided in a request, returns the next batch of the results.
  , repositoryName :: Core.Maybe Types.RepositoryName
    -- ^ The name of the repository that contains the pull request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCommentsForPullRequest' value with any optional fields omitted.
mkGetCommentsForPullRequest
    :: Types.PullRequestId -- ^ 'pullRequestId'
    -> GetCommentsForPullRequest
mkGetCommentsForPullRequest pullRequestId
  = GetCommentsForPullRequest'{pullRequestId,
                               afterCommitId = Core.Nothing, beforeCommitId = Core.Nothing,
                               maxResults = Core.Nothing, nextToken = Core.Nothing,
                               repositoryName = Core.Nothing}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprPullRequestId :: Lens.Lens' GetCommentsForPullRequest Types.PullRequestId
gcfprPullRequestId = Lens.field @"pullRequestId"
{-# INLINEABLE gcfprPullRequestId #-}
{-# DEPRECATED pullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead"  #-}

-- | The full commit ID of the commit in the source branch that was the tip of the branch at the time the comment was made.
--
-- /Note:/ Consider using 'afterCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprAfterCommitId :: Lens.Lens' GetCommentsForPullRequest (Core.Maybe Types.CommitId)
gcfprAfterCommitId = Lens.field @"afterCommitId"
{-# INLINEABLE gcfprAfterCommitId #-}
{-# DEPRECATED afterCommitId "Use generic-lens or generic-optics with 'afterCommitId' instead"  #-}

-- | The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was created.
--
-- /Note:/ Consider using 'beforeCommitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprBeforeCommitId :: Lens.Lens' GetCommentsForPullRequest (Core.Maybe Types.CommitId)
gcfprBeforeCommitId = Lens.field @"beforeCommitId"
{-# INLINEABLE gcfprBeforeCommitId #-}
{-# DEPRECATED beforeCommitId "Use generic-lens or generic-optics with 'beforeCommitId' instead"  #-}

-- | A non-zero, non-negative integer used to limit the number of returned results. The default is 100 comments. You can return up to 500 comments with a single request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprMaxResults :: Lens.Lens' GetCommentsForPullRequest (Core.Maybe Core.Int)
gcfprMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gcfprMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprNextToken :: Lens.Lens' GetCommentsForPullRequest (Core.Maybe Types.NextToken)
gcfprNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcfprNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The name of the repository that contains the pull request.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprRepositoryName :: Lens.Lens' GetCommentsForPullRequest (Core.Maybe Types.RepositoryName)
gcfprRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE gcfprRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

instance Core.ToQuery GetCommentsForPullRequest where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCommentsForPullRequest where
        toHeaders GetCommentsForPullRequest{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.GetCommentsForPullRequest")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCommentsForPullRequest where
        toJSON GetCommentsForPullRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pullRequestId" Core..= pullRequestId),
                  ("afterCommitId" Core..=) Core.<$> afterCommitId,
                  ("beforeCommitId" Core..=) Core.<$> beforeCommitId,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("repositoryName" Core..=) Core.<$> repositoryName])

instance Core.AWSRequest GetCommentsForPullRequest where
        type Rs GetCommentsForPullRequest =
             GetCommentsForPullRequestResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCommentsForPullRequestResponse' Core.<$>
                   (x Core..:? "commentsForPullRequestData") Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetCommentsForPullRequest where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"commentsForPullRequestData" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetCommentsForPullRequestResponse' smart constructor.
data GetCommentsForPullRequestResponse = GetCommentsForPullRequestResponse'
  { commentsForPullRequestData :: Core.Maybe [Types.CommentsForPullRequest]
    -- ^ An array of comment objects on the pull request.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that can be used in a request to return the next batch of the results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetCommentsForPullRequestResponse' value with any optional fields omitted.
mkGetCommentsForPullRequestResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCommentsForPullRequestResponse
mkGetCommentsForPullRequestResponse responseStatus
  = GetCommentsForPullRequestResponse'{commentsForPullRequestData =
                                         Core.Nothing,
                                       nextToken = Core.Nothing, responseStatus}

-- | An array of comment objects on the pull request.
--
-- /Note:/ Consider using 'commentsForPullRequestData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprrrsCommentsForPullRequestData :: Lens.Lens' GetCommentsForPullRequestResponse (Core.Maybe [Types.CommentsForPullRequest])
gcfprrrsCommentsForPullRequestData = Lens.field @"commentsForPullRequestData"
{-# INLINEABLE gcfprrrsCommentsForPullRequestData #-}
{-# DEPRECATED commentsForPullRequestData "Use generic-lens or generic-optics with 'commentsForPullRequestData' instead"  #-}

-- | An enumeration token that can be used in a request to return the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprrrsNextToken :: Lens.Lens' GetCommentsForPullRequestResponse (Core.Maybe Types.NextToken)
gcfprrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcfprrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfprrrsResponseStatus :: Lens.Lens' GetCommentsForPullRequestResponse Core.Int
gcfprrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcfprrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

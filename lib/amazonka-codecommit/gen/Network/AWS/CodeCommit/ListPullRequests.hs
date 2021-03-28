{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.ListPullRequests
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of pull requests for a specified repository. The return list can be refined by pull request status or pull request author ARN.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.ListPullRequests
    (
    -- * Creating a request
      ListPullRequests (..)
    , mkListPullRequests
    -- ** Request lenses
    , lprRepositoryName
    , lprAuthorArn
    , lprMaxResults
    , lprNextToken
    , lprPullRequestStatus

    -- * Destructuring the response
    , ListPullRequestsResponse (..)
    , mkListPullRequestsResponse
    -- ** Response lenses
    , lprrrsPullRequestIds
    , lprrrsNextToken
    , lprrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPullRequests' smart constructor.
data ListPullRequests = ListPullRequests'
  { repositoryName :: Types.RepositoryName
    -- ^ The name of the repository for which you want to list pull requests.
  , authorArn :: Core.Maybe Types.Arn
    -- ^ Optional. The Amazon Resource Name (ARN) of the user who created the pull request. If used, this filters the results to pull requests created by that user.
  , maxResults :: Core.Maybe Core.Int
    -- ^ A non-zero, non-negative integer used to limit the number of returned results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that, when provided in a request, returns the next batch of the results.
  , pullRequestStatus :: Core.Maybe Types.PullRequestStatusEnum
    -- ^ Optional. The status of the pull request. If used, this refines the results to the pull requests that match the specified status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPullRequests' value with any optional fields omitted.
mkListPullRequests
    :: Types.RepositoryName -- ^ 'repositoryName'
    -> ListPullRequests
mkListPullRequests repositoryName
  = ListPullRequests'{repositoryName, authorArn = Core.Nothing,
                      maxResults = Core.Nothing, nextToken = Core.Nothing,
                      pullRequestStatus = Core.Nothing}

-- | The name of the repository for which you want to list pull requests.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprRepositoryName :: Lens.Lens' ListPullRequests Types.RepositoryName
lprRepositoryName = Lens.field @"repositoryName"
{-# INLINEABLE lprRepositoryName #-}
{-# DEPRECATED repositoryName "Use generic-lens or generic-optics with 'repositoryName' instead"  #-}

-- | Optional. The Amazon Resource Name (ARN) of the user who created the pull request. If used, this filters the results to pull requests created by that user.
--
-- /Note:/ Consider using 'authorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprAuthorArn :: Lens.Lens' ListPullRequests (Core.Maybe Types.Arn)
lprAuthorArn = Lens.field @"authorArn"
{-# INLINEABLE lprAuthorArn #-}
{-# DEPRECATED authorArn "Use generic-lens or generic-optics with 'authorArn' instead"  #-}

-- | A non-zero, non-negative integer used to limit the number of returned results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprMaxResults :: Lens.Lens' ListPullRequests (Core.Maybe Core.Int)
lprMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lprMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprNextToken :: Lens.Lens' ListPullRequests (Core.Maybe Types.NextToken)
lprNextToken = Lens.field @"nextToken"
{-# INLINEABLE lprNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Optional. The status of the pull request. If used, this refines the results to the pull requests that match the specified status.
--
-- /Note:/ Consider using 'pullRequestStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprPullRequestStatus :: Lens.Lens' ListPullRequests (Core.Maybe Types.PullRequestStatusEnum)
lprPullRequestStatus = Lens.field @"pullRequestStatus"
{-# INLINEABLE lprPullRequestStatus #-}
{-# DEPRECATED pullRequestStatus "Use generic-lens or generic-optics with 'pullRequestStatus' instead"  #-}

instance Core.ToQuery ListPullRequests where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListPullRequests where
        toHeaders ListPullRequests{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.ListPullRequests")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListPullRequests where
        toJSON ListPullRequests{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("repositoryName" Core..= repositoryName),
                  ("authorArn" Core..=) Core.<$> authorArn,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("pullRequestStatus" Core..=) Core.<$> pullRequestStatus])

instance Core.AWSRequest ListPullRequests where
        type Rs ListPullRequests = ListPullRequestsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPullRequestsResponse' Core.<$>
                   (x Core..:? "pullRequestIds" Core..!= Core.mempty) Core.<*>
                     x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPullRequests where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"pullRequestIds") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListPullRequestsResponse' smart constructor.
data ListPullRequestsResponse = ListPullRequestsResponse'
  { pullRequestIds :: [Types.PullRequestId]
    -- ^ The system-generated IDs of the pull requests.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ An enumeration token that allows the operation to batch the next results of the operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPullRequestsResponse' value with any optional fields omitted.
mkListPullRequestsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPullRequestsResponse
mkListPullRequestsResponse responseStatus
  = ListPullRequestsResponse'{pullRequestIds = Core.mempty,
                              nextToken = Core.Nothing, responseStatus}

-- | The system-generated IDs of the pull requests.
--
-- /Note:/ Consider using 'pullRequestIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrrsPullRequestIds :: Lens.Lens' ListPullRequestsResponse [Types.PullRequestId]
lprrrsPullRequestIds = Lens.field @"pullRequestIds"
{-# INLINEABLE lprrrsPullRequestIds #-}
{-# DEPRECATED pullRequestIds "Use generic-lens or generic-optics with 'pullRequestIds' instead"  #-}

-- | An enumeration token that allows the operation to batch the next results of the operation.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrrsNextToken :: Lens.Lens' ListPullRequestsResponse (Core.Maybe Types.NextToken)
lprrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lprrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrrsResponseStatus :: Lens.Lens' ListPullRequestsResponse Core.Int
lprrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lprrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

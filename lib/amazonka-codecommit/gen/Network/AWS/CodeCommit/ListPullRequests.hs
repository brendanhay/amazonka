{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListPullRequests (..),
    mkListPullRequests,

    -- ** Request lenses
    lprRepositoryName,
    lprAuthorArn,
    lprMaxResults,
    lprNextToken,
    lprPullRequestStatus,

    -- * Destructuring the response
    ListPullRequestsResponse (..),
    mkListPullRequestsResponse,

    -- ** Response lenses
    lprrrsPullRequestIds,
    lprrrsNextToken,
    lprrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPullRequests' smart constructor.
data ListPullRequests = ListPullRequests'
  { -- | The name of the repository for which you want to list pull requests.
    repositoryName :: Types.RepositoryName,
    -- | Optional. The Amazon Resource Name (ARN) of the user who created the pull request. If used, this filters the results to pull requests created by that user.
    authorArn :: Core.Maybe Types.Arn,
    -- | A non-zero, non-negative integer used to limit the number of returned results.
    maxResults :: Core.Maybe Core.Int,
    -- | An enumeration token that, when provided in a request, returns the next batch of the results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Optional. The status of the pull request. If used, this refines the results to the pull requests that match the specified status.
    pullRequestStatus :: Core.Maybe Types.PullRequestStatusEnum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPullRequests' value with any optional fields omitted.
mkListPullRequests ::
  -- | 'repositoryName'
  Types.RepositoryName ->
  ListPullRequests
mkListPullRequests repositoryName =
  ListPullRequests'
    { repositoryName,
      authorArn = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      pullRequestStatus = Core.Nothing
    }

-- | The name of the repository for which you want to list pull requests.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprRepositoryName :: Lens.Lens' ListPullRequests Types.RepositoryName
lprRepositoryName = Lens.field @"repositoryName"
{-# DEPRECATED lprRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | Optional. The Amazon Resource Name (ARN) of the user who created the pull request. If used, this filters the results to pull requests created by that user.
--
-- /Note:/ Consider using 'authorArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprAuthorArn :: Lens.Lens' ListPullRequests (Core.Maybe Types.Arn)
lprAuthorArn = Lens.field @"authorArn"
{-# DEPRECATED lprAuthorArn "Use generic-lens or generic-optics with 'authorArn' instead." #-}

-- | A non-zero, non-negative integer used to limit the number of returned results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprMaxResults :: Lens.Lens' ListPullRequests (Core.Maybe Core.Int)
lprMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lprMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | An enumeration token that, when provided in a request, returns the next batch of the results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprNextToken :: Lens.Lens' ListPullRequests (Core.Maybe Types.NextToken)
lprNextToken = Lens.field @"nextToken"
{-# DEPRECATED lprNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optional. The status of the pull request. If used, this refines the results to the pull requests that match the specified status.
--
-- /Note:/ Consider using 'pullRequestStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprPullRequestStatus :: Lens.Lens' ListPullRequests (Core.Maybe Types.PullRequestStatusEnum)
lprPullRequestStatus = Lens.field @"pullRequestStatus"
{-# DEPRECATED lprPullRequestStatus "Use generic-lens or generic-optics with 'pullRequestStatus' instead." #-}

instance Core.FromJSON ListPullRequests where
  toJSON ListPullRequests {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("repositoryName" Core..= repositoryName),
            ("authorArn" Core..=) Core.<$> authorArn,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("pullRequestStatus" Core..=) Core.<$> pullRequestStatus
          ]
      )

instance Core.AWSRequest ListPullRequests where
  type Rs ListPullRequests = ListPullRequestsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.ListPullRequests")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPullRequestsResponse'
            Core.<$> (x Core..:? "pullRequestIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListPullRequests where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"pullRequestIds") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListPullRequestsResponse' smart constructor.
data ListPullRequestsResponse = ListPullRequestsResponse'
  { -- | The system-generated IDs of the pull requests.
    pullRequestIds :: [Types.PullRequestId],
    -- | An enumeration token that allows the operation to batch the next results of the operation.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPullRequestsResponse' value with any optional fields omitted.
mkListPullRequestsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPullRequestsResponse
mkListPullRequestsResponse responseStatus =
  ListPullRequestsResponse'
    { pullRequestIds = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The system-generated IDs of the pull requests.
--
-- /Note:/ Consider using 'pullRequestIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrrsPullRequestIds :: Lens.Lens' ListPullRequestsResponse [Types.PullRequestId]
lprrrsPullRequestIds = Lens.field @"pullRequestIds"
{-# DEPRECATED lprrrsPullRequestIds "Use generic-lens or generic-optics with 'pullRequestIds' instead." #-}

-- | An enumeration token that allows the operation to batch the next results of the operation.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrrsNextToken :: Lens.Lens' ListPullRequestsResponse (Core.Maybe Types.NextToken)
lprrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lprrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrrsResponseStatus :: Lens.Lens' ListPullRequestsResponse Core.Int
lprrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lprrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

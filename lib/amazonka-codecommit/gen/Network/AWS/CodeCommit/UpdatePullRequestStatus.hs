{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdatePullRequestStatus (..),
    mkUpdatePullRequestStatus,

    -- ** Request lenses
    uprsPullRequestId,
    uprsPullRequestStatus,

    -- * Destructuring the response
    UpdatePullRequestStatusResponse (..),
    mkUpdatePullRequestStatusResponse,

    -- ** Response lenses
    uprsrrsPullRequest,
    uprsrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdatePullRequestStatus' smart constructor.
data UpdatePullRequestStatus = UpdatePullRequestStatus'
  { -- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
    pullRequestId :: Types.PullRequestId,
    -- | The status of the pull request. The only valid operations are to update the status from @OPEN@ to @OPEN@ , @OPEN@ to @CLOSED@ or from @CLOSED@ to @CLOSED@ .
    pullRequestStatus :: Types.PullRequestStatusEnum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePullRequestStatus' value with any optional fields omitted.
mkUpdatePullRequestStatus ::
  -- | 'pullRequestId'
  Types.PullRequestId ->
  -- | 'pullRequestStatus'
  Types.PullRequestStatusEnum ->
  UpdatePullRequestStatus
mkUpdatePullRequestStatus pullRequestId pullRequestStatus =
  UpdatePullRequestStatus' {pullRequestId, pullRequestStatus}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsPullRequestId :: Lens.Lens' UpdatePullRequestStatus Types.PullRequestId
uprsPullRequestId = Lens.field @"pullRequestId"
{-# DEPRECATED uprsPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The status of the pull request. The only valid operations are to update the status from @OPEN@ to @OPEN@ , @OPEN@ to @CLOSED@ or from @CLOSED@ to @CLOSED@ .
--
-- /Note:/ Consider using 'pullRequestStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsPullRequestStatus :: Lens.Lens' UpdatePullRequestStatus Types.PullRequestStatusEnum
uprsPullRequestStatus = Lens.field @"pullRequestStatus"
{-# DEPRECATED uprsPullRequestStatus "Use generic-lens or generic-optics with 'pullRequestStatus' instead." #-}

instance Core.FromJSON UpdatePullRequestStatus where
  toJSON UpdatePullRequestStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just ("pullRequestStatus" Core..= pullRequestStatus)
          ]
      )

instance Core.AWSRequest UpdatePullRequestStatus where
  type Rs UpdatePullRequestStatus = UpdatePullRequestStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.UpdatePullRequestStatus")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePullRequestStatusResponse'
            Core.<$> (x Core..: "pullRequest") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdatePullRequestStatusResponse' smart constructor.
data UpdatePullRequestStatusResponse = UpdatePullRequestStatusResponse'
  { -- | Information about the pull request.
    pullRequest :: Types.PullRequest,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdatePullRequestStatusResponse' value with any optional fields omitted.
mkUpdatePullRequestStatusResponse ::
  -- | 'pullRequest'
  Types.PullRequest ->
  -- | 'responseStatus'
  Core.Int ->
  UpdatePullRequestStatusResponse
mkUpdatePullRequestStatusResponse pullRequest responseStatus =
  UpdatePullRequestStatusResponse' {pullRequest, responseStatus}

-- | Information about the pull request.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsrrsPullRequest :: Lens.Lens' UpdatePullRequestStatusResponse Types.PullRequest
uprsrrsPullRequest = Lens.field @"pullRequest"
{-# DEPRECATED uprsrrsPullRequest "Use generic-lens or generic-optics with 'pullRequest' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsrrsResponseStatus :: Lens.Lens' UpdatePullRequestStatusResponse Core.Int
uprsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uprsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

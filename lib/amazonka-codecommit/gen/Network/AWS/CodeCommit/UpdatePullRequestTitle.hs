{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestTitle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the title of a pull request.
module Network.AWS.CodeCommit.UpdatePullRequestTitle
  ( -- * Creating a request
    UpdatePullRequestTitle (..),
    mkUpdatePullRequestTitle,

    -- ** Request lenses
    uprtPullRequestId,
    uprtTitle,

    -- * Destructuring the response
    UpdatePullRequestTitleResponse (..),
    mkUpdatePullRequestTitleResponse,

    -- ** Response lenses
    uprtrrsPullRequest,
    uprtrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdatePullRequestTitle' smart constructor.
data UpdatePullRequestTitle = UpdatePullRequestTitle'
  { -- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
    pullRequestId :: Types.PullRequestId,
    -- | The updated title of the pull request. This replaces the existing title.
    title :: Types.Title
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePullRequestTitle' value with any optional fields omitted.
mkUpdatePullRequestTitle ::
  -- | 'pullRequestId'
  Types.PullRequestId ->
  -- | 'title'
  Types.Title ->
  UpdatePullRequestTitle
mkUpdatePullRequestTitle pullRequestId title =
  UpdatePullRequestTitle' {pullRequestId, title}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprtPullRequestId :: Lens.Lens' UpdatePullRequestTitle Types.PullRequestId
uprtPullRequestId = Lens.field @"pullRequestId"
{-# DEPRECATED uprtPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The updated title of the pull request. This replaces the existing title.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprtTitle :: Lens.Lens' UpdatePullRequestTitle Types.Title
uprtTitle = Lens.field @"title"
{-# DEPRECATED uprtTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Core.FromJSON UpdatePullRequestTitle where
  toJSON UpdatePullRequestTitle {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just ("title" Core..= title)
          ]
      )

instance Core.AWSRequest UpdatePullRequestTitle where
  type Rs UpdatePullRequestTitle = UpdatePullRequestTitleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeCommit_20150413.UpdatePullRequestTitle")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePullRequestTitleResponse'
            Core.<$> (x Core..: "pullRequest") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdatePullRequestTitleResponse' smart constructor.
data UpdatePullRequestTitleResponse = UpdatePullRequestTitleResponse'
  { -- | Information about the updated pull request.
    pullRequest :: Types.PullRequest,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdatePullRequestTitleResponse' value with any optional fields omitted.
mkUpdatePullRequestTitleResponse ::
  -- | 'pullRequest'
  Types.PullRequest ->
  -- | 'responseStatus'
  Core.Int ->
  UpdatePullRequestTitleResponse
mkUpdatePullRequestTitleResponse pullRequest responseStatus =
  UpdatePullRequestTitleResponse' {pullRequest, responseStatus}

-- | Information about the updated pull request.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprtrrsPullRequest :: Lens.Lens' UpdatePullRequestTitleResponse Types.PullRequest
uprtrrsPullRequest = Lens.field @"pullRequest"
{-# DEPRECATED uprtrrsPullRequest "Use generic-lens or generic-optics with 'pullRequest' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprtrrsResponseStatus :: Lens.Lens' UpdatePullRequestTitleResponse Core.Int
uprtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uprtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

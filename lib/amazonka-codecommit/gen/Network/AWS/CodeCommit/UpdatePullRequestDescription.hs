{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdatePullRequestDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the contents of the description of a pull request.
module Network.AWS.CodeCommit.UpdatePullRequestDescription
  ( -- * Creating a request
    UpdatePullRequestDescription (..),
    mkUpdatePullRequestDescription,

    -- ** Request lenses
    uprdPullRequestId,
    uprdDescription,

    -- * Destructuring the response
    UpdatePullRequestDescriptionResponse (..),
    mkUpdatePullRequestDescriptionResponse,

    -- ** Response lenses
    uprdrrsPullRequest,
    uprdrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdatePullRequestDescription' smart constructor.
data UpdatePullRequestDescription = UpdatePullRequestDescription'
  { -- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
    pullRequestId :: Types.PullRequestId,
    -- | The updated content of the description for the pull request. This content replaces the existing description.
    description :: Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePullRequestDescription' value with any optional fields omitted.
mkUpdatePullRequestDescription ::
  -- | 'pullRequestId'
  Types.PullRequestId ->
  -- | 'description'
  Types.Description ->
  UpdatePullRequestDescription
mkUpdatePullRequestDescription pullRequestId description =
  UpdatePullRequestDescription' {pullRequestId, description}

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- /Note:/ Consider using 'pullRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprdPullRequestId :: Lens.Lens' UpdatePullRequestDescription Types.PullRequestId
uprdPullRequestId = Lens.field @"pullRequestId"
{-# DEPRECATED uprdPullRequestId "Use generic-lens or generic-optics with 'pullRequestId' instead." #-}

-- | The updated content of the description for the pull request. This content replaces the existing description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprdDescription :: Lens.Lens' UpdatePullRequestDescription Types.Description
uprdDescription = Lens.field @"description"
{-# DEPRECATED uprdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON UpdatePullRequestDescription where
  toJSON UpdatePullRequestDescription {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pullRequestId" Core..= pullRequestId),
            Core.Just ("description" Core..= description)
          ]
      )

instance Core.AWSRequest UpdatePullRequestDescription where
  type
    Rs UpdatePullRequestDescription =
      UpdatePullRequestDescriptionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "CodeCommit_20150413.UpdatePullRequestDescription"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePullRequestDescriptionResponse'
            Core.<$> (x Core..: "pullRequest") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdatePullRequestDescriptionResponse' smart constructor.
data UpdatePullRequestDescriptionResponse = UpdatePullRequestDescriptionResponse'
  { -- | Information about the updated pull request.
    pullRequest :: Types.PullRequest,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdatePullRequestDescriptionResponse' value with any optional fields omitted.
mkUpdatePullRequestDescriptionResponse ::
  -- | 'pullRequest'
  Types.PullRequest ->
  -- | 'responseStatus'
  Core.Int ->
  UpdatePullRequestDescriptionResponse
mkUpdatePullRequestDescriptionResponse pullRequest responseStatus =
  UpdatePullRequestDescriptionResponse'
    { pullRequest,
      responseStatus
    }

-- | Information about the updated pull request.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprdrrsPullRequest :: Lens.Lens' UpdatePullRequestDescriptionResponse Types.PullRequest
uprdrrsPullRequest = Lens.field @"pullRequest"
{-# DEPRECATED uprdrrsPullRequest "Use generic-lens or generic-optics with 'pullRequest' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprdrrsResponseStatus :: Lens.Lens' UpdatePullRequestDescriptionResponse Core.Int
uprdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uprdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

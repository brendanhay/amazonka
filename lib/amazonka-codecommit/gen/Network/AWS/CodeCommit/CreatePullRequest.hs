{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.CreatePullRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pull request in the specified repository.
module Network.AWS.CodeCommit.CreatePullRequest
  ( -- * Creating a request
    CreatePullRequest (..),
    mkCreatePullRequest,

    -- ** Request lenses
    cprTitle,
    cprTargets,
    cprClientRequestToken,
    cprDescription,

    -- * Destructuring the response
    CreatePullRequestResponse (..),
    mkCreatePullRequestResponse,

    -- ** Response lenses
    cprrrsPullRequest,
    cprrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePullRequest' smart constructor.
data CreatePullRequest = CreatePullRequest'
  { -- | The title of the pull request. This title is used to identify the pull request to other users in the repository.
    title :: Types.Title,
    -- | The targets for the pull request, including the source of the code to be reviewed (the source branch) and the destination where the creator of the pull request intends the code to be merged after the pull request is closed (the destination branch).
    targets :: [Types.Target],
    -- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
    clientRequestToken :: Core.Maybe Types.ClientRequestToken,
    -- | A description of the pull request.
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePullRequest' value with any optional fields omitted.
mkCreatePullRequest ::
  -- | 'title'
  Types.Title ->
  CreatePullRequest
mkCreatePullRequest title =
  CreatePullRequest'
    { title,
      targets = Core.mempty,
      clientRequestToken = Core.Nothing,
      description = Core.Nothing
    }

-- | The title of the pull request. This title is used to identify the pull request to other users in the repository.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprTitle :: Lens.Lens' CreatePullRequest Types.Title
cprTitle = Lens.field @"title"
{-# DEPRECATED cprTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The targets for the pull request, including the source of the code to be reviewed (the source branch) and the destination where the creator of the pull request intends the code to be merged after the pull request is closed (the destination branch).
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprTargets :: Lens.Lens' CreatePullRequest [Types.Target]
cprTargets = Lens.field @"targets"
{-# DEPRECATED cprTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprClientRequestToken :: Lens.Lens' CreatePullRequest (Core.Maybe Types.ClientRequestToken)
cprClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED cprClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | A description of the pull request.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprDescription :: Lens.Lens' CreatePullRequest (Core.Maybe Types.Description)
cprDescription = Lens.field @"description"
{-# DEPRECATED cprDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON CreatePullRequest where
  toJSON CreatePullRequest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("title" Core..= title),
            Core.Just ("targets" Core..= targets),
            ("clientRequestToken" Core..=) Core.<$> clientRequestToken,
            ("description" Core..=) Core.<$> description
          ]
      )

instance Core.AWSRequest CreatePullRequest where
  type Rs CreatePullRequest = CreatePullRequestResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.CreatePullRequest")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePullRequestResponse'
            Core.<$> (x Core..: "pullRequest") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePullRequestResponse' smart constructor.
data CreatePullRequestResponse = CreatePullRequestResponse'
  { -- | Information about the newly created pull request.
    pullRequest :: Types.PullRequest,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreatePullRequestResponse' value with any optional fields omitted.
mkCreatePullRequestResponse ::
  -- | 'pullRequest'
  Types.PullRequest ->
  -- | 'responseStatus'
  Core.Int ->
  CreatePullRequestResponse
mkCreatePullRequestResponse pullRequest responseStatus =
  CreatePullRequestResponse' {pullRequest, responseStatus}

-- | Information about the newly created pull request.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrrsPullRequest :: Lens.Lens' CreatePullRequestResponse Types.PullRequest
cprrrsPullRequest = Lens.field @"pullRequest"
{-# DEPRECATED cprrrsPullRequest "Use generic-lens or generic-optics with 'pullRequest' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrrsResponseStatus :: Lens.Lens' CreatePullRequestResponse Core.Int
cprrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

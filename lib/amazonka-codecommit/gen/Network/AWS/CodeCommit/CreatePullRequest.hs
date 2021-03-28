{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreatePullRequest (..)
    , mkCreatePullRequest
    -- ** Request lenses
    , cprTitle
    , cprTargets
    , cprClientRequestToken
    , cprDescription

    -- * Destructuring the response
    , CreatePullRequestResponse (..)
    , mkCreatePullRequestResponse
    -- ** Response lenses
    , cprrrsPullRequest
    , cprrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreatePullRequest' smart constructor.
data CreatePullRequest = CreatePullRequest'
  { title :: Types.Title
    -- ^ The title of the pull request. This title is used to identify the pull request to other users in the repository.
  , targets :: [Types.Target]
    -- ^ The targets for the pull request, including the source of the code to be reviewed (the source branch) and the destination where the creator of the pull request intends the code to be merged after the pull request is closed (the destination branch).
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the pull request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePullRequest' value with any optional fields omitted.
mkCreatePullRequest
    :: Types.Title -- ^ 'title'
    -> CreatePullRequest
mkCreatePullRequest title
  = CreatePullRequest'{title, targets = Core.mempty,
                       clientRequestToken = Core.Nothing, description = Core.Nothing}

-- | The title of the pull request. This title is used to identify the pull request to other users in the repository.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprTitle :: Lens.Lens' CreatePullRequest Types.Title
cprTitle = Lens.field @"title"
{-# INLINEABLE cprTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

-- | The targets for the pull request, including the source of the code to be reviewed (the source branch) and the destination where the creator of the pull request intends the code to be merged after the pull request is closed (the destination branch).
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprTargets :: Lens.Lens' CreatePullRequest [Types.Target]
cprTargets = Lens.field @"targets"
{-# INLINEABLE cprTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

-- | A unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprClientRequestToken :: Lens.Lens' CreatePullRequest (Core.Maybe Types.ClientRequestToken)
cprClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE cprClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

-- | A description of the pull request.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprDescription :: Lens.Lens' CreatePullRequest (Core.Maybe Types.Description)
cprDescription = Lens.field @"description"
{-# INLINEABLE cprDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery CreatePullRequest where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreatePullRequest where
        toHeaders CreatePullRequest{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.CreatePullRequest")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreatePullRequest where
        toJSON CreatePullRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("title" Core..= title),
                  Core.Just ("targets" Core..= targets),
                  ("clientRequestToken" Core..=) Core.<$> clientRequestToken,
                  ("description" Core..=) Core.<$> description])

instance Core.AWSRequest CreatePullRequest where
        type Rs CreatePullRequest = CreatePullRequestResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreatePullRequestResponse' Core.<$>
                   (x Core..: "pullRequest") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreatePullRequestResponse' smart constructor.
data CreatePullRequestResponse = CreatePullRequestResponse'
  { pullRequest :: Types.PullRequest
    -- ^ Information about the newly created pull request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreatePullRequestResponse' value with any optional fields omitted.
mkCreatePullRequestResponse
    :: Types.PullRequest -- ^ 'pullRequest'
    -> Core.Int -- ^ 'responseStatus'
    -> CreatePullRequestResponse
mkCreatePullRequestResponse pullRequest responseStatus
  = CreatePullRequestResponse'{pullRequest, responseStatus}

-- | Information about the newly created pull request.
--
-- /Note:/ Consider using 'pullRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrrsPullRequest :: Lens.Lens' CreatePullRequestResponse Types.PullRequest
cprrrsPullRequest = Lens.field @"pullRequest"
{-# INLINEABLE cprrrsPullRequest #-}
{-# DEPRECATED pullRequest "Use generic-lens or generic-optics with 'pullRequest' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrrsResponseStatus :: Lens.Lens' CreatePullRequestResponse Core.Int
cprrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cprrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
